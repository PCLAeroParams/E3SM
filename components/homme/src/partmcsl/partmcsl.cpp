#include "partmcsl.hpp"
#include <iostream>
#include <sstream>
#include <typeinfo>

namespace partmcsl {

static partmcsl::SlSourcePartition::Ptr src_partition;

void ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx) {
    slmm_assert( (subcell_idx >= 0 and subcell_idx < 4) );
    slmm_assert( (vert_idx >= 0 and vert_idx < 4 ) );

    const bool west = (vert_idx == 0 or vert_idx == 3);
    const bool south = (vert_idx < 2);
    switch (subcell_idx) {
      case 0: {
          a = ( west  ? -1.0 : 0.0);
          b = ( south ? -1.0 : 0.0);
        }
        break;
      case 1: {
          a = ( west  ?  0.0 : 1.0 );
          b = ( south ? -1.0 : 0.0 );
        }
        break;
      case 2: {
          a = ( west  ? 0.0 : 1.0 );
          b = ( south ? 0.0 : 1.0 );
        }
        break;
      case 3: {
          a = ( west  ? -1.0 : 0.0 );
          b = ( south ?  0.0 : 1.0 );
        }
        break;
    }
  } // ref_coords_ab

void src_partition_init(const Int nelem) {
  std::ostringstream ss;
  if (!src_partition) {
    src_partition = std::make_shared<partmcsl::SlSourcePartition>(nelem);
//     ss << "partmcsl.cpp src_partition_init : initializing with nelem = " << nelem << ".\n";
  }
  else {
    ss << "partmcsl.cpp : ERROR src_partition is already initialized.\n";
  }
  std::cout << ss.str();
}

void calc_source_partition(const Int ie, const Int nelemd, const Int n_elem_neighbors,
  const Int elem_self_idx, const Int lev_idx, const Int nlev,
  const homme::Cartesian3D* points_p,
  const Real* area_p,
  const homme::Cartesian3D* adv_points_p,
  Int* ndest_p,
  Int* dest_idx_p,
  Real* frac_p) {
    using siqk::slice;

    slmm_assert(src_partition);
    slmm_throw_if(!src_partition, "src_partition not allocated.");

    std::stringstream ss;

    // input views
    homme::FA5<const homme::Real> points(reinterpret_cast<const homme::Real*>(points_p),
      ndim, nverts, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
    homme::FA3<const homme::Real> area(area_p, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
    homme::FA3<const homme::Real> adv_points(reinterpret_cast<const homme::Real*>(adv_points_p), ndim, nverts,
      n_subcells_per_elem);
    // output views
    homme::FA3<homme::Int> ndest(ndest_p, nlev, n_subcells_per_elem, nelemd);
    homme::FA4<homme::Int> dest_idx(dest_idx_p, nlev, max_ndest_cell, n_subcells_per_elem, nelemd);
    homme::FA4<homme::Real> dest_frac(frac_p, nlev, max_ndest_cell, n_subcells_per_elem, nelemd);

    ss << "partmcsl::calc_source_partition received ie " << ie << " nelemd " << nelemd
       << " n_elem_neighbors " << n_elem_neighbors << " elem_self_idx " << elem_self_idx
       << " level " << lev_idx << " of " << nlev << "\n";
    std::cout << ss.str();

    src_partition->init_local_mesh_if_needed(ie, n_elem_neighbors, elem_self_idx, points, area);

    // reset for new time step
    for (int subcell_idx=0; subcell_idx<n_subcells_per_elem; ++subcell_idx) {
      ndest(lev_idx, subcell_idx, ie) = 0;
      for (int i=0; i<max_ndest_cell; ++i) {
        dest_idx(i, lev_idx, subcell_idx, ie) = -1;
        dest_frac(i,lev_idx, subcell_idx, ie) = 0.0;
      }
    }

    const auto& mesh = src_partition->mesh(ie);
    const auto& mesh_area = src_partition->area(ie);

    const Int ncells_in_mesh = mesh.e.extent(0);
    ss.str("");
    ss << "partmcsl::calc_source_partition output initialized to null for new time step; ncells = " << ncells_in_mesh << ".\n";
    std::cout << ss.str();

    // workspace buffers will be wrapped in unmanaged views for easier indexing shortly
    Real vi_buf[3 * nverts];
    Real vo_buf[3 * max_num_intersections];
    Real wrk_buf[4 * max_num_intersections];



    slmm_assert(ncells_in_mesh == n_elem_neighbors * n_subcells_per_elem);
    slmm_throw_if( ncells_in_mesh != n_elem_neighbors * n_subcells_per_elem, "unexpected number of cells in mesh.");

    const Int start_cell_idx = src_partition->elem_self_idx(ie) * n_subcells_per_elem;
    for (int adv_cell_idx = 0; adv_cell_idx<n_subcells_per_elem; ++adv_cell_idx) {
      // loop over advected subcells of elem(ie)

      const Real src_area = mesh_area(start_cell_idx + adv_cell_idx);
      ss.str("");
      ss << "elem " << ie << " subcell " << adv_cell_idx << " starts has mesh cell idx " << start_cell_idx + adv_cell_idx << " of " << ncells_in_mesh << " area = " << src_area << "\n";
      std::cout << ss.str();

      for (int cell_idx=0; cell_idx<ncells_in_mesh; ++cell_idx) {
        // loop over static cells in mesh
        //
        // compute intersection (if any) with advected cells
        //
        const siqk::RawVec3s verts_in(vi_buf, nverts, 3);
        siqk::RawVec3s verts_out(vo_buf, max_num_intersections, 3);
        Int n_overlap_verts = 0;
        { // clip_against_poly scope

          siqk::RawVec3s wrk(wrk_buf, max_num_intersections, 3);
          // TODO: can this input buffer copy move to the outer loop?
          //   it is inside this local scope, likely for good reason,
          //   but it only depends on aci, not ci.
          //   need to check if clip_against_poly modifies its content
          for (int vi=0; vi<nverts; ++vi) {
            siqk::SphereGeometry::copy(slice(verts_in, vi),
              Kokkos::subview(adv_points, Kokkos::ALL, vi, adv_cell_idx));
          }

          siqk::sh::clip_against_poly<siqk::SphereGeometry>(mesh, cell_idx, verts_in, nverts,
           verts_out, n_overlap_verts, wrk);
        } // clip_against_poly scope
        slmm_assert(n_overlap_verts <= max_num_intersections);

        if (n_overlap_verts > 0) {
          // advected subcell aci has an intersection with static cell ci
          // compute area of overlap region
          // TODO: Replace the barycenter area computation with a simple triangulation
          Real bc[3];
          barycenter(bc, verts_out, n_overlap_verts);
          Real ov_area = 0.0;
          for (int i=0; i<n_overlap_verts; ++i) {
            ov_area += tri_area(slice(verts_out, i), slice(verts_out, (i+1)%n_overlap_verts), bc);
          }

          const Int dest_insert_idx = ndest(lev_idx, adv_cell_idx, ie)++;
          dest_idx(dest_insert_idx, lev_idx, adv_cell_idx, ie) = cell_idx;
          dest_frac(dest_insert_idx, lev_idx, adv_cell_idx, ie) = ov_area / src_area;
        }
      } // loop over static cells in mesh
      Real total_frac = 0.0;
      for (int j=0; j<ndest(lev_idx, adv_cell_idx, ie); ++j) {
        total_frac += dest_frac(j, lev_idx, adv_cell_idx, ie);
      }
      slmm_throw_if(std::abs(total_frac - 1.0) > fp_tol, "source total fraction error");
    } // loop over advected subcells of elem(ie)
  }

} // namespace partmcsl


extern "C" void calc_src_partition(
  const homme::Int ie ,
  const homme::Int nelemd,
  const homme::Int n_elem_neighbors,
  const homme::Int self_idx,
  const homme::Int lev_idx,
  const homme::Int nlev,
  const homme::Cartesian3D* points,
  const homme::Real* area,
  const homme::Cartesian3D* adv_points,
  homme::Int* ndest,
  homme::Int* dest,
  homme::Real* frac
) {
  return partmcsl::calc_source_partition(
    ie -1,
    nelemd,
    n_elem_neighbors,
    self_idx -1,
    lev_idx -1,
    nlev,
    points,
    area,
    adv_points,
    ndest,
    dest,
    frac
    );
}

extern "C" void ref_coords_ab(homme::Real* a,
                    homme::Real* b,
                    const homme::Int subcell_idx,
                    const homme::Int vert_idx) {
    return partmcsl::ref_coords_ab(*a, *b, subcell_idx, vert_idx);
}

extern "C" void init_local_meshes(const homme::Int nelemd,
                        const homme::Int* nneighbors_p,
                        const homme::Int* elem_self_idx_p,
                        const homme::Cartesian3D* points_p,
                        const homme::Real* area_p) {

  slmm_assert(partmcsl::src_partition);

  homme::FA1<const homme::Int> nneighbors(nneighbors_p, nelemd);
  homme::FA1<const homme::Int> elem_self_idx(elem_self_idx_p, nelemd);
  homme::FA5<const homme::Real> points(reinterpret_cast<const homme::Real*>(points_p),
      partmcsl::ndim, partmcsl::nverts, partmcsl::n_subcells_per_elem, partmcsl::max_num_elem_neighbors, nelemd);
  homme::FA3<const homme::Real> area(area_p, partmcsl::n_subcells_per_elem, partmcsl::max_num_elem_neighbors, nelemd);

  for (int ie=0; ie<nelemd; ++ie) {
    partmcsl::src_partition->init_local_mesh_if_needed(ie, nneighbors(ie), elem_self_idx(ie),
      points, area);
  }
}

extern "C" void partmcsl_init_local(const homme::Int nelemd) {
  partmcsl::src_partition_init(nelemd);
}

extern "C" void test_int_array(const homme::Int* array, const homme::Int n) {
    partmcsl::test_int_array(homme::FA1<const homme::Int>(array,n),n) ;
    partmcsl::test_int_array(array, n);
}




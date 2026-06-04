#include "partmcsl.hpp"
#include <iostream>
#include <sstream>

namespace partmcsl {

static partmcsl::SlSourcePartition::Ptr src_partition;

void ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx) {
    slmm_assert( (subcell_idx >= 0 and subcell_idx < 4) );
    slmm_assert( (vert_idx >= 0 and vert_idx < 4 ) );

    const bool low_a = (vert_idx == 0 or vert_idx == 3);
    const bool low_b = (vert_idx < 2);
    switch (subcell_idx) {
      case 0: {
          a = ( low_a  ? -1.0 : 0.0);
          b = ( low_b ? -1.0 : 0.0);
        }
        break;
      case 1: {
          a = ( low_a  ?  0.0 : 1.0 );
          b = ( low_b ? -1.0 : 0.0 );
        }
        break;
      case 2: {
          a = ( low_a  ? 0.0 : 1.0 );
          b = ( low_b ? 0.0 : 1.0 );
        }
        break;
      case 3: {
          a = ( low_a  ? -1.0 : 0.0 );
          b = ( low_b ?  0.0 : 1.0 );
        }
        break;
    }
  } // ref_coords_ab

void src_partition_init(const Int nelem) {
  if (!src_partition) {
    src_partition = std::make_shared<partmcsl::SlSourcePartition>(nelem);
  }
  else {
    std::cout << "partmcsl.cpp : ERROR src_partition is already initialized.\n";
  }
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

    constexpr Real area_tol = 1e-6;

    slmm_assert(src_partition);
    slmm_throw_if(!src_partition, "src_partition not allocated.");

    // input views
    homme::FA5<const homme::Real> points(reinterpret_cast<const homme::Real*>(points_p),
      ndim, nverts, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
    homme::FA3<const homme::Real> area(area_p, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
    homme::FA3<const homme::Real> adv_points(reinterpret_cast<const homme::Real*>(adv_points_p), ndim, nverts,
      n_subcells_per_elem);
    // output views.  FA4 dim order must match the Fortran allocation
    // (max_ndest, nlev, n_subcells, nelemd) since FA{N} is LayoutLeft and
    // dest_cell_idxs/dest_portions are allocated that way in
    // partmcsl_advection.F90.
    homme::FA3<homme::Int> ndest(ndest_p, nlev, n_subcells_per_elem, nelemd);
    homme::FA4<homme::Int> dest_idx(dest_idx_p, max_ndest_cell, nlev, n_subcells_per_elem, nelemd);
    homme::FA4<homme::Real> dest_frac(frac_p, max_ndest_cell, nlev, n_subcells_per_elem, nelemd);

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

    Real vi_buf[3 * nverts];
    Real vo_buf[3 * max_num_intersections];
    Real wrk_buf[4 * max_num_intersections];

    slmm_assert(ncells_in_mesh == n_elem_neighbors * n_subcells_per_elem);
    slmm_throw_if( ncells_in_mesh != n_elem_neighbors * n_subcells_per_elem, "unexpected number of cells in mesh.");

    const Int start_cell_idx = (src_partition->elem_self_idx(ie) - 1) * n_subcells_per_elem;
    slmm_assert( (start_cell_idx >=0 and start_cell_idx <= mesh_area.extent(0) - n_subcells_per_elem ) );
    for (int adv_cell_idx = 0; adv_cell_idx<n_subcells_per_elem; ++adv_cell_idx) {
      // loop over advected subcells of elem(ie)

      const Real src_area = tri_area(Kokkos::subview(adv_points, Kokkos::ALL, 0, adv_cell_idx),
                                     Kokkos::subview(adv_points, Kokkos::ALL, 1, adv_cell_idx),
                                     Kokkos::subview(adv_points, Kokkos::ALL, 2, adv_cell_idx)) +
                            tri_area(Kokkos::subview(adv_points, Kokkos::ALL, 0, adv_cell_idx),
                                     Kokkos::subview(adv_points, Kokkos::ALL, 2, adv_cell_idx),
                                     Kokkos::subview(adv_points, Kokkos::ALL, 3, adv_cell_idx));

      slmm_assert(src_area > 0);
      int n_elem_overlap = 0;
      for (int cell_idx=0; cell_idx<ncells_in_mesh; ++cell_idx) {
        // loop over static cells in mesh
        //
        // compute intersection (if any) with advected cells
        //
        siqk::RawVec3s verts_in(vi_buf, nverts, 3);
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
        n_elem_overlap += n_overlap_verts;
        if (n_overlap_verts > 0) {
          // advected subcell aci has an intersection with static cell ci;
          // compute area of overlap region by triangulating from vertex 0.
          Real ov_area = 0.0;
          for (int i=0; i<n_overlap_verts-2; ++i) {
            ov_area += tri_area(slice(verts_out, 0), slice(verts_out, i+1), slice(verts_out, i+2));
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

      // sum of all overlap subregions' fractions must equal 1
      if (std::abs(total_frac - 1.0) > area_tol) {
        std::ostringstream ss;
        ss << "partmcsl calc_source_partition error: total frac = " << total_frac << "\n"
           << "   at ie = " << ie << " of " << nelemd << " src_area = " << src_area
           << " found " << n_elem_overlap << " overlap vertices in this elem\n";
        slmm_throw_if(true, ss.str());
      }

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



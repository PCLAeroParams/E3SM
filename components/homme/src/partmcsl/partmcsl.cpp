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
  }
  else {
    ss << "partmcsl.cpp : ERROR src_partition is already initialized.\n";
  }
  std::cout << ss.str();
}

void calc_partmcsl_source_partition(const int ie,
            const int nelemd,
            const int n_elem_neighbors,
            const int elem_self_idx,
            const int level_idx,
            const int nlev,
            const homme::Cartesian3D* cell_corners_p,
            const Real* cell_area_p,
            const homme::Cartesian3D* adv_points_p,
            Int* ndest_p,
            Int* dest_idx_p,
            Real* frac_p
            )
  {
  using siqk::slice;

  slmm_assert(ie < nelemd);
  slmm_assert(n_elem_neighbors <= max_num_elem_neighbors);

  slmm_assert(src_partition);
  slmm_throw_if(!src_partition, "src_partition pointer not associated.");

  // input ptrs come from fortran
  // convert them to kokkos views
    homme::FA5<const Real> corners(reinterpret_cast<const Real*>(cell_corners_p), ndim, nverts, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
    homme::FA3<const Real> area(cell_area_p, 4, max_num_elem_neighbors, nelemd);
    homme::FA4<const Real> adv_points(reinterpret_cast<const Real*>(adv_points_p), ndim, nverts, n_subcells_per_elem, nelemd);

  // convert output ptrs to kokkos views
  homme::FA3<Int> ndest(ndest_p, nlev, n_subcells_per_elem, nelemd);
  homme::FA4<Int> dest_cells(dest_idx_p, max_ndest_cell, nlev, n_subcells_per_elem, nelemd);
  homme::FA4<Real> dest_fracs(frac_p,    max_ndest_cell, nlev, 4, nelemd);

  // reset output for new computations
  for (int sci=0; sci<n_subcells_per_elem; ++sci) {
    ndest(level_idx, sci, ie) = 0;
    for (int i=0; i<max_ndest_cell; ++i) {
        dest_cells(i, level_idx, sci, ie) = -1;
        dest_fracs(i, level_idx, sci, ie) = 0.0;
    }
  }

  // get local fv cell mesh
  const auto& mesh = src_partition->mesh(ie);
  const auto& mesh_area = src_partition->area(ie);

  // workspace buffers will be wrapped in unmanaged views for easier indexing shortly
  Real vi_buf[3 * nverts];
  Real vo_buf[3 * max_num_intersections];
  Real wrk_buf[4 * max_num_intersections];

  const Int n_cells_in_mesh = n_subcells_per_elem * n_elem_neighbors;
  // index, in local mesh, of first of elem(ie)'s owned subcells
  const Int start_cell_idx = n_subcells_per_elem * elem_self_idx;

  slmm_assert(mesh.p.extent(1) == n_cells_in_mesh);
  slmm_throw_if(mesh.p.extent(1) != n_cells_in_mesh, "mesh points mismatch with n_cells_in_mesh");

  for (int aci=0; aci < n_subcells_per_elem; ++aci) {// loop over advected subcells of elem(ie)

    const Real src_area = mesh_area(start_cell_idx + aci);

    for (Int ci=0; ci < n_cells_in_mesh; ++ci) {// loop over subcells in mesh
      //
      // compute intersection of advected subcell aci with static cell ci
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
            Kokkos::subview(adv_points, Kokkos::ALL, vi, aci, ie));
        }

        siqk::sh::clip_against_poly<siqk::SphereGeometry>(mesh, ci, verts_in, nverts,
           verts_out, n_overlap_verts, wrk);

      } // clip_against_poly scope

      slmm_assert(n_overlap_verts <= max_num_intersections);
      if (n_overlap_verts > 0) {
        // advected subcell aci has an intersection with static cell ci
        // compute area of overlap region
        Real bc[3];
        barycenter(bc, verts_out, n_overlap_verts);
        Real ov_area = 0.0;
        for (int i=0; i<n_overlap_verts; ++i) {
          ov_area += tri_area(slice(verts_out, i), slice(verts_out, (i+1)%n_overlap_verts), bc);
        }

        const Int dest_insert_idx = ndest(level_idx, aci, ie)++;
        dest_cells(dest_insert_idx, level_idx, aci, ie) = ci;
        dest_fracs(dest_insert_idx, level_idx, aci, ie) = ov_area / src_area;

      } // n_overlap_verts > 0
    } // loop over cells in mesh

    Real total_frac = 0.0;
    for (int j=0; j<ndest(level_idx, aci, ie); ++j) {
      total_frac += dest_fracs(j, level_idx, aci, ie);
    }
    slmm_throw_if(std::abs(total_frac - 1.0) > fp_tol, "source total fraction error");

  } // loop over subcells that elem(ie) owns
} // calc_partmcsl_source_partition

void init_local_meshes(const homme::Int nelemd, const homme::Int* nneighbors, const homme::Int* elem_self_idx,
  const homme::Cartesian3D* points, const homme::Real* subcell_area) {

  slmm_assert(src_partition);

  homme::FA1<const homme::Int> nneighbors_view(nneighbors, nelemd);
  homme::FA1<const homme::Int> elem_self_view(elem_self_idx, nelemd);
  homme::FA5<const homme::Real> points_view(reinterpret_cast<const Real*>(points),
    ndim, nverts, n_subcells_per_elem, max_num_elem_neighbors, nelemd);
  homme::FA3<const homme::Real> area_view(subcell_area, n_subcells_per_elem, max_num_elem_neighbors, nelemd);

  for (int ie=0; ie<nelemd; ++ie) {
    auto m = src_partition->mesh(ie);
    auto a = src_partition->area(ie);

    const Int nn = nneighbors_view(ie);
    const Int ncells = n_subcells_per_elem * nn;
    const Int npts = nverts * ncells;

    m.p = R3Array("p", npts, ndim);
    m.e = I2Array("e", ncells, nverts);
    a = R1Array("a", ncells);

    Int pt_idx = 0;
    Int cell_idx = 0;
    for (int nbr_idx = 0; nbr_idx < nn; ++nbr_idx) {
      for (int subcell_idx = 0; subcell_idx < n_subcells_per_elem; ++subcell_idx) {
        for (int vert_idx = 0; vert_idx < nverts; ++vert_idx) {
          for (int j=0; j<ndim; ++j) {
            m.p(pt_idx, j) = points_view(j, vert_idx, subcell_idx, nbr_idx, ie);
          }
          m.e(cell_idx, vert_idx) = pt_idx++;
        }
        const Real area_check = tri_area(Kokkos::subview(m.p, m.e(cell_idx, 0), Kokkos::ALL),
                                        Kokkos::subview(m.p, m.e(cell_idx, 1), Kokkos::ALL),
                                        Kokkos::subview(m.p, m.e(cell_idx, 2), Kokkos::ALL)) +
                                tri_area(Kokkos::subview(m.p, m.e(cell_idx, 0), Kokkos::ALL),
                                        Kokkos::subview(m.p, m.e(cell_idx, 2), Kokkos::ALL),
                                        Kokkos::subview(m.p, m.e(cell_idx, 3), Kokkos::ALL));
        const bool area_pass = ( std::abs(area_check - area_view(subcell_idx, nbr_idx, ie)) < fp_tol) ;

        slmm_assert(area_pass);

        a(cell_idx++) = area_view(subcell_idx, nbr_idx, ie);
      }
    }
    //
    // an updated version of fill_normals shows up in compose_slmm_departure_point.hpp
    // but it has some extra stuff that we don't need.
    // this is an older version that comes from siqk_intersect.hpp.
    //
    siqk::test::fill_normals<siqk::SphereGeometry>(m);


  }

}


} // namespace partmcsl


extern "C" void calc_source_partition(
  homme::Int* ie ,
  homme::Int* nelemd,
  homme::Int* n_elem_neighbors,
  homme::Int* self_idx,
  homme::Int* lev_idx,
  homme::Int* nlev,
  homme::Cartesian3D* corners,
  homme::Real* area,
  homme::Cartesian3D* adv_points,
  homme::Int* ndest,
  homme::Int* dest,
  homme::Real* frac
) {
  return partmcsl::calc_partmcsl_source_partition(*ie -1 ,
    *nelemd,
    *self_idx -1,
    *n_elem_neighbors -1,
    *lev_idx -1,
    *nlev,
    corners,
    area,
    adv_points,
    ndest,
    dest,
    frac);
}

extern "C" void ref_coords_ab(homme::Real* a,
                    homme::Real* b,
                    const homme::Int* subcell_idx,
                    const homme::Int* vert_idx) {
    return partmcsl::ref_coords_ab(*a, *b, *subcell_idx, *vert_idx);
}

extern "C" void init_local_meshes_(const homme::Int* nelemd,
                        const homme::Int* nneighbors,
                        const homme::Int* elem_self_idx,
                        const homme::Cartesian3D* points,
                        const homme::Real* areas) {
  return partmcsl::init_local_meshes(*nelemd, nneighbors, elem_self_idx, points, areas);
}

extern "C" void partmcsl_init_local_(const homme::Int* nelemd) {
  partmcsl::src_partition_init(*nelemd);
}



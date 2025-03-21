#include "partmc_sl.hpp"
#include "compose_homme.hpp" // FA types

namespace partmcsl {

static partmcsl::SlSourcePartition::Ptr src_partition;

void SlSourcePartition::ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx) const {
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
  src_partition = std::make_shared<partmcsl::SlSourcePartition>(nelem);
}

/*
    called from fortran:

      do ie = nets, nete
        do k = 1, nlev
          ... advect cells fwd ...
          *here*
        enddo
      enddo

      "clip_against_poly" : given a polygon, clip the mesh
*/
void calc_partmcsl_source_partition(const Int lev_idx,
  const Int ie, const Int elem_self_idx, const Int nelemd, const Int max_num_neighbors,
  const Real* adv_points_r const Real* cell_points_r, const Int* cells_i,
  const Int* nneighbors_i,
  Int* ndest_i, Int* dest_i, Real* frac_r
  ) {

  using siqk::slice;

  slmm_assert(src_partition);
  slmm_throw_if(!src_partition, "src_partition pointer not associated.");

  // wrap input ptrs in views
  homme::FA1<const Int>  nneighbors(reinterpret_cast<const Int*>(nneighbors_i), nelemd);
  homme::FA3<const Real> cell_points(reinterpret_cast<const Real*>(cell_points_r), 3, 4*max_num_neighbors, nelemd);
  homme::FA3<const Real> adv_points(reinterpret_cast<const Real*>(adv_points_r), 3, 4, 4);
  homme::FA3<const Int>  cells(reinterpret_cast<const Int*>(cells_i), 4, max_num_neighbors, nelemd);

  // wrap output ptrs in views
  homme::FA1<Int> ndest(ndest_i, n_subcells_per_elem);
  homme::FA2<Int> dest_cells(ndest_i, max_ndest_cell, n_subcells_per_elem);
  homme::FA2<Real> dest_fracs(frac_r, max_ndest_cell, n_subcells_per_elem);

  // get local fv cell mesh
  src_partition->init_local_mesh_if_needed(ie,
    Kokkos::subview(cell_points, Kokkos::ALL, std::pair<Int, Int>(0, 4*nneighbors(ie)), ie),
    Kokkos::subview(cells, Kokkos::ALL, std::pair<Int,Int>(0, 4*nneighbors(ie)), ie));
  const auto& m = src_partition->mesh[ie];

  // workspace buffers
  // will be wrapped in unmanaged views for easier indexing shortly
  Real vi_buf[3 * nverts];
  Real vo_buf[3 * max_num_intersections];
  Real wrk_buf[4 * max_num_intersections];

  // reset output
  for (int sci=0; sci < n_subcells_per_elem; ++sci) {
    ndest(sci) = 0;
    for (int i=0; i<max_ndest_cell; ++i) {
      dest_cells(sci, i) = -1;
      dest_fracs(sci, i) = 0.0;
    }
  }

  const Int cell_start_idx = n_subcells_per_elem * elem_self_idx;
  const Int n_cells_in_mesh = n_subcells_per_elem * nneighbors(ie);

  slmm_assert(m.p.dimension_1() == n_cells_in_mesh);
  slmm_throw_if(m.p.dimension_1() != n_cells_in_mesh, "mesh points mismatch with n_cells_in_mesh");

  for (int sci=0; sci < n_subcells_per_elem; ++sci) {// loop over subcells that elem(ie) owns


    for (Int ci=0; ci < n_cells_in_mesh; ++ci) {// loop over cells in mesh

      // compute intersections
      const siqk::RawVec3s verts_in(vi_buf, nverts, 3);
      siqk::RawVec3s verts_out(vo_buf, max_num_intersections, 3);
      Int n_overlap_verts = 0;
      { // clip_against_poly scope
        siqk::RawVec3s wrk(wrk_buf, max_num_intersections, 3);

        // todo: can this input buffer copy move to the outer loop?
        //   it is inside this local scope, likely for good reason,
        //   but it only depends on sci, not ci.
        //   need to check if clip_against_poly modifies its content
        for (int vi=0; vi<nverts; ++vi) {
          siqk::SphereGeometry::copy(slice(verts_in, vi),
            Kokkos::subview(adv_points, Kokkos::ALL, sci));
        }

        siqk::sh::clip_against_poly<siqk::SphereGeometry>(m, ci, verts_in, nverts,
           verts_out, n_overlap_verts, wrk);
      } // clip_against_poly scope

      slmm_assert(n_overlap_verts <= max_num_intersections);
      if (n_overlap_verts > 0) {
        // compute area of overlap region


      } // n_overlap_verts > 0
    } // loop over cells in mesh
  } // loop over subcells that elem(ie) owns
} // calc_partmcsl_source_partition




}

} // namespace partmcsl



typedef siqk::Int Int;
typedef siqk::Real Real;

struct Cartesian3D { Real x, y, z; };





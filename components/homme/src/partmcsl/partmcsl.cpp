#include "partmcsl.hpp"
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

*/
void calc_partmcsl_source_partition(
  const Int ie, // index, in [0, nelemd-1] of active owned element on this rank
  const Int nelemd, // number of owned elements on this rank
  const Int max_num_neighbors,  // maximum number of neighboring elements (usually 9)
  const Int elem_self_idx, // index of "self" element in neighbors list in [0, nneighbors_i[ie]-1]
  const Int lev_idx, // index of active level in [0, nlev-1]
  const Int nlev, // number of vertical levels
  const Cartesian3D* adv_points_r, // adv_points(1:4,1:4) coordinates of 16 advected corners of subcells of elem[ie]
  const Cartesian3D* cell_points_r, // cell_points(1:nneighbors(ie), ie) coordinates of static corners of local elem[ie] fv mesh
  const Int* cells_i, // cells(:,1:nneighbors(ie), ie) give the quad vertices of fv mesh
  const Int* nneighbors_i, // number of neighbors for each owned element
  Int* ndest_i, // number of destination cells each source sends to
  Int* dest_i, // indices of cells that receive sent source
  Real* frac_r // fraction of source cells to send
  )
  {
  using siqk::slice;

  slmm_assert(src_partition);
  slmm_throw_if(!src_partition, "src_partition pointer not associated.");

  // input ptrs come from fortran
  // convert them to views
  homme::FA1<const Int>  nneighbors(reinterpret_cast<const Int*>(nneighbors_i), nelemd);
  homme::FA3<const Real> cell_points(reinterpret_cast<const Real*>(cell_points_r), 3, 4*max_num_neighbors, nelemd);
  homme::FA3<const Int>  cells(reinterpret_cast<const Int*>(cells_i), 4, max_num_neighbors, nelemd);
  homme::FA3<const Real> adv_points(reinterpret_cast<const Real*>(adv_points_r), 3, 4, 4);

  // convert output ptrs to views
  homme::FA2<Int> ndest(ndest_i, nlev, n_subcells_per_elem);
  homme::FA3<Int> dest_cells(ndest_i, max_ndest_cell, nlev, n_subcells_per_elem);
  homme::FA3<Real> dest_fracs(frac_r, max_ndest_cell, nlev, n_subcells_per_elem);

  // reset output for new computations
  for (int sci=0; sci < n_subcells_per_elem; ++sci) {
    ndest(lev_idx, sci) = 0;
    for (int i=0; i<max_ndest_cell; ++i) {
      dest_cells(i, lev_idx, sci) = -1;
      dest_fracs(i, lev_idx, sci) = 0.0;
    }
  }

  // get local fv cell mesh
  src_partition->init_local_mesh_if_needed(ie, elem_self_idx,
    Kokkos::subview(cell_points, Kokkos::ALL, std::pair<Int, Int>(0, 4*nneighbors(ie)), ie),
    Kokkos::subview(cells, Kokkos::ALL, std::pair<Int,Int>(0, 4*nneighbors(ie)), ie) );
  const auto& mesh = src_partition->mesh[ie];
  const auto& area = src_partition->area[ie];

  // workspace buffers will be wrapped in unmanaged views for easier indexing shortly
  Real vi_buf[3 * nverts];
  Real vo_buf[3 * max_num_intersections];
  Real wrk_buf[4 * max_num_intersections];

  const Int n_cells_in_mesh = n_subcells_per_elem * nneighbors(ie);

  slmm_assert(m.p.dimension_1() == n_cells_in_mesh);
  slmm_throw_if(m.p.dimension_1() != n_cells_in_mesh, "mesh points mismatch with n_cells_in_mesh");

  for (int aci=0; aci < n_subcells_per_elem; ++aci) {// loop over advected subcells of elem(ie)

    const Int src_idx = ie_start_idx_[ie] + aci;
    const Real src_area = area(src_idx);

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
            Kokkos::subview(adv_points, Kokkos::ALL, aci));
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
        dest_fracs(ndest(aci)  , lev_idx, aci) = ov_area / src_area;
        dest_cells(ndest(aci)++, lev_idx, aci) = ci;
      } // n_overlap_verts > 0
    } // loop over cells in mesh

    Real total_frac = 0.0;
    for (int j=0; j<ndest(aci); ++j) {
      total_frac += dest_fracs(j, lev_idx, aci);
    }
    slmm_throw_if(std::abs(total_frac - 1.0) > fp_tol, "source total fraction error");

  } // loop over subcells that elem(ie) owns
} // calc_partmcsl_source_partition






} // namespace partmcsl


extern "C" void calc_source_partition_(
  homme::Int* ie, homme::Int* nelemd,
  homme::Int* self_idx,
  homme::Int* lev_idx, homme::Int* nlev,
  homme::Int* max_num_neighbors,
  homme::Cartesian3D* adv_points, homme::Cartesian3D** cell_points,
  homme::Int** cells, homme::Int* nneighbors,
  homme::Int* ndest, homme::Int* dest, homme::Real* frac) {

  partmcsl::calc_partmcsl_source_partition(*ie-1, *nelemd,
    *self_idx-1, *lev_idx-1, *nlev, *max_num_neighbors,
    adv_points, *cell_points, *cells, nneighbors,
    ndest, dest, frac);


}





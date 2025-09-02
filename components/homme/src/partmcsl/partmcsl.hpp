#ifndef PARTMC_SL_HPP
#define PARTMC_SL_HPP

#include "compose_slmm.hpp"
#include "compose_slmm_siqk.hpp" // geometry, sqr, slice, kokkos view types
#include "compose_homme.hpp" // FA types, Cartesian3D

#include "siqk_exe_space.hpp"
#include "siqk_intersect.hpp" // Mesh, and polygonal intersections based on them.

#include "partmcsl_sphere_geometry.hpp" // spherical triangle area

namespace partmcsl {
  using siqk::Real;
  using siqk::Int;

  // see siqk_intersect.hpp for details.
  using LocalMesh = siqk::sh::Mesh<Kokkos::HostSpace>;

  // Convex quad-quad intersection yields <= 8 intersection points.
  static constexpr Int max_num_intersections = 8;
  // We don't do RRM, so elements have <= 9 neighbors
  static constexpr Int max_num_elem_neighbors = 9;
  // we use symmetric order-12 quadrature.  see siqk_quadrature.hpp for more detail.
  static constexpr Int tri_quadrature_order = 12;
  // computations are in R3
  static constexpr Int ndim = 3;
  // we assume pg2
  static constexpr Int n_subcells_per_elem = 4;
  // always quads
  static constexpr Int nverts = 4;
  // the maximum number of element-element intersections (including self interactions)
  // with timestep limited by a 1-halo constraint
  static constexpr Int max_ndest_elem = 9;
  // upper bound on number of cell-cell intersections
  static constexpr Int max_ndest_cell = n_subcells_per_elem * max_ndest_elem;
  static constexpr Real fp_tol = 1e-14;

  struct SlSourcePartition {
    using Ptr = std::shared_ptr<SlSourcePartition>;
    using R3Array = siqk::InExeSpace<siqk::ConstVec3s, Kokkos::HostSpace>::type;
    using I2Array = siqk::InExeSpace<siqk::Idxs, Kokkos::HostSpace>::type;
    using R1Array = siqk::InExeSpace<Kokkos::View<Real*>, Kokkos::HostSpace>::type;
    using I1Array = siqk::InExeSpace<Kokkos::View<Int*>, Kokkos::HostSpace>::type;

    explicit SlSourcePartition(const Int nelem)
    {
      mesh_.resize(nelem);
      area_.resize(nelem);
      elem_self_idx_.resize(nelem);
    }

    const LocalMesh& mesh(const Int ie) const {
      return mesh_[ie];
    }

    const R1Array& area(const Int ie) const {
        return area_[ie];
    }

    /*
      Copy the local fv mesh from fortran into the siqk Mesh struct.

      ie [in] is the element, amongst those owned by this rank, that we're working on.

      Array3D corners [in] dimension (3, 4*ncells), passed from Fortran; its entries are the
        3D cartesian coordinates of the physgrid quadrilaterals' corners.

      CellArray [in] dimension (4, ncells), passed from Fortran; its entries are the quad
        connectivities of the physgrid cell vertices in the corners array.

      Firt, this routine just copies the fortran arrays into LayoutRight views. This
      could be optimized away in the future by passing only those fortran
      pointers to this local mesh structure.  For now, we'll keep it this way
      because we need to compute edge normals here (fortran doesn't provide them)
      and this ensures we conform to the array layouts that fill_normals expects.

      Second, it computes the static source cell areas.

      the original impl of init_local_mesh_if_needed, from SLMMIR commit 80d80b7822...,
      only needed element-element intersections. It therefore only needed basic
      info from fortran and could construct its own meshes.
      For PartMCSL, however, we need physgrid cells; this impl assumes
      they've already been constructed in fortran and that they're passed to this function
      as array arguments.
    */
    template <typename Array3D, typename RArray>
    void init_local_mesh_if_needed(const Int ie, const Int nneighbor_elem, const Int elem_self_idx, const Array3D& corners, const RArray& area) {

      slmm_assert( (ie >= 0 and ie < static_cast<Int>(mesh_.size())) );

      if (mesh_[ie].p.extent(0) != 0) return;

      auto& m = mesh_[ie];
      auto& a = area_[ie];
      elem_self_idx_[ie] = elem_self_idx;

      const Int ncells = n_subcells_per_elem * nneighbor_elem;
      const Int npts = 4 * ncells;

      m.p = R3Array("p", npts, ndim);
      m.e = I2Array("e", ncells, nverts);
      a = R1Array("a", ncells);

      Int pt_idx = 0;
      Int cell_idx = 0;
      for (Int ni=0; ni<nneighbor_elem; ++ni) {
          for (Int ci=0; ci<n_subcells_per_elem; ++ci) {
              for (Int vi=0; vi<nverts; ++vi) {
                  for (Int j=0; j<ndim; ++j) {
                      m.p(pt_idx, j) = corners(j, vi, ci, ni, ie);
                  }
                  m.e(cell_idx, vi) = pt_idx++;
              }
              a(cell_idx++) = area(ci, ni, ie);
          }
      }

      //
      // an updated version of fill_normals shows up in compose_slmm_departure_point.hpp
      // but it has some extra stuff that we don't need.
      // this is an older version that comes from siqk_intersect.hpp.
      //
      siqk::test::fill_normals<siqk::SphereGeometry>(m);
    }

    private:
      std::vector<LocalMesh> mesh_;
      std::vector<R1Array> area_;
      std::vector<Int> elem_self_idx_;

      /*
      Given a subcell index, return the reference coordinates of the vertex at vert_idx,
      in ccw order starting at SW corner.

                (0,1)
     (-1,1) o----x----o (1,1)
            | 3  | 2  |
            |    |    |
     (-1,0) x----x----x (1,0)
            | 0  | 1  |
            |    |    |
    (-1,-1) o----x----o (1,-1)
                (0,-1)

      */
      void ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx) const;

  }; // struct SlSourcePartition

  void src_partition_init(const Int nelem);

  void calc_partmcsl_source_partition(const int ie,
            const int nelemd,
            const int n_elem_neighbors,
            const int elem_self_idx,
            const int level_idx,
            const int nlev,
            const homme::Cartesian3D* cell_corners_p,
            const Real* cell_area,
            const homme::Cartesian3D* adv_points_p,
            Int* ndest,
            Int* dest_idx,
            Real* frac_p
            );

} // namespace partmcsl
#endif



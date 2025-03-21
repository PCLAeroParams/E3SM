#ifndef PARTMC_SL_HPP
#define PARTMC_SL_HPP

#include "compose_slmm.hpp"
#include "compose_slmm_siqk.hpp" // geometry, sqr, slice, kokkos view types

#include "siqk_exe_space.hpp"
#include "siqk_intersect.hpp" // Mesh, and polygonal intersections based on them.

namespace partmcsl {
  using siqk::Real;
  using siqk::Int;

  // see siqk_intersect.hpp for details.
  using LocalMesh = siqk::sh::Mesh<Kokkos::HostSpace>;

  // Convex quad-quad intersection yields <= 8 intersection points.
  static constexpr Int max_num_intersections = 8;
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

  struct SlSourcePartition {
    using Ptr = std::shared_ptr<SlSourcePartition>;
    using R3Array = siqk::InExeSpace<siqk::ConstVec3s, Kokkos::HostSpace>::type;
    using I2Array = siqk::InExeSpace<siqk::Idxs, Kokkos::HostSpace>::type;

    explicit SlSourcePartition(const Int nelem)
    {
      mesh_.resize(nelem);
    }

    const LocalMesh& mesh(const Int ie) const {
      return mesh_[ie];
    }


    /*
      the original impl of init_local_mesh_if_needed, from SLMMIR commit 80d80b7822...,
      only needed element-element intersections. It therefore only needed basic
      info from fortran and could construct its own meshes.
      For PartMCSL, however, we need physgrid cells; this impl assumes
      they've already been constructed in fortran and that they're passed to this function
      as array arguments.

      Array3D corners has dimension (3, 4*ncells), passed from Fortran; its entries are the
        3D cartesian coordinates of the physgrid quadrilaterals' corners.
      CellArray has dimension (4, ncells), passed from Fortran; its entries are the quad
        connectivities of the physgrid cell vertices in the corners array.

      Basically, this routine just copies the fortran arrays into LayoutRight views. This
      could be optimized away in the future by passing only those fortran
      pointers to this local mesh structure.  For now, we'll keep it this way
      because we need to compute edge normals here (fortran doesn't provide them)
      and this ensures we conform to the array layouts fill_normals expects.
    */
    template <typename Array3D, typename CellArray>
    void init_local_mesh_if_needed(const Int ie, const Array3D& corners, const CellArray& cells) {

      slmm_assert( (ie >= 0 and ie < static_cast<Int>(mesh_.size())) );
      slmm_assert( nverts == cells.dimension_0() );

      slmm_throw_if( nverts != cells.dimension_0(), "unexpected cells array shape");

      if (mesh_[ie].p.dimension_0() != 0) return;

      auto& m = mesh_[ie];

      const Int ncells = cells.dimension_1();
      const Int npts = nverts * ncells;

      slmm_assert(npts == corners.dimension_1);

      m.p = R3Array("p", npts, ndim);
      m.e = I2Array("e", ncells, nverts);

      Int pt_idx=0;
      for (Int ci=0; ci<ncells; ++ci) {
        for (Int vi=0; vi<nvertss; ++vi) {
          for (int j=0; j<ndim; ++j) {
            m.p(pt_idx,j) = corners(j, pt_idx);
          }
          m.e(ci,vi) = pt_idx;
          ++pt_idx;
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

  void calc_partmcsl_source_partition();

} // namespace partmcsl
#endif



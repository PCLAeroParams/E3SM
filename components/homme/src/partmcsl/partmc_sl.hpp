#ifndef PARTMC_SL_HPP
#define PARTMC_SL_HPP

#include "compose_homme.hpp"
#include "compose_slmm.hpp"
#include "compose_slmm_siqk.hpp"

#include "siqk_exe_space.hpp"
#include "siqk_intersect.hpp" // Mesh, and polygonal intersections based on them.
#include "siqk_sqr.hpp" // sphere to ref etc.

namespace partmcsl {
  using siqk::Real;
  using siqk::Int;

  // see siqk_intersect.hpp
  using LocalMesh = siqk::sh::Mesh<Kokkos::HostSpace>;

  struct SlSourcePartition {
    using Ptr = std::shared_ptr<SlSourcePartition>;
    using RealArray = InExeSpace<ConstVec3s, Kokkos::HostSpace>::type;
    using IntArray = InExeSpace<Idxs, Kokkos::HostSpace>::type;

    static constexpr Int tri_quadrature_order = 12;
    static constexpr Int ndim = 3;
    static constexpr Int n_subcells_per_elem = 4; // we assume pg2

    explicit SlSourcePartition(const Int nelem)
    {
      mesh_.resize(nelem);
    }

    const LocalMesh& mesh(const Int ie) const {
      return mesh_[ie];
    }

    /**
      Build a local siqk::sh mesh for use with polygonal intersection algorithms.
      The corners array is passed from Homme's spectral element type's desc%neigh_corners.
      This function builds the pg2 mesh from that data.

      corners is a Fortran Array of type(cartesian3D_t) with dimension(4, num_neighbors)
      for each element, accessed as elem(ie)%desc%neigh_corners in fortran, recast as
      an array of Reals with layout left here, dimension (3, 4, num_neighbors).
    */
    template <typename Array3D>
    void init_local_mesh_if_needed(const Int ie, const Array3D& corners) {

      slmm_assert(ie < static_cast<Int>(mesh_.size());
      slmm_assert(corners.dimension_0() == 3);
      slmm_assert(corners.dimension_1() == 4);

      // for now, make sure that checks happen even in non-debug builds
      // remove these later
      slmm_throw_if(corners.dimension_0() != 3, "unexpected corners dimension");
      slmm_throw_if(corners.dimension_1() != 4, "unexpected corners dimension");

      if (mesh_[ie].p.dimension_0() != 0) return;

      auto& m = mesh_[ie];
      // spectral element corners are input
      const Int nvert = corners.dimension_1(); // should always be 4
      const Int nelem_in = corners.dimension_2();  // depends on cubed sphere topology

      // finite volume corners/cells are output
      const Int ncell_out = 4 * nelem_in; // 4 subcells per elem
      const Int N_out = 4 * ncell_out; // 4 vertices per subcell

      m.p = RealArray("p", N_out, ndim);
      m.e = IntArray("e", ncell_out, nvert);
      siqk::Vec3s elem_corners("elem_corners", 4);
      const Int quad[4] = {0,1,2,3};

      Int k=0;
      Int co = 0;
      for (Int ci = 0; ci < ncell_in; ++ci) {
        // loop over input elements
        //
        // unpack element corner data from homme to use with siqk::sqr functions
        for (Int vert_idx = 0; vert_idx<nvert; ++vert_idx) {
          for (int j=0; j<ndim; ++j) {
            elem_corners(vert_idx, j) = corners(j, vert_idx, ci);
          }
        }

        // use element corner data to construct the physgrid cells
        // with subcells defined in reference quad by calling siqk::sqr
        for (Int subcell_idx=0; subcell_idx < n_subcells_per_elem; ++subcell_idx) {
          for (Int vert_idx = 0; vert_idx < 4; ++vert_idx ) {
            Real a, b;
            ref_coords_ab(a, b, subcell_idx, vert_idx);
            Real p_cart[3];
            siqk::sqr::calc_ref_to_sphere(elem_corners, quad, a, b, p_cart);

            for (int j=0; j<ndim; ++j) {
              m.p(k,j) = p_cart[j];
            }
            m.e(co, vert_idx) = k;
            ++k;
          }
          ++co;
        }
        slmm_assert(k == N_out);
        slmm_assert(co == ncell_out);

        // for now, make sure that checks happen even in non-debug builds
        // remove these later
        slmm_throw_if(k != N_out, "unexpected point count");
        slmm_throw_if(co != ncell_out, "unexpected fv cell count");
      }
      //
      // an updated version of fill_normals shows up in compose_slmm_departure_point.hpp
      // but it has some extra stuff that we don't need.
      // this version comes from siqk_intersect.hpp.
      //
      siqk::test::fill_normals<siqk::SphereGeometry>(m);
    }

    private:
      std::vector<LocalMesh> mesh_;

      std::vector<Int> source_cell_id_;
      std::vector<Int> dest_cell_id_;
      std::vector<Real> source_to_dest_;

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
} // namespace partmcsl
#endif

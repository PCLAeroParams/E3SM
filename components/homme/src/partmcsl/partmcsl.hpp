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
  using R3Array = siqk::InExeSpace<siqk::ConstVec3s, Kokkos::HostSpace>::type;
  using I2Array = siqk::InExeSpace<siqk::Idxs, Kokkos::HostSpace>::type;
  using R1Array = siqk::InExeSpace<Kokkos::View<Real*>, Kokkos::HostSpace>::type;
  using I1Array = siqk::InExeSpace<Kokkos::View<Int*>, Kokkos::HostSpace>::type;

  // see siqk_intersect.hpp for details.
  using LocalMesh = siqk::sh::Mesh<Kokkos::HostSpace>;

  // Convex quad-quad intersection yields <= 8 intersection points.
  static constexpr Int max_num_intersections = 8;
  // We don't do RRM, so elements have <= 9 neighbors
  static constexpr Int max_num_elem_neighbors = 9;
  // we use symmetric order-12 quadrature.  see siqk_quadrature.hpp for more detail.
//   static constexpr Int tri_quadrature_order = 12;
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


    private:
      std::vector<LocalMesh> mesh_;
      std::vector<R1Array> area_;
      std::vector<Int> elem_self_idx_;



  }; // struct SlSourcePartition

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
  void ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx);

  void src_partition_init(const Int nelem);

  void init_local_meshes(const homme::Int nelemd,
                         const homme::Int* nneighbors,
                         const homme::Int* elem_self_idx,
                         const homme::Cartesian3D* points,
                         const homme::Real** areas);

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

  void test_interface(const homme::Int i) {}

  bool areas_match();
} // namespace partmcsl
#endif



#ifndef PARTMC_SL_HPP
#define PARTMC_SL_HPP

#include "compose_slmm.hpp"
#include "compose_slmm_siqk.hpp" // geometry, sqr, slice, kokkos view types
#include "compose_homme.hpp" // FA types, Cartesian3D

#include "siqk_exe_space.hpp"
#include "siqk_intersect.hpp" // Mesh, and polygonal intersections based on them.

#include "partmcsl_sphere_geometry.hpp" // spherical triangle area

#include <iostream>
#include <sstream>

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
      elem_self_idx_.resize(nelem, -1);
    }

    const LocalMesh& mesh(const Int ie) const {
      return mesh_[ie];
    }

    const R1Array& area(const Int ie) const {
        return area_[ie];
    }

    Int elem_self_idx(const Int ie) const {
      return elem_self_idx_[ie];
    }

    template <typename PointsArray, typename AreaArray>
    void init_local_mesh_if_needed(const homme::Int ie,
                         const homme::Int nneighbors,
                         const homme::Int elem_self_idx,
                         const PointsArray& points,
                         const AreaArray& areas) {
      if (elem_self_idx_[ie] >= 0) return;

      auto m = mesh_[ie];
      auto a = area_[ie];
      elem_self_idx_[ie] = elem_self_idx;

      const Int ncells = n_subcells_per_elem * nneighbors;
      const Int npts = nverts * ncells;
      m.p = R3Array("p", npts);
      m.e = I2Array("e", ncells, nverts);
      a = R1Array("a", ncells);

      std::stringstream ss;
      ss << "partmcsl::SlSourcePartition::init_local_mesh_if_needed: initializing new mesh for ie "
         << ie << " with " << npts << " points and " << ncells << " cells.\n";
      std::cout << ss.str();

      Int pt_idx = 0;
      Int cell_idx = 0;
      for (int nbr_idx = 0; nbr_idx < nneighbors; ++nbr_idx) {
        for (int subcell_idx=0; subcell_idx<n_subcells_per_elem; ++subcell_idx) {
          for (int vert_idx=0; vert_idx < nverts; ++vert_idx) {
            for (int j=0; j<ndim; ++j) {
              m.p(pt_idx, j) = points(j, vert_idx, subcell_idx, nbr_idx, ie);
            }
            m.e(cell_idx, vert_idx) = pt_idx++;
          }
          std::cout << ss.str();
          const Real area_check = tri_area(Kokkos::subview(m.p, m.e(cell_idx, 0), Kokkos::ALL),
                                    Kokkos::subview(m.p, m.e(cell_idx, 1), Kokkos::ALL),
                                    Kokkos::subview(m.p, m.e(cell_idx, 2), Kokkos::ALL)) +
                                  tri_area(Kokkos::subview(m.p, m.e(cell_idx, 0), Kokkos::ALL),
                                    Kokkos::subview(m.p, m.e(cell_idx, 2), Kokkos::ALL),
                                    Kokkos::subview(m.p, m.e(cell_idx, 3), Kokkos::ALL));
          const bool area_pass = ( std::abs(area_check - areas(subcell_idx, nbr_idx, ie)) < fp_tol) ;

          slmm_assert(area_pass);

          if (!area_pass) {
            ss.str("");
            ss << "partmcsl.hpp : init_local_mesh_if_needed area mismatch error.  area = "
               << areas(subcell_idx, nbr_idx, ie) << " area_check = " << area_check
               << " at ie " << ie << " nbr " << nbr_idx << " subcell_idx " << subcell_idx
               << "\n";
            slmm_throw_if(!area_pass, ss.str());
          }

          a(cell_idx++) = areas(subcell_idx, nbr_idx, ie);
        }
      }

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

  void calc_source_partition(
            const Int ie,
            const Int nelemd,
            const Int n_elem_neighbors,
            const Int elem_self_idx,
            const Int level_idx,
            const Int nlev,
            const homme::Cartesian3D* cell_corners_p,
            const Real* cell_area,
            const homme::Cartesian3D* adv_points_p,
            Int* ndest_p,
            Int* dest_idx_p,
            Real* frac_p
            );

  template <typename ArrayType>
  void test_int_array(const ArrayType& arr, const homme::Int n) {
    std::stringstream ss;
    ss << "partmcsl: c++ received (" << n << ") = (";
    for (int i=0; i<n; ++i) {
      ss << arr[i] << " ";
    }
    ss << ")\n";
    std::cout << ss.str();
  }

  bool areas_match();
} // namespace partmcsl
#endif



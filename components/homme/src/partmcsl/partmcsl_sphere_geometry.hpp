#ifndef PARTMC_SPHERE_GEOMETRY_HPP
#define PARTMC_SPHERE_GEOMETRY_HPP

#include "compose_slmm.hpp"
#include "compose_slmm_siqk.hpp" // geometry, sqr, slice, kokkos view types

namespace partmcsl {

  using siqk::Real;
  using siqk::Int;
    /** \brief Computes the great circle distance between two points on the sphere

      \param a view of a position vector a = [a0,a1,a2]
      \param b view of a position vector b = [b0,b1,b2]
    */
    template <typename CV, typename CV2>
    KOKKOS_INLINE_FUNCTION Real distance(const CV a, const CV2 b) {
      Real cp[3];
      siqk::SphereGeometry::cross(cp, a, b);
      const Real dp = siqk::SphereGeometry::dot(a, b);
      return std::atan2(std::sqrt(siqk::SphereGeometry::norm2(cp), dp);
    }
  };

    /** \brief  Computes the area of the spherical triangle whose vertices a
    defined (in ccw order) by a, b, c.

    \param a vertex a = [a0,a1,a2]
    \param b vertex b = [b0,b1,b2]
    \param c vertex c = [c0,c1,c2]
  */
  template <typename CV, typename CV2>
  KOKKOS_INLINE_FUNCTION Real tri_area(const CV& a, const CV2& b,
                                              const CV2& c) {
    const Real s1 = distance(a, b);
    const Real s2 = distance(b, c);
    const Real s3 = distance(c, a);
    const Real half_perim = 0.5 * (s1 + s2 + s3);
    Real zz = std::tan(0.5 * half_perim) * std::tan(0.5 * (half_perim - s1)) *
              std::tan(0.5 * (half_perim - s2)) *
              std::tan(0.5 * (half_perim - s3));
    if (FloatingPoint<Real>::zero(zz)) {
      // guard against (0 - epsilon)
      zz = 0;
    }
    return 4 * atan(sqrt(zz));
  }

}

#include "partmcsl_sphere_geometry.hpp"

extern "C" homme::Real tri_area(homme::Cartesian3D* vertex_a, homme::Cartesian3D* vertex_b, homme::Cartesian3D* vertex_c) {
    return partmcsl::tri_area(reinterpret_cast<const homme::Real*>(vertex_a),
                              reinterpret_cast<const homme::Real*>(vertex_b),
                              reinterpret_cast<const homme::Real*>(vertex_c));
}

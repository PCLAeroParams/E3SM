# Experimental Stochastic transport with semi-Lagrangian remap
# for use with PartMC aerosol parameterizations.
#
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR} 
  ${CMAKE_CURRENT_BINARY_DIR}
  ${HOMME_SOURCE_DIR}/src/share 
  ${HOMME_SOURCE_DIR}/src/share/compose 
)

add_definitions(-DHOMME_ENABLE_PARTMCSL)

message(STATUS "PARTMCSL stochastic transport enabled")

add_library( partmcsl
    partmcsl.cpp
    partmcsl_sphere_geometry.cpp
)

message(STATUS "partmcsl will link with Kokkos and ${COMPOSE_LIBRARY}")
target_link_libraries(partmcsl Kokkos::kokkos ${COMPOSE_LIBRARY})


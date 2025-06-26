# Experimental Stochastic transport with semi-Lagrangian remap
# for use with PartMC aerosol parameterizations.
#
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR} # polygonal intersection, and area quadrature
  ${CMAKE_CURRENT_BINARY_DIR}
  ${HOMME_SOURCE_DIR}/src/share # for compose_mod, physgrid stuff 
  ${HOMME_SOURCE_DIR}/src/share/compose # for SL infrastructure 
)

add_definitions(-DHOMME_ENABLE_PARTMCSL)

message(STATUS "PARTMCSL stochastic transport enabled")

#add_library( partmcsl
#)
#target_link_libraries(partmcsl Kokkos::kokkos ${COMPOSE_LIBRARY})
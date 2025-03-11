include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  ${HOMME_SOURCE_DIR}/src/share # for physgrid stuff (gllfvremap*mod.F90)
  ${HOMME_SOURCE_DIR}/src/share/compose # for SL infrastructure, polygonal intersection, and area quadrature
)

message(STATUS "PARTMCSL stochastic transport enabled")

#add_library( partmcsl
#)
#target_link_libraries(partmcsl Kokkos::kokkos ${COMPOSE_LIBRARY})
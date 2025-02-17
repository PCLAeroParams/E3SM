include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  ${HOMME_SOURCE_DIR}/src/share # for physgrid stuff (gllfvremap*mod.F90)
  ${HOMME_SOURCE_DIR}/src/share/compose # for SL infrastructure, polygonal intersection, and area quadrature
)

message("partmcsl enabled.")

#add_library( ${PARTMCSL_LIBRARY}
#)
#target_link_libraries(${PARTMCSL_LIBRARY} Kokkos::kokkos ${COMPOSE_LIBRARY})
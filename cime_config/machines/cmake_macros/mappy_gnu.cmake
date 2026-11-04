# PartMC (-partmc): each user builds their own PartMC with this machine's modules and
# exports PARTMC_PATH (its build directory) before building a case. Export MOSAIC_PATH too
# only if that PartMC was built with ENABLE_MOSAIC=ON. See components/cmake/build_model.cmake.
if (DEFINED ENV{PARTMC_PATH})
  set(PARTMC_PATH "$ENV{PARTMC_PATH}")
endif()
if (DEFINED ENV{MOSAIC_PATH})
  set(MOSAIC_PATH "$ENV{MOSAIC_PATH}")
endif()
if (COMP_NAME STREQUAL gptl)
  string(APPEND CPPDEFS " -DHAVE_SLASHPROC")
endif()
string(APPEND CMAKE_C_FLAGS_RELEASE " -O2")
string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -O2")
if (MPILIB STREQUAL mpi-serial AND NOT compile_threaded)
  set(PFUNIT_PATH "$ENV{SEMS_PFUNIT_ROOT}")
endif()

set(KOKKOS_USE_EKAT_MACH_FILE "TRUE")

# PartMC (-partmc) is built from externals/partmc with the case (components/cmake/build_model.cmake).
# To build it with MOSAIC, export MOSAIC_PATH (the directory with libmosaic and its .mod files).
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

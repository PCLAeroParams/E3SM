SET(CMAKE_C_COMPILER "mpicc" CACHE STRING "")
SET(CMAKE_CXX_COMPILER "mpicxx" CACHE STRING "")
SET(CMAKE_Fortran_COMPILER "mpifort" CACHE STRING "")
SET (WITH_PNETCDF FALSE CACHE FILEPATH "")

message(STATUS "--- Setting Flight configuration --- ")
SET (NETCDF_DIR "$ENV{NETCDF_PATH}" CACHE STRING "")
#SET (ADD_LINKER_FLAGS "-L/$ENV{NETCDF_PATH}/lib -lnetcdff -lnetcdf" CACHE STRING "")

SET (HOMME_FIND_BLASLAPACK TRUE CACHE BOOL "")
SET (HOMME_USE_MKL FALSE CACHE BOOL "") # for Intel
message(STATUS "HOMME_USE_MKL is ${HOMME_USE_MKL}")
# turn on additional intel compiler flags
SET (ADD_Fortran_FLAGS "-traceback" CACHE STRING "")
SET (ADD_C_FLAGS       "-traceback" CACHE STRING "")
SET (ADD_CXX_FLAGS     "-traceback" CACHE STRING "")

SET (USE_MPIEXEC "srun" CACHE STRING "")
SET (USE_MPI_OPTIONS " --mpi=pmi2 --kill-on-bad-exit --cpu_bind=cores" CACHE STRING "")

SET (USE_QUEUING FALSE CACHE BOOL "")
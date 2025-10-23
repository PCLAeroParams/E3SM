# PARTMCSL / E3SM status

## Standalone Homme

A convenience script that handles the configure/build/run steps can be found at `<e3sm-pclap>/components/homme/run_homme_partmcsl.sh`.  

### Configure, build, and run

```
cd <work-dir>
cmake -Wno-dev -C <homme_machine_file> -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=DEBUG -DHOMME_USE_MKL=FALSE <e3sm-pclap>/components/homme
```

The `<homme_machine_file>` listed above must be the standalone Homme machine file, in `homme/cmake/machineFiles`, (not the CIME machine files).

Build the code with 
```
cd <work-dir>
make -j 24 theta-l-nlev20-native
```

On an interactive node, or a machine without batch submissions, run the test with a command like
```
mpirun --map-by ppr:56:socket:PE=1 --bind-to hwthread --n 56 <work-dir>/test_execs/theta-l-nlev20-native/theta-l-nlev20-native < <e3sm-pclap>/components/homme/partmcsl_dcmip12_transport.nl 2>&1 | tee homme-out.txt
```

## Current status

The code necessary to compute the portion of cell `i` that moves to cell `j` over a tracer time step is kept in `homme/src/partmcsl/`.   

Some of these functions must be called or enabled from other parts of Homme, e.g., `prim_driver_base.F90`, and the code requires some preprocessor definitions to work -- currently these are hard-coded into this branch's CMake files.  

Running the test should output some items to the log, preceded with `partmcsl`.   

Some assertions are failing.   
The per-timestep code attempts to compute the portion of physics cell `i` that moves to physics cell `j` in an element's local area mesh.   These portions include self-interaction (i.e., some of cell `i` remains in cell `i`) and therefore should sum to 1, always.


## Remaining to-do

### Vertical transport

We need to derive the algorithm for vertical remap-based transport and implement it; it should be similar in character (a piecewise constant polynomial basis is equivalent to a portion `i` to `j` scheme) to the horizontal scheme, but simpler to implement because vertical dynamics do not require MPI.  


### Test case driver

We need to test this transport scheme using existing tracer tests.   The test case I've set up so far is one of these, but it runs on the dynamics grid.   We need to modify the test to define tracers on the physics grid, and to output these tracers when I/O is called. 

An example test in standalone homme that mixes dynamics variables with physics variables is `homme/tests/run_tests/thetah-sl-dcmip16_test1pg2.cmake` whose namelist is `homme/reg_test/namelists/thetah-sl-dcmip16_test1pg2.nl`.  

To find the source code that this test uses, grep for `dcmip2016_test1_pg2`, which will eventually lead to `homme/src/test_src/dcmip16_wrapper.F90`.

In that file is a data structure
```
type :: PhysgridData_t
   integer :: nphys
   real(rl), allocatable :: ps(:,:), zs(:,:), T(:,:,:), uv(:,:,:,:), omega_p(:,:,:), q(:,:,:,:)
end type PhysgridData_t

type (PhysgridData_t) :: pg_data
```
that holds data on the physics grid related to the test case's forcing functions. 

We need to make a similar data structure to hold the tracer data `Q`, `Q2`, `Q3`, and `Q4` used by the dcmip 2012 test case that our PartMCsl test runs.  These data need to be computed using PartMCsl and then output in the same way that the `dcmip16_test1pg2` writes its data. 

Then, we'll be able to check the convergence of our 3D transport scheme.


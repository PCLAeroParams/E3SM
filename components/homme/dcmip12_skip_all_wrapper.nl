!
! namelist for dcmip2012 test1-1 with partmcsl COMPILED IN but every
! partmcsl-conditional code path -- init + step forward + phys_to_dyn +
! WRAPPER init (gfr_init, pg_data alloc, gfr_dyn_to_fv_phys seed) --
! compiled out via the four SKIP flags.
!
! Build side: -DHOMME_ENABLE_PARTMCSL=ON,
!             -DPARTMCSL_SKIP_INIT=ON,
!             -DPARTMCSL_SKIP_STEP_FORWARD=ON,
!             -DPARTMCSL_SKIP_PHYS_TO_DYN=ON,
!             -DPARTMCSL_SKIP_WRAPPER_INIT=ON.
!
! At this point the ONLY residual difference against the sl_only_q8 build
! is what the HOMME_ENABLE_PARTMCSL macro does at CMake/link time:
!   - src/partmcsl/CMakeLists.txt: builds+links the partmcsl C++ library
!   - src/theta-l/CMakeLists.txt: adds partmcsl F90 sources to theta-l
!   - cmake/HommeMacros.cmake: TARGET_LINK_LIBRARIES(theta-l partmcsl)
! Plus the `use partmcsl_advection_mod, only: ...` Fortran imports in
! prim_step and prim_finalize (module symbols resolved at compile/link
! but no runtime call happens).
!
! Outcomes:
!   * Q1 matches sl_only_q8 (~3.30e-2 mean, ~4315 nz)
!     => the wrapper init IS the 40% residual.  gfr_init or
!     gfr_dyn_to_fv_phys has a side effect on the SL/CEDR path even though
!     both are ostensibly intent(in) on elem.
!   * Q1 still degraded (~2.3e-2, ~3400 nz)
!     => mere presence of HOMME_ENABLE_PARTMCSL at link/CMake time is
!     the residual; look at partmcsl F90 module imports, compose SL for
!     internal partmcsl-aware branches, or Kokkos init/teardown ordering.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_1"
  ne                = 30
  qsize             = 8
  ndays             = 1
  statefreq         = 200
  restartfreq       = -1
  runtype           = 0
  tstep             = 33
  integration       = 'explicit'
  tstep_type        = 1
  smooth            = 0
  nu                = 1.585e13
  nu_s              = 1.585e13
  hypervis_order    = 2
  hypervis_subcycle = 1
  prescribed_wind   = 1
  se_ftype          = -1
  dt_tracer_factor  = 2
  dt_remap_factor   = 2
  transport_alg     = 12
  semi_lagrange_cdr_alg   = 2
  semi_lagrange_cdr_check = .false.
  semi_lagrange_nearest_point_lev = 100
  vert_remap_q_alg   = 10
  nu_q = 0
/
&filter_nl/
&solver_nl
  precon_method     = "identity"
  maxits            = 50
  tol               = 1.e-7
/
&vert_nl
  vanalytic         = 1
  vtop              = 0.2549944
/
&analysis_nl
  output_dir        = "./movies_skip_all_wrapper/"
  output_timeunits  = 1,
  output_frequency  = 1,
  output_varnames1  ='u','geo','ps','Q','Q2','Q3','Q4','Q5','Q6','Q7','Q8'
  interp_type       = 0
  output_type       ='netcdf'
  num_io_procs      = 16
  interp_nlat       = 256
  interp_nlon       = 512
  interp_gridtype   = 2
/
&prof_inparm
  profile_outpe_num   = 100
  profile_single_file	= .true.
/

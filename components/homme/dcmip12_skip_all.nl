!
! namelist for dcmip2012 test1-1 with partmcsl enabled but ALL partmcsl
! subroutines compiled out (init + step_forward + phys_to_dyn).
!
! Build side: -DHOMME_ENABLE_PARTMCSL=ON, -DPARTMCSL_SKIP_INIT=ON,
! -DPARTMCSL_SKIP_STEP_FORWARD=ON, -DPARTMCSL_SKIP_PHYS_TO_DYN=ON.
!
! What still differs from the SL-only build:
!   - HOMME_ENABLE_PARTMCSL macro is defined (so the Fortran `use partmcsl_*`
!     imports still resolve, and dcmip12_wrapper.F90's HOMME_ENABLE_PARTMCSL-
!     guarded init block still runs: qsize>=8 check, gfr_init, pg_data alloc,
!     q(5:8)=q(1:4), set_tracers(qsize=8), gfr_dyn_to_fv_phys)
!   - qsize=8 (SL/CEDR loads 8 tracers instead of 4)
!   - QSIZE_D=9 (compile-time upper bound)
!
! Q1 outcome:
!   * matches SL-only ~4.17e-2 mean, ~5466 nonzero cells @ day1 lev37
!     => partmcsl_init IS the leak (memory corruption / C++ side effect)
!   * still degraded to skip-both level ~2.54e-2 mean, ~3848 nz
!     => leak is in the dcmip12_wrapper HOMME_ENABLE_PARTMCSL init block
!        or in compose/CEDR's qsize=8 loading (compare vs an sl_only_q8
!        variant to distinguish)
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
  output_dir        = "./movies_skip_all/"
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

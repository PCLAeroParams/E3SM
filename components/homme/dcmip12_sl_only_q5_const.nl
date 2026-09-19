!
! qsize sweep point: qsize=5, sl-only, PARTMCSL_CONST_MIRROR=ON.
! q(5) = 0.1 (spatial constant); q(1..4) are the DCMIP tracers.
!
! Reuses the sl_only_q8_const build tree (compile-time flags identical:
! HOMME_ENABLE_PARTMCSL=OFF, QSIZE_D=9, PARTMCSL_CONST_MIRROR=ON).
!
! Goal: does adding just ONE trivial extra tracer trigger the Q1
! fragmentation seen at qsize=8?  If yes, the mechanism is anything-
! more-than-qsize=4.  If no, adds granularity to what qsize count
! actually matters.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_1"
  ne                = 30
  qsize             = 5
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
  output_dir        = "./movies_sl_only_q5_const/"
  output_timeunits  = 1,
  output_frequency  = 1,
  output_varnames1  ='u','geo','ps','Q','Q2','Q3','Q4','Q5'
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

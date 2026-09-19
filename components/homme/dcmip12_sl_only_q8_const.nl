!
! namelist for dcmip2012 test1-1, SL-ONLY (no partmcsl code compiled),
! qsize=8, with q(5:8) set to spatial constants (0.1, 0.3, 0.5, 0.7)
! at t=0 instead of mirroring q(1:4).
!
! Purpose: A/B against dcmip12_sl_only_q8.nl (mirror) and dcmip12_sl_only.nl
! (qsize=4) to distinguish whether the Q1 fragmentation in sl_only_q8 is
! caused by (a) qsize=8 tracer count alone, or (b) duplicating DCMIP tracer
! content into the upper 4 slots.
!
! Build side: -DHOMME_ENABLE_PARTMCSL=OFF, -DQSIZE_D=9, -DPARTMCSL_CONST_MIRROR=ON.
!
! Outcome:
!   * Q1 matches sl_only_q4 (smooth)  => duplicate DCMIP content triggers
!     the fragmentation; qsize=8 alone is safe.  Points to something in the
!     transport that treats correlated tracers (q4 = 1 - 0.3*(q1+q2+q3)
!     etc.) in slots 5..8 in a way that leaks into slots 1..4.
!   * Q1 still fragments  => qsize=8 tracer count alone triggers it,
!     independent of tracer content.  Contradicts the assumed compose
!     qsize=4/40 test coverage and points to a real qsize-count bug.
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
  output_dir        = "./movies_sl_only_q8_const/"
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

!
! qsize=5, sl-only, EXACT MIRROR: q(5) = q(1) (DCMIP cosine bell)
! at t=0.  Reuses the sl_only_q8 (mirror) build tree -- no compile-time
! flags beyond HOMME_ENABLE_PARTMCSL=OFF and QSIZE_D=9.  The default
! #else branch in the wrapper sets q(5:8) = q(1:4) unconditionally;
! set_tracers(q, qsize=5, ...) reads only q(1:5), so effectively
! state%Q(5) = state%Q(1) and slots 6..8 are untouched.
!
! Purpose: isolate whether duplicate DCMIP content in slot 5 alone
! triggers Q1 fragmentation, or whether the trigger requires the higher
! tracer count regardless of content.  Pairs with sl_only_q5_const
! (bit-identical to sl_only_q4).
!
! Outcomes:
!   * Q1 matches sl_only_q4  => duplicate content in slot 5 alone is
!     harmless; the qsize=8 fragmentation must come from something
!     specific to counts 6-8, not from the q(5)=q(1) mirror.
!   * Q1 fragments          => the q(5)=q(1) mirror IS the poison, and
!     the qsize sweep result (q5_const clean, q8_const broken) was
!     coincidental.  Would sharply implicate CEDR / SL treating a
!     duplicate mass row non-independently -- despite the earlier
!     ×0.99999 perturb result seeming to rule this out.
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
  output_dir        = "./movies_sl_only_q5_mirror/"
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

!
! Namelist: Vertical Translation Test (dcmip2012_test1_vt).
!
! Two-lobe prescribed vertical wind, u=v=0, ps=p0, single Gaussian
! tracer centered in the lower lobe (eta_norm=0.75).  Flow parameters
! (w_amp_vt, eta_norm0_vt, gauss_width_vt) live in
! src/test_src/dcmip12_wrapper.F90 near the top of the module.
!
! Small grid (ne=2): every column evolves identically since the flow
! and IC are horizontally uniform, so ne=2 is enough to exercise the
! partmcsl vertical step.
!
! qsize=8: partmcsl transports slots 5:8 hard-coded (pmcsl_nq=4 in
! partmcsl_advection.F90), so we can't shrink below 8 without touching
! the partmcsl signature.  Q1 rides the standard SL path (reference),
! Q5 mirrors Q1 at t=0 and is what the partmcsl vertical step
! transports on the FV grid.  Q2..Q4 / Q6..Q8 are inert padding.
! Compare Q5 vs a Python analytic-exact reference.
!
! nlev sweep uses separate builds:
!   theta-l-nlev20-native
!   theta-l-nlev64-native
!   theta-l-nlev128-native   (already built for horizontal SBR)
!   theta-l-nlev256-native
! Keep tstep and output cadence constant across the sweep so vertical
! time error is negligible relative to spatial error.
!
! Test window: 1 hour, output every 15 min (t = 0, 15, 30, 45, 60 min).
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_vt"
  ne                = 2
  qsize             = 8
  ndays             = 0
  nmax              = 240                       ! 240 steps * 15 s = 1 hour
  statefreq         = 60                        ! screen dump every 60 steps (15 min)
  restartfreq       = -1
  runtype           = 0
  tstep             = 15
  integration       = 'explicit'
  tstep_type        = 1
  smooth            = 0
  nu                = 0                         ! no hyperviscosity; prescribed_wind path is inert here anyway
  nu_s              = 0
  hypervis_order    = 2
  hypervis_subcycle = 1
  prescribed_wind   = 1
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
  vtop              = 0.2549944                 ! matches exp(-12000/H), H = Rd*300/g
/
&analysis_nl
  output_dir        = "./movies_vt/"
  output_timeunits  = 1,                        ! 1 = minutes
  output_frequency  = 15,                       ! output every 15 min => t = 0,15,30,45,60
  output_varnames1  = 'Q','Q5'
  interp_type       = 0
  output_type       = 'netcdf'
  num_io_procs      = 16
  interp_nlat       = 64
  interp_nlon       = 128
  interp_gridtype   = 2
/
&prof_inparm
  profile_outpe_num   = 100
  profile_single_file = .true.
/

!
! namelist for dcmip2012 test1-1 with partmcsl enabled but
! dcmip2012_test1_1_phys_to_dyn compiled out via -DPARTMCSL_SKIP_PHYS_TO_DYN.
!
! Companion to dcmip12_sl_only.nl.  Identical transport/dycore switches so
! the only difference between the three A/B builds is:
!   SL-only        (dcmip12_sl_only.nl,   -DHOMME_ENABLE_PARTMCSL=OFF)         qsize=4
!   partmcsl-full  (partmcsl_dcmip12_transport.nl, PARTMCSL=ON, SKIP=OFF)      qsize=8
!   partmcsl-skip  (this file,            PARTMCSL=ON, SKIP=ON)                qsize=8
! Run one day only; the fragmentation signature is already visible at day 1.
! Q5..Q8 in the netcdf will be stale (frozen at t=0 IC) because the FV->GLL
! copy-back is skipped -- ignore them, we only care about Q here.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_1"
  ne                = 30
  qsize             = 8                         ! matches the partmcsl-full build for a clean A/B
  ndays             = 1                         ! 1 day is enough; fragmentation is visible at day 1
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
  output_dir        = "./movies_skip_p2d/"
  output_timeunits  = 1,                        ! days
  output_frequency  = 1,                        ! daily (t = 0, 1)
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

!
! namelist for dcmip2012 test1-1 with PARTMCSL DISABLED at build time.
!
! Diagnostic A/B against partmcsl_dcmip12_transport.nl: same ne/nlev/tstep/
! transport switches, but qsize=4 (no partmcsl Q5..Q8 slots) and 2-day run
! since the day-1 SL peak is already the failure mode we want to isolate.
! No partmcsl-adjacent code is compiled in this build (see the paired
! run_homme_sl_only.sh with -DHOMME_ENABLE_PARTMCSL=OFF), so this run
! gives us ground-truth SL Q1 on this ne/nlev/tstep config.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4                         ! mesh parition method: 4 = space filling curve
  topology          = "cube"                    ! mesh type: cubed sphere
  test_case         = "dcmip2012_test1_1"       ! test identifier
  ne                = 30                        ! number of elements per cube face
  qsize             = 4                         ! four tracers, standard DCMIP 1-1 configuration
  ndays             = 2                         ! short A/B window; day-1 already shows the deficit
  statefreq         = 200
  restartfreq       = -1
  runtype           = 0
  tstep             = 33                        ! matches dcmip_tests/dcmip2012_test1.1/preqx/namelist-default.nl
  integration       = 'explicit'
  tstep_type        = 1
  smooth            = 0
  nu                = 1.585e13
  nu_s              = 1.585e13
  hypervis_order    = 2
  hypervis_subcycle = 1
  prescribed_wind   = 1
  se_ftype          = -1                        ! disable applyCAMforcing_tracers (no physics anyway)
  dt_tracer_factor  = 2
  dt_remap_factor   = 2
  transport_alg     = 12                        ! compose semi-Lagrangian
  semi_lagrange_cdr_alg   = 2                   ! CEDR
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
  output_dir        = "./movies_sl_only/"
  output_timeunits  = 1,                        ! days
  output_frequency  = 1,                        ! daily
  output_varnames1  ='u','geo','ps','Q','Q2','Q3','Q4'
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

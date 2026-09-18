!
! namelist for dcmip2012 test1-1, SL-ONLY (no partmcsl code compiled),
! but with qsize=8 so SL/CEDR loads 8 tracers.
!
! Purpose: A/B against dcmip12_sl_only.nl (qsize=4) to isolate whether
! qsize alone -- independent of any partmcsl-related init -- drives the
! Q1 degradation seen in the skip-all build.
!
! Build side: -DHOMME_ENABLE_PARTMCSL=OFF, -DQSIZE_D=9.  Slots 5..8 are
! mirrored from 1..4 at t=0 by the sl-only branch of dcmip12_wrapper.F90
! (the wrapper's #else block was extended to size q(8) + mirror to make
! this variant safe).
!
! Outcome:
!   * Q1 matches SL-only q=4 (~4.17e-2 mean, ~5466 nz)
!     => qsize alone doesn't affect Q1; the residual leak in skip-all is
!     in the HOMME_ENABLE_PARTMCSL-guarded init block of dcmip12_wrapper
!     (gfr_init, pg_data alloc, gfr_dyn_to_fv_phys at t=0)
!   * Q1 drops to ~2.3e-2 mean, ~3400 nz (matches skip-all)
!     => compose/CEDR is genuinely tracer-count-sensitive; the "leak" is
!     a real property of the transport layer, not anything we did
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
  output_dir        = "./movies_sl_only_q8/"
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

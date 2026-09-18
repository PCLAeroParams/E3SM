!
! namelist for dcmip2012 test1-1 with partmcsl enabled but BOTH the runtime
! transport calls (partmcsl_step_forward + partmcsl_vertical_step +
! partmcsl_permute_pg_q_cells) AND the phys_to_dyn copy-back compiled out.
!
! Build side: -DHOMME_ENABLE_PARTMCSL=ON, -DPARTMCSL_SKIP_STEP_FORWARD=ON,
! -DPARTMCSL_SKIP_PHYS_TO_DYN=ON.  With this pair, partmcsl code compiles,
! partmcsl_init runs at prim_init, pg_data%q is allocated and seeded from
! the GLL IC, but no partmcsl subroutine is called during timestepping.
!
! A/B ladder (all at ne=30, nlev=64, tstep=33, transport_alg=12, etc.):
!   SL-only        (dcmip12_sl_only.nl,      HOMME_ENABLE_PARTMCSL=OFF)     qsize=4
!   partmcsl-full  (partmcsl_dcmip12_transport.nl, PARTMCSL=ON everywhere)  qsize=8
!   partmcsl-skip-p2d  (dcmip12_skip_p2d.nl, +SKIP_PHYS_TO_DYN)             qsize=8
!   partmcsl-skip-both (this file,           +SKIP_PHYS_TO_DYN +SKIP_STEP_FORWARD) qsize=8
!
! Q5..Q8 in the netcdf will be stale (frozen at t=0 IC) because both the
! runtime transport and the copy-back are gone -- ignore them, we only care
! about Q here.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_1"
  ne                = 30
  qsize             = 8                         ! keeps SL/CEDR loading identical to partmcsl-full A/B
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
  output_dir        = "./movies_skip_stepfwd/"
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

!
! partmcsl SBR space-sweep namelist template.  Placeholder tokens are
! replaced by generate_sbr_sweep.py to emit one concrete namelist per
! sweep point.  ne / tstep / nmax combinations chosen so dt*ne = 900
! (constant CFL) with a 6-hour simulation window.  Each output_dir is
! per-ne to avoid file collisions.
!
! Requires the HOMME executable built with -DPARTMCSL_SBR_DIAG so the
! Test S SBR wind override and Q7 analytic-exact routine are compiled in.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4                         ! mesh parition method: 4 = space filling curve
  topology          = "cube"                    ! mesh type: cubed sphere
  test_case         = "dcmip2012_test1_1"       ! test identifier
  ne                = 120                      ! number of elements per cube face
  qsize             = 8                         ! num tracer fields
  ndays             = 0                         ! use nmax steps
  nmax              = 2880                    ! total number of dynamics steps (6 h / tstep)
  statefreq         = 200                       ! number of steps between screen dumps
  restartfreq       = -1                        ! don't write restart files if < 0
  runtype           = 0                         ! 0 = new run
  tstep             = 7.5000                   ! largest timestep in seconds
  integration       = 'explicit'                ! explicit time integration
  tstep_type        = 1                         ! 1 => default method
  smooth            = 0                         ! timestep smoothing
  nu                = 1.585e13                  ! hyperviscosity minimum L2 error at 10^13.2
  nu_s              = 1.585e13
  hypervis_order    = 2                         ! 2 = hyperviscosity
  hypervis_subcycle = 1                         ! 1 = no hyperviz subcycling
  prescribed_wind   = 1
  dt_tracer_factor  = 2
  dt_remap_factor   = 2
  transport_alg     = 12
  semi_lagrange_cdr_alg   = 2
  semi_lagrange_cdr_check = .false.
  semi_lagrange_nearest_point_lev = 100
  vert_remap_q_alg  = 10
  nu_q = 0
/
&filter_nl/
&solver_nl
  precon_method     = "identity"
  maxits            = 50
  tol               = 1.e-7
/
&vert_nl
  vanalytic         = 1                         ! set vcoords in initialization routine
  vtop              = 0.2549944                 ! vertical coordinate at top of atm 254.9 hPa (12km)
/
&analysis_nl
  output_dir        = "./movies_sbr_sweep_ne120/"            ! destination dir for netcdf file
  output_timeunits  = 2,                        ! 1=days, 2=hours, 0=timesteps
  output_frequency  = 1,                        ! output every hour
  output_varnames1  ='u','geo','Q','Q2','Q3','Q4','Q5','Q6','Q7','Q8'
  interp_type       = 0                         ! 0=native grid, 1=bilinear
  output_type       ='netcdf'                   ! netcdf or pnetcdf
  num_io_procs      = 16
  interp_nlat       = 256
  interp_nlon       = 512
  interp_gridtype   = 2                         ! gauss grid
/
&prof_inparm
  profile_outpe_num   = 100
  profile_single_file	= .true.
/

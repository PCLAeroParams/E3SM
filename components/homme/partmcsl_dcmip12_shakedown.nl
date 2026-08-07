!
! partmcsl shakedown: 3-day deformational-flow (dcmip 2012 test 1-1),
! horizontal-only via the Test W zero-eta_dot override still active in
! dcmip12_wrapper.F90.  Test S SBR overrides are gated behind
! PARTMCSL_SBR_DIAG and OFF in this build.  Purpose: fast confirmation
! that the src/dst normalization fix keeps Q6 constant-tracer drift at
! roundoff under deformational flow before committing to the full
! 12-day convergence sweep.
!
! ne = 30, tstep = 33 s (matches the full-run namelist); 12-hour output
! → 6 snapshots.  Bump to ndays=12 (matching partmcsl_dcmip12_transport.nl)
! for the full sweep once this passes.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4                         ! mesh parition method: 4 = space filling curve
  topology          = "cube"                    ! mesh type: cubed sphere
  test_case         = "dcmip2012_test1_1"       ! test identifier
  ne                = 30                        ! number of elements per cube face
  qsize             = 8                         ! num tracer fields
  ndays             = 3                         ! num simulation days: 0 = use nmax steps
  statefreq         = 200                       ! number of steps between screen dumps
  restartfreq       = -1                        ! don't write restart files if < 0
  runtype           = 0                         ! 0 = new run
  tstep             = 33                        ! largest timestep in seconds
  integration       = 'explicit'                ! explicit time integration
  tstep_type        = 1                         ! 1 => default method
  smooth            = 0                         ! timestep smooting
  nu                = 1.585e13                  ! hyperviscosity minimum L2 error at 10^13.2
  nu_s              = 1.585e13
  hypervis_order    = 2                         ! 2 = hyperviscosity
  hypervis_subcycle = 1                         ! 1 = no hyperviz subcycling
  prescribed_wind   = 1
  dt_tracer_factor  = 2
  dt_remap_factor   = 2
  transport_alg     = 12
  semi_lagrange_cdr_alg   = 2
  semi_lagrange_cdr_check = .false.             ! .true. does mass conservation checks at each time step
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
  vanalytic         = 1                         ! set vcoords in initialization routine
  vtop              = 0.2549944                 ! vertical coordinate at top of atm 254.9 hPa (12km)
/
&analysis_nl
  output_dir        = "./movies_shakedown/"     ! destination dir for netcdf file
  output_timeunits  = 2,                        ! 1=days, 2=hours, 0=timesteps
  output_frequency  = 12,                       ! output every 12 hours
  output_varnames1  ='u','geo','Q','Q2','Q3','Q4','Q5','Q6','Q7','Q8' ! variables to write to file
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

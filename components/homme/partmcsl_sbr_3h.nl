!
! Namelist: short-time SBR convergence check for partmcsl.
!
! Runs dcmip2012_test1_1 with the "Test S" solid-body-rotation wind
! override + "Test W" zero-eta_dot override that are hard-coded in
! src/test_src/dcmip12_wrapper.F90.  Total simulation time is T = 3
! hours (well below the ~day-3 dissipation saturation observed in the
! standard 12-day sweep), with hourly output.  Q6 is a spatial
! constant (see dcmip12_wrapper.F90 Q6 diagnostic override) so its
! drift diagnoses the arrival-side Σ frac = 1 invariant on live
! departure quads.
!
! ne / tstep / nmax settings for the ~constant-CFL sweep:
!   ne = 16, tstep = 60, nmax =  180   (dt*ne =  960)
!   ne = 30, tstep = 30, nmax =  360   (dt*ne =  900)
!   ne = 60, tstep = 15, nmax =  720   (dt*ne =  900)
! ndays = 0 disables the ndays->nmax conversion so nmax below is used
! directly.  Change output_dir when switching ne so runs don't stomp
! each other.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4                         ! mesh parition method: 4 = space filling curve
  topology          = "cube"                    ! mesh type: cubed sphere
  test_case         = "dcmip2012_test1_1"       ! test identifier (SBR + zero-eta_dot overrides hardcoded)
  ne                = 30                        ! elements per cube face
  qsize             = 8                         ! num tracer fields
  ndays             = 0                         ! 0 => use nmax directly
  nmax              = 360                       ! 360 steps * 30 s = 3 hours
  statefreq         = 60                        ! screen dump every 60 steps (=30 min at ne=30)
  restartfreq       = -1                        ! don't write restart files if < 0
  runtype           = 0                         ! 0 = new run
  tstep             = 30                        ! time step in seconds
  integration       = 'explicit'                ! explicit time integration
  tstep_type        = 1                         ! 1 => default method
  smooth            = 0                         ! timestep smoothing
  nu                = 1.585e13                  ! hyperviscosity (inert under prescribed_wind)
  nu_s              = 1.585e13
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
  vtop              = 0.2549944
/
&analysis_nl
  output_dir        = "./movies_sbr_3h/"        ! destination dir for netcdf (change per ne)
  output_timeunits  = 2,                        ! 2 = hours
  output_frequency  = 1,                        ! output every 1 hour => t=0,1,2,3
  output_varnames1  ='u','geo','Q','Q2','Q3','Q4','Q5','Q6','Q7','Q8'
  interp_type       = 0
  output_type       ='netcdf'
  num_io_procs      = 16
  interp_nlat       = 256
  interp_nlon       = 512
  interp_gridtype   = 2
/
&prof_inparm
  profile_outpe_num   = 100
  profile_single_file = .true.
/

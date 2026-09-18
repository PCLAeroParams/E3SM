!
! namelist for dcmip2012 test1-1, SL-ONLY (no partmcsl code compiled),
! qsize=4 at runtime, QSIZE_D=9 at compile time.
!
! Isolates whether the compile-time bound QSIZE_D=9 alone triggers the
! fragmentation seen in sl_only_q8, or whether the trigger is actually
! *using* qsize=8 at runtime.  All other runs in the ladder use
! QSIZE_D=9 (necessary once HOMME_ENABLE_PARTMCSL is on); this rung is
! the only one that can decouple the two.
!
! Build side: -DHOMME_ENABLE_PARTMCSL=OFF, -DQSIZE_D=9, but namelist
! qsize=4 -- SL/CEDR only touches slots 1..4, slots 5..9 remain untouched
! but the arrays are dimensioned to hold them.
!
! Outcomes:
!   * Q1 smooth like sl_only_q4 (~4.17e-2 mean, ~5466 nz)
!     => QSIZE_D alone isn't the trigger; corruption comes from actually
!     running 8 tracers.  Next look: compose/CEDR path when it loops
!     across tracers 5..8, or the mirror-duplicate q(5:8)=q(1:4) hitting
!     a limiter degeneracy.
!   * Q1 fragmented like sl_only_q8 (~3.30e-2, ~4315 nz)
!     => the QSIZE_D=9 compile-time bound triggers something inside
!     compose/CEDR or shared HOMME arrays.  Deeper investigation of the
!     shared code assumptions is warranted.
!_______________________________________________________________________
&ctl_nl
  nthreads          = 1
  partmethod        = 4
  topology          = "cube"
  test_case         = "dcmip2012_test1_1"
  ne                = 30
  qsize             = 4
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
  output_dir        = "./movies_sl_only_qsized9_q4/"
  output_timeunits  = 1,
  output_frequency  = 1,
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

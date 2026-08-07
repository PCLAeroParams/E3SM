



#ifndef CAM
#include "config.h"

module dcmip12_wrapper

! Implementation of the dcmip2012 dycore tests for the preqx dynamics target

use control_mod,          only: test_case, dcmip4_moist, dcmip4_X, vanalytic
use dcmip2012_test1_2_3,  only: test1_advection_deformation, test1_advection_hadley, test1_advection_orography, &
                                test2_steady_state_mountain, test2_schaer_mountain,test3_gravity_wave
use dcmip2012_test1_conv_mod, only: test1_conv_advection, test1_conv_print_results
use dcmip2012_test4,      only: test4_baroclinic_wave 
use mtests,               only: mtest_state
use derivative_mod,       only: derivative_t, gradient_sphere
use dimensions_mod,       only: np, nlev, nlevp, qsize, qsize_d, nelemd
use element_mod,          only: element_t
use element_state,        only: nt=>timelevels
use hybrid_mod,           only: hybrid_t
use hybvcoord_mod,        only: hvcoord_t, set_layer_locations
use kinds,                only: rl=>real_kind, iulog
use parallel_mod,         only: abortmp

#ifdef HOMME_ENABLE_PARTMCSL
use partmcsl_advection_mod, only: source_partition_t, src_partition
#endif 

! model specific routines - must be provided by each model:
use element_ops,          only: set_state, set_state_i, copy_state, tests_finalize, set_forcing_rayleigh_friction


implicit none

! physical constants used by dcmip2012 test 3.1
real(rl), parameter ::              &
  g       = 9.80616,                & ! grav const
  a       = 6371229.0,              & ! earth radius in meters
  Rd      = 287.0,                  & ! dry gas const
  cp      = 1004.5,                 & ! heat capacity const pressure
  kappa   = Rd/cp,                  &
  pi      = 3.141592654,            &
  p0      = 100000.0                  ! reference pressure

real(rl), dimension(:,:,:,:), allocatable :: u0, v0                     ! storage for dcmip2-x sponge layer
real(rl):: zi(nlevp), zm(nlev)                                          ! z coordinates
real(rl):: ddn_hyai(nlevp), ddn_hybi(nlevp)                             ! vertical derivativess of hybrid coefficients
real(rl):: tau
real(rl):: ztop

#ifdef PARTMCSL_SBR_DIAG
!! DIAGNOSTIC ONLY (Test S, vivid-napping-lighthouse): SBR wind override
!! parameters, promoted to module scope so set_pg_q7_analytic_exact below can
!! use the same axis/tau as the Test S override at dcmip2012_test1_1.
!! Williamson SW1 axis tilt alpha = pi/4; one full revolution per 12 days.
real(rl), parameter :: sbr_tau   = 12.0_rl * 86400.0_rl
real(rl), parameter :: sbr_alpha = pi / 4.0_rl
real(rl), parameter :: sbr_u0    = 2.0_rl * pi * a / sbr_tau
#endif

!! DCMIP 2012 Gaussian-hills parameters for the Q1 override in
!! dcmip2012_test1_1 (and the matching analytic exact in
!! set_pg_q7_analytic_exact).  Bell centers coincide with the cosine-bell
!! centers in test1_advection_deformation (dcmip2012_test1_2_3.F90:128-131).
!! h_max and b match the standard DCMIP 2012 Gaussian-hills prescription.
!! Vertical modulation is a Gaussian in (z-gh_z0)/gh_zz so Q1 is C^infinity
!! and avoids the C^0 kink of the cosine-bell + min(1,.) cutoff, which was
!! the source of CEDR-clipping mass loss under the SL scheme
!! (see partmcsl_half_order_sbr_handoff.md §7c).
real(rl), parameter :: gh_lam0 = 5.0_rl*pi/6.0_rl, gh_phi0 = 0.0_rl
real(rl), parameter :: gh_lam1 = 7.0_rl*pi/6.0_rl, gh_phi1 = 0.0_rl
real(rl), parameter :: gh_hmax = 0.95_rl, gh_b = 5.0_rl
real(rl), parameter :: gh_z0   = 5000.0_rl, gh_zz = 1000.0_rl

#ifdef HOMME_ENABLE_PARTMCSL
!
! PhysgridData_t is copied from dcmip16_wrapper.F90,
! since dcmip16 already "uses" dcmip12_wrapper.F90.
!
type :: PhysgridData_t
   integer :: nphys
   real(rl), allocatable :: ps(:,:), zs(:,:), T(:,:,:), uv(:,:,:,:), omega_p(:,:,:), q(:,:,:,:)
end type PhysgridData_t

type (PhysgridData_t) :: pg_data

! Zero-valued T and uv tendency buffers, sized to match pg_data.  Reused on
! every call to gfr_fv_phys_to_dyn from dcmip2012_test1_1_phys_to_dyn so that
! the remap-back affects only Q (the partmcsl-advected tracers).
real(rl), allocatable :: pg_zero_T(:,:,:)        ! (ncol, nlev, nelemd)
real(rl), allocatable :: pg_zero_uv(:,:,:,:)     ! (ncol, 2, nlev, nelemd)
#endif

contains

!_____________________________________________________________________
subroutine dcmip2012_test1_1(elem,hybrid,hvcoord,nets,nete,time,n0,n1)
#ifdef HOMME_ENABLE_PARTMCSL  
  use gllfvremap_mod
  use perf_mod, only: t_startf, t_stopf
  use partmcsl_advection_mod, only : src_partition
#endif

  ! 3d deformational flow

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat                                                    ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,dp,eta_dot,dp_dn
#ifdef HOMME_ENABLE_PARTMCSL
  real(rl):: q(8)
  integer, parameter :: nphys = 2, ncol=4
#else
  real(rl):: q(4) ! pointwise field values
#endif

  ! Test S, vivid-napping-lighthouse: SBR wind override.  Constants sbr_tau,
  ! sbr_alpha, sbr_u0 are now declared at module scope (top of file) so
  ! set_pg_q7_analytic_exact can reuse them.

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-1: 3d deformational flow'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
#ifdef HOMME_ENABLE_PARTMCSL
    if (qsize < 8) then
      if (hybrid%masterthread) write(iulog,*) 'partmcsl dcmip2012 test 1-1: 3d deformational flow requires qsize >= 8'
      call abortmp('qsize set too small for dcmip test case')
    endif
    if (hybrid%ithr == 0) then
       pg_data%nphys = nphys
       call gfr_init(hybrid%par, elem, nphys)
       allocate(pg_data%ps(ncol,nelemd), pg_data%zs(ncol,nelemd), pg_data%T(ncol,nlev,nelemd), &
            pg_data%omega_p(ncol,nlev,nelemd), pg_data%uv(ncol,2,nlev,nelemd), &
            pg_data%q(ncol,nlev,qsize,nelemd))
       allocate(pg_zero_T(ncol,nlev,nelemd), pg_zero_uv(ncol,2,nlev,nelemd))
       pg_zero_T = 0.0_rl
       pg_zero_uv = 0.0_rl
    endif
    !$omp barrier
#endif    ! HOMME_ENABLE_PARTMCSL
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H * log(1.0d0/hvcoord%etam(k))
      p = p0 * hvcoord%etam(k)
      call test1_advection_deformation(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2),q(3),q(4))
      !! Replace the DCMIP cosine-bell Q1 with the DCMIP Gaussian-hills
      !! variant (C^infinity, no bell-edge kink).  Q2..Q4 stay on their
      !! upstream definitions; note Q2 = 0.9 - 0.8*q1^2 and Q4 = 1 - 0.3*(q1+q2+q3)
      !! inherit the smoother q1 while Q3 (slotted ellipse) is unaffected.
      q(1) = q1_gaussian_hills(lat, lon, z)
#ifdef PARTMCSL_SBR_DIAG
      !! DIAGNOSTIC ONLY (Test S, vivid-napping-lighthouse):
      !! Override the deformational (u,v) with Williamson SW1 solid-body
      !! rotation.  Bells (q1..q4) returned by test1_advection_deformation
      !! are kept as the IC; only the wind is replaced so partmcsl's
      !! 4-corner GLL sample + bilinear (a,b) interp captures the
      !! velocity exactly up to O(h^2) chord-vs-arc per step.  Revert
      !! before shipping.
      u = sbr_u0 * (cos(lat)*cos(sbr_alpha) + sin(lat)*cos(lon)*sin(sbr_alpha))
      v = -sbr_u0 * sin(lon) * sin(sbr_alpha)
      w = 0.0_rl
#endif

      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)

#ifdef HOMME_ENABLE_PARTMCSL
      if (time == 0) then
        ! Mirror q1..q4 into q5..q8 so the partmcsl-advected physgrid tracers
        ! share the IC of the dynamics-grid tracers.
        q(5:8) = q(1:4)
        !! DIAGNOSTIC ONLY (Q6 constant-tracer): overwrite Q6 with a spatial
        !! constant so any drift of Q6 during transport diagnoses whether the
        !! arrival-side Σ frac = 1 invariant holds on the live departure
        !! quads (as opposed to the synthetic uniform partition tested by
        !! test_sum_to_one).  Revert before shipping.
        q(6) = 1.0_rl
#ifdef PARTMCSL_SBR_DIAG
        !! DIAGNOSTIC ONLY (Q7 analytic-exact): seed Q7 with Q1 (cosine bells)
        !! instead of Q3 (slotted cylinder).  At subsequent snapshots Q7 is
        !! overwritten in set_pg_q7_analytic_exact with the analytically
        !! SBR-rotated IC evaluated at FV cell centroids; the ||Q5 - Q7|| diff
        !! in the NetCDF then measures partmcsl's true error against the
        !! analytic exact (no dependence on SL Q as a reference).  Revert
        !! before shipping.
        q(7) = q(1)
#endif
        call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
      endif
#else
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
#endif
  enddo; enddo; enddo; enddo

#ifdef HOMME_ENABLE_PARTMCSL
  if (time == 0) then
    ! Initialize physgrid tracer state once from the GLL IC.  Subsequent
    ! evolution of pg_data%q will be done by partmcsl (TODO).
    call t_startf('gfr_dyn_to_fv_phys')
    call gfr_dyn_to_fv_phys(hybrid, nt, hvcoord, elem, nets, nete, &
         pg_data%ps, pg_data%zs, pg_data%T, pg_data%uv, pg_data%omega_p, pg_data%q)
    call t_stopf('gfr_dyn_to_fv_phys')
  endif
#endif

  ! set prescribed state at level interfaces
  do ie = nets,nete; do k=1,nlevp; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H  * log(1.0d0/hvcoord%etai(k))
      p = p0 * hvcoord%etai(k)
      call test1_advection_deformation(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2),q(3),q(4))
#ifdef PARTMCSL_SBR_DIAG
      !! DIAGNOSTIC ONLY (Test S, vivid-napping-lighthouse): zero w so
      !! theta-l's set_state_i writes state%w_i = 0 (consistent with the
      !! midpoint-loop solid-body override; see vivid-napping-lighthouse.md
      !! Step 3 notes).
      w = 0.0_rl
#endif
      call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)

      ! get vertical derivative of p at point i,j,k
      dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps

      ! get vertical eta velocity at point i,j,k
      eta_dot = -g*rho*w/p0

      ! store vertical mass flux
      elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn

  enddo; enddo; enddo; enddo

  !! DIAGNOSTIC ONLY (partmcsl Test W, vivid-napping-lighthouse):
  !! zero the prescribed vertical eta-velocity so the V step (both partmcsl
  !! and SL vertical remap) becomes the identity, isolating the H step in
  !! the ||Q5-Q||_L2 convergence rate.  Revert before shipping.
  do ie = nets, nete
    elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,:) = 0.0_rl
  end do

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_2(elem,hybrid,hvcoord,nets,nete,time,n0,n1)

  !  Hadley-like Meridional Circulation

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat                                                    ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(2),dp,eta_dot,dp_dn       ! pointwise field values

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-2: Hadley-like Meridional Circulation'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H * log(1.0d0/hvcoord%etam(k))
      p = p0 * hvcoord%etam(k)
      call test1_advection_hadley(time,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q(1),q(2))
      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))

  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete; do k=1,nlevp; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H  * log(1.0d0/hvcoord%etai(k))
      p = p0 * hvcoord%etai(k)
      call test1_advection_hadley(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2))
      call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)


      ! get vertical derivative of p at point i,j,k
      dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps

      ! get vertical eta velocity at point i,j,k
      eta_dot = -g*rho*w/p0

      ! store vertical mass flux
      elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn

  enddo; enddo; enddo; enddo

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_3(elem,hybrid,hvcoord,nets,nete,time,n0,n1,deriv)

  !  Horizontal advection of thin cloud-like tracers over orography

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type (derivative_t),intent(in)            :: deriv
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: cfv     = 0                                    ! h-vel is not coordinate following
  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(4),dp,gc                 ! pointwise field values
  real(rl):: grad_p(np,np,2),p_i(np,np),u_i(np,np),v_i(np,np)

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-3: Advection of thin clouds over orography'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      hyam=hvcoord%hyam(k); hybm=hvcoord%hybm(k)
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      call test1_advection_orography(lon,lat,p,z,zcoords,cfv,use_eta,hyam,hybm,gc,u,v,w,t,phis,ps,rho,q(1),q(2),q(3),q(4))
      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))

  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete;
    do k=1,nlevp;
      do j=1,np; do i=1,np
        hyai=hvcoord%hyai(k); hybi=hvcoord%hybi(k)
        lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
        call test1_advection_orography (lon,lat,p,z,zcoords,cfv,use_eta,hyai,hybi,gc,u,v,w,t,phis,ps,rho,q(1),q(2),q(3),q(4))
        call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)
        p_i(i,j) = p
        u_i(i,j) = u
        v_i(i,j) = v
      enddo; enddo

      ! get vertical mass flux
      grad_p = gradient_sphere(p_i,deriv,elem(ie)%Dinv)
      elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,k) = -u_i*grad_p(:,:,1) - v_i*grad_p(:,:,2)
    enddo;
    elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,1)     = 0
    elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,nlevp) = 0
  enddo;

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_conv(test_case,elem,hybrid,hvcoord,deriv,nets,nete,time,n0,n1)

  ! 3D tracer transport tests, modified to permit good convergence testing.

  ! Use physical constants consistent with HOMME
  use physical_constants, only: Rd => Rgas, p0

  character(len=*),   intent(in)            :: test_case
  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  type (derivative_t),intent(in)            :: deriv
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyai,hyam,hybi,hybm                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(5),dp,eta_dot,dp_dn      ! pointwise field values
  logical :: use_w
  real(rl):: grad_p(np,np,2),p_i(np,np),u_i(np,np),v_i(np,np)

  ! set analytic vertical coordinates at t=0
  if (.not. initialized) then
     !$omp barrier
     !$omp master
     if (hybrid%masterthread) then
        write(iulog,*) 'initializing dcmip2012 test 3(a-e): &
             &modified 3d deformational flows for convergence testing'
     end if
     call get_evenly_spaced_z(zi,zm,0.0_rl,ztop)                        ! get evenly spaced z levels
     hvcoord%etai = exp(-zi/H)                                          ! set eta levels from z
     call set_hybrid_coefficients(hvcoord,hybrid,hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
     call set_layer_locations(hvcoord, .true., hybrid%masterthread)
     initialized = .true.
     !$omp end master
     !$omp barrier
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
     hyam = hvcoord%hyam(k); hybm = hvcoord%hybm(k)
     lon = elem(ie)%spherep(i,j)%lon; lat = elem(ie)%spherep(i,j)%lat
     z = H * log(1.0d0/hvcoord%etam(k))
     p = p0 * hvcoord%etam(k)
     call test1_conv_advection(test_case,time,lon,lat,hyam,hybm,p,z,u,v,w,use_w, &
          &                    T,phis,ps,rho,q)
     dp = pressure_thickness(ps,k,hvcoord)
     call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
     if (time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete
     do k = 1,nlevp
        do j = 1,np
           do i = 1,np
              hyai = hvcoord%hyai(k); hybi = hvcoord%hybi(k)
              lon = elem(ie)%spherep(i,j)%lon; lat = elem(ie)%spherep(i,j)%lat
              z = H  * log(1.0d0/hvcoord%etai(k))
              p = p0 * hvcoord%etai(k)
              call test1_conv_advection(test_case,time,lon,lat,hyai,hybi,p,z,u,v,w,use_w, &
                   &                    T,phis,ps,rho,q)
              call set_state_i(u,v,w,T,ps,phis,p,zi(k),g,i,j,k,elem(ie),n0,n1)
              if (use_w) then
                 ! get vertical derivative of p at point i,j,k
                 dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps
                 ! get vertical eta velocity at point i,j,k
                 eta_dot = -g*rho*w/p0
                 ! store vertical mass flux
                 elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn
              else
                 p_i(i,j) = p
                 u_i(i,j) = u
                 v_i(i,j) = v
              end if
           enddo
        enddo
        if (.not. use_w) then
           ! get vertical mass flux
           grad_p = gradient_sphere(p_i,deriv,elem(ie)%Dinv)
           elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,k) = -u_i*grad_p(:,:,1) - v_i*grad_p(:,:,2)
        end if
     enddo
     if (.not. use_w) then
        elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,1)     = 0
        elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,nlevp) = 0
     end if
  enddo
end subroutine dcmip2012_test1_conv

!_____________________________________________________________________
subroutine dcmip2012_test2_0(elem,hybrid,hvcoord,nets,nete)

  ! steady state atmosphere with orography

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      gamma   = 0.0065d0,     &                                         ! temperature lapse rate (K/m)
      ztop    = 12000.d0,     &                                         ! model top (m)
      exponent= g/(Rd*gamma)

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values
  real(rl):: dpp(np,np,nlev), he

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 2-0: steady state atmosphere with orography'

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = (1.d0 - gamma/T0*zi)**exponent                        ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test2_steady_state_mountain(lon,lat,p,z,zcoords,use_eta,hyam,hybm,u,v,w,T,phis,ps,rho,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        !let's get an analytical \phi
        he = (T0 - T)/gamma
        call set_state(u,v,w,T,ps,phis,p,dp,he,g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test2_steady_state_mountain(lon,lat,p,z,zcoords,use_eta,hyai,hybi,u,v,w,T,phis,ps,rho,q(1))
        !let's get an analytical \phi
        he = (T0 - T)/gamma
        call set_state_i(u,v,w,T,ps,phis,p,he,g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo
  
  end subroutine dcmip2012_test2_0



!_____________________________________________________________________
subroutine dcmip2012_test2_x(elem,hybrid,hvcoord,nets,nete,shear)

  ! nonhydrostatic orographic waves (with or without shear)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: shear                    ! flag: 1=shear 0=no shear

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::   &
      Teq     = 300.d0,    &                                            ! temperature at equator
      ztop    = 30000.d0,	 &                                            ! model top (m)
      H       = Rd*Teq/g                                                ! characteristic height scale

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 2-x: steady state atmosphere with orography'

  !set \tau to 25.0, bound to X, [\tau]=[sec]
  tau = 25.0d0

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test2_schaer_mountain(lon,lat,p,z,zcoords,use_eta,hyam,hybm,shear,u,v,w,T,phis,ps,rho,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        ! original
        !    call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),1,nt)
        ! This test obtains analytical height and returns it, so, we use it for \phi ...
        call set_state(u,v,w,T,ps,phis,p,dp,z,g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
        ! ... or we can use discrete hydro state to init \phi. 
        
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test2_schaer_mountain(lon,lat,p,z,zcoords,use_eta,hyai,hybi,shear,u,v,w,T,phis,ps,rho,q(1))
        call set_state_i(u,v,w,T,ps,phis,p,z,g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

  ! store initial velocity fields for use in sponge layer
  allocate( u0(np,np,nlev,nelemd) )
  allocate( v0(np,np,nlev,nelemd) )

  do ie = nets,nete
    u0(:,:,:,ie) = elem(ie)%state%v(:,:,1,:,1)
    v0(:,:,:,ie) = elem(ie)%state%v(:,:,2,:,1)
  enddo

end subroutine


!_____________________________________________________________________
subroutine mtest_init(elem,hybrid,hvcoord,nets,nete,testid)

  ! nonhydrostatic orographic waves (with or without shear)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: testid                   ! 1 is m1,2 is m2, 3 is m3

  real(rl), parameter ::   &
      Teq     = 300.d0,    &                                            ! temperature at equator
      H       = Rd*Teq/g                                                ! characteristic height scale
  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values
  real(rl):: ztop

  if (testid .eq. 1) then
     ztop = 20000.d0
  else
     ztop = 30000.d0
  endif

  if (hybrid%masterthread) write(iulog,*) 'initializing m test'

  !set \tau to 25*3 for this test, [\tau]=[sec], X=500/3
  tau = 75.0d0

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call mtest_state(lon,lat,p,z,hyam,hybm,u,v,w,T,phis,ps,rho,testid)
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,p,dp,z,g, i,j,k,elem(ie),1,nt)
        
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call mtest_state(lon,lat,p,z,hyai,hybi,u,v,w,T,phis,ps,rho,testid)
        call set_state_i(u,v,w,T,ps,phis,p,z,g, i,j,k,elem(ie),1,nt)
        
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

  ! store initial velocity fields for use in sponge layer
  allocate( u0(np,np,nlev,nelemd) )
  allocate( v0(np,np,nlev,nelemd) )

  do ie = nets,nete
    u0(:,:,:,ie) = elem(ie)%state%v(:,:,1,:,1)
    v0(:,:,:,ie) = elem(ie)%state%v(:,:,2,:,1)
  enddo

end subroutine mtest_init


!_____________________________________________________________________
subroutine dcmip2012_test2_x_forcing(elem,hybrid,hvcoord,nets,nete,n,dt)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(in)            :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: n                        ! time level index
  real(rl),           intent(in)            :: dt                       ! time-step size

  integer  :: ie, k
  real(rl) :: ztop, zc, z(np,np,nlev), z_i(np,np,nlevp)
  real(rl) :: f_d(nlev) 

  if (test_case == "mtest1") then
    ztop    = 20000.d0        ! model top
    zc      = 10000.d0        ! sponge-layer cutoff height
  else 
    ztop    = 30000.d0        ! model top
    zc      = 20000.d0        ! sponge-layer cutoff height
  endif

  forall(k=1:nlev) z(:,:,k)=zm(k)
  forall(k=1:nlevp) z_i(:,:,k)=zi(k)

  ! Compute damping as a function of layer-midpoint height
  !where(zm .ge. zh)
  !  f_d = sin(pi/2 *(zm - zh)/(ztop - zh))**2
  !elsewhere
  !  f_d = 0.0d0
  !end where

  ! apply sponge layer forcing to momentum terms
  !f_d = -f_d/tau

  do ie=nets,nete
     call set_forcing_rayleigh_friction(elem(ie),z,z_i,ztop,zc,tau,u0(:,:,:,ie),v0(:,:,:,ie),n)
  enddo


end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test3(elem,hybrid,hvcoord,nets,nete)

  ! nonhydrostatic gravity waves

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords

  real(rl), parameter ::    &                                           ! parameters needed to get eta from z
    T0      = 300.d0,       &	! temperature (k)
    ztop    = 10000.d0,     & ! model top (m)
    N       = 0.01d0,       & ! Brunt-Vaisala frequency
    bigG    = (g*g)/(N*N*Cp)  ! temperature, isothermal

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,T_mean,phis_ps,ps,rho,rho_mean,q(1),dp    ! pointwise field values

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 3-0: nonhydrostatic gravity waves'

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                   ! get evenly spaced z levels
  hvcoord%etai  = ( (bigG/T0)*(exp(-zi*N*N/g) -1 )+1 ) **(1.0/kappa)    ! set eta levels from z at equator
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test3_gravity_wave(lon,lat,p,z,zcoords,use_eta,hyam,hybm,u,v,w,T,T_mean,phis,ps,rho,rho_mean,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize, dp,i,j,k,lat,lon,elem(ie))
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test3_gravity_wave(lon,lat,p,z,zcoords,use_eta,hyai,hybi,u,v,w,T,T_mean,phis,ps,rho,rho_mean,q(1))
        call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test4_init(elem,hybrid,hvcoord,nets,nete)

  type(element_t),    intent(inout), target :: elem(:)
  type(hybrid_t),     intent(in)            :: hybrid 
  type(hvcoord_t),    intent(inout)         :: hvcoord        
  integer,            intent(in)            :: nets,nete      
  integer,  parameter :: zcoords = 0                          ! we are not using z coords
  logical,  parameter :: use_eta = .true.                     ! we are using hybrid eta coords

  real(rl), parameter :: ps_test = 100000.0d0

  integer :: i,j,k,ie                                                   !
  real(rl):: lon,lat,hyam,hybm,hyai,hybi
  real(rl):: p,z,phis,u,v,w,T,ps,rho,dp    !pointwise field values
  real(rl):: q,q1,q2,qarray(3),pressure
  integer :: qs
  real(rl):: ztop    = 10000.d0
  real(rl):: H       = Rd * 300d0 / g

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 4: baroclinic wave'

  if (vanalytic==1) then
     if (hybrid%masterthread) write(iulog,*) 'using analytic veritcal coordinates'
     call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
     hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
     call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
     call set_layer_locations(hvcoord, .true., hybrid%masterthread)
  endif
     
  ! set initial conditions

  do ie = nets,nete; 
    do k=1,nlev
      pressure=hvcoord%hyam(k)*hvcoord%ps0 + hvcoord%hybm(k)*ps_test
      do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)

        !test4_baroclinic_wave(moist,X,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q,q1,q2)
        !moist 0 or 1, X is Earth scale factor, zcoord=0, q is vapor, q1, q2
        call test4_baroclinic_wave(dcmip4_moist,dcmip4_X,lon,lat,&
                                   pressure,z,zcoords,u,v,w,T,phis,ps,rho,q,q1,q2)
        qarray(1)=q; qarray(2)=q1; qarray(3)=q2;
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,pressure,dp,z,g, i,j,k,elem(ie),1,nt)

        !init only <=qsize tracers
        qs = min(qsize,3)
        call set_tracers(qarray(1:qs),qs, dp,i,j,k,lat,lon,elem(ie))
      enddo; enddo; enddo; 

    do k=1,nlevp
      pressure=hvcoord%hyai(k)*hvcoord%ps0 + hvcoord%hybi(k)*ps_test
      do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)

        !test4_baroclinic_wave(moist,X,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q,q1,q2)
        !moist 0 or 1, X is Earth scale factor, zcoord=0, q is vapor, q1, q2
        call test4_baroclinic_wave(dcmip4_moist,dcmip4_X,lon,lat,&
                                   pressure,z,zcoords,u,v,w,T,phis,ps,rho,q,q1,q2)
        qarray(1)=q; qarray(2)=q1; qarray(3)=q2;
        call set_state_i(u,v,w,T,ps,phis,pressure,z,g, i,j,k,elem(ie),1,nt)

      enddo; enddo; enddo; 


    call tests_finalize(elem(ie),hvcoord)
  enddo ! ie loop
end subroutine dcmip2012_test4_init

!_____________________________________________________________________
subroutine get_evenly_spaced_z(zi,zm, zb,zt)

  real(rl), intent(in)    :: zb,zt      ! top and bottom coordinates
  real(rl), intent(inout) :: zi(nlevp)  ! z at interfaces
  real(rl), intent(inout) :: zm(nlev)   ! z at midpoints
  integer :: k

  forall(k=1:nlevp) zi(k) = zt-(k-1)*(zt-zb)/(nlevp-1)
  zm = 0.5_rl*( zi(2:nlevp) + zi(1:nlev) )

end subroutine

!_____________________________________________________________________
subroutine get_evenly_spaced_p(zi,zm,zb,zt,H)
  real(rl), intent(in)    :: zb,zt,H    ! top and bottom coordinates
  real(rl), intent(inout) :: zi(nlevp)  ! z at interfaces
  real(rl), intent(inout) :: zm(nlev)   ! z at midpoints
  integer :: k
  real(rl) :: etab, etat, deta

  etab = 1.0d0
  etat = exp(-zt/H)
  deta = (etab - etat)/nlev
  do k = 1, nlevp
     zi(k) = H*log(1.0d0/(etat + (k-1)*deta))
  end do
  zm = 0.5_rl*(zi(2:nlevp) + zi(1:nlev))
end subroutine get_evenly_spaced_p

!_____________________________________________________________________
subroutine set_hybrid_coefficients(hv, hybrid, eta_t, c)

  ! create an analytical set of A,B coefficients, given known eta levels

  type(hvcoord_t),    intent(inout) :: hv       ! hybrid vertical coordinate stucture
  type(hybrid_t),     intent(in)    :: hybrid   ! hybrid parallal structure
  real(rl),           intent(in)    :: eta_t    ! top eta level
  real(rl),           intent(in)    :: c        ! exponent

  real(rl)  :: eta_c, tmp
  integer   :: k

  ! place cutoff halfway between bottom and top eta coordiantes
  eta_c = hv%etai(nlev/2)

  ! place cutoff at model top
  eta_c = eta_t

  do k=1,nlevp
    ! get values of hybrid coefficients
    tmp        = max( (hv%etai(k)-eta_c)/(1.0-eta_c), 0.0_rl)
    hv%hybi(k) = tmp**c
    hv%hyai(k) = hv%etai(k) - hv%hybi(k)
    if(hybrid%masterthread) write(*,'(i4,a,f18.15,a,f18.15,a,f18.15)') &
         k,': etai=',hv%etai(k),' Ai=',hv%hyai(k),' Bi=',hv%hybi(k);

    ! get derivatives of hybrid coefficients
    ddn_hybi(k) = c*tmp**(c-1)
    if(hv%etai(k)>eta_c) ddn_hybi(k)=0.0d0
    ddn_hyai(k) = 1.0d0 - ddn_hybi(k)
  enddo

  hv%hyam = 0.5_rl *(hv%hyai(2:nlev+1) + hv%hyai(1:nlev))
  hv%hybm = 0.5_rl *(hv%hybi(2:nlev+1) + hv%hybi(1:nlev))
  hv%etam = hv%hyam + hv%hybm

end subroutine

!_____________________________________________________________________
subroutine get_coordinates(lat,lon,hyam,hybm, i,j,k,elem,hvcoord)

  ! get lat,lon, vertical coords at node(i,j,k)

  real(rl),         intent(out):: lon,lat,hyam,hybm
  integer,          intent(in) :: i,j,k
  type(element_t),  intent(in) :: elem
  type(hvcoord_t),  intent(in) :: hvcoord

  ! get horizontal coordinates at column i,j
  lon  = elem%spherep(i,j)%lon
  lat  = elem%spherep(i,j)%lat

  ! get hybrid coeffiecients at midpoint of vertical level k
  hyam = hvcoord%hyam(k)
  hybm = hvcoord%hybm(k)

end subroutine

!_____________________________________________________________________
real(rl) function pressure_thickness(ps,k,hv)

  real(rl),         intent(in) :: ps
  integer,          intent(in) :: k
  type(hvcoord_t),  intent(in) :: hv
  pressure_thickness = (hv%hyai(k+1)-hv%hyai(k))*p0 + (hv%hybi(k+1)-hv%hybi(k))*ps

end function

!_____________________________________________________________________
real(rl) function q1_gaussian_hills(lat, lon, height) result(q1)
  !! DCMIP 2012 Gaussian-hills tracer used as Q1 in dcmip2012_test1_1
  !! (replaces the cosine-bell Q1 from test1_advection_deformation to eliminate
  !! the bell-edge first-derivative kink; see comment on gh_* parameters
  !! at module scope for context).  Sum of two Gaussians centered at
  !! (gh_lam0, gh_phi0) and (gh_lam1, gh_phi1) with a Gaussian vertical
  !! modulation about gh_z0.  Chord-distance-squared on the unit sphere
  !! matches the DCMIP q_gh formula in dcmip2012_test1_conv_mod.F90:133-142.
  real(rl), intent(in) :: lat, lon, height

  real(rl) :: xp, yp, zp, xc1, yc1, zc1, xc2, yc2, zc2
  real(rl) :: r2_1, r2_2, zshape

  xp  = cos(lat)*cos(lon)
  yp  = cos(lat)*sin(lon)
  zp  = sin(lat)
  xc1 = cos(gh_phi0)*cos(gh_lam0)
  yc1 = cos(gh_phi0)*sin(gh_lam0)
  zc1 = sin(gh_phi0)
  xc2 = cos(gh_phi1)*cos(gh_lam1)
  yc2 = cos(gh_phi1)*sin(gh_lam1)
  zc2 = sin(gh_phi1)
  r2_1 = (xp - xc1)**2 + (yp - yc1)**2 + (zp - zc1)**2
  r2_2 = (xp - xc2)**2 + (yp - yc2)**2 + (zp - zc2)**2
  zshape = exp(-((height - gh_z0)/gh_zz)**2)
  q1 = gh_hmax * zshape * (exp(-gh_b*r2_1) + exp(-gh_b*r2_2))
end function


!_____________________________________________________________________
subroutine set_tracers(q,nq, dp,i,j,k,lat,lon,elem)

  ! set tracer values at node(i,j,k)

  real(rl),         intent(in)    :: q(nq), dp, lat, lon
  integer,          intent(in)    :: i,j,k,nq
  type(element_t),  intent(inout) :: elem

  real(rl), parameter :: wl = 1.0 ! checkerboard wavelength in dg
  integer :: qi

  if (nq>qsize) call abortmp('qsize set too small for dcmip test case')
  ! set known tracers to q and the rest to a checkerboard pattern
  elem%state%Q(i,j,k,1:nq) = q

  ! compute tracer mass qdp from mixing ratio q
  do qi = 1,nq
    elem%state%Qdp (i,j,k,qi,:) = q(qi)*dp
  enddo

  ! set any remaining tracers to 1
  do qi = nq+1,qsize
     elem%state%Q(i,j,k,qi)    = 1
     elem%state%Qdp (i,j,k,qi,:) = elem%state%Q(i,j,k,qi)*dp
  enddo
end subroutine

subroutine dcmip2012_print_test1_conv_results(test_case, elem, tl, hvcoord, par, subnum)
  use time_mod, only: timelevel_t
  use parallel_mod, only: parallel_t

  character(len=*), intent(in) :: test_case
  type(element_t), intent(in) :: elem(:)
  type(timelevel_t), intent(in) :: tl
  type(hvcoord_t), intent(in) :: hvcoord
  type(parallel_t), intent(in) :: par
  integer, intent(in) :: subnum

  call test1_conv_print_results(test_case, elem, tl, hvcoord, par, subnum)
end subroutine dcmip2012_print_test1_conv_results

#ifdef HOMME_ENABLE_PARTMCSL
subroutine dcmip2012_test1_1_phys_to_dyn(elem, hybrid, hvcoord, tl, nets, nete)
  ! Map the partmcsl-advected physgrid tracer state pg_data%q back onto the
  ! GLL grid so the existing NetCDF writer (which reads elem%state%Q) can
  ! emit Q5..Q8.  No-op if pg_data%q has not been allocated -- i.e. this
  ! test was not the active one.
  use gllfvremap_mod, only: gfr_fv_phys_to_dyn, gfr_f2g_dss
  use time_mod,       only: TimeLevel_t, tstep
  use perf_mod,       only: t_startf, t_stopf

  type(element_t),   intent(inout) :: elem(:)
  type(hybrid_t),    intent(in)    :: hybrid
  type(hvcoord_t),   intent(in)    :: hvcoord
  type(TimeLevel_t), intent(in)    :: tl
  integer,           intent(in)    :: nets, nete

  integer  :: ie, qi
  real(rl) :: elapsed_time

  if (.not. allocated(pg_data%q)) return

  call t_startf('partmcsl_phys_to_dyn')
#ifdef PARTMCSL_SBR_DIAG
  !! DIAGNOSTIC ONLY (Q7 analytic-exact): overwrite pg_data%q(:,:,7,:) with the
  !! analytic SBR-rotated IC at FV centroids before the fv->gll projection.  See
  !! set_pg_q7_analytic_exact below for the rotation-axis derivation.  Revert
  !! before shipping.
  elapsed_time = real(tl%nstep, rl) * tstep
  call set_pg_q7_analytic_exact(elem, hvcoord, elapsed_time, nets, nete)
#endif

  ! gfr_fv_phys_to_dyn writes the new state into derived%FQ.  T and uv are
  ! treated as tendencies; we pass zero buffers so FT and FM are unchanged
  ! in any meaningful sense for this prescribed-wind test.
  call gfr_fv_phys_to_dyn(hybrid, tl%n0, hvcoord, elem, nets, nete, &
       pg_zero_T, pg_zero_uv, pg_data%q)
  call gfr_f2g_dss(hybrid, elem, nets, nete)

  ! Copy the partmcsl-advected slots back into state%Q so the NetCDF writer
  ! sees them.  Slots 1..4 are the dynamics-grid tracers; we leave those as
  ! the SL transport produced them.
  do ie = nets, nete
    do qi = 5, qsize
      elem(ie)%state%Q(:,:,:,qi) = elem(ie)%derived%FQ(:,:,:,qi)
    end do
  end do
  call t_stopf('partmcsl_phys_to_dyn')
end subroutine dcmip2012_test1_1_phys_to_dyn

#ifdef PARTMCSL_SBR_DIAG
subroutine set_pg_q7_analytic_exact(elem, hvcoord, time, nets, nete)
  !! DIAGNOSTIC ONLY (Q7 analytic-exact): overwrite pg_data%q(:,:,7,:) with the
  !! analytic SBR-rotated Q1 IC evaluated at FV cell centroids.  Q1 is
  !! the Gaussian-hills tracer defined by q1_gaussian_hills; see comment
  !! on the gh_* module parameters for the formula and rationale.  At the
  !! next output snapshot, Q7 in the NetCDF is the analytic exact solution
  !! and ||Q5 - Q7|| is partmcsl's true convergence error against that
  !! analytic exact -- independent of the SL Q reference (which was shown
  !! to be unreliable under the Test S SBR override; see
  !! partmcsl_half_order_sbr_handoff.md §6a.3).
  !!
  !! Rotation axis n = (-sin(sbr_alpha), 0, cos(sbr_alpha)), angular velocity
  !! omega = 2*pi / sbr_tau.  Derived by matching u,v in the Test S override at
  !! dcmip2012_test1_1 (this file) to omega x r; see
  !! partmcsl_half_order_sbr_handoff.md §6a.2 for the term-by-term derivation.
  !! To rotate a point at (lat, lon) at time t BACK to its t=0 origin, apply
  !! the Rodrigues formula with angle = -omega*t (both u_east and v_north
  !! signs verified against the override formulae).
  !!
  !! FV centroid (lat, lon) obtained from gfr_f_get_latlon; matches the
  !! centroid gllfvremap uses so gfr_fv_phys_to_dyn's projection back to GLL
  !! is self-consistent.  Partmcsl's ci ordering
  !! (low-a low-b, high-a low-b, high-a high-b, low-a high-b) maps to
  !! gllfvremap's flat k = nphys_side*(j-1) + i via the same (1,2,4,3)
  !! permutation used in partmcsl_advection.F90:170.
  !!
  !! Cell centroid vs cell mean: pg_data%q stores FV cell-mean tracer values,
  !! but this routine samples the analytic exact at the centroid.  For a
  !! smooth field the centroid-vs-mean offset is O(h^2*|∇^2 q|) per cell -- an
  !! order below partmcsl's O(h) target rate, so it does not corrupt the
  !! measured rate.  Revert before shipping.
  use gllfvremap_mod, only: gfr_f_get_latlon

  type(element_t),   intent(in)    :: elem(:)
  type(hvcoord_t),   intent(in)    :: hvcoord
  real(rl),          intent(in)    :: time   ! elapsed simulation time (s)
  integer,           intent(in)    :: nets, nete

  ! Vertical geometry constant for pressure -> height (matches
  ! test1_advection_deformation's H = Rd*T0/g with T0 = 300 K).
  real(rl), parameter :: T0_h = 300.0_rl
  real(rl), parameter :: H_h  = Rd * T0_h / g
  ! FV subcell layout for pg2 (nphys=2).
  integer,  parameter :: nphys_side_l = 2
  integer,  parameter :: nphys_cell_l = 4
  integer,  parameter :: pmcsl_ci_to_gfr_k(nphys_cell_l) = (/1, 2, 4, 3/)

  real(rl) :: omega_sbr, nx_axis, nz_axis, angle, cos_a, sin_a
  real(rl) :: lat_c, lon_c, x, y, z, dot, xr, yr, zr, rnorm
  real(rl) :: lat0, lon0, p_mid, height
  integer  :: ie, ci, k, kk, i_fv, j_fv

  if (.not. allocated(pg_data%q)) return

  omega_sbr = 2.0_rl * pi / sbr_tau
  nx_axis   = -sin(sbr_alpha)
  nz_axis   =  cos(sbr_alpha)
  angle     = -omega_sbr * time
  cos_a     = cos(angle)
  sin_a     = sin(angle)

  do ie = nets, nete
    do ci = 1, nphys_cell_l
      kk   = pmcsl_ci_to_gfr_k(ci)
      i_fv = mod(kk - 1, nphys_side_l) + 1
      j_fv = (kk - 1) / nphys_side_l + 1
      call gfr_f_get_latlon(ie, i_fv, j_fv, lat_c, lon_c)

      ! Rodrigues rotation about n = (nx_axis, 0, nz_axis) by `angle`.
      x = cos(lat_c)*cos(lon_c)
      y = cos(lat_c)*sin(lon_c)
      z = sin(lat_c)
      dot = nx_axis*x + nz_axis*z
      xr = x*cos_a + (-nz_axis*y)*sin_a          + nx_axis*dot*(1.0_rl - cos_a)
      yr = y*cos_a + ( nz_axis*x - nx_axis*z)*sin_a
      zr = z*cos_a + ( nx_axis*y)*sin_a          + nz_axis*dot*(1.0_rl - cos_a)
      rnorm = sqrt(xr*xr + yr*yr + zr*zr)
      xr = xr/rnorm; yr = yr/rnorm; zr = zr/rnorm
      lat0 = asin(max(-1.0_rl, min(1.0_rl, zr)))
      lon0 = atan2(yr, xr)

      do k = 1, nlev
        p_mid  = hvcoord%hyam(k)*p0 + hvcoord%hybm(k)*p0   ! ps = p0 for SBR
        height = H_h * log(p0 / p_mid)
        pg_data%q(ci, k, 7, ie) = q1_gaussian_hills(lat0, lon0, height)
      end do
    end do
  end do
end subroutine set_pg_q7_analytic_exact
#endif
#endif

end module dcmip12_wrapper

#endif

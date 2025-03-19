#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module partmc_sl_advection_mod

  use coordinate_systems_mod, only : cartesian3D_t, cartesian2D_t, &
                  spherical_polar_t, distance
  use control_mod, only: cubed_sphere_map
  use cube_mod, only: ref2sphere
  use dimensions_mod, only     : nlev, nlevp, np, nelemd
  use element_mod, only        : element_t
  use kinds, only              : real_kind, iulog
  use parallel_mod, only       : parallel_t, abortmp
  use physical_constants, only : rearth
  use time_mod, only           : TimeLevel_t

  implicit none
  private
  
  public :: partmcsl_init, partmcsl_finish
!   public :: partmcsl_step_forward
  
  ! we assume pg2 grid 
  integer, parameter :: nphys = 2, &
                        nverts = 4, & ! always quads
                        nphys_cell_per_elem = 4 ! 2 x 2 subcells per element
                        
  type :: local_fv_mesh_t
    type(cartesian3D_t), allocatable :: points(:, :) ! (nverts * nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: elem(:, :, :) ! (nverts, nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_local_id(:,:) ! (nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_global_id(:,:) !(nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_ij_corners(:,:,:) ! (2, 4, nelemd)
    integer, allocatable :: nneighbors(:) ! (nelemd)
  end type
  
!   elem%derived%vstar stores the velocity at the beginning of the tracer time step, t0
!   elem(ie)%state%v(:,:,:,:,tl%np1)stores v at t1
  
  !
  ! for testing
  !
  logical :: do_checks = .true.
  real(kind=real_kind), parameter :: fp_tol = 1e-14_real_kind

  ! Constants
  real(real_kind), parameter :: zero = 0.0_real_kind, &
                                 one = 1.0_real_kind
  
  
  type(local_fv_mesh_t), private :: fv_mesh
  
  contains

  subroutine partmcsl_init(par, elem)
    type(parallel_t), intent(in) :: par
    type(element_t), intent(in) :: elem(:)
    
    !
    integer :: ie, in, ci, vi, pt_idx, cell_idx, i, j, iloc, jloc
    type(spherical_polar_t) :: p_sph
    type(cartesian3D_t) :: p_cart, gll_cart
    integer :: num_neighbors ! elem(ie)'s number of neighboring elements, called "patch_size" in bndry_mod.F90
    integer :: max_num_neighbors
    real(real_kind) :: a, b, dist
    !
    integer :: facenum ! not used since we don't support cubed_sphere_map = 0, but needed for ref2sphere interface
    
    
    if (cubed_sphere_map /= 2) then
      call abortmp("partmcsl only supports cubed_sphere_map = 2.")
    endif
    
    ! allocate memory
    allocate(fv_mesh%nneighbors(nelemd))
    allocate(fv_mesh%gll_ij_corners(2,4,nelemd))
    max_num_neighbors = 0
    do ie = 1, nelemd
      num_neighbors = elem(ie)%desc%actual_neigh_edges + 1
      fv_mesh%nneighbors(ie) = num_neighbors
      if (num_neighbors > max_num_neighbors) max_num_neighbors = num_neighbors
    enddo
    allocate(fv_mesh%points(nverts * nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%elem(nverts, nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%gll_local_id(nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%gll_global_id(nphys_cell_per_elem * max_num_neighbors, nelemd))
    
    !--------------------------------------------
    ! construct local mesh for each local element
    !--------------------------------------------
    !
    !  fv cell j in elem(ie)'s mesh has vertices fv_mesh%elem(:,j,nelemd) in fv_mesh%points,
    !  where j = 1, ..., fv_mesh%nneighbors(ie).
    !  entries in fv_mesh%elem(:,j,ie) for j > fv_mesh%nneighbors(ie), if there are any,
    !  will be set to -1.  
    fv_mesh%elem = -1 
    
    do ie = 1, nelemd ! loop over elements owned by this rank
      pt_idx = 1
      cell_idx = 1
      ! create a local mesh of physics cells for each element
      do in = 1, fv_mesh%nneighbors(ie) 
      ! loop over element neighbors using corners defined by bndry_mod.F90
      ! and stored in elem(ie)%desc%neigh_corners(:,in)
        do ci = 1, 4 ! loop over subcells in element
          do vi = 1, 4 ! loop over vertices in subcell
            call ref_coords_ab(a, b, ci-1, vi-1) ! ref_coords_ab uses 0-based indexing
            ! see cube_mod.F90.  On output, p_cart has the sphere point's xyz coords.
            p_sph = ref2sphere(a, b, elem(ie)%desc%neigh_corners(:,in), cubed_sphere_map, elem(ie)%corners, facenum, p_cart)

            fv_mesh%points(pt_idx, ie) = p_cart
            fv_mesh%advected_points(pt_idx, ie) = p_cart
            fv_mesh%elem(vi, cell_idx, ie) = pt_idx
            
            pt_idx = pt_idx + 1
          enddo
          fv_mesh%gll_local_id(cell_idx, ie) = in 
          fv_mesh%gll_global_id(cell_idx, ie) = elem(ie)%desc%globalID_neigh_corners(in)
          cell_idx = cell_idx + 1
        enddo
      enddo
      
      do vi=1,4 ! loop over corners of current element, find gll i,j indices that match corner points
        iloc = -1
        jloc = -1
        do i=1,np
          do j=1,np
            gll_cart = change_coordinates(elem(ie)%spherep(i,j))
            dist = distance(elem(ie)%corners3D(vi), gll_cart)
            if (dist < fp_tol) then
              iloc = i
              jloc = j
            endif
          enddo
        enddo
        if (iloc == -1 .or. jloc == -1) then
          call abortmp("unable to match a corner with a gll node")
        endif
        ! use these i,j indices to pull the correct nodal velocity values to advect 
        ! our fv cells 
        fv_mesh%gll_ij_corners(1, vi, ie) = iloc
        fv_mesh%gll_ij_corners(2, vi, ie) = jloc
      enddo
    enddo
  end subroutine partmcsl_init
  
  subroutine partmcsl_finish()
    if (allocated(fv_mesh%points)) then 
      deallocate(fv_mesh%points)
      deallocate(fv_mesh%elem)
      deallocate(fv_mesh%nneighbors)
      deallocate(fv_mesh%gll_local_id)
      deallocate(fv_mesh%gll_global_id)
      deallocate(fv_mesh%gll_ij_corners)
    endif
  end subroutine  

  subroutine ref_coords_ab(a, b, subcell_idx, vert_idx) 
    real(real_kind), intent(out) :: a, b
    integer, intent(in) :: subcell_idx, vert_idx
    
    logical :: left, south
    
    left = ( vert_idx == 0 .or. vert_idx == 3)
    south = ( vert_idx < 2 )
    
    select case (subcell_idx)
      case (0)
        if (left) then
          a = -one
        else 
          a = zero
        endif
        if (south) then
          b = -one
        else 
          b = zero
        endif
      case (1)
        if (left) then
          a = zero
        else 
          a = one
        endif
        if (south) then
          b = -one
        else 
          b = zero
        endif
      case (2)
        if (left) then
          a = zero
        else 
          a = one
        endif
        if (south) then
          b = zero
        else 
          b = one
        endif
      case (3)
        if (left) then
          a = -one 
        else 
          a = zero
        endif
        if (south) then
          b = zero
        else 
          b = one
        endif
    end select
  end subroutine
 

  

  

  
!   subroutine partmcsl_step_forward(elem, dt, nets, nete, tl)
!     type (element_t)     , intent(inout) :: elem(:)
!     real(kind=real_kind) , intent(in   ) :: dt  ! time step size
!     integer              , intent(in   ) :: nets
!     integer              , intent(in   ) :: nete
!     type (TimeLevel_t)   , intent(in   ) :: tl 
!     ! local variables
!     integer :: num_neighbors
!     type(cartesian3D_t) :: advected_pts(np, np)
!     integer :: ie, k ! loop iterators
!     integer :: t1 ! time point 1 (end of advection timestep)
!     
!     ! TODO: barrier (if necessary)
!     ! TODO: timer start
! 
!     do ie = nets, nete
!       num_neighbors = elem(ie)%desc%actual_neigh_edges + 1
!       
!       do k=1, nlev
!         call partmcsl_elem_fwd_advection(advected_pts, elem(ie)%derived%vstar(:,:,:,k)
!           elem(ie)%state%v(:,:,:,k,tl%np1), elem, dt)
!       enddo
!     enddo
!     
!     ! TODO timer stop
!   end subroutine partmcsl_step_forward
!   
  subroutine partmcsl_elem_fwd_advection(acart, vt0, vt1, elem, dt)
    !
    !
    ! 
    !
    type(cartesian3D_t), intent(out) :: acart(np, np)  ! output: cartesian coordinates of advected pts
    real(kind=real_kind), intent(in) :: vt0(np, np, 2) ! input: spherical coordinate velocity components at beginning of tracer time step
    real(kind=real_kind), intent(in) :: vt1(np, np, 2) ! input: spherical coordinate velocity components at end of tracer time step
    type(element_t), intent(in) :: elem
    real(kind=real_kind), intent(in) :: dt
    !
    ! local variables
    !
    real (kind=real_kind) :: uxyz0(np,np,3), & ! velocity at beginning of advection time step
                             uxyz1(np,np,3), & ! velocity at end of advection time step
                             uxyzhalf(np,np,3) ! velocity at midpoint of advection time step
    integer :: i, j
    
    ! convert velocity from lat/lon to cartesian 3D
    ! see sl_advection.F90 subroutine ALE_departure_from_gll for explanation of this dot product
    do i=1,3
       uxyz0(:,:,i)=sum( elem%vec_sphere2cart(:,:,i,:)*vt0(:,:,:), 3)
       uxyz1(:,:,i)=sum( elem%vec_sphere2cart(:,:,i,:)*vt1(:,:,:), 3)
    end do
    uxyzhalf = 0.5_real_kind * (uxyz0 + uxyz1)
    ! 
    ! we can use velocity data at both time points, u(x0, t0) and u(x0, t1).
    ! however, since a simple temporal midpoint doesn't account
    ! for motion along the trajectory (note both are evaluated at x0), 
    ! don't expect anything better than first order; could be improved later.
    !
    do i=1,np
      do j=1,np
        acart(i,j) = change_coordinates(elem%spherep(i,j))
        acart(i,j)%x = acart(i,j)%x + dt * uxyzhalf(i,j,1)/rearth
        acart(i,j)%y = acart(i,j)%z + dt * uxyzhalf(i,j,2)/rearth
        acart(i,j)%z = acart(i,j)%z + dt * uxyzhalf(i,j,3)/rearth
      enddo
    enddo
  end subroutine partmcsl_elem_fwd_advection

end module partmc_sl_advection_mod
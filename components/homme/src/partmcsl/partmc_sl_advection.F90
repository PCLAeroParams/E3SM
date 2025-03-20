#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module partmc_sl_advection_mod
!   Terms like "elem" and "element" refer to Homme's spectral elements and related data structures.
!   Terms such as "cell" and "fv" refer to the physics grid's finite volume cells.

  use coordinate_systems_mod, only : cartesian3D_t, cartesian2D_t, &
                  spherical_polar_t, distance, change_coordinates
  use control_mod, only: cubed_sphere_map, dt_tracer_factor, dt_remap_factor
  use cube_mod, only: ref2sphere
  use dimensions_mod, only     : nlev, np, nelemd
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
  integer, parameter :: nphys = 2, & ! the "2" in pg2, 2 physics cell edges per spectral element edge
                        nverts = 4, & ! always quads
                        nphys_cell_per_elem = 4 ! 2 x 2 subcells per element
                        
  ! the maximum number elements than any 1 element can overlap after an advection time step
  ! (assumes timestep guarantees "halo-1" constraint)
  integer, parameter :: max_ndest_elem = 9
  ! the maximum number of fv cells that any 1 fv cell can overlap after an advection time step
  integer, parameter :: max_ndest = nphys_cell_per_elem * max_ndest_elem
                        

  !=====================================
  ! PartMCSL local finite volume mesh
  !
  ! One mesh for each element owned by this rank.
  ! For each elem(ie), ie = 1,...,nelemd:
  !
  !  Coordinates of fv cell vertices are kept in fv_mesh%points(:,ie).
  !
  !  Fell j in elem(ie)'s mesh has vertices fv_mesh%cells(:,j,ie) that point to fv_mesh%points,
  !  where j = 1, ..., 4*fv_mesh%nneighbors(ie).
  !  Entries in fv_mesh%cells(:,j,ie) for j > 4*fv_mesh%nneighbors(ie), if there are any,
  !  will be set to -1.
  !
  !  fv_mesh%gll_local_id(i,ie) gives the local index, in [1,num_neighbors], of the element
  !  that contains subcell i.
  !
  !  fv_mesh%gll_global_id(i,ie) gives Homme's global index of the element that contains
  !  subcell i.
  ! 
  !  fv_mesh%gll_ij_corners(:,1:4,ie) gives the (i,j) indices, for i,j in [1,np], of the gll nodes
  !  at the corners of elem(ie).
  !
  !  fv_mesh%nneighbors(ie) gives the number of neighboring elements that elem(ie) has.
  !
  !  fv_mesh%my_local_idx(ie) gives the local index, in [1,nneighbors(ie)], of elem(ie); 
  !  i.e., it's the index of "self" in the list of neighbors.
  ! 
  !  fv_mesh%subcell_idx(i, ie) gives the subcell index, in [1,4], of fv cell i relative to its
  !     enclosing element, fv_mesh%gll_local_id(i,ie).  
  type :: local_fv_mesh_t
    type(cartesian3D_t), allocatable :: points(:, :) ! (nverts * nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: cells(:, :, :) ! (nverts, nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: subcell_idx(:,:) ! (nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_local_id(:,:) ! (nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_global_id(:,:) !(nphys_cell_per_elem * max_num_neighbors, nelemd)
    integer, allocatable :: gll_ij_corners(:,:,:) ! (2, 4, nelemd)
    integer, allocatable :: nneighbors(:) ! (nelemd)
    integer, allocatable :: my_local_idx(:) ! (nelemd)
  end type
  
  !=====================================
  ! PartMCSL local source partition
  !
  ! dest_cell_idxs(j, k, ci, ie) for j in [1, ndest(k,ci,ie)] lists the local cell indices 
  !     (in a local_fv_mesh_t) of fv cells that subcell ci of elem(ie) sends to at level k.
  !     Its values are in [1,nphys_cell_per_elem*fv_mesh%nneighbors(ie)].
  !     Entries for j > ndest(k,ci,ie) are set to -1.
  ! dest_portions(j, k, ci, ie) for j in [1, ndest(k,ci,ie)] is the fraction, in [0,1], of
  !     subcell ci of elem(ie) that needs to be sent to fv_mesh cell dest_cell_idxs(j, k, ci, ie)
  !     at vertical level k.
  ! ndest(k,ci,ie) is the number of fv cells that subcell ci of elem(ie) sends to at
  !     vertical level k.
  type :: source_partition_t
    integer, allocatable :: dest_cell_idxs(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    real(real_kind), allocatable :: dest_portions(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: ndest(:,:,:) ! (nlev, nphys_cell_per_elem, nelemd)
  end type
  
!   elem%derived%vstar stores the velocity at the beginning of the tracer time step, t0
!   elem(ie)%state%v(:,:,:,:,tl%np1)stores v at t1
  
  !
  ! for testing
  !
  logical :: do_checks = .true.
  

  ! Constants
  real(real_kind), parameter :: zero = 0.0_real_kind, &
                                 one = 1.0_real_kind, &
                                half = 0.5_real_kind
  real(kind=real_kind), parameter :: fp_tol = 1e-14_real_kind
  
  
  type(local_fv_mesh_t), private :: fv_mesh
  type(source_partition_t), private :: src_partition
  
  contains

  subroutine partmcsl_init(par, elem)
    type(parallel_t), intent(in) :: par
    type(element_t), intent(in) :: elem(:)
    !
    ! local variables
    !
    integer :: ie, in, ci, vi, pt_idx, cell_idx, i, j, iloc, jloc    
    type(cartesian3D_t) :: p_cart, gll_cart
    real(real_kind) :: a, b, dist
    integer :: num_neighbors, max_num_neighbors
    !
    ! not used, but needed for interfaces
    !
    integer :: facenum 
    type(spherical_polar_t) :: p_sph
    
    !--------------------------------------------
    ! make sure we're ok to get started
    !--------------------------------------------
    if (cubed_sphere_map /= 2) then
      call abortmp("partmcsl only supports cubed_sphere_map = 2.")
    endif
    if (dt_tracer_factor > dt_remap_factor) then
      call abortmp("partmcsl requires dt_tracer_factor <= dt_remap_factor.")
    endif
    if (mod(dt_tracer_factor, dt_remap_factor) /= 0) then
      call abortmp("partmcsl requires dt_tracer_factor to be a multiple of dt_remap_factor.")
    endif
    
    !--------------------------------------------
    ! allocate memory for fv meshes
    !--------------------------------------------
    allocate(fv_mesh%nneighbors(nelemd))
    allocate(fv_mesh%gll_ij_corners(2,4,nelemd))
    allocate(fv_mesh%my_local_idx(nelemd))
    max_num_neighbors = 0
    do ie = 1, nelemd
      num_neighbors = elem(ie)%desc%actual_neigh_edges + 1
      fv_mesh%nneighbors(ie) = num_neighbors
      if (num_neighbors > max_num_neighbors) max_num_neighbors = num_neighbors
    enddo
    allocate(fv_mesh%points(nverts * nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%cells(nverts, nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%gll_local_id(nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%gll_global_id(nphys_cell_per_elem * max_num_neighbors, nelemd))
    allocate(fv_mesh%subcell_idx(nphys_cell_per_elem * max_num_neighbors, nelemd))
    
    !--------------------------------------------
    ! allocate memory for source partitions
    !--------------------------------------------
    allocate(src_partition%dest_cell_idxs(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    allocate(src_partition%dest_portions(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    allocate(src_partition%ndest(nlev, nphys_cell_per_elem, nelemd))
    src_partition%ndest = 0
    src_partition%dest_cell_idxs = -1
    src_partition%dest_portions = zero
    
    !--------------------------------------------
    ! construct local mesh for each local element
    !--------------------------------------------
    fv_mesh%cells = -1 
    fv_mesh%my_local_idx = -1
    fv_mesh%gll_ij_corners = -1
    
    do ie = 1, nelemd ! loop over elements owned by this rank
      pt_idx = 1
      cell_idx = 1
      ! create a local mesh of physics cells for each element
      do in = 1, fv_mesh%nneighbors(ie) ! loop over element neighbors
      ! neighbor corners defined by bndry_mod.F90
      ! and stored in elem(ie)%desc%neigh_corners(:,in)
      
        ! find this element is in its own neighbors list
        if (elem(ie)%GlobalId == elem(ie)%desc%globalID_neigh_corners(in)) then
          fv_mesh%my_local_idx(ie) = in
          if (par%masterproc) then
            write(iulog,*) 'partmcsl init: "elem self" is local index ', in
          endif
        endif
                
        do ci = 1, nphys_cell_per_elem ! loop over subcells in element
          do vi = 1, nverts ! loop over vertices in subcell
          
            ! subcells are defined in the reference quadrilateral's (a,b) coordinates;
            ! see subroutine ref_coords_ab.
            call ref_coords_ab(a, b, ci-1, vi-1) ! ref_coords_ab uses 0-based indexing
            
            ! spherical coordinates of subcells are then defined through the reference quad.
            ! to sphere map.
            !
            ! see cube_mod.F90.  On output, p_cart has the sphere point's xyz coords.
            p_sph = ref2sphere(a, b, elem(ie)%desc%neigh_corners(:,in), cubed_sphere_map, elem(ie)%corners, facenum, p_cart)

            fv_mesh%points(pt_idx, ie) = p_cart
            fv_mesh%cells(vi, cell_idx, ie) = pt_idx
            
            pt_idx = pt_idx + 1
          enddo ! loop over vertices in subcell
          fv_mesh%subcell_idx(cell_idx, ie) = ci
          fv_mesh%gll_local_id(cell_idx, ie) = in 
          fv_mesh%gll_global_id(cell_idx, ie) = elem(ie)%desc%globalID_neigh_corners(in)
          cell_idx = cell_idx + 1
        enddo ! loop over subcells in element
      enddo ! loop over element neighbors
      
      !--------------------------------------------
      ! Match gll node indices (i,j) for i,j in [1,np] to elem(ie) corners
      !--------------------------------------------
      !
      ! This is likely a convention defined in Homme, which means we don't need
      ! to do this search procedure.  However, since our local mesh indexing may differ
      ! from Homme's indexing, we'll do it this way to be sure. 
      !
      do vi=1,4 ! loop over corners of current element, find gll i,j indices that match corner points
        iloc = -1
        jloc = -1
        
        ! get the element corner
        call ref_coords_ab(a, b, vi-1, vi-1)
        p_sph = ref2sphere(a, b, elem(ie)%corners3D, cubed_sphere_map, elem(ie)%corners, facenum, p_cart)
        
        ! find matching nodal indices
        do i=1,np
          do j=1,np
            gll_cart = change_coordinates(elem(ie)%spherep(i,j))
            dist = distance(p_cart, gll_cart)
            if (dist < fp_tol) then
              ! match found
              iloc = i
              jloc = j
            endif
          enddo
        enddo
        if (iloc == -1 .or. jloc == -1) then
          call abortmp("unable to match a corner with a gll node")
        endif
        ! use these i,j indices to pull the correct nodal velocity values to advect 
        ! our element corners
        fv_mesh%gll_ij_corners(1, vi, ie) = iloc
        fv_mesh%gll_ij_corners(2, vi, ie) = jloc
      enddo ! loop over corners of current element
    enddo ! loop over elements owned by this rank
    
    ! check to make sure we found each element in the sets of neighbors
    do ie = 1, nelemd
      if (fv_mesh%my_local_idx(ie) == -1) then
        call abortmp('elem "self" not found in neighbors')
      endif
    enddo
  end subroutine partmcsl_init
  
  subroutine partmcsl_finish()
    if (allocated(fv_mesh%points)) then 
      deallocate(fv_mesh%points)
      deallocate(fv_mesh%cells)
      deallocate(fv_mesh%gll_local_id)
      deallocate(fv_mesh%gll_global_id)
      deallocate(fv_mesh%gll_ij_corners)
      deallocate(fv_mesh%nneighbors)
      deallocate(fv_mesh%my_local_idx)
      deallocate(fv_mesh%subcell_idx)
    endif
    if (allocated(src_partition%ndest)) then
      deallocate(src_partition%dest_cell_idxs)
      deallocate(src_partition%dest_portions)
      deallocate(src_partition%ndest)
    endif
  end subroutine partmcsl_finish

  subroutine ref_coords_ab(a, b, subcell_idx, vert_idx) 
    !   Warning: subcell_idx and vert_idx are 0-based indices.
    ! 
    !       Given a subcell index, subcell_idx, return the (a,b) 
    !       reference coordinates of the vertex at vert_idx,
    !       in ccw order starting at SW corner.
    ! 
    !                 (0,1)
    !      (-1,1) o----x----o (1,1)
    !             | 3  | 2  |
    !             |    |    |
    !      (-1,0) x----x----x (1,0)
    !             | 0  | 1  |
    !             |    |    |
    !     (-1,-1) o----x----o (1,-1)
    !                 (0,-1)
    !       
    real(real_kind), intent(out) :: a, b ! output: (a,b) coordinates in ref. quad.
    integer, intent(in) :: subcell_idx, vert_idx ! input: *0-based* subcell and vertex indices
    ! local
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
  
  subroutine partmcsl_step_forward(elem, dt, nets, nete, tl)
    type (element_t)     , intent(inout) :: elem(:)
    real(kind=real_kind) , intent(in   ) :: dt  ! time step size
    integer              , intent(in   ) :: nets ! thread starting element idx in [1,nelemd]
    integer              , intent(in   ) :: nete ! thread ending element idx in [1,nelemd]
    type (TimeLevel_t)   , intent(in   ) :: tl 
    ! local variables
    type(cartesian3D_t) :: advected_pts(nverts * nphys_cell_per_elem)
    integer :: ie, k ! loop iterators
    integer :: t1 ! time point 1 (end of advection timestep)
    
    ! TODO: barrier (if necessary)
    ! TODO: timer start

    do ie = nets, nete ! loop over elements worked by this thread
      do k=1, nlev ! loop over vertical levels
        call partmcsl_fwd_advection(advected_pts, elem(ie)%derived%vstar(:,:,:,k), &
          elem(ie)%state%v(:,:,:,k,tl%np1), fv_mesh, elem, ie, dt)
      enddo
    enddo ! loop over elements worked by this thread

    ! TODO: barrier (if necessary)    
    ! TODO timer stop
  end subroutine partmcsl_step_forward
 
  subroutine partmcsl_fwd_advection(acart, vt0, vt1, fvm, elem, ie, dt)
    type(cartesian3D_t), intent(out) :: acart(16)  ! output: cartesian coordinates of advected fv cell corners
    real(kind=real_kind), intent(in) :: vt0(np, np, 2) ! input: spherical coordinate velocity components at beginning of tracer time step
    real(kind=real_kind), intent(in) :: vt1(np, np, 2) ! input: spherical coordinate velocity components at end of tracer time step
    type(local_fv_mesh_t), intent(in) :: fvm
    type(element_t), intent(in) :: elem(:)
    integer, intent(in) :: ie
    real(kind=real_kind), intent(in) :: dt
    !
    ! local variables
    !    
    real(kind=real_kind) :: uxyz0(3,4), uxyz1(3,4), uxyzhalf(3,4)
    type(cartesian3D_t) :: adv_corners(4)
    type(spherical_polar_t) :: p_sph
    integer :: i, j, ci, cj, vi, pt_idx, facenum
    real(kind=real_kind) :: a, b
    
    !--------------------------------------------
    ! Step 1: Advect element forward.
    !--------------------------------------------
    ! gather velocity at element corners
    do vi=1,4
      ci = fvm%gll_ij_corners(1,vi,ie)
      cj = fvm%gll_ij_corners(2,vi,ie)
      ! convert velocity from lat/lon to cartesian 3D
      ! see cube_mod.F90 vec_sphere2cart and sl_advection.F90 subroutine ALE_departure_from_gll 
      ! for explanation of this dot product      
      do i=1,3
        uxyz0(i,vi) = sum(elem(ie)%vec_sphere2cart(ci,cj,i,:) * vt0(ci, cj, :))
        uxyz1(i,vi) = sum(elem(ie)%vec_sphere2cart(ci,cj,i,:) * vt1(ci, cj, :))
      enddo
    enddo
    uxyzhalf = half * (uxyz0 + uxyz1)    
    ! 
    ! We use velocity data at both time points, u(x0, t0) and u(x0, t1).
    ! However, since a simple temporal midpoint doesn't account
    ! for motion along the trajectory (note both velocities are evaluated at x0), 
    ! we don't expect anything better than first order. This could be improved later.
    !
    do vi=1,4
      ci = fvm%gll_ij_corners(1,vi,ie)
      cj = fvm%gll_ij_corners(2,vi,ie)
      adv_corners(vi) = change_coordinates(elem(ie)%spherep(ci,cj))
      adv_corners(vi)%x = adv_corners(vi)%x + dt * uxyzhalf(1,vi)/rearth
      adv_corners(vi)%y = adv_corners(vi)%y + dt * uxyzhalf(2,vi)/rearth
      adv_corners(vi)%z = adv_corners(vi)%z + dt * uxyzhalf(3,vi)/rearth
    enddo
    
    !--------------------------------------------
    ! Step 2: Reconstruct advected subcell points from advected element corners
    !--------------------------------------------
    pt_idx = 1
    do ci = 1, 4 ! loop over subcells in element
      do vi = 1, 4 ! loop over vertices in subcell
        call ref_coords_ab(a, b, ci-1, vi-1) ! ref_coords_ab uses 0-based indexing
        p_sph = ref2sphere(a, b, adv_corners, cubed_sphere_map, elem(ie)%corners, facenum, acart(pt_idx))
        pt_idx = pt_idx + 1
      enddo
    enddo
    
  end subroutine partmcsl_fwd_advection


  

  


end module partmc_sl_advection_mod
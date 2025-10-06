#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module partmcsl_advection_mod
!   Terms like "elem" and "element" refer to Homme's spectral elements and related data structures.
!   Terms such as "cell" and "fv" refer to the physics grid's finite volume cells.

  use coordinate_systems_mod, only : cartesian3D_t, cartesian2D_t, &
                  spherical_polar_t, distance, change_coordinates, sphere_tri_area
  use control_mod, only: cubed_sphere_map, dt_tracer_factor, dt_remap_factor
  use cube_mod, only: ref2sphere
  use dimensions_mod, only     : nlev, np, nelemd
  use element_mod, only        : element_t
  use kinds, only              : real_kind, iulog
  use parallel_mod, only       : parallel_t, abortmp
  use physical_constants, only : rearth
  use time_mod, only           : TimeLevel_t
  use partmcsl_mod

  implicit none
  private
  
  public :: partmcsl_init, partmcsl_finalize, partmcsl_test
  public :: partmcsl_step_forward
  
  
                        

  !=====================================
  ! PartMCSL local finite volume mesh
  !
  ! One local mesh for each element owned by this rank...
  ! For each elem(ie), ie = 1,...,nelemd:
  !
  !  fv_mesh%points(1:4, ci, in, ie) give the coordinates of the 4 vertices of fv subcell
  !     ci (in [1,4]) of element `in` (in [1, nneighbors]) in elem(ie)'s neighbor list.
  !  fv_mesh%elem_global_id(i,ie) gives Homme's global index of the element that contains
  !  subcell i.
  ! 
  !  fv_mesh%elem_ij_corners(:,1:4,ie) gives the (i,j) indices, for i,j in [1,np], of the gll nodes
  !  at the corners of elem(ie).
  !
  !  fv_mesh%nneighbors(ie) gives the number of neighboring elements that elem(ie) has.
  !
  !  fv_mesh%my_elem_local_idx(ie) gives the local index, in [1,nneighbors(ie)], of elem(ie); 
  !  i.e., it's the index of "self" in the list of neighbors.
  ! 
  !  fv_mesh%subcell_idx(i, ie) gives the subcell index, in [1,4], of fv cell i relative to its
  !     enclosing element, fv_mesh%elem_local_id(i,ie).  
  type :: local_fv_mesh_t
    type(cartesian3D_t), allocatable :: points(:,:,:,:) ! (nverts, nphys_cell_per_elem, max_num_neighbors, nelemd)
    real(real_kind), allocatable :: subcell_area(:,:,:) ! (nphys_cell_per_elem, max_num_neighbors, nelemd)
    integer, allocatable :: elem_global_id(:,:,:) !(nphys_cell_per_elem, max_num_neighbors, nelemd)
!     integer, allocatable :: elem_ij_corners(:,:,:) ! (2, 4, nelemd)
    integer, allocatable :: nneighbors(:) ! (nelemd)
    integer, allocatable :: my_elem_local_idx(:) ! (nelemd)
    integer :: max_nneighbors
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
    integer, allocatable :: ndest(:,:,:) ! (nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: dest_cell_idxs(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    real(real_kind), allocatable :: dest_portions(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)    
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
    integer :: ie, in, ci, vi, i, j, iloc, jloc    
    type(cartesian3D_t) :: p_cart, elem_cart
    real(real_kind) :: a, b, dist, elem_area, elem_area_sum, atmp
    integer :: num_neighbors, max_num_neighbors
    logical :: error_out
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
    if (dt_tracer_factor /= dt_remap_factor) then
      call abortmp("partmcsl requires dt_tracer_factor == dt_remap_factor.")
    endif
    
    if (par%masterproc) then
      write(iulog,*) 'partmcsl: entering partmcsl_init, nelemd ', nelemd
    endif
    
    !--------------------------------------------
    ! allocate memory for fv meshes
    !--------------------------------------------
    allocate(fv_mesh%nneighbors(nelemd))
    allocate(fv_mesh%my_elem_local_idx(nelemd))
    fv_mesh%my_elem_local_idx = -1
    max_num_neighbors = 0
    do ie = 1, nelemd
      num_neighbors = elem(ie)%desc%actual_neigh_edges + 1
      fv_mesh%nneighbors(ie) = num_neighbors
      if (num_neighbors > max_num_neighbors) max_num_neighbors = num_neighbors
    enddo
    
    if (minval(fv_mesh%nneighbors) < 9) then
      if (minval(fv_mesh%nneighbors) /= 8) then
        call abortmp("partmcsl expects 8 or 9 neighbors for each element.")
      endif
    else 
      if (max_num_neighbors /= 9) then
        call abortmp("partmcsl does assumes regular cubed sphere meshes (no RRM).")
      endif
    endif
!     if (par%masterproc) then
!       write(iulog,*) 'partmcsl init: ie = ', ie, ' max_num_neighbors = ', max_num_neighbors, ' nneighbors = ', fv_mesh%nneighbors
!     endif
    
    fv_mesh%max_nneighbors = max_num_neighbors
    allocate(fv_mesh%points(nverts, nphys_cell_per_elem, max_num_neighbors, nelemd))
    allocate(fv_mesh%elem_global_id(nphys_cell_per_elem, max_num_neighbors, nelemd))
    allocate(fv_mesh%subcell_area(nphys_cell_per_elem, max_num_neighbors, nelemd))
    fv_mesh%elem_global_id = -1
    fv_mesh%subcell_area = zero
    
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
    do ie = 1, nelemd ! loop over elements owned by this rank
      !
      ! create a local mesh of physics cells 
      !
      do in = 1, fv_mesh%nneighbors(ie) ! loop over element neighbors
        ! neighbor corners defined by bndry_mod.F90 and stored in 
        !           elem(ie)%desc%neigh_corners(:,in)
        ! 
        ! find this element is in its own neighbors list
        if (elem(ie)%GlobalId == elem(ie)%desc%globalID_neigh_corners(in)) then
          fv_mesh%my_elem_local_idx(ie) = in
        endif
                
        do ci = 1, nphys_cell_per_elem ! loop over subcells in element
          do vi = 1, nverts ! loop over vertices in subcell
            ! subcells are defined in the reference quadrilateral's (a,b) coordinates;
            ! see subroutine ref_coords_ab.
            call ref_coords_ab(a, b, ci-1, vi-1) ! ref_coords_ab uses 0-based indexing
            ! spherical coordinates of subcells are then defined through the reference quad.
            ! to sphere map.
            ! see cube_mod.F90.  On output, p_cart has the sphere point's xyz coords.
            p_sph = ref2sphere(a, b, elem(ie)%desc%neigh_corners(:,in), cubed_sphere_map, elem(ie)%corners, facenum, p_cart)
            fv_mesh%points(vi, ci, in, ie) = p_cart
          enddo ! loop over vertices in subcell
          fv_mesh%elem_global_id(ci, in, ie) = elem(ie)%desc%globalID_neigh_corners(in)
    !           call sphere_tri_area(fv_mesh%points(1, ci, in, ie), &
    !                           fv_mesh%points(2, ci, in, ie), &
    !                           fv_mesh%points(3, ci, in, ie), fv_mesh%subcell_area(ci, in, ie))
    !           call sphere_tri_area(fv_mesh%points(1, ci, in, ie), &
    !                           fv_mesh%points(3, ci, in, ie), &
    !                           fv_mesh%points(4, ci, in, ie), atmp)
    !           fv_mesh%subcell_area(ci, in, ie) = fv_mesh%subcell_area(ci, in, ie) + atmp
          fv_mesh%subcell_area(ci, in, ie) = &
              tri_area(fv_mesh%points(1, ci, in, ie), &
                       fv_mesh%points(2, ci, in, ie), &
                       fv_mesh%points(3, ci, in, ie)) + &
              tri_area(fv_mesh%points(1, ci, in, ie), &
                       fv_mesh%points(3, ci, in, ie), &
                       fv_mesh%points(4, ci, in, ie))
        enddo ! loop over subcells in element
      enddo ! loop over element neighbors
      
!       do vi=1,4
!         call ij_idx_from_corner_idx(iloc, jloc, vi)
!         fv_mesh%elem_ij_corners(1, vi, ie) = iloc
!         fv_mesh%elem_ij_corners(2, vi, ie) = jloc
!       enddo
    enddo ! loop over elements owned by this rank
       
    !
    ! check to make sure we found each element in the sets of neighbors
    !
    do ie = 1, nelemd
      if (fv_mesh%my_elem_local_idx(ie) == -1) then
        call abortmp('elem "self" not found in neighbors')
      endif
    enddo
    
    call partmcsl_init_local(nelemd)
    call init_local_meshes(nelemd, fv_mesh%nneighbors, fv_mesh%my_elem_local_idx, &
      fv_mesh%points, fv_mesh%subcell_area)
    
    if (par%masterproc) then
      write(iulog,*) 'partmcsl: exiting partmcsl_init'
    endif
end subroutine partmcsl_init
  
subroutine ij_idx_from_corner_idx(iloc, jloc, corner_idx)
    integer, intent(out) :: iloc, jloc
    integer, intent(in) :: corner_idx
    select case (corner_idx)
        case (1)
            iloc = 1; jloc = 1
        case (2)
            iloc = 4; jloc = 1
        case (3)
            iloc = 4; jloc = 4
        case (4)
            iloc = 1; jloc = 4
        case default
            call abortmp("corner_idx out of range")
    end select
    !     if (corner_idx == 1) then
    !         iloc = 1; jloc = 1
    !     else if (corner_idx == 2) then
    !         iloc = 4; jloc = 1
    !     else if (corner_idx == 3) then
    !         iloc = 4; jloc = 4
    !     else if (corner_idx == 4) then
    !         iloc = 1; jloc = 4
    !     endif
end subroutine 

subroutine partmcsl_test(par, elem)
  type(parallel_t), intent(in) :: par
  type(element_t), intent(in) :: elem(:)
  
  if (do_checks) then
      call check_ij_corners(par, elem)
      call partmcsl_check_elem_area(par, elem)
  endif
  if (par%masterproc) then
      write(iulog,*) "partmcsl_test: all tests passed."
  endif
end subroutine   
  
subroutine check_ij_corners(par, elem)
    type(parallel_t), intent(in) :: par
    type(element_t), intent(in) :: elem(:)
    !
    integer :: vi, iloc, jloc, ie, i, j
    type(cartesian3D_t) :: p_cart, elem_cart
    real(real_kind) :: a, b, dist
    integer :: facenum ! not used, but needed for interfaces
    type(spherical_polar_t) :: p_sph ! not used, but needed for interfaces
    logical :: error_out
    
    do ie = 1, nelemd
      do vi=1,4 ! loop over corners of current element, find gll i,j indices that match corner points
        iloc = -1
        jloc = -1
        
        ! get the element corner
        call ref_coords_ab(a, b, vi-1, vi-1)
        p_sph = ref2sphere(a, b, elem(ie)%corners3D, cubed_sphere_map, elem(ie)%corners, facenum, p_cart)
        
        ! find matching nodal indices
        do i=1,np
          do j=1,np
            elem_cart = change_coordinates(elem(ie)%spherep(i,j))
            dist = distance(p_cart, elem_cart)
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
    !         write(iulog,*) 'partmcsl init: corner ', vi, ' has (i,j) index ', fv_mesh%elem_ij_corners(:, vi, ie)
        error_out = .false.
        if (vi == 1) then
            if (iloc /= 1 .or. jloc /= 1) then
                write(iulog,*) 'partmcsl init: corner ', vi, ' has (i,j) index ', iloc, jloc
                error_out = .true.
            endif
        else if (vi == 2) then
            if (iloc /= 4 .or. jloc /= 1) then
                write(iulog,*) 'partmcsl init: corner ', vi, ' has (i,j) index ', iloc, jloc
                error_out = .true.
            endif
        else if (vi == 3) then
            if (iloc /= 4 .or. jloc /= 4) then
                write(iulog,*) 'partmcsl init: corner ', vi, ' has (i,j) index ', iloc, jloc
                error_out = .true.
            endif
        else
            if (iloc /= 1 .or. jloc /= 4) then
                write(iulog,*) 'partmcsl init: corner ', vi, ' has (i,j) index ', iloc, jloc
                error_out = .true.
            endif
        endif
      enddo
      if (error_out) then
        call abortmp('partmcsl_init error: unexpected indices in ij corners.')
      endif
    enddo
    if (par%masterproc) then
        write(iulog,*) "partmcsl_test: check_ij_corners passed."
    endif
end subroutine 
  
  subroutine partmcsl_check_elem_area(par, elem)
    type(parallel_t), intent(in) :: par
    type(element_t), intent(in) :: elem(:)
    real(real_kind) :: elem_area_sum, elem_area
    integer :: ie, ci    
    !
    ! check that subcell areas sum to element area
    !
    do ie=1, nelemd
        elem_area_sum = zero
        do ci = 1, nphys_cell_per_elem
          elem_area_sum = elem_area_sum + fv_mesh%subcell_area(ci, fv_mesh%my_elem_local_idx(ie), ie)
          if (fv_mesh%subcell_area(ci, fv_mesh%my_elem_local_idx(ie), ie) < fp_tol) then
            write(iulog,*) 'partmcsl init: zero area subcell found.'
            call abortmp('partmcsl init: zero area subcell found.')
          endif
        enddo
        elem_area = tri_area(elem(ie)%corners3D(1), &
                           elem(ie)%corners3D(2), &
                           elem(ie)%corners3D(3)) + &
                  tri_area(elem(ie)%corners3D(1), &
                           elem(ie)%corners3D(3), &
                           elem(ie)%corners3D(4))          
        if (abs(elem_area - elem_area_sum) > fp_tol) then
          write(iulog,*) 'partmcsl init: elem_area_sum = ', elem_area_sum, &
                        ' elem_area = ', elem_area, &
                        ' abs(diff) = ', abs(elem_area - elem_area_sum) 
          call abortmp("partmcsl element area mismatch.")
        endif
    enddo
!     write(iulog,*) "partmcsl_test: check_elem_area passed."
  end subroutine
  
  subroutine partmcsl_finalize()
    if (allocated(fv_mesh%points)) then 
      deallocate(fv_mesh%points)
!       deallocate(fv_mesh%elem_local_id)
      deallocate(fv_mesh%elem_global_id)
!       deallocate(fv_mesh%elem_ij_corners)
      deallocate(fv_mesh%nneighbors)
      deallocate(fv_mesh%my_elem_local_idx)
      deallocate(fv_mesh%subcell_area)
!       deallocate(fv_mesh%subcell_idx)
    endif
    if (allocated(src_partition%ndest)) then
      deallocate(src_partition%dest_cell_idxs)
      deallocate(src_partition%dest_portions)
      deallocate(src_partition%ndest)
    endif
  end subroutine partmcsl_finalize

  
  
  subroutine partmcsl_step_forward(elem, dt, nets, nete, tl)
    use iso_c_binding, only: c_int
    type (element_t)     , intent(inout) :: elem(:)
    real(kind=real_kind) , intent(in   ) :: dt  ! time step size
    integer              , intent(in   ) :: nets ! thread starting element idx in [1,nelemd]
    integer              , intent(in   ) :: nete ! thread ending element idx in [1,nelemd]
    type (TimeLevel_t)   , intent(in   ) :: tl 
    ! local variables
    type(cartesian3D_t) :: advected_pts(nverts, nphys_cell_per_elem)
    integer :: ie, k ! loop iterators
    integer :: t1 ! time point 1 (end of advection timestep)
    integer :: di, ci, dest_idx, src_idx
    real(kind=real_kind) :: dest_frac
!     integer(kind=c_int) :: test_array(5)
!     
!     test_array = 5
!     call test_const_int_array1(test_array, 5)
    
    ! TODO: barrier (if necessary)
    ! TODO: timer start

    do ie = nets, nete ! loop over elements worked by this thread
      do k=1, nlev ! loop over vertical levels
      
        !------------------------
        ! step 1: advect fv cells forward
        call partmcsl_fwd_advection(advected_pts, elem(ie)%derived%vstar(:,:,:,k), &
          elem(ie)%state%v(:,:,:,k,tl%np1), fv_mesh, elem, ie, dt)
        write(iulog,*) "partmcsl_step_forward: advection done at elem ", ie, " lev ", k
        !------------------------
        ! step 2: compute overlap portions (c++)
        call calc_src_partition(ie, nelemd, fv_mesh%nneighbors(ie), fv_mesh%my_elem_local_idx(ie), &
             k, nlev, fv_mesh%points, fv_mesh%subcell_area, advected_pts, src_partition%ndest, & 
             src_partition%dest_cell_idxs, src_partition%dest_portions)
        !------------------------
        ! step 3: move partmc particles
        do ci=1,4 ! loop over subcells owned by this element
          ! TODO: get partmc instance from source cell
          do di=1, src_partition%ndest(k,ci,ie)
            ! TODO: get partmc instance from destination cell
            dest_idx = src_partition%dest_cell_idxs(di, k, ci, ie)
            dest_frac = src_partition%dest_portions(di, k, ci, ie)
            !------------------------
            ! step 3a: accumulate particle info, make sure send/receive buffers are big enough
            !------------------------
            ! step 3b: send particles from src_idx to dest_idx
            ! TODO: send dest_frac of src particles from src_idx to dst_idx
            ! TODO: PartMC MPI send/receive subroutines
          enddo
        enddo
      enddo
    enddo ! loop over elements worked by this thread

    ! TODO: barrier (if necessary)    
    ! TODO timer stop
  end subroutine partmcsl_step_forward
 
  subroutine partmcsl_fwd_advection(acart, vt0, vt1, fvm, elem, ie, dt)
    type(cartesian3D_t), intent(out) :: acart(4,4)  ! output: cartesian coordinates of advected fv cell corners ; shared corners are duplicated -- could be changed later.
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
      call ij_idx_from_corner_idx(ci, cj, vi)
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
    ! we don't expect anything better than first order. 
    !
    do vi=1,4
      call ij_idx_from_corner_idx(ci, cj, vi)
      adv_corners(vi) = change_coordinates(elem(ie)%spherep(ci,cj))
      adv_corners(vi)%x = adv_corners(vi)%x + dt * uxyzhalf(1,vi)/rearth
      adv_corners(vi)%y = adv_corners(vi)%y + dt * uxyzhalf(2,vi)/rearth
      adv_corners(vi)%z = adv_corners(vi)%z + dt * uxyzhalf(3,vi)/rearth
    enddo
    
    !--------------------------------------------
    ! Step 2: Reconstruct advected subcell points from advected element corners
    !--------------------------------------------
    do ci = 1, 4 ! loop over subcells in element
      do vi = 1, 4 ! loop over vertices in subcell
        call ref_coords_ab(a, b, ci-1, vi-1) ! ref_coords_ab uses 0-based indexing
        p_sph = ref2sphere(a, b, adv_corners, cubed_sphere_map, elem(ie)%corners, facenum, acart(vi, ci))
      enddo
    enddo
    
  end subroutine partmcsl_fwd_advection  

end module partmcsl_advection_mod
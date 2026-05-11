#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module partmcsl_advection_mod
!   Terms like "elem" and "element" refer to Homme's spectral elements and related data structures.
!   Terms such as "cell" and "fv" refer to the physics grid's finite volume cells.

  use bndry_mod, only          : ghost_exchangeVfull
  use coordinate_systems_mod, only : cartesian3D_t, cartesian2D_t, &
                  spherical_polar_t, distance, change_coordinates, sphere_tri_area
  use control_mod, only: cubed_sphere_map, dt_tracer_factor, dt_remap_factor
  use cube_mod, only: ref2sphere
  use dimensions_mod, only     : nlev, np, nelemd
  use edge_mod, only           : initGhostBuffer3D, FreeGhostBuffer3D
  use edgetype_mod, only       : GhostBuffer3D_t
  use element_mod, only        : element_t
  use kinds, only              : real_kind, iulog
  use parallel_mod, only       : parallel_t, abortmp
  use perf_mod, only           : t_startf, t_stopf
  use physical_constants, only : rearth
  use time_mod, only           : TimeLevel_t
  use partmcsl_mod

  implicit none
  private

  public :: partmcsl_init, partmcsl_finalize, partmcsl_test
  public :: partmcsl_step_forward
  public :: partmcsl_exchange_source_partition
  public :: source_partition_t, src_partition
  
  
                        

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

  !=====================================
  ! PartMCSL arrival partition (foreign contributions received from other ranks)
  !
  ! For each element je owned by this rank, for each destination subcell cj of je
  ! and each vertical level k:
  !   nsrc(k, cj, je) is the number of foreign-source records contributing to (je, cj, k).
  !   src_gid(d, k, cj, je) is the GlobalID of the source element on the remote rank.
  !   src_subcell(d, k, cj, je) is the source subcell within that foreign element [1..4].
  !   src_frac(d, k, cj, je) is the fraction of the source cell delivered into (je, cj).
  !
  ! Self-contributions and contributions from other locally-owned source elements are
  ! NOT stored here; they are read directly from src_partition by tests/consumers that
  ! need the local+remote sum.
  type :: arrival_partition_t
    integer, allocatable :: nsrc(:,:,:)             ! (nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: src_gid(:,:,:,:)        ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: src_subcell(:,:,:,:)    ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    real(real_kind), allocatable :: src_frac(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
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

  !=====================================
  ! Ghost-exchange buffer sizing for source-partition exchange.
  !
  ! Per element, per vertical level, the packed payload is:
  !   nphys_cell_per_elem ndest values (one per source subcell), cast int->real
  !   nphys_cell_per_elem * max_ndest * 3 record words: (gid_dest, subcell_dest, frac)
  !
  ! Records beyond ndest(k,ci,ie) are zero-padded; ndest is the trusted count at unpack.
  ! Choose ghost-buffer dims (np, nhc) so np*(nhc+1) >= pmcsl_payload_words.
  integer, parameter :: pmcsl_payload_words = nphys_cell_per_elem &
                          + nphys_cell_per_elem * max_ndest * 3
  ! For default constants (4, 36) -> 4 + 432 = 436. (21, 20) gives 21*21 = 441.
  integer, parameter :: pmcsl_ghost_np  = 21
  integer, parameter :: pmcsl_ghost_nhc = 20
  integer, parameter :: pmcsl_ghost_slot = pmcsl_ghost_np * (pmcsl_ghost_nhc + 1)

  type(local_fv_mesh_t), private :: fv_mesh
  type(source_partition_t) :: src_partition
  type(arrival_partition_t) :: arrival_partition
  type(GhostBuffer3D_t), private :: partmcsl_ghostbuf
  logical, private :: ghostbuf_initialized = .false.
  
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
    
    call t_startf('partmcsl_init')
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
    ! allocate memory for arrival partition (foreign contributions in)
    !--------------------------------------------
    allocate(arrival_partition%nsrc(nlev, nphys_cell_per_elem, nelemd))
    allocate(arrival_partition%src_gid(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    allocate(arrival_partition%src_subcell(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    allocate(arrival_partition%src_frac(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    arrival_partition%nsrc = 0
    arrival_partition%src_gid = -1
    arrival_partition%src_subcell = -1
    arrival_partition%src_frac = zero

    !--------------------------------------------
    ! allocate the ghost-exchange buffer used for source-partition communication
    !--------------------------------------------
    if (pmcsl_ghost_slot < pmcsl_payload_words) then
      call abortmp('partmcsl: pmcsl_ghost_slot too small for payload; recompute (np, nhc).')
    endif
    call initGhostBuffer3D(partmcsl_ghostbuf, nlev, pmcsl_ghost_np, pmcsl_ghost_nhc)
    ghostbuf_initialized = .true.
    
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
    call t_stopf('partmcsl_init')
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
      call test_identity_exchange(par, elem)
      call test_topology_coverage(par, elem)
      call test_sum_to_one(par, elem)
  endif
  if (par%masterproc) then
      write(iulog,*) "partmcsl_test: all tests passed."
  endif
end subroutine

! Reset src_partition to all-zero / no destinations (every test starts from a
! known-empty state so leftover entries don't contaminate the next test).
subroutine reset_src_partition()
  src_partition%ndest          = 0
  src_partition%dest_cell_idxs = -1
  src_partition%dest_portions  = zero
end subroutine reset_src_partition

! Populate src_partition with the identity mapping: each (ie, ci, k) sends 100%
! to its own (ie, ci, k).  dest_cell_idxs uses the flat 0-based index produced
! by the C++ side: (in_self - 1) * nphys_cell_per_elem + (ci - 1).
subroutine fill_identity_src_partition()
  integer :: ie, ci, k, in_self
  call reset_src_partition()
  do ie = 1, nelemd
    in_self = fv_mesh%my_elem_local_idx(ie)
    do k = 1, nlev
      do ci = 1, nphys_cell_per_elem
        src_partition%ndest(k, ci, ie) = 1
        src_partition%dest_cell_idxs(1, k, ci, ie) = &
            (in_self - 1) * nphys_cell_per_elem + (ci - 1)
        src_partition%dest_portions(1, k, ci, ie) = one
      enddo
    enddo
  enddo
end subroutine fill_identity_src_partition

! Populate src_partition so that each (ie, ci, k) distributes evenly across all
! nneighbors(ie) of its neighbors (including self), targeting the same subcell
! ci in each neighbor.  Each fraction equals 1/nneighbors(ie); per-source totals
! sum exactly to 1.0.
subroutine fill_uniform_src_partition()
  integer :: ie, ci, k, in
  real(real_kind) :: frac
  call reset_src_partition()
  do ie = 1, nelemd
    frac = one / real(fv_mesh%nneighbors(ie), real_kind)
    do k = 1, nlev
      do ci = 1, nphys_cell_per_elem
        src_partition%ndest(k, ci, ie) = fv_mesh%nneighbors(ie)
        do in = 1, fv_mesh%nneighbors(ie)
          src_partition%dest_cell_idxs(in, k, ci, ie) = &
              (in - 1) * nphys_cell_per_elem + (ci - 1)
          src_partition%dest_portions(in, k, ci, ie) = frac
        enddo
      enddo
    enddo
  enddo
end subroutine fill_uniform_src_partition

! Test 1: identity exchange.
!   With every source cell mapping 100% to itself, foreign and local-non-self
!   neighbor slots contribute nothing (their gid_dest never matches je).  Only
!   self-arrivals appear: exactly one record per (je, cj, k) with src_gid ==
!   je%GlobalID, src_subcell == cj, src_frac == 1.
subroutine test_identity_exchange(par, elem)
  type(parallel_t), intent(in) :: par
  type(element_t),  intent(in) :: elem(:)
  integer :: ie, ci, k
  real(real_kind), parameter :: id_tol = 1e-12_real_kind

  call fill_identity_src_partition()
  call partmcsl_exchange_source_partition(par, 0, elem, 1, nelemd)

  do ie = 1, nelemd
    do k = 1, nlev
      do ci = 1, nphys_cell_per_elem
        if (arrival_partition%nsrc(k, ci, ie) /= 1) then
          write(iulog,*) 'test_identity_exchange: arrival count mismatch at ie=', ie, &
              ' cj=', ci, ' k=', k, ' got=', arrival_partition%nsrc(k, ci, ie), &
              ' expected= 1'
          call abortmp('test_identity_exchange failed: arrival count.')
        endif
        if (arrival_partition%src_gid(1, k, ci, ie) /= elem(ie)%GlobalID) then
          call abortmp('test_identity_exchange failed: src_gid not self.')
        endif
        if (arrival_partition%src_subcell(1, k, ci, ie) /= ci) then
          call abortmp('test_identity_exchange failed: src_subcell /= cj.')
        endif
        if (abs(arrival_partition%src_frac(1, k, ci, ie) - one) > id_tol) then
          call abortmp('test_identity_exchange failed: src_frac /= 1.')
        endif
      enddo
    enddo
  enddo

  if (par%masterproc) then
    write(iulog,*) 'partmcsl_test: identity exchange passed.'
  endif
end subroutine test_identity_exchange

! Look up nneighbors of an element by global id, scanning locally-owned elements.
! Returns -1 if the gid is not on this rank.
function lookup_owned_nneighbors(elem, gid) result (n)
  type(element_t), intent(in) :: elem(:)
  integer,         intent(in) :: gid
  integer :: n, ie_local
  n = -1
  do ie_local = 1, nelemd
    if (elem(ie_local)%GlobalID == gid) then
      n = fv_mesh%nneighbors(ie_local)
      return
    endif
  enddo
end function lookup_owned_nneighbors

! Test 2: topology coverage.
!   Use the uniform synthetic src_partition (each ie sends 1/nneighbors(ie) to
!   each of its neighbors at the same subcell, including self).  After exchange,
!   under the full-stencil contract, every owned je must receive exactly one
!   record per neighbor of je (foreign, local non-self, and self) at every
!   (cj, k).  Each record's source-subcell equals cj (the receiver subcell),
!   src_gid is one of je's neighbor GlobalIDs, and src_frac is 1/nneighbors(src_gid)
!   which on a cubed-sphere mesh is either 1/8 (corner element) or 1/9 (interior).
subroutine test_topology_coverage(par, elem)
  type(parallel_t), intent(in) :: par
  type(element_t),  intent(in) :: elem(:)
  integer :: je, in, ci, k, d, expected_count
  integer :: nbr_gid
  logical :: found_in_neighbors
  real(real_kind) :: expected_frac

  call fill_uniform_src_partition()
  call partmcsl_exchange_source_partition(par, 0, elem, 1, nelemd)

  do je = 1, nelemd
    expected_count = fv_mesh%nneighbors(je)

    do k = 1, nlev
      do ci = 1, nphys_cell_per_elem
        if (arrival_partition%nsrc(k, ci, je) /= expected_count) then
          write(iulog,*) 'test_topology_coverage: arrival count mismatch at je=', je, &
              ' cj=', ci, ' k=', k, ' got=', arrival_partition%nsrc(k, ci, je), &
              ' expected=', expected_count
          call abortmp('test_topology_coverage failed: arrival count.')
        endif
        do d = 1, arrival_partition%nsrc(k, ci, je)
          ! source subcell must equal the receiver subcell ci
          if (arrival_partition%src_subcell(d, k, ci, je) /= ci) then
            call abortmp('test_topology_coverage failed: unexpected src_subcell.')
          endif
          ! source gid must appear in je's neighbor list (self is allowed)
          nbr_gid = arrival_partition%src_gid(d, k, ci, je)
          found_in_neighbors = .false.
          do in = 1, fv_mesh%nneighbors(je)
            if (fv_mesh%elem_global_id(1, in, je) == nbr_gid) then
              found_in_neighbors = .true.
              exit
            endif
          enddo
          if (.not. found_in_neighbors) then
            call abortmp('test_topology_coverage failed: src_gid not a neighbor of je.')
          endif
          ! frac must be 1/nneighbors(source) -- either 1/8 (corner) or 1/9 (interior).
          expected_frac = one / 9.0_real_kind
          if (abs(arrival_partition%src_frac(d, k, ci, je) - expected_frac) > 1e-10_real_kind &
              .and. abs(arrival_partition%src_frac(d, k, ci, je) - one/8.0_real_kind) &
                    > 1e-10_real_kind) then
            write(iulog,*) 'test_topology_coverage: bad frac=', &
                arrival_partition%src_frac(d, k, ci, je)
            call abortmp('test_topology_coverage failed: src_frac not 1/8 or 1/9.')
          endif
        enddo
      enddo
    enddo
  enddo

  if (par%masterproc) then
    write(iulog,*) 'partmcsl_test: topology coverage passed.'
  endif
end subroutine test_topology_coverage

! Test 3: conservation / sum-to-one with cross traffic.
!   Use the uniform synthetic src_partition.  At every owned (je, cj, k) the
!   total contribution (local-self + local-other + remote) should equal
!   sum over n in je's neighbors of 1/nneighbors(n).  We can compute this exact
!   expected value when every neighbor of je is locally owned (so we know its
!   nneighbors directly); when je has any foreign neighbor we still know the
!   count but not nneighbors of those foreign neighbors, so we accept either
!   1/8 or 1/9 contributions for foreign records and verify the resulting sum
!   is consistent with one of the admissible totals.  In all-9-neighbor
!   stencils the expected total is exactly 1.0.
subroutine test_sum_to_one(par, elem)
  type(parallel_t), intent(in) :: par
  type(element_t),  intent(in) :: elem(:)
  integer :: je, in, ci, k, d
  integer :: src_gid, owned_n
  real(real_kind) :: sum_total, expected
  real(real_kind), parameter :: sum_tol = 1e-12_real_kind

  call fill_uniform_src_partition()
  call partmcsl_exchange_source_partition(par, 0, elem, 1, nelemd)

  do je = 1, nelemd
    ! Build expected total inflow at (je, cj, k) under the uniform synthetic:
    ! each neighbor n of je (including self) contributes exactly one record per
    ! (cj, k) with frac = 1/nneighbors(n).  For owned n we read nneighbors
    ! directly; for foreign n we read frac from arrival_partition at (cj=1,k=1).
    expected = zero
    do in = 1, fv_mesh%nneighbors(je)
      src_gid = fv_mesh%elem_global_id(1, in, je)
      owned_n = lookup_owned_nneighbors(elem, src_gid)
      if (owned_n > 0) then
        expected = expected + one / real(owned_n, real_kind)
      else
        do d = 1, arrival_partition%nsrc(1, 1, je)
          if (arrival_partition%src_gid(d, 1, 1, je) == src_gid) then
            expected = expected + arrival_partition%src_frac(d, 1, 1, je)
            exit
          endif
        enddo
      endif
    enddo

    do k = 1, nlev
      do ci = 1, nphys_cell_per_elem
        ! Full stencil: total inflow at (je, cj, k) is the sum over every
        ! arrival_partition record (foreign + local non-self + self).
        sum_total = zero
        do d = 1, arrival_partition%nsrc(k, ci, je)
          sum_total = sum_total + arrival_partition%src_frac(d, k, ci, je)
        enddo

        if (abs(sum_total - expected) > sum_tol) then
          write(iulog,*) 'test_sum_to_one: mismatch at je=', je, ' cj=', ci, &
              ' k=', k, ' total=', sum_total, ' expected=', expected
          call abortmp('test_sum_to_one failed: total /= expected.')
        endif
      enddo
    enddo
  enddo

  ! Reset to a clean state so we don't leave synthetic data in place.
  call reset_src_partition()
  arrival_partition%nsrc        = 0
  arrival_partition%src_gid     = -1
  arrival_partition%src_subcell = -1
  arrival_partition%src_frac    = zero

  if (par%masterproc) then
    write(iulog,*) 'partmcsl_test: sum-to-one passed.'
  endif
end subroutine test_sum_to_one
  
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
    if (allocated(arrival_partition%nsrc)) then
      deallocate(arrival_partition%nsrc)
      deallocate(arrival_partition%src_gid)
      deallocate(arrival_partition%src_subcell)
      deallocate(arrival_partition%src_frac)
    endif
    if (ghostbuf_initialized) then
      call FreeGhostBuffer3D(partmcsl_ghostbuf)
      ghostbuf_initialized = .false.
    endif
  end subroutine partmcsl_finalize

  
  
  ! Decode a flat C++ cell index (0-based) from src_partition%dest_cell_idxs into
  ! a 1-based (in_dest, ci_dest) pair.  See partmcsl.hpp:init_local_mesh_if_needed
  ! for the layout: cell_idx = nbr_idx * n_subcells_per_elem + subcell_idx (0-based).
  subroutine decode_local_dest_idx(local_dest_idx, in_dest, ci_dest)
    integer, intent(in)  :: local_dest_idx
    integer, intent(out) :: in_dest, ci_dest
    integer :: nbr0, sub0
    nbr0    = local_dest_idx / nphys_cell_per_elem
    sub0    = mod(local_dest_idx, nphys_cell_per_elem)
    in_dest = nbr0 + 1
    ci_dest = sub0 + 1
  end subroutine decode_local_dest_idx

  ! Pack the per-element, per-level payload for src_partition into a flat 1D buffer
  ! sized pmcsl_ghost_slot.  Layout (1-based indices):
  !   payload(1..nphys_cell_per_elem)              : ndest(k, ci=1..4, ie) cast to real
  !   payload(nphys_cell_per_elem + (ci-1)*max_ndest*3 + (d-1)*3 + 1) : gid_dest
  !   payload(... + 2)                             : ci_dest (subcell in destination element)
  !   payload(... + 3)                             : dest_portions (fraction)
  ! Trailing entries (d > ndest) are zero.
  subroutine pack_payload(ie, k, payload)
    integer,              intent(in)  :: ie, k
    real(real_kind),      intent(out) :: payload(pmcsl_ghost_slot)
    integer :: ci, d, base, in_dest, ci_dest, gid_dest, ndest_here

    payload = zero

    do ci = 1, nphys_cell_per_elem
      payload(ci) = real(src_partition%ndest(k, ci, ie), real_kind)
    enddo

    do ci = 1, nphys_cell_per_elem
      base = nphys_cell_per_elem + (ci - 1) * max_ndest * 3
      ndest_here = src_partition%ndest(k, ci, ie)
      do d = 1, ndest_here
        call decode_local_dest_idx(src_partition%dest_cell_idxs(d, k, ci, ie), &
                                   in_dest, ci_dest)
        gid_dest = fv_mesh%elem_global_id(1, in_dest, ie)
        if (real(gid_dest, real_kind) > 2.0_real_kind**52) then
          call abortmp('partmcsl pack_payload: gid exceeds safe int->real cast range.')
        endif
        payload(base + (d-1)*3 + 1) = real(gid_dest, real_kind)
        payload(base + (d-1)*3 + 2) = real(ci_dest,  real_kind)
        payload(base + (d-1)*3 + 3) = src_partition%dest_portions(d, k, ci, ie)
      enddo
    enddo
  end subroutine pack_payload

  ! Pack one ie's payload into every neighbor slot in the ghost buffer for every level.
  subroutine partmcsl_pack_source_partition(elem, nets, nete)
    type(element_t), intent(in) :: elem(:)
    integer,         intent(in) :: nets, nete
    real(real_kind) :: payload(pmcsl_ghost_slot)
    integer :: ie, k, l_local, l, is

    do ie = nets, nete
      do k = 1, nlev
        call pack_payload(ie, k, payload)
        do l_local = 1, elem(ie)%desc%actual_neigh_edges
          l  = elem(ie)%desc%loc2buf(l_local)
          is = elem(ie)%desc%putmapP_ghost(l)
          partmcsl_ghostbuf%buf(:, :, k, is) = &
              reshape(payload, (/ pmcsl_ghost_np, pmcsl_ghost_nhc + 1 /))
        enddo
      enddo
    enddo
  end subroutine partmcsl_pack_source_partition

  ! Unpack ghost buffer into arrival_partition.  Full stencil: for each owned je
  ! we collect every record that targets je from every source -- foreign, local
  ! non-self, and self.  Foreign + local non-self come through je's neighbor
  ! ghost slots; self records don't traverse ghost exchange (no self-edge) and
  ! are injected directly from src_partition at the end.
  subroutine partmcsl_unpack_arrival_partition(elem, nets, nete)
    type(element_t), intent(in) :: elem(:)
    integer,         intent(in) :: nets, nete
    real(real_kind) :: payload(pmcsl_ghost_slot)
    integer :: ie, k, l_local, l, is, ci, d, base
    integer :: src_elem_gid, src_ndest, gid_dest, ci_dest, cj, slot
    integer :: in_dest
    real(real_kind) :: frac

    do ie = nets, nete
      arrival_partition%nsrc(:, :, ie) = 0
    enddo

    do ie = nets, nete
      do l_local = 1, elem(ie)%desc%actual_neigh_edges
        l  = elem(ie)%desc%loc2buf(l_local)
        is = elem(ie)%desc%getmapP_ghost(l)
        src_elem_gid = elem(ie)%desc%globalID(l)

        do k = 1, nlev
          payload = reshape(partmcsl_ghostbuf%buf(:, :, k, is), &
                            (/ pmcsl_ghost_slot /))
          do ci = 1, nphys_cell_per_elem
            src_ndest = nint(payload(ci))
            base = nphys_cell_per_elem + (ci - 1) * max_ndest * 3
            do d = 1, src_ndest
              gid_dest = nint(payload(base + (d-1)*3 + 1))
              ci_dest  = nint(payload(base + (d-1)*3 + 2))
              frac     =     payload(base + (d-1)*3 + 3)

              if (gid_dest == elem(ie)%GlobalID) then
                cj = ci_dest
                slot = arrival_partition%nsrc(k, cj, ie) + 1
                if (slot > max_ndest) then
                  call abortmp('partmcsl unpack: arrival_partition slot overflow.')
                endif
                arrival_partition%nsrc(k, cj, ie)            = slot
                arrival_partition%src_gid(slot, k, cj, ie)     = src_elem_gid
                arrival_partition%src_subcell(slot, k, cj, ie) = ci
                arrival_partition%src_frac(slot, k, cj, ie)    = frac
              endif
            enddo
          enddo
        enddo
      enddo
    enddo

    ! Self-arrivals: walk each owned ie's own src_partition and inject records
    ! whose destination is ie itself.  These would otherwise be missed because
    ! ghost exchange has no self-edge.
    do ie = nets, nete
      do k = 1, nlev
        do ci = 1, nphys_cell_per_elem
          do d = 1, src_partition%ndest(k, ci, ie)
            call decode_local_dest_idx( &
                src_partition%dest_cell_idxs(d, k, ci, ie), in_dest, ci_dest)
            if (fv_mesh%elem_global_id(1, in_dest, ie) /= elem(ie)%GlobalID) cycle
            cj   = ci_dest
            slot = arrival_partition%nsrc(k, cj, ie) + 1
            if (slot > max_ndest) then
              call abortmp('partmcsl unpack: arrival_partition slot overflow (self).')
            endif
            arrival_partition%nsrc(k, cj, ie)              = slot
            arrival_partition%src_gid(slot, k, cj, ie)     = elem(ie)%GlobalID
            arrival_partition%src_subcell(slot, k, cj, ie) = ci
            arrival_partition%src_frac(slot, k, cj, ie)    = &
                src_partition%dest_portions(d, k, ci, ie)
          enddo
        enddo
      enddo
    enddo
  end subroutine partmcsl_unpack_arrival_partition

  ! Top-level driver: pack -> ghost_exchangeVfull -> unpack.
  subroutine partmcsl_exchange_source_partition(par, ithr, elem, nets, nete)
    type(parallel_t), intent(in)    :: par
    integer,          intent(in)    :: ithr, nets, nete
    type(element_t),  intent(in)    :: elem(:)

    if (.not. ghostbuf_initialized) then
      call abortmp('partmcsl_exchange_source_partition: ghost buffer not initialized.')
    endif
    call t_startf('partmcsl_exchange_src_partition')
    call partmcsl_pack_source_partition(elem, nets, nete)
    call ghost_exchangeVfull(par, ithr, partmcsl_ghostbuf)
    call partmcsl_unpack_arrival_partition(elem, nets, nete)
    call t_stopf('partmcsl_exchange_src_partition')
  end subroutine partmcsl_exchange_source_partition

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

    do ie = nets, nete ! loop over elements worked by this thread
      do k=1, nlev ! loop over vertical levels

        !------------------------
        ! step 1: advect fv cells forward
        call t_startf('partmcsl_fwd_advection')
        call partmcsl_fwd_advection(advected_pts, elem(ie)%derived%vstar(:,:,:,k), &
          elem(ie)%state%v(:,:,:,k,tl%np1), fv_mesh, elem, ie, dt)
        call t_stopf('partmcsl_fwd_advection')
!         write(iulog,*) "partmcsl_step_forward: advection done at elem ", ie, " lev ", k
        !------------------------
        ! step 2: compute overlap portions (c++)
        call t_startf('partmcsl_calc_src_partition')
        call calc_src_partition(ie, nelemd, fv_mesh%nneighbors(ie), fv_mesh%my_elem_local_idx(ie), &
             k, nlev, fv_mesh%points, fv_mesh%subcell_area, advected_pts, src_partition%ndest, &
             src_partition%dest_cell_idxs, src_partition%dest_portions)
        call t_stopf('partmcsl_calc_src_partition')
        !------------------------
        ! step 3: move partmc particles
        call t_startf('partmcsl_step3_move')
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
        call t_stopf('partmcsl_step3_move')
      enddo
    enddo ! loop over elements worked by this thread

    ! TODO: barrier (if necessary)
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
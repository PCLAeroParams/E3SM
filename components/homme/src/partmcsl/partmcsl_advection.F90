#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module partmcsl_advection_mod
!   Terms like "elem" and "element" refer to Homme's spectral elements and related data structures.
!   Terms such as "cell" and "fv" refer to the physics grid's finite volume cells.

  use bndry_mod, only          : ghost_exchangeVfull
  use coordinate_systems_mod, only : cartesian3D_t, spherical_polar_t, &
                                     distance, change_coordinates
  use control_mod, only: cubed_sphere_map, dt_tracer_factor, dt_remap_factor, vert_remap_q_alg
  use cube_mod, only: ref2sphere
  use dimensions_mod, only     : nlev, nlevp, np, nelemd
  use edge_mod, only           : initGhostBuffer3D, FreeGhostBuffer3D
  use edgetype_mod, only       : GhostBuffer3D_t
  use element_mod, only        : element_t
  use gllfvremap_mod, only     : gfr_g2f_scalar
  use hybvcoord_mod, only      : hvcoord_t
  use kinds, only              : real_kind, iulog
  use parallel_mod, only       : parallel_t, abortmp
  use perf_mod, only           : t_startf, t_stopf
  use physical_constants, only : rearth
  use time_mod, only           : TimeLevel_t
  use vertremap_base, only     : remap1
  use partmcsl_mod

  implicit none
  private

  public :: partmcsl_init, partmcsl_finalize, partmcsl_test
  public :: partmcsl_step_forward, partmcsl_vertical_step
  public :: partmcsl_exchange_source_partition
  public :: partmcsl_permute_pg_q_cells
  public :: source_partition_t, src_partition

  !=====================================
  ! PartMCSL local finite volume mesh
  !
  ! One local mesh for each element owned by this rank.  For each elem(ie),
  ! ie = 1,...,nelemd:
  !
  !  fv_mesh%points(1:4, ci, in, ie) gives the coordinates of the 4 vertices of fv subcell
  !     ci (in [1,4]) of element `in` (in [1, nneighbors]) in elem(ie)'s neighbor list.
  !  fv_mesh%elem_global_id(ci, in, ie) gives Homme's global index of element `in` in
  !     elem(ie)'s neighbor list.
  !  fv_mesh%nneighbors(ie) gives the number of neighboring elements that elem(ie) has.
  !  fv_mesh%my_elem_local_idx(ie) gives the local index, in [1,nneighbors(ie)], of elem(ie);
  !     i.e., it's the index of "self" in the list of neighbors.
  type :: local_fv_mesh_t
    type(cartesian3D_t), allocatable :: points(:,:,:,:) ! (nverts, nphys_cell_per_elem, max_num_neighbors, nelemd)
    real(real_kind), allocatable :: subcell_area(:,:,:) ! (nphys_cell_per_elem, max_num_neighbors, nelemd)
    integer, allocatable :: elem_global_id(:,:,:) !(nphys_cell_per_elem, max_num_neighbors, nelemd)
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
  ! PartMCSL arrival partition (full-stencil contributions received at each owned destination)
  !
  ! For each element je owned by this rank, for each destination subcell cj of je
  ! and each vertical level k:
  !   nsrc(k, cj, je) is the number of source records contributing to (je, cj, k).
  !   src_gid(d, k, cj, je) is the GlobalID of the source element (any rank).
  !   src_subcell(d, k, cj, je) is the source subcell within that element [1..4].
  !   src_frac(d, k, cj, je) is the fraction of the source cell delivered into (je, cj).
  !   src_lneighbor(d, k, cj, je) is the local index of the source in je's neighbor list:
  !     0 sentinel means "self" (source == je) -- consumer reads q from local state;
  !     1..nneighbors(je) means use je's halo at slot src_lneighbor (foreign or local-non-self).
  !
  ! Full stencil: holds every record targeting je from every source -- foreign,
  ! local-non-self, and self.  Foreign + local-non-self arrive via ghost exchange;
  ! self records are injected directly from src_partition at unpack time.
  type :: arrival_partition_t
    integer, allocatable :: nsrc(:,:,:)             ! (nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: src_gid(:,:,:,:)        ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: src_subcell(:,:,:,:)    ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    real(real_kind), allocatable :: src_frac(:,:,:,:) ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
    integer, allocatable :: src_lneighbor(:,:,:,:)  ! (max_ndest, nlev, nphys_cell_per_elem, nelemd)
  end type

  !   elem%derived%vstar stores the velocity at the beginning of the tracer time step, t0
  !   elem(ie)%state%v(:,:,:,:,tl%np1) stores v at t1

  ! Gate for partmcsl_test's internal checks.  Always on for now; flip to .false.
  ! to skip the (cheap but non-trivial) self-tests at startup.
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

  !=====================================
  ! Ghost-exchange buffer sizing for the q halo.
  !
  ! Per element, per vertical level, the packed payload is:
  !   nphys_cell_per_elem * pmcsl_nq reals -- pg_q(ci, k, t, ie) for ci=1..4, t=1..pmcsl_nq.
  ! pmcsl_nq is the number of partmcsl-advected tracers.  For dcmip 2012 test 1.1
  ! these are slots 5..8 of pg_data%q (Q5..Q8); see memory entry
  ! "partmcsl_project_interface_refactor" for why this is scaffolding.
  integer, parameter :: pmcsl_nq = 4
  integer, parameter :: pmcsl_q_payload_words = nphys_cell_per_elem * pmcsl_nq
  ! For (4, 3): np*(nhc+1) = 4*4 = 16, exact fit for payload = 16.
  integer, parameter :: pmcsl_q_ghost_np  = 4
  integer, parameter :: pmcsl_q_ghost_nhc = 3
  integer, parameter :: pmcsl_q_ghost_slot = pmcsl_q_ghost_np * (pmcsl_q_ghost_nhc + 1)

  type(local_fv_mesh_t), private :: fv_mesh
  type(source_partition_t) :: src_partition
  type(arrival_partition_t) :: arrival_partition
  type(GhostBuffer3D_t), private :: partmcsl_ghostbuf
  logical, private :: ghostbuf_initialized = .false.
  type(GhostBuffer3D_t), private :: partmcsl_q_ghostbuf
  logical, private :: q_ghostbuf_initialized = .false.
  ! Permutation from gllfvremap's flat FV cell order (k = nphys*(j-1) + i, i.e.
  ! SW, NW, SE, NE for pg2) to partmcsl's ref_coords_ab order (ci=1..4 = SW,
  ! SE, NE, NW, CCW from SW).  Used to map gfr_g2f_scalar output and to
  ! translate pg_data%q at the partmcsl/dycore boundary.
  !
  ! TODO (follow-up): harmonize partmcsl's internal ci convention with
  ! gllfvremap's (option A in the convention discussion) so this permutation
  ! and the boundary translation in partmcsl_permute_pg_q_cells can be deleted.
  ! Touches ref_coords_ab on the C++ side and any consumer of fv_mesh that
  ! assumes CCW-from-SW ordering.
  integer, parameter, private :: gfr_to_partmcsl_ci(nphys_cell_per_elem) = (/ 1, 3, 4, 2 /)
  ! q_halo(ci, t, k, l_local, je) holds neighbor-q values after exchange:
  !   ci: source subcell [1..nphys_cell_per_elem]
  !   t : partmcsl tracer slot [1..pmcsl_nq]
  !   k : vertical level [1..nlev]
  !   l_local: local neighbor index of source in je's neighbor list [1..nneighbors(je)]
  !   je: this rank's owned element index [1..nelemd]
  ! Self contributions are NOT in the halo; consumer reads them from local pg_q.
  real(real_kind), allocatable, private :: q_halo(:,:,:,:,:)
  
  contains

subroutine partmcsl_init(par, elem)
    type(parallel_t), intent(in) :: par
    type(element_t), intent(in) :: elem(:)
    !
    ! local variables
    !
    integer :: ie, in, ci, vi
    type(cartesian3D_t) :: p_cart
    real(real_kind) :: a, b
    integer :: num_neighbors, max_num_neighbors
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
        call abortmp("partmcsl assumes regular cubed sphere meshes (no RRM).")
      endif
    endif

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
    allocate(arrival_partition%src_lneighbor(max_ndest, nlev, nphys_cell_per_elem, nelemd))
    arrival_partition%nsrc = 0
    arrival_partition%src_gid = -1
    arrival_partition%src_subcell = -1
    arrival_partition%src_frac = zero
    arrival_partition%src_lneighbor = -1

    !--------------------------------------------
    ! allocate q_halo: per owned je, per neighbor slot, holds neighbor q values
    ! after the q-halo exchange.  Self values are NOT stored here.
    !--------------------------------------------
    allocate(q_halo(nphys_cell_per_elem, pmcsl_nq, nlev, fv_mesh%max_nneighbors, nelemd))
    q_halo = zero

    !--------------------------------------------
    ! allocate the ghost-exchange buffer used for source-partition communication
    !--------------------------------------------
    if (pmcsl_ghost_slot < pmcsl_payload_words) then
      call abortmp('partmcsl: pmcsl_ghost_slot too small for payload; recompute (np, nhc).')
    endif
    call initGhostBuffer3D(partmcsl_ghostbuf, nlev, pmcsl_ghost_np, pmcsl_ghost_nhc)
    ghostbuf_initialized = .true.

    !--------------------------------------------
    ! allocate the ghost-exchange buffer used for q-halo communication
    !--------------------------------------------
    if (pmcsl_q_ghost_slot < pmcsl_q_payload_words) then
      call abortmp('partmcsl: pmcsl_q_ghost_slot too small for payload; recompute (np, nhc).')
    endif
    call initGhostBuffer3D(partmcsl_q_ghostbuf, nlev, pmcsl_q_ghost_np, pmcsl_q_ghost_nhc)
    q_ghostbuf_initialized = .true.
    
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
          fv_mesh%subcell_area(ci, in, ie) = &
              tri_area(fv_mesh%points(1, ci, in, ie), &
                       fv_mesh%points(2, ci, in, ie), &
                       fv_mesh%points(3, ci, in, ie)) + &
              tri_area(fv_mesh%points(1, ci, in, ie), &
                       fv_mesh%points(3, ci, in, ie), &
                       fv_mesh%points(4, ci, in, ie))
        enddo ! loop over subcells in element
      enddo ! loop over element neighbors
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
  end subroutine
  
  subroutine partmcsl_finalize()
    if (allocated(fv_mesh%points)) then
      deallocate(fv_mesh%points)
      deallocate(fv_mesh%elem_global_id)
      deallocate(fv_mesh%nneighbors)
      deallocate(fv_mesh%my_elem_local_idx)
      deallocate(fv_mesh%subcell_area)
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
      deallocate(arrival_partition%src_lneighbor)
    endif
    if (allocated(q_halo)) deallocate(q_halo)
    if (ghostbuf_initialized) then
      call FreeGhostBuffer3D(partmcsl_ghostbuf)
      ghostbuf_initialized = .false.
    endif
    if (q_ghostbuf_initialized) then
      call FreeGhostBuffer3D(partmcsl_q_ghostbuf)
      q_ghostbuf_initialized = .false.
    endif
  end subroutine partmcsl_finalize

  ! Translate pg_data%q's first dim between gllfvremap's flat FV cell order
  ! (SW, NW, SE, NE for pg2 -- the order used by gfr_dyn_to_fv_phys to seed
  ! the IC and by the NetCDF output writer) and partmcsl's CCW-from-SW order
  ! (SW, SE, NE, NW -- the order used by ref_coords_ab and fv_mesh).
  ! Call with to_partmcsl=.true. on entry to partmcsl's per-step routines,
  ! and to_partmcsl=.false. before returning control to the dycore.
  subroutine partmcsl_permute_pg_q_cells(q, to_partmcsl)
    real(real_kind), intent(inout) :: q(:, :, :, :)
    logical,         intent(in)    :: to_partmcsl
    real(real_kind) :: tmp(nphys_cell_per_elem)
    integer :: ci, k, t, ie

    if (size(q, 1) /= nphys_cell_per_elem) then
      call abortmp('partmcsl_permute_pg_q_cells: unexpected first-dim size.')
    endif

    do ie = 1, size(q, 4)
      do t = 1, size(q, 3)
        do k = 1, size(q, 2)
          if (to_partmcsl) then
            do ci = 1, nphys_cell_per_elem
              tmp(ci) = q(gfr_to_partmcsl_ci(ci), k, t, ie)
            enddo
          else
            do ci = 1, nphys_cell_per_elem
              tmp(gfr_to_partmcsl_ci(ci)) = q(ci, k, t, ie)
            enddo
          endif
          q(:, k, t, ie) = tmp
        enddo
      enddo
    enddo
  end subroutine partmcsl_permute_pg_q_cells

  
  
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
                arrival_partition%nsrc(k, cj, ie)              = slot
                arrival_partition%src_gid(slot, k, cj, ie)     = src_elem_gid
                arrival_partition%src_subcell(slot, k, cj, ie) = ci
                arrival_partition%src_frac(slot, k, cj, ie)    = frac
                arrival_partition%src_lneighbor(slot, k, cj, ie) = l_local
              endif
            enddo
          enddo
        enddo
      enddo
    enddo

    ! Self-arrivals: walk each owned ie's own src_partition and inject records
    ! whose destination is ie itself.  These would otherwise be missed because
    ! ghost exchange has no self-edge.  src_lneighbor=0 flags "self": consumers
    ! read q from local pg_q rather than from the halo.
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
            arrival_partition%src_lneighbor(slot, k, cj, ie) = 0
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

  ! Pack pg_q(:, k, :, ie) into a flat 1D buffer sized pmcsl_q_ghost_slot.
  ! Layout: payload((t-1)*nphys_cell_per_elem + ci) = pg_q(ci, k, t, ie)
  !   for ci = 1..nphys_cell_per_elem and t = 1..pmcsl_nq.
  subroutine pack_q_payload(pg_q, ie, k, payload)
    real(real_kind), intent(in)  :: pg_q(:,:,:,:)
    integer,         intent(in)  :: ie, k
    real(real_kind), intent(out) :: payload(pmcsl_q_ghost_slot)
    integer :: ci, t

    payload = zero
    do t = 1, pmcsl_nq
      do ci = 1, nphys_cell_per_elem
        payload((t-1)*nphys_cell_per_elem + ci) = pg_q(ci, k, t, ie)
      enddo
    enddo
  end subroutine pack_q_payload

  ! For each owned ie, pack its q payload into every neighbor slot in the q
  ! ghost buffer for every vertical level.  Mirrors partmcsl_pack_source_partition.
  subroutine partmcsl_pack_q_halo(pg_q, elem, nets, nete)
    real(real_kind), intent(in) :: pg_q(:,:,:,:)
    type(element_t), intent(in) :: elem(:)
    integer,         intent(in) :: nets, nete
    real(real_kind) :: payload(pmcsl_q_ghost_slot)
    integer :: ie, k, l_local, l, is

    do ie = nets, nete
      do k = 1, nlev
        call pack_q_payload(pg_q, ie, k, payload)
        do l_local = 1, elem(ie)%desc%actual_neigh_edges
          l  = elem(ie)%desc%loc2buf(l_local)
          is = elem(ie)%desc%putmapP_ghost(l)
          partmcsl_q_ghostbuf%buf(:, :, k, is) = &
              reshape(payload, (/ pmcsl_q_ghost_np, pmcsl_q_ghost_nhc + 1 /))
        enddo
      enddo
    enddo
  end subroutine partmcsl_pack_q_halo

  ! Unpack q ghost buffer into q_halo(:, :, :, l_local, je).  Self values are
  ! NOT in the halo (no self-edge in ghost exchange); consumers read those
  ! directly from local pg_q.
  subroutine partmcsl_unpack_q_halo(elem, nets, nete)
    type(element_t), intent(in) :: elem(:)
    integer,         intent(in) :: nets, nete
    real(real_kind) :: payload(pmcsl_q_ghost_slot)
    integer :: ie, k, l_local, l, is, ci, t

    do ie = nets, nete
      do l_local = 1, elem(ie)%desc%actual_neigh_edges
        l  = elem(ie)%desc%loc2buf(l_local)
        is = elem(ie)%desc%getmapP_ghost(l)
        do k = 1, nlev
          payload = reshape(partmcsl_q_ghostbuf%buf(:, :, k, is), &
                            (/ pmcsl_q_ghost_slot /))
          do t = 1, pmcsl_nq
            do ci = 1, nphys_cell_per_elem
              q_halo(ci, t, k, l_local, ie) = payload((t-1)*nphys_cell_per_elem + ci)
            enddo
          enddo
        enddo
      enddo
    enddo
  end subroutine partmcsl_unpack_q_halo

  ! Top-level driver: pack -> ghost_exchangeVfull -> unpack.
  subroutine partmcsl_exchange_q_halo(par, ithr, pg_q, elem, nets, nete)
    type(parallel_t), intent(in) :: par
    integer,          intent(in) :: ithr, nets, nete
    real(real_kind),  intent(in) :: pg_q(:,:,:,:)
    type(element_t),  intent(in) :: elem(:)

    if (.not. q_ghostbuf_initialized) then
      call abortmp('partmcsl_exchange_q_halo: q ghost buffer not initialized.')
    endif
    call t_startf('partmcsl_exchange_q_halo')
    call partmcsl_pack_q_halo(pg_q, elem, nets, nete)
    call ghost_exchangeVfull(par, ithr, partmcsl_q_ghostbuf)
    call partmcsl_unpack_q_halo(elem, nets, nete)
    call t_stopf('partmcsl_exchange_q_halo')
  end subroutine partmcsl_exchange_q_halo

  ! Step forward: phase A (advect + calc_src_partition per ie,k), phase B
  ! (exchange src_partition -> arrival_partition AND exchange q halo), phase C
  ! (per-cell mixing-ratio update using arrival_partition + halo).
  !
  ! pg_q is the partmcsl-advected tracer state on the FV grid; for dcmip 2012
  ! test 1.1 this is pg_data%q(:, :, 5:8, :).  Eventually this signature will
  ! change to accept a particle-payload-shaped state.
  subroutine partmcsl_step_forward(par, ithr, elem, dt, nets, nete, tl, pg_q)
    type(parallel_t),     intent(in)    :: par
    integer,              intent(in)    :: ithr
    type (element_t)     , intent(inout) :: elem(:)
    real(kind=real_kind) , intent(in   ) :: dt   ! time step size
    integer              , intent(in   ) :: nets ! thread starting element idx in [1,nelemd]
    integer              , intent(in   ) :: nete ! thread ending element idx in [1,nelemd]
    type (TimeLevel_t)   , intent(in   ) :: tl
    real(real_kind)      , intent(inout) :: pg_q(:, :, :, :)
        ! (nphys_cell_per_elem, nlev, pmcsl_nq, nelemd) -- partmcsl's tracers
    ! local variables
    type(cartesian3D_t) :: advected_pts(nverts, nphys_cell_per_elem)
    integer :: ie, je, k, ci, cj, d
    integer :: src_ci, l_loc
    real(real_kind) :: frac
    real(real_kind), allocatable :: q_new(:,:,:,:)

    if (size(pg_q, 3) /= pmcsl_nq) then
      call abortmp('partmcsl_step_forward: pg_q tracer count != pmcsl_nq.')
    endif

    !-----------------------------------------------------------
    ! Phase A: step 1 (advect) + step 2 (calc_src_partition).
    !-----------------------------------------------------------
    do ie = nets, nete
      do k = 1, nlev
        call t_startf('partmcsl_fwd_advection')
        call partmcsl_fwd_advection(advected_pts, elem(ie)%derived%vstar(:,:,:,k), &
          elem(ie)%state%v(:,:,:,k,tl%np1), fv_mesh, elem, ie, dt)
        call t_stopf('partmcsl_fwd_advection')

        call t_startf('partmcsl_calc_src_partition')
        call calc_src_partition(ie, nelemd, fv_mesh%nneighbors(ie), &
             fv_mesh%my_elem_local_idx(ie), &
             k, nlev, fv_mesh%points, fv_mesh%subcell_area, advected_pts, &
             src_partition%ndest, &
             src_partition%dest_cell_idxs, src_partition%dest_portions)
        call t_stopf('partmcsl_calc_src_partition')
      enddo
    enddo

    !-----------------------------------------------------------
    ! Phase B: exchanges -- collective.  Populate arrival_partition
    ! (full-stencil source records) and q_halo (neighbor q values).
    !-----------------------------------------------------------
    call partmcsl_exchange_source_partition(par, ithr, elem, nets, nete)
    call partmcsl_exchange_q_halo(par, ithr, pg_q, elem, nets, nete)

    !-----------------------------------------------------------
    ! Phase C: step 3 -- per-cell mixing-ratio update.
    !   q_new(je, cj, k, :) = sum over arrival records of frac * q_src
    ! where q_src is local pg_q if src_lneighbor == 0 (self), else q_halo
    ! at the recorded local neighbor index.  Writes go through a temp
    ! array because pg_q reads at (src_ci, k, :, je) overlap with the
    ! pending writes at (cj, k, :, je) for different cj at the same je.
    !-----------------------------------------------------------
    call t_startf('partmcsl_step3_move')
    allocate(q_new(nphys_cell_per_elem, nlev, pmcsl_nq, nets:nete))
    q_new = zero
    do je = nets, nete
      do k = 1, nlev
        do cj = 1, nphys_cell_per_elem
          do d = 1, arrival_partition%nsrc(k, cj, je)
            src_ci = arrival_partition%src_subcell(d, k, cj, je)
            frac   = arrival_partition%src_frac(d, k, cj, je)
            l_loc  = arrival_partition%src_lneighbor(d, k, cj, je)
            if (l_loc == 0) then
              ! self: read q from local pg_q
              q_new(cj, k, :, je) = q_new(cj, k, :, je) &
                                    + frac * pg_q(src_ci, k, :, je)
            else
              ! foreign or local-non-self: read q from halo
              q_new(cj, k, :, je) = q_new(cj, k, :, je) &
                                    + frac * q_halo(src_ci, :, k, l_loc, je)
            endif
          enddo
        enddo
      enddo
    enddo
    pg_q(:, :, :, nets:nete) = q_new(:, :, :, nets:nete)
    deallocate(q_new)
    call t_stopf('partmcsl_step3_move')
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
    integer :: i, ci, cj, vi, facenum
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

  !=====================================================================
  ! Vertical transport step (column-local; PPM remap via vertremap_base).
  !
  ! Lagrangian-advect Eulerian interface pressures by the prescribed
  ! vertical mass flux eta_dot_dpdn (Pa/s) over dt, giving a per-column
  ! Lagrangian thickness dp_lagr.  Then remap from the Lagrangian grid
  ! back to the Eulerian grid using HOMME's remap1 (algorithm chosen by
  ! the namelist parameter vert_remap_q_alg), i.e. the same kernel the SL
  ! tracer path uses.  No MPI: each FV cell column is independent.
  !
  ! Working coord is interface pressure p_i = hyai(i)*p0 + hybi(i)*ps.
  ! Lagrangian motion is then simply
  !   p_new = p_old + eta_dot_dpdn * dt
  ! since eta_dot_dpdn already has units of dp/dt along eta-following
  ! surfaces.  Top (k=1) and surface (k=nlevp) are pinned to their
  ! Eulerian values so no mass leaves the column.
  !
  ! ps_v and eta_dot_dpdn_prescribed are sampled at FV cell centers by
  ! delegating to gllfvremap's gfr_g2f_scalar -- sphere-area-weighted FV
  ! cell mean of the GLL polynomial, using the cubed-sphere Jacobian.
  ! gfr_init must have been called upstream (dcmip12_wrapper.F90 does this).
  !=====================================================================

  ! Column-local vertical transport for the partmcsl tracers.  For each
  ! owned (ie, ci) column:
  !   1. Sample ps_v and eta_dot_dpdn_prescribed at the FV cell center.
  !   2. Build fixed Eulerian interface pressures p_dst and the displaced
  !      Lagrangian interfaces p_src = p_dst + edd*dt (walls pinned).
  !   3. Convert mixing ratio to source-cell mass Qdp = pg_q * dp_dst
  !      (Lagrangian invariant: the displaced parcel carries its original
  !      Eulerian cell mass).
  !   4. Call remap1 with dp1=dp_lagr, dp2=dp_dst, alg=vert_remap_q_alg --
  !      same kernel and algorithm choice as the SL tracer path.
  !   5. Convert back to mixing ratio: pg_q = Qdp / dp_dst.
  !
  ! TODO: for non-prescribed-wind cases the vertical-velocity source
  ! becomes elem%derived%omega_p.
  subroutine partmcsl_vertical_step(elem, hvcoord, dt, nets, nete, tl, pg_q)
    type(element_t),   intent(in)    :: elem(:)
    type(hvcoord_t),   intent(in)    :: hvcoord
    real(real_kind),   intent(in)    :: dt
    integer,           intent(in)    :: nets, nete
    type(TimeLevel_t), intent(in)    :: tl
    real(real_kind),   intent(inout) :: pg_q(:, :, :, :)

    real(real_kind) :: ps_fv(nphys_cell_per_elem)
    real(real_kind) :: edd_fv(nphys_cell_per_elem, nlevp)
    ! gfr_g2f_scalar output buffers (gllfvremap flat cell order)
    real(real_kind) :: ps_g(np, np, 1)
    real(real_kind) :: ps_fv_gfr(nphys_cell_per_elem, 1)
    real(real_kind) :: edd_fv_gfr(nphys_cell_per_elem, nlevp)
    real(real_kind) :: p_dst(nlevp), p_src(nlevp)
    real(real_kind) :: dp_dst(1, 1, nlev), dp_lagr(1, 1, nlev)
    real(real_kind) :: Qdp(1, 1, nlev, pmcsl_nq)
    integer :: ie, ci, k, t

    if (size(pg_q, 3) /= pmcsl_nq) then
      call abortmp('partmcsl_vertical_step: pg_q tracer count != pmcsl_nq.')
    endif

    call t_startf('partmcsl_vertical_step')
    do ie = nets, nete
      ! ps at FV cell centers: sphere-area mean over the FV subcell.
      ps_g(:,:,1) = elem(ie)%state%ps_v(:,:,tl%n0)
      call gfr_g2f_scalar(ie, elem(ie)%metdet, ps_g, ps_fv_gfr)
      do ci = 1, nphys_cell_per_elem
        ps_fv(ci) = ps_fv_gfr(gfr_to_partmcsl_ci(ci), 1)
      enddo

      ! eta_dot_dpdn at FV cell centers, per interface.
      call gfr_g2f_scalar(ie, elem(ie)%metdet, &
                          elem(ie)%derived%eta_dot_dpdn_prescribed, edd_fv_gfr)
      do k = 1, nlevp
        do ci = 1, nphys_cell_per_elem
          edd_fv(ci, k) = edd_fv_gfr(gfr_to_partmcsl_ci(ci), k)
        enddo
      enddo

      do ci = 1, nphys_cell_per_elem
        do k = 1, nlevp
          p_dst(k) = hvcoord%hyai(k) * hvcoord%ps0 + hvcoord%hybi(k) * ps_fv(ci)
        enddo
        p_src(1)     = p_dst(1)
        p_src(nlevp) = p_dst(nlevp)
        do k = 2, nlev
          p_src(k) = p_dst(k) + edd_fv(ci, k) * dt
        enddo
        do k = 1, nlev
          if (p_src(k+1) <= p_src(k)) then
            call abortmp('partmcsl vertical: advected interfaces not monotonic; reduce dt or check eta_dot_dpdn.')
          endif
        enddo

        do k = 1, nlev
          dp_dst (1, 1, k) = p_dst(k+1) - p_dst(k)
          dp_lagr(1, 1, k) = p_src(k+1) - p_src(k)
        enddo
        do t = 1, pmcsl_nq
          do k = 1, nlev
            Qdp(1, 1, k, t) = pg_q(ci, k, t, ie) * dp_dst(1, 1, k)
          enddo
        enddo

        call remap1(Qdp, 1, pmcsl_nq, dp_lagr, dp_dst, vert_remap_q_alg)

        do t = 1, pmcsl_nq
          do k = 1, nlev
            pg_q(ci, k, t, ie) = Qdp(1, 1, k, t) / dp_dst(1, 1, k)
          enddo
        enddo
      enddo
    enddo
    call t_stopf('partmcsl_vertical_step')
  end subroutine partmcsl_vertical_step

end module partmcsl_advection_mod
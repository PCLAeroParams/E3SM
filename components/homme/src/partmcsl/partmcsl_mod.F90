#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! This file provides the interfaces to the PartMCSL C++ functions

module partmcsl_mod

  use element_mod, only: element_t
  use parallel_mod, only: parallel_t
  use kinds, only : iulog
  
  implicit none
  public
  
  ! we assume pg2 grid 
  integer, parameter :: nphys = 2, & ! the "2" in pg2, 2 physics cell edges per spectral element edge
                        nverts = 4, & ! always quads
                        nphys_cell_per_elem = 4 ! 2 x 2 subcells per element
                        
  ! the maximum number elements than any 1 element can overlap after an advection time step
  ! (assumes timestep guarantees "halo-1" constraint)
  integer, parameter :: max_ndest_elem = 9
  ! the maximum number of fv cells that any 1 fv cell can overlap after an advection time step
  integer, parameter :: max_ndest = nphys_cell_per_elem * max_ndest_elem
  
  interface
  
#ifdef HOMME_ENABLE_PARTMCSL
  
      function tri_area(va, vb, vc) bind(c)
        use coordinate_systems_mod, only: cartesian3D_t
        use iso_c_binding, only : c_double
        real(kind=c_double) :: tri_area
        type(cartesian3D_t), intent(in) :: va, vb, vc
      end function
      
      subroutine calc_src_partition(ie, n_elemd, n_elem_neighbors, elem_self_idx, &
        level_idx, nlev, &
        points, area, advected_pts, ndest, dest_cells, dest_fracs) bind(c) 
        use coordinate_systems_mod, only: cartesian3D_t
        use iso_c_binding, only : c_double, c_int
        integer(kind=c_int), intent(in), value :: ie, n_elemd, n_elem_neighbors, elem_self_idx, level_idx, nlev
        type(cartesian3D_t), dimension(4,4,n_elem_neighbors,n_elemd), intent(in) :: points
        real(kind=c_double), dimension(4,n_elem_neighbors,n_elemd), intent(in) :: area
        type(cartesian3D_t), dimension(4,4), intent(in) :: advected_pts
        integer(kind=c_int), dimension(nlev,4,n_elemd), intent(inout) :: ndest
        integer(kind=c_int), dimension(36,nlev,4,n_elemd), intent(inout) :: dest_cells
        real(kind=c_double), dimension(36,nlev,4,n_elemd), intent(inout) :: dest_fracs
      end subroutine
      
      subroutine ref_coords_ab(a, b, subcell_idx, vert_idx) bind(c)
        use iso_c_binding, only: c_int, c_double
        real(kind=c_double), intent(out) :: a, b
        integer(kind=c_int), intent(in), value :: subcell_idx, vert_idx
      end subroutine
      
      subroutine init_local_meshes(n_elemd, nneighbors, elem_self_idx, points, subcell_area) bind(c)
        use iso_c_binding, only: c_int, c_double
        use coordinate_systems_mod, only: cartesian3D_t
        integer(kind=c_int), intent(in), value :: n_elemd
        integer(kind=c_int), intent(in) :: nneighbors(n_elemd), elem_self_idx(n_elemd)
        type(cartesian3D_t), intent(in) :: points(4,4,9, n_elemd)
        real(kind=c_double), intent(in) :: subcell_area(4, 9, n_elemd)
      end subroutine
      
      subroutine partmcsl_init_local(n_elemd) bind(c)
        use iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: n_elemd
      end subroutine
      
      subroutine test_int_array(int_array, n) bind(c)
        use iso_c_binding, only : c_int
        integer(kind=c_int), intent(in) :: int_array(n)
        integer(kind=c_int), intent(in), value :: n
      end subroutine
  
  end interface

contains

subroutine test_const_int_array1(int_array, n) 
  use iso_c_binding, only : c_int
  integer(kind=c_int), intent(in) :: int_array(n), n
  write(iulog,*) 'partmcsl: sending to c++ ', int_array
  call test_int_array(int_array, n)
end subroutine

! subroutine ref_coords_ab(a, b, subcell_idx, vert_idx) 
!     !   heads up: subcell_idx and vert_idx are 0-based indices.
!     ! 
!     !       Given a subcell index return the (a,b) 
!     !       reference coordinates of the vertex at vert_idx,
!     !       in ccw order starting at SW corner.
!     ! 
!     !                 (0,1)
!     !      (-1,1) o----x----o (1,1)
!     !             | 3  |  2 |
!     !             |    |    |
!     !      (-1,0) x----x----x (1,0)
!     !             |    |    |
!     !             | 0  |  1 |
!     !     (-1,-1) o----x----o (1,-1)
!     !                 (0,-1)
!     !       
!     !TODO: replace this function with an interface to its c++ counterpart.
!     !
!     real(real_kind), intent(out) :: a, b ! output: (a,b) coordinates in ref. quad.
!     integer, intent(in) :: subcell_idx, vert_idx ! input: *0-based* subcell and vertex indices
!     ! local
!     logical :: left, south
!     
!     left = ( vert_idx == 0 .or. vert_idx == 3)
!     south = ( vert_idx < 2 )
!     
!     select case (subcell_idx)
!       case (0)
!         if (left) then
!           a = -one
!         else 
!           a = zero
!         endif
!         if (south) then
!           b = -one
!         else 
!           b = zero
!         endif
!       case (1)
!         if (left) then
!           a = zero
!         else 
!           a = one
!         endif
!         if (south) then
!           b = -one
!         else 
!           b = zero
!         endif
!       case (2)
!         if (left) then
!           a = zero
!         else 
!           a = one
!         endif
!         if (south) then
!           b = zero
!         else 
!           b = one
!         endif
!       case (3)
!         if (left) then
!           a = -one 
!         else 
!           a = zero
!         endif
!         if (south) then
!           b = zero
!         else 
!           b = one
!         endif
!     end select
!   end subroutine
  
#endif 
end module
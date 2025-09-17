#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! This file provides the interfaces to the PartMCSL C++ functions

module partmcsl_mod

  use element_mod, only: element_t
  use parallel_mod, only: parallel_t
  
  implicit none
  public
  
  interface
  
#ifdef HOMME_ENABLE_PARTMCSL
  
      function tri_area(va, vb, vc) bind(c)
        use coordinate_systems_mod, only: cartesian3D_t
        use iso_c_binding, only : c_double
        use kinds, only : real_kind
        real(kind=c_double) :: tri_area
        type(cartesian3D_t), intent(in) :: va, vb, vc
      end function
      
      subroutine calc_src_partition(ie, n_elemd, n_elem_neighbors, elem_self_idx, &
        level_idx, nlev, &
        corners, area, advected_pts, ndest, dest_cells, dest_fracs) bind(c) 
        use coordinate_systems_mod, only: cartesian3D_t
        use kinds, only : real_kind
        integer, intent(in) :: ie, n_elemd, n_elem_neighbors, elem_self_idx, level_idx, nlev
        type(cartesian3D_t), dimension(:,:,:,:), intent(in) :: corners
        real(real_kind), dimension(:,:,:), intent(in) :: area
        type(cartesian3D_t), dimension(4,4), intent(in) :: advected_pts
        integer, dimension(:,:,:), intent(inout) :: ndest
        integer, dimension(:,:,:,:), intent(inout) :: dest_cells
        real(real_kind), dimension(:,:,:,:), intent(inout) :: dest_fracs
      end subroutine
      
      subroutine ref_coords_ab(a, b, subcell_idx, vert_idx) bind(c)
        use kinds, only : real_kind
        real(real_kind), intent(out) :: a, b
        integer, intent(in) :: subcell_idx, vert_idx
      end subroutine
      
      subroutine init_local_meshes(nneighbors, elem_self_idx, points, subcell_area) bind(c)
        use kinds, only : real_kind
        use coordinate_systems_mod, only: cartesian3D_t
        integer, intent(in) :: nneighbors(:), elem_self_idx(:)
        type(cartesian3D_t), intent(in) :: points(:,:,:,:)
        real(real_kind) ,intent(in) :: subcell_area(:,:,:)
      end subroutine
      
      subroutine partmcsl_init_local(nelemd) bind(c)
        integer, intent(in) :: nelemd
      end subroutine
  
#endif 

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
  
  end interface
end module
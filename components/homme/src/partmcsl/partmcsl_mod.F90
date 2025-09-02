#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! This file provides the interfaces to the PartMCSL C++ functions

module partmcsl_mod

  implicit none
  private
#ifdef HOMME_ENABLE_PARTMCSL  
  public :: tri_area, calc_src_partition
#endif 
  
  interface
  
#ifdef HOMME_ENABLE_PARTMCSL
  
      function tri_area(va, vb, vc)
        use coordinate_systems_mod, only: cartesian3D_t
        use kinds, only : real_kind
        real(real_kind) :: tri_area
        type(cartesian3D_t), intent(in) :: va, vb, vc
      end function
      
      subroutine calc_src_partition(ie, n_elemd, n_elem_neighbors, elem_self_idx, &
        level_idx, nlev, &
        corners, area, advected_pts, ndest, dest_cells, dest_fracs)
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
  
#endif 
  
  end interface
end module
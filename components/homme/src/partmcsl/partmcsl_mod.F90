#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! This file provides the interfaces to the PartMCSL C++ functions

module partmcsl_mod

  implicit none
  public

  ! we assume pg2 grid
  integer, parameter :: nverts = 4, & ! always quads
                        nphys_cell_per_elem = 4 ! 2 x 2 subcells per element

  ! the maximum number of fv cells that any 1 fv cell can overlap after an
  ! advection time step (assumes timestep guarantees "halo-1" constraint:
  ! at most 9 neighbor elements x 4 subcells per element).
  integer, parameter :: max_ndest = nphys_cell_per_elem * 9

#ifdef HOMME_ENABLE_PARTMCSL
  interface

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

  end interface
#endif

end module

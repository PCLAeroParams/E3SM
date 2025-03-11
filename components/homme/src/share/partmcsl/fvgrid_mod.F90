#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module fvgrid_mod

  use hybrid_mod, only: hybrid_t
  use kinds, only: real_kind
  use dimensions_mod, only: nlev, np, npsq, qsize, nelemd
  use element_mod, only: element_t
  use coordinate_systems_mod, only: cartesian3D_t
  

  implicit none
  
  private
  
  public :: &
    ! finite volume grid type
    fvgrid_t, &
    ! initialize this module
    fvgrid_init, &
    ! finalize this module
    fvgrid_finish !&
    ! advect forward one physics time step
    ! fvgrid_fwd_advect

  type fvgrid_t
    type (cartesian3D_t), allocatable :: &
          corners_f(:,:,:,:) ! (4,nphys,nphys,nelemd)
  end type

  type(fvgrid_t), public :: fvgrid
contains 

subroutine fvgrid_init(par, elem, nphys, check)
  use kinds, only: iulog
  use parallel_mod, only: parallel_t
  use gllfvremap_mod, only : gfr_init
  
  type(parallel_t), intent(in) :: par
  type(element_t), intent(in) :: elem(:)
  integer, intent(in) :: nphys ! nphys is N in pgN (usually 2)
  integer, intent(in), optional :: check ! see gfr_init in gllfvremap_mod
  
  integer :: chk
  type (GllFvRemap_t) :: gfr
  chk = 0
  if (present(check)) chk = check
  
  if (par%masterproc) then
    write(iulog, *) 'partmcsl> Initializing fvgrid.'
  endif
  
  ! initialize the physgrid remapper, copy the parts that we need
  call gfr_init(par, elem, nphys, chk)
  
  allocate(fvgrid%corners(4, nphys, nphys, nelemd))
  fvgrid%corners = gfr%corners
end subroutine fvgrid_init

subroutine fvgrid_finish()
  ! deallocate the fvgrid 
  if (.not. allocated(fvgrid%corners)) return
  
  deallocate(fvgrid%corners)
end subroutine fvgrid_finish

end module 
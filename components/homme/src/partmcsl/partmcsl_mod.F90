#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! This file provides the interfaces to the PartMCSL C++ functions

module partmcsl_mod

  implicit none
  private
#ifdef HOMME_ENABLE_PARTMCSL  
  public :: tri_area
#endif 
  
  interface
  
#ifdef HOMME_ENABLE_PARTMCSL
  
      function tri_area(va, vb, vc)
        use coordinate_systems_mod, only: cartesian3D_t
        use kinds, only : real_kind
        real(real_kind) :: tri_area
        type(cartesian3D_t), intent(in) :: va, vb, vc
      end function
  
#endif 
  
  end interface
end module
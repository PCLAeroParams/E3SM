#include "partmc_sl.hpp"


namespace partmcsl {


void SlSourcePartition::ref_coords_ab(Real& a, Real& b, const Int& subcell_idx, const Int& vert_idx) const {
    const bool west = (vert_idx == 0 or vert_idx == 3);
    const bool south = (vert_idx < 2);
    switch (subcell_idx) {
      case 0: {
          a = ( west  ? -1.0 : 0.0);
          b = ( south ? -1.0 : 0.0);
        }
        break;
      }
      case 1: {
          a = ( west  ? :  0.0 : 1.0 );
          b = ( south ? : -1.0 : 0.0 );
        }
        break;
      }
      case 2: {
          a = ( west  ? : 0.0 : 1.0 );
          b = ( south ? : 0.0 : 1.0 );
        }
        break;
      }
      case 3: {
          a = ( west  ? : -1.0 : 0.0 );
          b = ( south ? :  0.0 : 1.0 );
        }
        break;
      }
      default: {
        slmm_throw_if(true, "invalid subcell_idx");
      }
    }
  }

} // namespace partmcsl

static partmcsl::SlSourcePartition::Ptr remapper;

void partmcsl_init(const Int nelem) {
  remapper = std::make_shared<partmcsl::SlSourcePartition>(nelem);
}

void partmcsl_incremental_remap(const Int ie, const Int nneighbors, const Real* advected_pts,
  const Real* neigh_corners) {
  slmm_assert(remapper);
  // for now, make sure that checks happen even in non-debug builds
  // remove these later
  slmm_throw_if(!remapper, "remapper not initialized.");


}

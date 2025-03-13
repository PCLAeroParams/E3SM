#ifndef SIQK_EXE_SPACE_HPP
#define SIQK_EXE_SPACE_HPP

namespace siqk {

// Get the host or device version of the array.
template <typename VT, typename ES> struct InExeSpace {
  typedef VT type;
};
template <typename VT> struct InExeSpace<VT, ko::HostSpace> {
  typedef typename VT::HostMirror type;
};

} // namespace siqk
#endif

module partmcsl_mod

interface

  subroutine calc_partmcsl_source_partition(ie, nelemd, max_nneighbors, self_idx, &
    lev_idx, nlev, adv_pts, cell_pts, cells, nneigbhors, ndest, dest_cells, dest_frac)
    use coordinate_systems_mod, only : cartesian3D_t
    use kinds, only : real_kind
    integer, intent(in) :: ie, nelemd, self_idx, lev_idx, nlev, max_nneighbors
    type(cartesian3D_t), intent(in) :: adv_pts(4,4), cell_pts(16*max_nneighbors, nelemd)
    integer, intent(in) :: cells(4, 4*max_nneighbors, nelemd), nneigbhors(nelemd)
    integer, intent(out) :: ndest(nlev, 4, nelemd), dest_cells(36, nlev, 4, nelemd)
    real(real_kind), intent(out) :: dest_frac(36, nlev, 4, nelemd)
  end subroutine
  
end interface

end module

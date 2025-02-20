module mo_partmc_interface
contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine partmc_inti()
    !-----------------------------------------------------------------------
    !	... initialize the hetero sox routine
    !-----------------------------------------------------------------------
    use ppgrid,       only : pver
    use spmd_utils,   only : masterproc
    use pmc_spec_file
    implicit none
    logical :: history_aerosol   ! Output aerosol diagnostics
    character(len=100) :: spec_name
    type(spec_file_t) :: file
    character(len=100) :: run_type
    !FIXME: part is hard-coded.
    spec_name="/home/odiazib/v3/partmc/partmc/scenarios/urban_plume.spec"

    if (masterproc) then
      call spec_file_open(spec_name, file)
      call spec_file_read_string(file, 'run_type', run_type)
      write(102,*) '-----------------------------------------'
      write(102,*) 'mozart will do partmc...'
      write(102,*) "spec_name", spec_name
      write(102,*) "file", file
      write(102,*) '-----------------------------------------'
    endif


  end subroutine partmc_inti

  subroutine invoke_partmc(ncol)
  use ppgrid,    only : pcols, pver
  implicit none
  integer,          intent(in)    :: ncol              ! num of columns in chunk

  end subroutine invoke_partmc

end module mo_partmc_interface

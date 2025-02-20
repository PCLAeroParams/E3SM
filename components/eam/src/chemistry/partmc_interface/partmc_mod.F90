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
    !use pmc_mpi
  !use pmc_bin_grid
  use pmc_aero_state
 ! use pmc_aero_dist
 ! use pmc_aero_binned
 ! use pmc_coag_kernel
  use pmc_aero_data
  use pmc_scenario
  use pmc_env_state
 use pmc_run_part
 ! use pmc_run_exact
  use pmc_run_sect
  use pmc_gas_data
  use pmc_gas_state
  use pmc_util

    implicit none
    logical :: history_aerosol   ! Output aerosol diagnostics
    character(len=100) :: spec_name
    type(spec_file_t) :: file_name
    character(len=100) :: run_type
    type(gas_data_t) :: gas_data
    type(gas_state_t) :: gas_state
    type(gas_state_t) :: gas_state_init
    type(aero_data_t) :: aero_data
    type(aero_dist_t) :: aero_dist_init
    type(aero_state_t) :: aero_state
    type(aero_state_t) :: aero_state_init
    type(scenario_t) :: scenario
    type(env_state_t) :: env_state
    type(env_state_t) :: env_state_init
    type(run_part_opt_t) :: run_part_opt
    integer :: i_repeat, i_group
    integer :: rand_init
    character, allocatable :: buffer(:)
    integer :: buffer_size, max_buffer_size
    integer :: position
    logical :: do_restart, do_init_equilibrate, aero_mode_type_exp_present
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    real(kind=dp) :: n_part

    !FIXME: part is hard-coded.
    spec_name="/home/odiazib/v3/partmc/partmc/scenarios/1_urban_plume/urban_plume.spec"

    if (masterproc) then
      call spec_file_open(spec_name, file_name)
      call spec_file_read_string(file_name, 'run_type', run_type)

      call spec_file_read_run_part(file_name, run_part_opt, aero_data, &
            aero_state_init, gas_data, gas_state_init, env_state_init, &
            aero_dist_init, scenario, &
            n_part, rand_init, do_init_equilibrate, do_restart)
      write(102,*) '-----------------------------------------'
      write(102,*) 'mozart will do partmc...'
      write(102,*) "spec_name: ", spec_name
      write(102,*) "file: ", file_name
      write(102,*) "n_part: ", n_part
      write(102,*) "gas_state_init%mix_rat(1): ", gas_state_init%mix_rat(1)
      write(102,*) "gas_data%name(1) ", gas_data%name(1)
      write(102,*) '-----------------------------------------'
    endif


  end subroutine partmc_inti

  subroutine invoke_partmc(ncol)
  use ppgrid,    only : pcols, pver
  implicit none
  integer,          intent(in)    :: ncol              ! num of columns in chunk

  end subroutine invoke_partmc

end module mo_partmc_interface

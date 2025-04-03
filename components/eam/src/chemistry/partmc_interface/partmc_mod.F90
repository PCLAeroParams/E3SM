module mo_partmc_interface
    use ppgrid,       only : pver, begchunk, endchunk
    use spmd_utils,   only : masterproc
    use pmc_spec_file
    use pmc_mpi
    use pmc_bin_grid
    use pmc_aero_state
    use pmc_aero_dist
    use pmc_aero_binned
    use pmc_coag_kernel
    use pmc_aero_data
    use pmc_scenario
    use pmc_env_state
    use pmc_run_part
    use pmc_run_exact
    use pmc_run_sect
    use pmc_gas_data
    use pmc_gas_state
    use pmc_util

    implicit none
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
contains
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine partmc_inti()
    !-----------------------------------------------------------------------
    !	... initialize the hetero sox routine
    !-----------------------------------------------------------------------


    implicit none
    logical :: history_aerosol   ! Output aerosol diagnostics
    character(len=100) :: spec_name
    type(spec_file_t) :: file_name
    character(len=100) :: run_type

    !FIXME: part is hard-coded.
    spec_name="/home/odiazib/v3/partmc/partmc/scenarios/1_urban_plume/urban_plume.spec"
    call pmc_srand(0, pmc_mpi_rank())

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

      call pmc_mpi_broadcast_run_part(run_part_opt, aero_data, &
         aero_state_init, gas_data, gas_state_init, env_state_init, &
         aero_dist_init, scenario, &
         n_part, rand_init, do_init_equilibrate, do_restart)
    endif
  ! re-initialize RNG with the given seed
    !call pmc_rand_finalize()
    !call pmc_srand(rand_init, pmc_mpi_rank())


  end subroutine partmc_inti

  subroutine partmc_mam_invoke(state)
    use physics_types,    only : physics_state
    implicit none
    type(physics_state), intent(in):: state
    integer :: i, n_species, icol, kk

    n_species=35 ! get from eam
    icol=1
    kk=1

   if (masterproc) then
         write(102,*) '-----------------------------------------'
         write(102,*) 'Setting gas_state_init'
   endif
   do i = 1,n_species
       !units?
       gas_state_init%mix_rat(i) = state%q(icol,kk,i)
       if (masterproc) then
         write(102,*) gas_data%name(i), " : ", gas_state_init%mix_rat(i)
       endif
   end do

  if (masterproc) then
    write(102,*) '-----------------------------------------'
  endif

  !
  scenario%temp_time(:)=0
  scenario%temp(:)=state%t(icol,kk)
  scenario%pressure_time=0
  scenario%pressure=state%pmid(icol,kk)
  scenario%height_time(:)=0
  scenario%height(:)=state%zm(icol,kk)

  !FIXME
  scenario%gas_emission_time(:)=0.0
  scenario%gas_emission_rate_scale(:)=0.0
  !scenario%gas_emission(:)=0.0

  scenario%gas_dilution_time(:) = 0.0
  scenario%gas_dilution_rate(:) = 0.0
  !scenario%gas_background(:) = 0.0

  scenario%aero_emission_time(:) = 0.0
  scenario%aero_emission_rate_scale(:) = 0.0
  !scenario%aero_emission(:) = 0.0

  scenario%aero_dilution_time(:) = 0.0
  scenario%aero_dilution_rate(:) = 0.0
  !scenario%aero_background(:) = 0.0

  !FIXME:
  env_state%rel_humid=0.95
  env_state%latitude=state%lat(icol)
  env_state%longitude=state%lon(icol)
  env_state%altitude=state%zm(icol,kk)
  env_state%start_time=0
  env_state%start_day=0
  env_state%temp=state%t(icol,kk)
  env_state%pressure=state%pmid(icol,kk)
  env_state%height=state%zm(icol,kk)
  env_state%elapsed_time=0d0
  ! FIXME: should compute this at some point
  env_state%solar_zenith_angle = 0d0

  if (masterproc) then
    write(102,*) '-----------------------------------------'
    write(102,*) 'Setting scenario'
    write(102,*)  "scenario%temp(:) ", scenario%temp(1)
    write(102,*)  "scenario%pressure(:) ", scenario%pressure(1)
    write(102,*)  "scenario%height(:) ", scenario%height(1)
    write(102,*) '-----------------------------------------'
  endif

  do i_repeat = 1,run_part_opt%n_repeat
    run_part_opt%i_repeat = i_repeat
    if (masterproc) then
      write(102,*) 'i_repeat ', i_repeat
    endif

    gas_state = gas_state_init
    !!aero_state = aero_state_init
    !!call aero_state_set_n_part_ideal(aero_state, n_part)
    call aero_state_zero(aero_state)
    aero_mode_type_exp_present &
      = aero_dist_contains_aero_mode_type(aero_dist_init, &
      AERO_MODE_TYPE_EXP) &
      .or. scenario_contains_aero_mode_type(scenario, &
      AERO_MODE_TYPE_EXP)

    if (aero_mode_type_exp_present) then
      if (masterproc) then
        write(102,*) 'aero_mode_type_exp_present true ... '
      endif
      call warn_msg(245301880, "using flat weighting only due to " &
         // "presence of exp aerosol mode")
      call aero_state_set_weight(aero_state, aero_data, &
        AERO_STATE_WEIGHT_FLAT)
    else
      if (masterproc) then
        write(102,*) 'aero_mode_type_exp_present false ... '
      endif
      call aero_state_set_weight(aero_state, aero_data, &
       run_part_opt%weighting_type, run_part_opt%weighting_exponent)
    end if

    call aero_state_set_n_part_ideal(aero_state, n_part)
    !call aero_state_add_aero_dist_sample(aero_state, aero_data, &
    !           aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
    !           run_part_opt%allow_halving)

    !env_state = env_state_init
    !call scenario_init_env_state(scenario, env_state, &
    !        env_state_init%elapsed_time)

    !call run_part(scenario, env_state, aero_data, aero_state, gas_data, &
    !           gas_state, run_part_opt)

    end do

  end subroutine partmc_mam_invoke

  subroutine partmc_mam_inti()

  use mo_tracname, only : solsym
  use pmc_gas_state, only : gas_state_set_size

  use mo_gas_phase_chemdr, only : map2chm

  implicit none

  !partmc has real for n_part
  !real(kind=dp) :: n_part
  integer :: i, n_species, n_aero_species, n_times
  integer :: lchnk, ncol
  type(spec_file_t) :: file_aero_data
  character(len=100) :: file_name
  logical :: read_aero_weight_classes

  run_part_opt%n_repeat=3
  run_part_opt%t_max=2
  run_part_opt%del_t=1
  run_part_opt%t_output=-1
  run_part_opt%do_camp_chem=.false.
  n_part=10
  n_species=46 ! get from eam
  n_aero_species=7 ! get from eam
  n_times=1

  run_part_opt%output_type = OUTPUT_TYPE_SINGLE
  run_part_opt%mix_timescale = 0d0
  run_part_opt%gas_average = .false.
  run_part_opt%env_average = .false.
  run_part_opt%parallel_coag_type = PARALLEL_COAG_TYPE_LOCAL

  call uuid4_str(run_part_opt%uuid)

  read_aero_weight_classes=.false.

  call ensure_string_array_size(gas_data%name, n_species)
  call gas_state_set_size(gas_state_init, n_species)
  call ensure_string_array_size(aero_data%name, n_aero_species)

  allocate(scenario%temp_time(n_times))
  allocate(scenario%temp(n_times))
  allocate(scenario%height_time(n_times))
  allocate(scenario%height(n_times))
  allocate(scenario%pressure_time(n_times))
  allocate(scenario%pressure(n_times))

  !FIXME
  allocate(scenario%gas_emission_time(n_times))
  allocate(scenario%gas_emission_rate_scale(n_times))
  allocate(scenario%gas_emission(n_times))

  allocate(scenario%gas_dilution_time(n_times))
  allocate(scenario%gas_dilution_rate(n_times))
  allocate(scenario%gas_background(n_times))

  allocate(scenario%aero_emission_time(n_times))
  allocate(scenario%aero_emission_rate_scale(n_times))
  allocate(scenario%aero_emission(n_times))

  allocate(scenario%aero_dilution_time(n_times))
  allocate(scenario%aero_dilution_rate(n_times))
  allocate(scenario%aero_background(n_times))

  scenario%loss_function_type = SCENARIO_LOSS_FUNCTION_NONE

  rand_init=0

  run_part_opt%do_coagulation=.true.
  run_part_opt%coag_kernel_type = COAG_KERNEL_TYPE_BROWN
  run_part_opt%parallel_coag_type = PARALLEL_COAG_TYPE_LOCAL
  run_part_opt%do_condensation=.false.
  run_part_opt%do_mosaic=.false.
  run_part_opt%do_optical = .false.
  run_part_opt%do_nucleation=.false.
  run_part_opt%allow_doubling=.true.
  run_part_opt%allow_halving=.true.
  run_part_opt%record_removals=.true.
  run_part_opt%do_parallel=.false.

  do i = 1,n_species
    gas_data%name(i) = solsym(i)
    if (masterproc) then
      write(102,*) gas_data%name(i)
    endif
  end do

  file_name='/home/odiazib/acme/scratch/data/aero_data.dat'
  call spec_file_open(file_name, file_aero_data)
  call spec_file_read_aero_data(file_aero_data, aero_data)
  call spec_file_close(file_aero_data)

  file_name="/home/odiazib/acme/scratch/data/aero_init_dist.dat"
  call spec_file_open(file_name, file_aero_data)
  call spec_file_read_aero_dist(file_aero_data, aero_data, &
            read_aero_weight_classes, aero_dist_init)
  call spec_file_close(file_aero_data)

  run_part_opt%weighting_type = AERO_STATE_WEIGHT_NUMMASS_SOURCE
  run_part_opt%weighting_exponent = 0.0d0

  env_state_init%elapsed_time=0
  run_part_opt%output_prefix="/home/odiazib/acme/scratch/outputs/urban_plume"

  !gas_state_init
  if (masterproc) then
      write(102,*) '-----------------------------------------'
      write(102,*) 'mozart will do partmc_mam_inti...'
      write(102,*) aero_data%name(1)
      write(102,*) aero_data%name(2)
      write(102,*) '-----------------------------------------'
  endif

  end subroutine partmc_mam_inti


  subroutine invoke_partmc(ncol)
  use ppgrid,    only : pcols, pver
  implicit none
  integer,          intent(in)    :: ncol              ! num of columns in chunk

  if (masterproc) then
#if 1
  call cpu_time(run_part_opt%t_wall_start)
      do i_repeat = 1,run_part_opt%n_repeat
       run_part_opt%i_repeat = i_repeat

       gas_state = gas_state_init
       if (do_restart) then
          aero_state = aero_state_init
          call aero_state_set_n_part_ideal(aero_state, n_part)
       else
          call aero_state_zero(aero_state)
          aero_mode_type_exp_present &
               = aero_dist_contains_aero_mode_type(aero_dist_init, &
               AERO_MODE_TYPE_EXP) &
               .or. scenario_contains_aero_mode_type(scenario, &
               AERO_MODE_TYPE_EXP)
          if (aero_mode_type_exp_present) then
             call warn_msg(245301880, "using flat weighting only due to " &
                  // "presence of exp aerosol mode")
             call aero_state_set_weight(aero_state, aero_data, &
                  AERO_STATE_WEIGHT_FLAT)
          else
             call aero_state_set_weight(aero_state, aero_data, &
                  run_part_opt%weighting_type, run_part_opt%weighting_exponent)
          end if
          call aero_state_set_n_part_ideal(aero_state, n_part)
          call aero_state_add_aero_dist_sample(aero_state, aero_data, &
               aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
               run_part_opt%allow_halving)
       end if
       env_state = env_state_init
       call scenario_init_env_state(scenario, env_state, &
            env_state_init%elapsed_time)

#ifdef PMC_USE_SUNDIALS
       if (do_init_equilibrate) then
          call condense_equilib_particles(env_state, aero_data, aero_state)
       end if
#endif

       if (run_part_opt%do_camp_chem) then
#ifdef PMC_USE_CAMP
          call run_part(scenario, env_state, aero_data, aero_state, gas_data, &
               gas_state, run_part_opt, camp_core=camp_core, &
               photolysis=photolysis)
#endif
       else
          call run_part(scenario, env_state, aero_data, aero_state, gas_data, &
               gas_state, run_part_opt)
       end if

    end do
#endif

    write(102,*) '-----------------------------------------'
    write(102,*) 'mozart will do invoke partmc...'
    write(102,*) "n_part: ", n_part
    write(102,*) "gas_state%mix_rat(1): ", gas_state%mix_rat(1)
    write(102,*) "env_state%temp: ", env_state%temp
    write(102,*) "env_state%pressure: ", env_state%pressure

    write(102,*) "gas_data%name(1) ", gas_data%name(1)
    write(102,*) '-----------------------------------------'

    endif
    !call pmc_rand_finalize()




  end subroutine invoke_partmc

end module mo_partmc_interface

module mo_partmc_interface
    use ppgrid,       only : pver, begchunk, endchunk, pcols
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
    integer, parameter, public :: n_part_max = 100
    integer, parameter, public :: n_aero_sp_max = 25
    character, allocatable :: buffer(:)
    integer :: buffer_size, max_buffer_size
    integer :: position
    logical :: do_restart, do_init_equilibrate, aero_mode_type_exp_present
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    real(kind=dp) :: n_part
contains
!-----------------------------------------------------------------------
  subroutine spec_file_read_run_part_eam(file, run_part_opt, aero_data, &
       aero_state_init, &
       !gas_data, gas_state_init,
       env_state_init, &
       aero_dist_init, &
       !scenario, &
       n_part, rand_init, do_init_equilibrate, do_restart)

    !> Spec file.
    type(spec_file_t), intent(inout) :: file
    !> Monte Carlo options.
    type(run_part_opt_t), intent(inout) :: run_part_opt
    !> Aerosol data.
    type(aero_data_t), intent(inout) :: aero_data
    !> Initial aerosol state.
    type(aero_state_t), intent(inout) :: aero_state_init
    !> Gas data.
    !type(gas_data_t), intent(inout) :: gas_data
    !> Initial gas state.
    !type(gas_state_t), intent(inout) :: gas_state_init
    !> Initial environmental state.
    type(env_state_t), intent(inout) :: env_state_init
    !> Initial aerosol distribution.
    type(aero_dist_t), intent(inout) :: aero_dist_init
    !> Scenario data.
    !type(scenario_t), intent(inout) :: scenario
    !> Ideal number of computational particles.
    real(kind=dp), intent(inout) :: n_part
    !> Random number generator seed.
    integer, intent(out) :: rand_init
    !> Whether to equilibrate.
    logical, intent(out) :: do_init_equilibrate
    !> Whether simulation is a restart.
    logical, intent(out) :: do_restart

    integer :: i_repeat, i_group
    logical :: read_aero_weight_classes
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    real(kind=dp) :: dummy_time, dummy_del_t
    character(len=PMC_MAX_FILENAME_LEN) :: sub_filename
    type(spec_file_t) :: sub_file
    character(len=PMC_MAX_FILENAME_LEN) :: camp_config_filename


    call spec_file_read_string(file, 'output_prefix', &
         run_part_opt%output_prefix)
    call spec_file_read_integer(file, 'n_repeat', run_part_opt%n_repeat)
    call spec_file_read_real(file, 'n_part', n_part)
    call spec_file_read_logical(file, 'restart', do_restart)
    if (do_restart) then
       call spec_file_read_string(file, 'restart_file', restart_filename)
    end if

    if (.not. do_restart) then
       call spec_file_read_logical(file, 'do_select_weighting', &
            run_part_opt%do_select_weighting)
       read_aero_weight_classes = .false.
       if (run_part_opt%do_select_weighting) then
          call spec_file_read_aero_state_weighting_type(file, &
               run_part_opt%weighting_type, run_part_opt%weighting_exponent)
          if (run_part_opt%weighting_type &
               >= AERO_STATE_WEIGHT_FLAT_SPECIFIED)then
             read_aero_weight_classes = .true.
          end if
       else
          run_part_opt%weighting_type = AERO_STATE_WEIGHT_NUMMASS_SOURCE
          run_part_opt%weighting_exponent = 0.0d0
       end if
    end if

    !call spec_file_read_real(file, 't_max', run_part_opt%t_max)
    !call spec_file_read_real(file, 'del_t', run_part_opt%del_t)
    call spec_file_read_real(file, 't_output', run_part_opt%t_output)
    call spec_file_read_real(file, 't_progress', run_part_opt%t_progress)

    !if (do_restart) then
    !   call input_state(restart_filename, dummy_index, dummy_time, &
    !        dummy_del_t, dummy_i_repeat, run_part_opt%uuid, aero_data, &
    !        aero_state_init, gas_data, gas_state_init, env_state_init)
    !end if

    if (.not. do_restart) then
       env_state_init%elapsed_time = 0d0
      call spec_file_read_string(file, 'aerosol_data', sub_filename)
      call spec_file_open(sub_filename, sub_file)
      call spec_file_read_aero_data(sub_file, aero_data)
      call spec_file_close(sub_file)
      call spec_file_read_fractal(file, aero_data%fractal)

      call spec_file_read_string(file, 'aerosol_init', sub_filename)
      call spec_file_open(sub_filename, sub_file)
      call spec_file_read_aero_dist(sub_file, aero_data, &
            read_aero_weight_classes, aero_dist_init)
      call spec_file_close(sub_file)
    end if

    !call spec_file_read_scenario(file, gas_data, aero_data, &
    !     read_aero_weight_classes, scenario)
    !call spec_file_read_env_state(file, env_state_init)

    call spec_file_read_logical(file, 'do_coagulation', &
         run_part_opt%do_coagulation)
    if (run_part_opt%do_coagulation) then
       call spec_file_read_coag_kernel_type(file, run_part_opt%coag_kernel_type)
       if (run_part_opt%coag_kernel_type == COAG_KERNEL_TYPE_ADDITIVE) then
          call spec_file_read_real(file, 'additive_kernel_coeff', &
               env_state_init%additive_kernel_coefficient)
       end if
    else
       run_part_opt%coag_kernel_type = COAG_KERNEL_TYPE_INVALID
    end if

    call spec_file_read_logical(file, 'do_condensation', &
         run_part_opt%do_condensation)
    do_init_equilibrate = .false.
    if (run_part_opt%do_condensation) then
       call spec_file_read_logical(file, 'do_init_equilibrate', &
            do_init_equilibrate)
    end if

    call spec_file_read_logical(file, 'do_mosaic', run_part_opt%do_mosaic)
    if (run_part_opt%do_mosaic .and. (.not. mosaic_support())) then
       call spec_file_die_msg(230495365, file, &
            'cannot use MOSAIC, support is not compiled in')
    end if
    if (run_part_opt%do_mosaic .and. run_part_opt%do_condensation) then
       call spec_file_die_msg(599877804, file, &
            'cannot use MOSAIC and condensation simultaneously')
    end if
    if (run_part_opt%do_mosaic) then
       call spec_file_read_logical(file, 'do_optical', &
            run_part_opt%do_optical)
    else
       run_part_opt%do_optical = .false.
    end if

    call spec_file_read_logical(file, 'do_nucleation', &
         run_part_opt%do_nucleation)
    if (run_part_opt%do_nucleation) then
       call spec_file_read_nucleate_type(file, aero_data, &
            run_part_opt%nucleate_type, run_part_opt%nucleate_source, &
            run_part_opt%nucleate_weight_class)
    else
       run_part_opt%nucleate_type = NUCLEATE_TYPE_INVALID
    end if

    call spec_file_read_integer(file, 'rand_init', rand_init)
    call spec_file_read_logical(file, 'allow_doubling', &
         run_part_opt%allow_doubling)
    call spec_file_read_logical(file, 'allow_halving', &
         run_part_opt%allow_halving)
    call spec_file_read_logical(file, 'record_removals', &
         run_part_opt%record_removals)

    run_part_opt%do_parallel=.false.
    run_part_opt%output_type = OUTPUT_TYPE_SINGLE
    run_part_opt%mix_timescale = 0d0
    run_part_opt%gas_average = .false.
    run_part_opt%env_average = .false.
    run_part_opt%parallel_coag_type = PARALLEL_COAG_TYPE_LOCAL
    call spec_file_close(file)

  end subroutine spec_file_read_run_part_eam

  subroutine partmc_mam_inti()
   use mo_tracname, only : solsym
   use cam_history,  only : addfld
   use cam_history_support, only: add_hist_coord
   implicit none
   integer :: i, n_species, n_aero_species, n_times
   character(len=100) :: file_name
   type(spec_file_t) :: file
   file_name='/home/odiazib/acme/scratch/data/urban_plume.spec'

  n_species=46 ! get from eam
  ! n_aero_species=7 ! get from eam
  n_times=1

  call ensure_string_array_size(gas_data%name, n_species)
  call gas_state_set_size(gas_state_init, n_species)
  !call ensure_string_array_size(aero_data%name, n_aero_species)

  ! allocate(scenario%temp_time(n_times))
  ! allocate(scenario%temp(n_times))
  ! allocate(scenario%height_time(n_times))
  ! allocate(scenario%height(n_times))
  ! allocate(scenario%pressure_time(n_times))
  ! allocate(scenario%pressure(n_times))

  ! !FIXME
  ! allocate(scenario%gas_emission_time(n_times))
  ! allocate(scenario%gas_emission_rate_scale(n_times))
  ! allocate(scenario%gas_emission(n_times))

  ! allocate(scenario%gas_dilution_time(n_times))
  ! allocate(scenario%gas_dilution_rate(n_times))
  ! allocate(scenario%gas_background(n_times))

  ! allocate(scenario%aero_emission_time(n_times))
  ! allocate(scenario%aero_emission_rate_scale(n_times))
  ! allocate(scenario%aero_emission(n_times))

  ! allocate(scenario%aero_dilution_time(n_times))
  ! allocate(scenario%aero_dilution_rate(n_times))
  ! allocate(scenario%aero_background(n_times))
  if (masterproc) then
      write(102,*) 'gas_data%name(i) = solsym(i) ... '
  endif

  do i = 1,n_species
    gas_data%name(i) = solsym(i)
  end do

  call spec_file_open(file_name, file)
  call spec_file_read_run_part_eam(file, run_part_opt, aero_data, &
       aero_state_init, &
       env_state_init, &
       aero_dist_init, &
       n_part, rand_init, do_init_equilibrate, do_restart)

  !call uuid4_str(run_part_opt%uuid)

  call aero_state_zero(aero_state)
  if (masterproc) then
      write(102,*) 'Before aero_mode_type_exp_present ... '
    endif
  aero_mode_type_exp_present &
      = aero_dist_contains_aero_mode_type(aero_dist_init, &
      AERO_MODE_TYPE_EXP)
      ! &
      ! .or. scenario_contains_aero_mode_type(scenario, &
      ! AERO_MODE_TYPE_EXP)
  if (masterproc) then
      write(102,*) 'After aero_mode_type_exp_present ... '
    endif

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
  call aero_state_add_aero_dist_sample(aero_state, aero_data, &
               aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
               run_part_opt%allow_halving)

  env_state = env_state_init

  call add_hist_coord('npartmax',    n_part_max,    'NPARTMAX')
  call add_hist_coord('na_spmax',    n_aero_sp_max,    'NAEROSPMAX')
  call add_hist_coord('na_sp_partmax',    n_aero_sp_max,    'NAEROSPPARTMAX')
  ! call addfld( 'aero_particle_mass', (/ 'lev     ', 'npartmax','na_spmax'/), 'A', 'kg', 'constituent masses of each aerosol particle' )
  ! call addfld( 'aero_particle_mass', (/'lev          ', 'na_sp_partmax' /), 'I', 'kg', 'constituent masses of each aerosol particle' )
  n_aero_species=aero_data_n_spec(aero_data)
  do i = 1, n_aero_species
    call addfld( 'aero_particle_mass_'// trim(aero_data%name(i)), (/'lev     ', 'npartmax' /), 'I', 'kg', 'constituent masses of each aerosol particle' )
  end do

  call addfld( 'aero_component_len', (/'lev     ', 'npartmax' /), 'I', '-', 'number of aero_components for each aerosol particle' )
  call addfld( 'number_of_particles', (/'lev     ' /), 'I', '-', 'number of particles per cell' )
  call addfld( 'aero_num_conc', (/'lev     ', 'npartmax' /), 'I', 'm^{-3}', 'number concentration for each particle' )

  end subroutine partmc_mam_inti

  subroutine partmc_mam_invoke(state, dt)
    use physics_types,    only : physics_state
    use cam_history,       only : outfld

    implicit none
    type(physics_state), intent(in):: state
    real(kind=dp),            intent(in)    :: dt              ! time step
    integer :: i, n_species, icol, kk, lchnk, ncol, n_aero_species
    ! real(kind=dp)                  ::  aero_particle_mass_out(pcols, pver,  n_part_max*n_aero_sp_max)
    real(kind=dp)                  ::  aero_particle_mass_out(pcols, pver,  n_part_max,n_aero_sp_max)
    real(kind=dp)                  ::  aero_component_len_out(pcols, pver,  n_part_max)
    real(kind=dp)                  ::  aero_num_conc_out(pcols, pver,  n_part_max)
    real(kind=dp)                  ::  number_of_particles_out(pcols, pver)

    !FIXME: get n_species this values from eam
    n_species=46 !
    !FIXME: we must pass a delta time factor
    run_part_opt%del_t = dt/10d0
    run_part_opt%t_max = dt

    lchnk = state%lchnk
    ncol  = state%ncol
    aero_particle_mass_out(:,:,:,:)=-1000d0
    aero_component_len_out(:,:,:)=-1
    number_of_particles_out(:,:)=-1

    if (masterproc) then
         write(102,*) '-----------------------------------------'
         write(102,*) 'Time step and Time max'
         write(102,*) 'run_part_opt%del_t: ', run_part_opt%del_t
         write(102,*) 'run_part_opt%t_max: ', run_part_opt%t_max
         write(102,*) '-----------------------------------------'
    endif

    ! if (masterproc) then
    !      write(102,*) '-----------------------------------------'
    !      write(102,*) 'Setting gas_state_init'
    ! endif

    ! scenario%temp_time(:)=0
    ! scenario%pressure_time=0
    ! scenario%height_time(:)=0
    !     !FIXME
    ! scenario%gas_emission_time(:)=0.0
    ! scenario%gas_emission_rate_scale(:)=0.0
    ! !scenario%gas_emission(:)=0.0

    ! scenario%gas_dilution_time(:) = 0.0
    ! scenario%gas_dilution_rate(:) = 0.0
    ! !scenario%gas_background(:) = 0.0

    ! scenario%aero_emission_time(:) = 0.0
    ! scenario%aero_emission_rate_scale(:) = 0.0
    ! !scenario%aero_emission(:) = 0.0

    ! scenario%aero_dilution_time(:) = 0.0
    ! scenario%aero_dilution_rate(:) = 0.0
    !scenario%aero_background(:) = 0.0

    env_state%start_time=0
    env_state%start_day=0
    env_state%elapsed_time=0d0

    do kk = 1,pver
      do icol = 1, ncol
        do i = 1,n_species
          !FIXME: units?
          gas_state_init%mix_rat(i) = state%q(icol,kk,i)
          ! if (masterproc) then
          !   write(102,*) gas_data%name(i), " : ", gas_state_init%mix_rat(i)
          !  endif
        end do ! species
        ! scenario%temp(:)  = state%t(icol,kk)
        ! scenario%pressure = state%pmid(icol,kk)
        ! scenario%height(:) = state%zm(icol,kk)
        !FIXME: we need to compute rel_humid
        env_state%rel_humid = 0.95
        env_state%latitude = state%lat(icol)
        env_state%longitude = state%lon(icol)
        env_state%altitude = state%zm(icol,kk)

        env_state%temp = state%t(icol,kk)
        env_state%pressure = state%pmid(icol,kk)
        env_state%height = state%zm(icol,kk)
        ! FIXME: should compute this at some point
        env_state%solar_zenith_angle = 0d0

        do i_repeat = 1,run_part_opt%n_repeat
          run_part_opt%i_repeat = i_repeat

          gas_state = gas_state_init
          call run_part(scenario, env_state, aero_data, aero_state, gas_data, &
                 gas_state, run_part_opt)
        end do

        call write_nc_aero_state(aero_state,aero_particle_mass_out,&
                            aero_component_len_out, &
                            number_of_particles_out, &
                            aero_num_conc_out, &
                            icol, kk)

      end do ! icol
    end do ! kk
    n_aero_species=aero_data_n_spec(aero_data)
    do i = 1, n_aero_species
        call outfld( 'aero_particle_mass_'// trim(aero_data%name(i)), &
         aero_particle_mass_out(:ncol, :, :, i), ncol, lchnk )
    end do

    call outfld( 'aero_component_len', aero_component_len_out(:ncol, :, :), ncol, lchnk )
    call outfld( 'number_of_particles', number_of_particles_out(:ncol, :), ncol, lchnk )
    call outfld( 'aero_num_conc', aero_num_conc_out(:ncol, :, :), ncol, lchnk )

  end subroutine partmc_mam_invoke

  subroutine write_nc_aero_state(aero_state, &
                              aero_particle_mass_out,&
                              aero_component_len_out,&
                              number_of_particles_out,&
                              aero_num_conc_out,&
                              icol, kk)

  implicit none
  !> aero_state to write.
  integer :: n_part, i_part, n_sp_aero
  type(aero_state_t), intent(in) :: aero_state
  ! real(kind=dp), intent(inout)  ::  aero_particle_mass_out(pcols,  pver, n_part_max*n_aero_sp_max)
  real(kind=dp), intent(inout)  ::  aero_particle_mass_out(pcols,  pver, n_part_max,n_aero_sp_max)
  real(kind=dp), intent(inout)  ::  aero_component_len_out(pcols, pver,  n_part_max)
  real(kind=dp), intent(inout)  ::  number_of_particles_out(pcols, pver)
  real(kind=dp), intent(inout)  ::  aero_num_conc_out(pcols, pver,  n_part_max)
  integer, intent(in) :: icol,kk

  real(kind=dp) :: aero_particle_mass(aero_state_n_part(aero_state), &
         aero_data_n_spec(aero_data))
  integer :: aero_component_len(aero_state_n_part(aero_state))
  integer :: array_position, i_comp, next_start_component_ind
  integer :: aero_component_particle_num(aero_state_total_n_components( &
         aero_state))
  integer :: aero_component_source_num(aero_state_total_n_components( &
         aero_state))
  real(kind=dp) :: aero_component_create_time( &
         aero_state_total_n_components(aero_state))
  integer :: aero_component_start_ind(aero_state_n_part(aero_state))
  real(kind=dp) :: aero_num_conc(aero_state_n_part(aero_state))

  n_part=aero_state_n_part(aero_state)
  n_sp_aero=aero_data_n_spec(aero_data)
  if ( n_part> 0) then
    next_start_component_ind = 1
    do i_part = 1,n_part
     aero_particle_mass(i_part, :) &
               = aero_state%apa%particle(i_part)%vol * aero_data%density
     aero_component_len(i_part) = aero_particle_n_components( &
              aero_state%apa%particle(i_part))
     aero_component_start_ind(i_part) = next_start_component_ind
          next_start_component_ind = next_start_component_ind &
               + aero_component_len(i_part)

    do i_comp = 1,aero_component_len(i_part)
             array_position = aero_component_start_ind(i_part) + i_comp - 1
             aero_component_particle_num(array_position) = i_part
             aero_component_source_num(array_position) &
                  = aero_state%apa%particle(i_part)%component(i_comp)%source_id
             aero_component_create_time(array_position) &
                  = aero_state%apa%particle(i_part)%component( &
                  i_comp)%create_time
    end do ! i_comp

    aero_num_conc(i_part) &
               = aero_state_particle_num_conc(aero_state, &
               aero_state%apa%particle(i_part), aero_data)


    end do !i-par

    if (masterproc) then
      write(102,*) 'n_part ', n_part
      write(102,*) 'n_sp_aero ', n_sp_aero
    endif

    aero_particle_mass_out(icol,kk, 1:n_part,1:n_sp_aero) = aero_particle_mass(:, :)
    aero_component_len_out(icol,kk,1:n_part) =aero_component_len(:)
    aero_num_conc_out(icol,kk,1:n_part) =aero_num_conc(:)
    number_of_particles_out(icol, kk) = n_part

  end if

  end subroutine

end module mo_partmc_interface

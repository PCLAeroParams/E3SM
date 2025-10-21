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
    type(aero_data_t) :: aero_data
    type aero_state_array_t
       type(aero_state_t), allocatable, dimension(:,:) :: aero_state
    end type aero_state_array_t
    type(aero_state_array_t), allocatable, dimension(:) :: aero_state_array
    type(scenario_t) :: scenario
    type(env_state_t) :: env_state
    type(env_state_t) :: env_state_init
    type(run_part_opt_t) :: run_part_opt
    integer :: i_repeat, i_group
    integer :: rand_init
    integer, parameter, public :: n_part_max = 100
    integer, parameter, public :: n_aero_sp_max = 25
    integer, parameter, public :: n_emit_mode = 1
    character, allocatable :: buffer(:)
    integer :: buffer_size, max_buffer_size
    integer :: position
    logical :: do_init_equilibrate, aero_mode_type_exp_present
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    integer :: nmodes,nspec_max_modes
    real(kind=dp) :: n_part
    ! mam information
    character(len=256), allocatable :: mam_num_names(:)
    character(len=256), allocatable :: mam_species_names(:,:)
contains
!-----------------------------------------------------------------------
  subroutine compute_nspec_max(nspec_max)

  use rad_constituents, only: rad_cnst_get_info

  ! Arguments
  integer, intent(out) :: nspec_max  ! Maximum number of species across all modes

  ! Local variables
  integer :: n           ! Loop index for modes
  integer :: nspec       ! Number of species in the current mode

  ! Initialize nspec_max
  nspec_max = 0
  ! Loop over modes to find the maximum number of species
  do n = 1, nmodes
    call rad_cnst_get_info(0, n, nspec=nspec)
    nspec_max = max(nspec_max, nspec)
  end do

end subroutine compute_nspec_max

subroutine save_num_and_species_names(num_names, species_names)

  use rad_constituents, only: rad_cnst_get_mode_num_idx, rad_cnst_get_mam_mmr_idx,rad_cnst_get_info
  use mo_tracname, only : solsym
  use mo_gas_phase_chemdr, only : map2chm

  ! Arguments
  character(len=*), intent(out) :: num_names(:)       ! Names of number fluxes per mode
  character(len=*), intent(out) :: species_names(:,:) ! Names of species per mode

  ! Local variables
  integer :: n, ispec , nspec    ! Loop indices for modes and species
  integer :: num_idx, spec_idx, idx_chm  ! Indices for number flux and species
  character(len=256) :: species_name     ! Temporary variable for species name
  character(len=256) :: num_name         ! Temporary variable for number flux name

  ! Loop over modes to retrieve names
  do n = 1, nmodes
    ! Get the number flux index for the mode
    call rad_cnst_get_mode_num_idx(n, num_idx)

    ! Convert num_idx to chemistry index and retrieve the name
    idx_chm = map2chm(num_idx)
    if (idx_chm > 0) then
      num_names(n) = solsym(idx_chm)
    else
      num_names(n) = "UNKNOWN"  ! Handle invalid index
    end if

    ! Loop over species in the mode to retrieve names
    ! Get the number of species in the mode
    call rad_cnst_get_info(0, n, nspec=nspec)
    do ispec = 1, nspec
      ! Get the species index for the mode and species
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)

      ! Convert spec_idx to chemistry index and retrieve the name
      idx_chm = map2chm(spec_idx)
      if (idx_chm > 0) then
        species_names(n, ispec) = solsym(idx_chm)
      else
        species_names(n, ispec) = "UNKNOWN"  ! Handle invalid index
      end if
    end do
  end do

end subroutine save_num_and_species_names

subroutine compute_partmc_emission_inputs(cflx, ncol, geom_mean_diameter, std_mam, num_fluxes, volume_fractions)

  ! Compute emission inputs for PartMC based on modal aerosol properties.
  use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_mode_props, rad_cnst_get_mode_num_idx, &
                              rad_cnst_get_mam_mmr_idx, rad_cnst_get_aer_props
  use chem_mods, only : adv_mass
  use physconst,        only: pi
  use mo_gas_phase_chemdr, only : map2chm
  use constituents,     only: pcnst, sflxnam
  use mo_tracname, only : solsym
  ! Arguments
  real(kind=dp), intent(in)  :: cflx(:,:)      ! constituent surface flux (kg/m^2/s for gas/aero species) or ( #/m^2/s) for num
  integer, intent(in)        :: ncol                 ! Number of columns
  real(kind=dp), intent(out) :: geom_mean_diameter(:,:)   ! Geometric dry mean diameter of the number distribution for each mode
  real(kind=dp), intent(out) :: std_mam(:)           ! Geometric standard deviation for each mode
  real(kind=dp), intent(out) :: num_fluxes(:,:)      ! Number fluxes for each mode ( # / m^2 / s)
  real(kind=dp), intent(out) :: volume_fractions(:,:,:)  ! Volume fractions for each species

  ! Local variables
  integer :: list_idx                          ! Index for climate or diagnostic list
  integer :: nspec                             ! Number of species in a mode
  integer :: n, ispec, icol                    ! Loop indices
  integer :: num_idx, spec_idx, idx_chm        ! Indices for number flux, species, and chemistry mapping
  real(kind=dp) :: sigmag                           ! Geometric standard deviation of mode
  real(kind=dp) :: alnsg                            ! Logarithm of sigmag
  real(kind=dp) :: dumfac                           ! Dummy factor for diameter calculation
  real(kind=dp) :: dummwdens                        ! Dummy density factor
  real(kind=dp) :: dryvol(ncol)                        ! Dry volume for each column
  real(kind=dp) :: specdens
  real(kind=dp) :: sum_vf_per_mode(ncol,nmodes)            ! Sum of mass mixing ratios per mode
  real(kind=dp), parameter :: third = 1.0 / 3.0  ! Constant for cube root calculation

  ! Initialize variables
  list_idx = 0  ! Climate list by default

  ! Loop over modes to compute properties
  geom_mean_diameter(:,:)=0.0
  do n = 1, nmodes
    ! Initialize dry volume
    dryvol(:) = 0.0

    ! Get mode properties
    call rad_cnst_get_mode_props(list_idx, n, sigmag=sigmag)
    std_mam(n) = sigmag
    alnsg = log(sigmag)
    dumfac = exp(4.5 * alnsg**2) * pi / 6.0

    ! Get number flux index
    call rad_cnst_get_mode_num_idx(n, num_idx)

    ! Get the number of species in the mode
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    if (masterproc) then
      idx_chm = map2chm(num_idx)
      write(102,*) "sflxnam(", num_idx, "):", sflxnam(num_idx), "solsym : ", solsym(idx_chm)
    end if

    ! Compute number fluxes
    do icol = 1, ncol
      num_fluxes(icol, n) = cflx(icol, num_idx)
    end do

    ! Compute dry volume
    do ispec = 1, nspec
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)
      call rad_cnst_get_aer_props(list_idx, n, ispec, density_aer=specdens)
      dummwdens = 1.0 / specdens
      do icol = 1, ncol
        dryvol(icol) = dryvol(icol) + max(0.0, cflx(icol, spec_idx)) * dummwdens
      end do
    end do

    ! Compute geometric mean diameter
    do icol = 1, ncol
      if (num_fluxes(icol, n) /= 0) then
        geom_mean_diameter(icol, n) = (dryvol(icol) / (dumfac * num_fluxes(icol, n)))**third
      end if
    end do
    if (masterproc) then
            write(102,*)  trim(adjustl(mam_num_names(n))) //": geom_mean_diameter(", 1, ",", n, "):", geom_mean_diameter(1, n)
    end if
  end do

  ! Compute volume fractions
  volume_fractions(:, :, :)=0.0
  sum_vf_per_mode(:,:) = 0.0
  do n = 1, nmodes
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    do ispec = 1, nspec
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)
      call rad_cnst_get_aer_props(list_idx, n, ispec, density_aer=specdens)
      dummwdens = 1.0 / specdens
      do icol = 1, ncol
        volume_fractions(icol, n, ispec) =  cflx(icol, spec_idx) * dummwdens
        sum_vf_per_mode(icol, n) = sum_vf_per_mode(icol, n) + volume_fractions(icol, n, ispec)
      end do
      if (masterproc) then
            write(102,*) "volume_fractions(", 1, n, ",", spec_idx, "):", volume_fractions(1,n, spec_idx)
      end if
    end do
  end do

  ! Normalize volume mixing ratio fractions
  do n = 1, nmodes
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    do ispec = 1, nspec
      idx_chm = map2chm(spec_idx)
        if (idx_chm > 0) then
          if (adv_mass(idx_chm) /= 0.0) then
            do icol = 1, ncol
              if (sum_vf_per_mode(icol, n) /= 0.0) then
                volume_fractions(icol, n, ispec) = volume_fractions(icol, n, ispec) / sum_vf_per_mode(icol, n)
              end if
            end do
          if (masterproc) then
            write(102,*)  trim(adjustl(mam_species_names( n, ispec))) //" : normalized volume_fractions(", 1, ",", n, ",", ispec, "):", volume_fractions(1, n, ispec)
          end if
          end if
        end if
    end do
  end do

end subroutine compute_partmc_emission_inputs

  subroutine spec_file_read_run_part_eam(run_part_opt, aero_data, &
       env_state_init, &
       aero_dist_init, &
       n_part, rand_init)

    !> Monte Carlo options.
    type(run_part_opt_t), intent(inout) :: run_part_opt
    !> Aerosol data.
    type(aero_data_t), intent(inout) :: aero_data
    !> Initial environmental state.
    type(env_state_t), intent(inout) :: env_state_init
    !> Initial aerosol distribution.
    type(aero_dist_t), intent(inout) :: aero_dist_init
    !> Ideal number of computational particles.
    real(kind=dp), intent(inout) :: n_part
    !> Random number generator seed.
    integer, intent(out) :: rand_init

    integer :: i_repeat, i_group
    logical :: read_aero_weight_classes
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    integer :: dummy
    real(kind=dp) :: dummy_time, dummy_del_t
    character(len=PMC_MAX_FILENAME_LEN) :: sub_filename
    type(spec_file_t) :: sub_file
    character(len=PMC_MAX_FILENAME_LEN) :: camp_config_filename
    character(len=AERO_MODE_NAME_LEN) :: mode_name

    integer, parameter :: n_aero_spec = 20
    integer, parameter :: n_gas_spec = 77
    integer :: n_swbands
    integer :: i_spec, i_mode

    character(AERO_NAME_LEN), parameter, dimension(n_aero_spec) :: &
         mosaic_spec_name = [ &
         "SO4   ", "NO3   ", "Cl    ", "NH4   ", "MSA   ", "ARO1  ", &
         "ARO2  ", "ALK1  ", "OLE1  ", "API1  ", "API2  ", "LIM1  ", &
         "LIM2  ", "CO3   ", "Na    ", "Ca    ", "OIN   ", "OC    ", &
         "BC    ", "H2O   "]
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

    n_part = 10

    env_state_init%elapsed_time = 0d0

    ! aero_data
    n_swbands = 1
    call ensure_string_array_size(aero_data%name, n_aero_spec)
    call ensure_integer_array_size(aero_data%mosaic_index, n_aero_spec)
    call ensure_real_array_size(aero_data%wavelengths, n_swbands)
    call ensure_real_array_size(aero_data%density, n_aero_spec)
    call ensure_integer_array_size(aero_data%num_ions, n_aero_spec)
    call ensure_real_array_size(aero_data%molec_weight, n_aero_spec)
    call ensure_real_array_size(aero_data%kappa, n_aero_spec)

    do i_spec = 1,n_aero_spec
       aero_data%name(i_spec) = mosaic_spec_name(i_spec)
       aero_data%density(i_spec) = 1800.0d0
       aero_data%kappa(i_spec) = 0.1d0
       aero_data%molec_weight(i_spec) = 18.0d0
       aero_data%num_ions(i_spec) = 0
       if (mosaic_spec_name(i_spec) == "H2O") then
          aero_data%i_water = i_spec
       end if
    end do
    aero_data%wavelengths = 550.0d0

    call aero_data_set_water_index(aero_data)
    call aero_data_set_mosaic_map(aero_data)

    call fractal_set_spherical(aero_data%fractal)

    ! Create a single mode to sample
    ! TODO: Replace with something informed by initial conditions
!    allocate(aero_dist_init%mode(1))
!    aero_dist_init%mode(1)%name = "TEST"
!    aero_dist_init%mode(1)%type = AERO_MODE_TYPE_LOG_NORMAL
!    aero_dist_init%mode(1)%source = aero_data_source_by_name(aero_data, &
!         aero_dist_init%mode(1)%name)
!    weight_class_name = aero_dist_init%mode(1)%name
!    aero_dist_init%mode(1)%weight_class = aero_data_weight_class_by_name(aero_data, &
!            weight_class_name)
!    aero_dist_init%mode(1)%char_radius = 1.0d-8
!    aero_dist_init%mode(1)%log10_std_dev_radius = log10(1.6d0)
!    aero_dist_init%mode(1)%num_conc = 1.0d9
!    allocate(aero_dist_init%mode(1)%vol_frac(aero_data_n_spec(aero_data)))
!    allocate(aero_dist_init%mode(1)%vol_frac_std(aero_data_n_spec(aero_data)))
!    aero_dist_init%mode(1)%vol_frac = 1.0d0 / 20
!    aero_dist_init%mode(1)%vol_frac_std = 0.0d0
!
!    aero_dist_init%mode(1)%sample_radius = [ real(kind=dp) :: ]
!    aero_dist_init%mode(1)%sample_num_conc = [ real(kind=dp) :: ]

    ! run_part_opt general settings
    run_part_opt%output_prefix = "./partmc_output/urban_plume"
    run_part_opt%n_repeat = 1
    run_part_opt%t_output = 0
    run_part_opt%t_progress = 0

    ! run_part_opt process settings
    run_part_opt%do_coagulation = .true.
    run_part_opt%coag_kernel_type = COAG_KERNEL_TYPE_BROWN
    run_part_opt%do_condensation = .false.
    run_part_opt%do_mosaic = .false.
    run_part_opt%do_optical = .false.
    run_part_opt%do_nucleation = .false.

    ! run_part_opt numerical/algorithm settings
    run_part_opt%do_select_weighting = .false.
    run_part_opt%weighting_type = AERO_STATE_WEIGHT_FLAT_SOURCE
    run_part_opt%weighting_exponent = 0.0d0
    rand_init = 0
    run_part_opt%allow_doubling = .true.
    run_part_opt%allow_halving = .true.
    run_part_opt%record_removals = .false.

    ! not applicable to this problem (MPI) settings
    run_part_opt%do_parallel=.false.
    run_part_opt%output_type = OUTPUT_TYPE_SINGLE
    run_part_opt%mix_timescale = 0d0
    run_part_opt%gas_average = .false.
    run_part_opt%env_average = .false.
    run_part_opt%parallel_coag_type = PARALLEL_COAG_TYPE_LOCAL

    do i_mode = 1,n_emit_mode
       write(mode_name,'(a,i2.2)') 'emit_mode_', i_mode
       dummy = aero_data_source_by_name(aero_data, mode_name)
       weight_class_name = mode_name
       dummy = aero_data_weight_class_by_name(aero_data, &
            weight_class_name)
    end do

  end subroutine spec_file_read_run_part_eam

  subroutine partmc_mam_inti(phys_state, species_class)
   use mo_tracname, only : solsym
   use cam_history,  only : addfld
   use cam_history_support, only: add_hist_coord
   use mo_chem_utls,        only : get_spc_ndx
   use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_mode_num_idx
   use constituents,     only: pcnst
   use physconst,    only: spec_class_aerosol, spec_class_gas
   use physics_types,    only : physics_state
   use mo_gas_phase_chemdr, only : map2chm
   use modal_aero_data, only: ntot_amode, modename_amode, sigmag_amode, &
       nspec_amode, numptr_amode, lmassptr_amode
   use mpi

   implicit none

   type(physics_state), intent(in) :: phys_state(begchunk:endchunk)
   integer, dimension(:), intent(in) :: species_class

   integer :: i, n_species, n_aero_species, i_spec,i_mode, nspec
   integer :: ncol, kk, icol, ichunk, idx_chm, num_idx, rank
   integer :: n_gas_species, nfs
   integer :: ierr
   character(len=100) :: file_name
   type(spec_file_t) :: file
   type(spec_file_t) :: sub_file
   type(aero_dist_t) :: aero_dist_init
   character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

   call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

   print*, 'MPI rank: ', rank, 'chunk start: ', begchunk, 'chunk end:', endchunk

   ! Allocate aero_state for each column of each chunk
   allocate(aero_state_array(begchunk:endchunk))
   do i = begchunk, endchunk
      ncol = phys_state(i)%ncol
      allocate(aero_state_array(i)%aero_state(ncol,pver))
   end do

  ! n_aero_species=7 ! get from eam
  ! Get number of actual (active) gas species.
  ! gas_pncst is "gas" species which apparently is not just gases.
  n_gas_species = 0
  do i =1,pcnst
     if (species_class(i) == spec_class_gas) then
        n_gas_species = n_gas_species + 1
     end if
  end do

  if (masterproc) then
     print*, 'number of active (?) gas species', n_gas_species
!     print*, 'number of fixed gas species if we need it', nfs
  end if

  call ensure_string_array_size(gas_data%name, n_gas_species)
  call gas_state_set_size(gas_state, n_gas_species)

  do i = 1,n_gas_species
     gas_data%name(i) = solsym(i)
  end do

  if (masterproc) then
     write(*,*) 'PartMC gas_data names'
     do i =1, gas_data_n_spec(gas_data)
        write(*,*) trim(gas_data%name(i))
     end do
  end if

  call spec_file_read_run_part_eam(run_part_opt, aero_data, &
       env_state_init, &
       aero_dist_init, &
       n_part, rand_init)
!  call aero_state_zero(aero_state)
!  call aero_state_set_weight(aero_state, aero_data, &
!       AERO_STATE_WEIGHT_FLAT_SOURCE)
!  call aero_state_set_n_part_ideal(aero_state, n_part)
!  call aero_state_add_aero_dist_sample(aero_state, aero_data, &
!       aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
!       run_part_opt%allow_halving)

    allocate(aero_dist_init%mode(ntot_amode))
    do i_mode = 1,ntot_amode
       aero_dist_init%mode(i_mode)%name =  modename_amode(i_mode)
       aero_dist_init%mode(i_mode)%type = AERO_MODE_TYPE_LOG_NORMAL
       aero_dist_init%mode(i_mode)%source = aero_data_source_by_name(aero_data, &
            aero_dist_init%mode(i_mode)%name)
       weight_class_name = aero_dist_init%mode(i_mode)%name
       aero_dist_init%mode(i_mode)%weight_class = aero_data_weight_class_by_name(aero_data, &
            weight_class_name)
       aero_dist_init%mode(i_mode)%log10_std_dev_radius = log10(sigmag_amode(i_mode))
       allocate(aero_dist_init%mode(i_mode)%vol_frac(aero_data_n_spec(aero_data)))
       allocate(aero_dist_init%mode(i_mode)%vol_frac_std(aero_data_n_spec(aero_data)))
       aero_dist_init%mode(i_mode)%vol_frac_std = 0.0d0
       aero_dist_init%mode(i_mode)%sample_radius = [ real(kind=dp) :: ]
       aero_dist_init%mode(i_mode)%sample_num_conc = [ real(kind=dp) :: ]
    end do

    do ichunk = begchunk,endchunk
    do kk = 1,pver
    do icol = 1, ncol
       do i_mode = 1,ntot_amode
          num_idx =  numptr_amode(i_mode)
          idx_chm = map2chm(num_idx)
          call rad_cnst_get_mode_num_idx(i_mode, num_idx)
          aero_dist_init%mode(i_mode)%char_radius = 1.0d-8
          aero_dist_init%mode(i_mode)%vol_frac = 1.0d0 / 20
          aero_dist_init%mode(i_mode)%num_conc = 1e6 + 1e6*phys_state(ichunk)%lon(icol) ** 2
!          aero_dist_init%mode(i_mode)%num_conc = phys_state(ichunk)%q(icol,kk,idx_chm)
       end do
       call aero_state_zero(aero_state_array(ichunk)%aero_state(icol,kk))
       call aero_state_set_weight(aero_state_array(ichunk)%aero_state(icol,kk), aero_data, &
            AERO_STATE_WEIGHT_FLAT_SOURCE)
       call aero_state_set_n_part_ideal(aero_state_array(ichunk)%aero_state(icol,kk), n_part)
       call aero_state_add_aero_dist_sample(aero_state_array(ichunk)%aero_state(icol,kk), &
            aero_data, aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
            run_part_opt%allow_halving)
    end do
    end do
    end do

  env_state = env_state_init

  call add_hist_coord('npartmax',    n_part_max,    'NPARTMAX')
  call add_hist_coord('na_spmax',    n_aero_sp_max,    'NAEROSPMAX')
  call add_hist_coord('na_sp_partmax',    n_aero_sp_max,    'NAEROSPPARTMAX')
  do i_spec = 1,aero_data_n_spec(aero_data)
    call addfld( 'aero_particle_mass_'// trim(aero_data%name(i_spec)), &
         (/'lev     ', 'npartmax' /), 'I', 'kg', 'constituent masses of each aerosol particle' )
  end do

  call addfld( 'number_concentration', (/'lev     ' /), 'I', 'm^{-3}', &
       'number concentration per cell' )
  call addfld( 'aero_num_conc', (/'lev     ', 'npartmax' /), 'I', 'm^{-3}', &
       'number concentration for each particle' )

  ! Get the number of modes
  call rad_cnst_get_info(0, nmodes=nmodes)
  call compute_nspec_max(nspec_max_modes)
  allocate(mam_num_names(nmodes))
  allocate(mam_species_names(nmodes, nspec_max_modes))
  call save_num_and_species_names(mam_num_names,mam_species_names)
  if (masterproc) then
    write(102,*) '-----------------------------------------'
    write(102,*) 'save_num_and_species_names'
    do i_mode = 1, nmodes
      write(102, "(A)", advance="no") "Mode " // trim(adjustl(mam_num_names(i_mode))) // ": "
      call rad_cnst_get_info(0, i_mode, nspec=nspec)
      do i_spec = 1, nspec
        write(102, "(A)", advance="no") trim(adjustl(mam_species_names(i_mode, i_spec))) // " "
      end do
      write(102,*)
    end do
    write(102,*) '-----------------------------------------'
  endif

  print*, 'done with PartMC initialization'

  end subroutine partmc_mam_inti

  subroutine partmc_mam_invoke(state, cflx, dt)
    use physics_types,    only : physics_state
    use cam_history,       only : outfld
    use constituents,     only: pcnst

    implicit none
    type(physics_state), intent(inout):: state
    real(kind=dp),       intent(in) :: cflx(pcols,pcnst)              ! constituent surface flux (kg/m^2/s)
    real(kind=dp),            intent(in)    :: dt              ! time step

    integer :: i, n_species, icol, kk, lchnk, ncol, n_aero_species
    real(kind=dp) ::  aero_particle_mass_out(pcols, pver,  n_part_max,n_aero_sp_max)
    real(kind=dp) ::  aero_component_len_out(pcols, pver,  n_part_max)
    real(kind=dp) ::  aero_num_conc_out(pcols, pver,  n_part_max)
    real(kind=dp) ::  number_conc_out(pcols, pver)
    real(kind=dp) ::  geom_mean_diameter(pcols, nmodes)
    real(kind=dp) ::  sigma_mam(nmodes)
    real(kind=dp) ::  num_fluxes(pcols, nmodes)
    real(kind=dp) ::  volume_fractions(pcols, nmodes, nspec_max_modes)

    integer ::  n_samp, n_coag, i_time, n_time, n_emit,nmodes
    real(kind=dp) :: emission_rate_scale, p
    real(kind=dp) :: characteristic_factor
    type(aero_dist_t) :: emissions

    !FIXME: get n_species this values from eam
    n_species= gas_data_n_spec(gas_data)
    !FIXME: we must pass a delta time factor
    n_time = 30
    run_part_opt%del_t = dt / n_time
    run_part_opt%t_max = dt
    run_part_opt%i_repeat = 1

    lchnk = state%lchnk
    ncol  = state%ncol

    ! Output arrays
    aero_particle_mass_out(:,:,:,:)=-1000d0
    number_conc_out(:,:) = 0d0
    aero_num_conc_out(:,:,:) = 0d0

    if (masterproc) then
         write(102,*) '-----------------------------------------'
         write(102,*) 'Time step and Time max'
         write(102,*) 'run_part_opt%del_t: ', run_part_opt%del_t
         write(102,*) 'run_part_opt%t_max: ', run_part_opt%t_max
         write(102,*) '-----------------------------------------'
    endif

    ! emissions

    ! FIXME: What time information does PartMC need here?
    env_state%start_time=0
    env_state%start_day=0
    env_state%elapsed_time=0d0

    ! emission inputs
    geom_mean_diameter(:,:)=0
    num_fluxes(:,:)=0
    call compute_partmc_emission_inputs(cflx, ncol, geom_mean_diameter, &
         sigma_mam, num_fluxes, volume_fractions)

    do kk = 1,pver
      do icol = 1, ncol
        ! Copy E3SM values to PartMC
        do i = 1,gas_data_n_spec(gas_data) !n_species
          ! TODO: Confirm units: PartMC is ppb. E3SM is vmr with units of mol/mol ?
          gas_state%mix_rat(i) = state%q(icol,kk,i) * 1d9
        end do ! species

        ! FIXME: Think about how to best do this scenario/env_state.
        ! scenario%temp(:)  = state%t(icol,kk)
        ! scenario%pressure = state%pmid(icol,kk)
        ! scenario%height(:) = state%zm(icol,kk)
        ! FIXME: we need to compute rel_humid
        !   See relhum array calculation in mo_gas_phase_chemdr.F90
        env_state%rel_humid = 0.95

        env_state%latitude = state%lat(icol)
        env_state%longitude = state%lon(icol)
        env_state%altitude = state%zm(icol,kk) ! Geopotential height (m)

        env_state%temp = state%t(icol,kk) ! Midpoint temperature (K)
        env_state%pressure = state%pmid(icol,kk) ! Midpoint pressure (Pa)
        ! FIXME: zm is midpoint geopotential height
        !        zi  is interface geopotential height
        !        we really want geometric height
        env_state%height = state%zi(icol,kk) - state%zi(icol,kk+1)
        ! FIXME: should compute this at some point
        !        see zenith() code in mo_gas_phase_chemdr.F90
        env_state%solar_zenith_angle = 0d0

        ! For now, lets just try coagulation + emission.
        ! We probably want a custom code here anyway to have better control:
        !    - We might need a custom time stepper for efficiency in TChem solving
        !    - E3SM probably will control emissions
        !    - We have to remove dilution
        !    - It appears we have no (stored) time varying scenario data.
        n_coag = 0
        n_samp = 0
        n_emit = 0
        call partmc_interface_e3sm_emissions(state, emissions, &
           geom_mean_diameter(icol,:), sigma_mam, num_fluxes(icol,:), volume_fractions(icol, : , :))


        if (masterproc) then
           if (icol == 1) then
           print*, kk, env_state%temp, env_state%pressure,env_state%height,  env_state%altitude
           end if
        end if 

        do i_time = 1,n_time

           ! Aerosol emissions
           if (kk == pver) then
              emission_rate_scale = 1.0d0
              characteristic_factor = 3600.0d0 / run_part_opt%del_t
              p = emission_rate_scale * run_part_opt%del_t / env_state%height
              call aero_state_add_aero_dist_sample(aero_state_array(lchnk)%aero_state(icol,kk), &
                   aero_data,  emissions, p, characteristic_factor, env_state%elapsed_time, & 
                   run_part_opt%allow_doubling, run_part_opt%allow_halving, n_emit)
           end if

           ! Coagulation
           ! DISABLED
           ! call mc_coag(run_part_opt%coag_kernel_type, env_state, &
           !      aero_data, aero_state, run_part_opt%del_t, n_samp, n_coag)

           ! Rebalance
           call aero_state_rebalance(aero_state_array(lchnk)%aero_state(icol,kk), aero_data, &
                run_part_opt%allow_doubling, &
                run_part_opt%allow_halving, initial_state_warning=.false.)

        end do
        call write_nc_aero_state(aero_state_array(lchnk)%aero_state(icol,kk), &
             aero_particle_mass_out, &
                            aero_num_conc_out, &
                            number_conc_out, &
                            icol, kk)
      end do ! icol
    end do ! kk

   ! Output to E3SM
    do i = 1,aero_data_n_spec(aero_data)
        call outfld( 'aero_particle_mass_'// trim(aero_data%name(i)), &
             aero_particle_mass_out(:ncol, :, :, i), ncol, lchnk )
    end do

    call outfld( 'number_concentration', number_conc_out(:ncol, :), ncol, lchnk )
    call outfld( 'aero_num_conc', aero_num_conc_out(:ncol, :, :), ncol, lchnk )

  end subroutine partmc_mam_invoke

  ! Compute bulk statistics
  subroutine write_nc_aero_state(aero_state, &
                              aero_particle_mass_out, &
                              aero_num_conc_out, &
                              number_conc_out, &
                              icol, kk)

  implicit none
  !> aero_state to write.
  integer :: n_part, i_part, n_sp_aero
  type(aero_state_t), intent(in) :: aero_state
  real(kind=dp), intent(inout)  ::  aero_particle_mass_out(pcols,  pver, n_part_max,n_aero_sp_max)
  real(kind=dp), intent(inout)  ::  aero_num_conc_out(pcols, pver,  n_part_max)
  real(kind=dp), intent(inout)  ::  number_conc_out(pcols, pver)
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
    do i_part = 1,n_part
     aero_particle_mass(i_part, :) &
         = aero_state%apa%particle(i_part)%vol * aero_data%density

     aero_num_conc(i_part) &
               = aero_state_particle_num_conc(aero_state, &
               aero_state%apa%particle(i_part), aero_data)

    end do !i-part

    if (masterproc) then
      write(102,*) 'n_part ', n_part
      write(102,*) 'n_sp_aero ', n_sp_aero
    endif

    aero_particle_mass_out(icol,kk, 1:n_part,1:n_sp_aero) = aero_particle_mass(:, :)
    aero_num_conc_out(icol,kk,1:n_part) = aero_num_conc(:)
    number_conc_out(icol,kk) = sum(aero_num_conc)

  end if

  end subroutine write_nc_aero_state

  subroutine partmc_interface_e3sm_emissions(state, emissions, geom_mean_diam, &
      sigma, num_fluxes, vol_frac)
    use physics_types,    only : physics_state

    implicit none
    type(physics_state), intent(in):: state
    type(aero_dist_t), intent(inout) :: emissions

    real(kind=dp), intent(in) ::  geom_mean_diam(nmodes)
    real(kind=dp), intent(in) ::  sigma(nmodes)
    real(kind=dp), intent(in) ::  num_fluxes(nmodes)
    real(kind=dp), intent(in) ::  vol_frac(nmodes, nspec_max_modes)

    integer :: i_mode
    character(len=AERO_MODE_NAME_LEN) :: mode_name
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

    if (allocated(emissions%mode)) deallocate(emissions%mode)
    allocate(emissions%mode(n_emit_mode))

    do i_mode = 1,n_emit_mode
       write(mode_name,'(a,i2.2)') 'emit_mode_', i_mode
       emissions%mode(i_mode)%name = mode_name
       emissions%mode(i_mode)%type = AERO_MODE_TYPE_LOG_NORMAL
       emissions%mode(i_mode)%source = aero_data_source_by_name(aero_data, &
            emissions%mode(i_mode)%name)
       weight_class_name = emissions%mode(i_mode)%name
       emissions%mode(i_mode)%weight_class = aero_data_weight_class_by_name(aero_data, &
               weight_class_name)
       emissions%mode(i_mode)%char_radius = 1.0d-8
       emissions%mode(i_mode)%log10_std_dev_radius = log10(1.6d0)
       emissions%mode(i_mode)%num_conc = 1.0d6
       allocate(emissions%mode(i_mode)%vol_frac(aero_data_n_spec(aero_data)))
       allocate(emissions%mode(i_mode)%vol_frac_std(aero_data_n_spec(aero_data)))
       emissions%mode(i_mode)%vol_frac = 1.0d0 / 20
       emissions%mode(i_mode)%vol_frac_std = 0.0d0

       emissions%mode(i_mode)%sample_radius = [ real(kind=dp) :: ]
       emissions%mode(i_mode)%sample_num_conc = [ real(kind=dp) :: ]
    end do

  end subroutine partmc_interface_e3sm_emissions

end module mo_partmc_interface

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

    ! Array of aero_state for each chunk. (columns, levels)
    type aero_state_array_t
       type(aero_state_t), allocatable, dimension(:,:) :: aero_state
    end type aero_state_array_t
    ! Array of aero_state_arrays. (chunks)
    type(aero_state_array_t), allocatable, dimension(:) :: aero_state_array

    type(scenario_t) :: scenario
    type(env_state_t) :: env_state
    type(env_state_t) :: env_state_init
    type(run_part_opt_t) :: run_part_opt
    integer :: i_repeat, i_group
    integer :: rand_init
    ! Maximum number of computational particles. Used for output.
    integer, parameter, public :: n_part_max = 500
    ! Maximum number of aerosol species. Used for output.
    integer, parameter, public :: n_aero_sp_max = 25

    ! Toggle between two emission pathways:
    !   .false. — original cflx pathway (5 generic emit modes)
    !   .true.  — sector-resolved pathway (one PartMC mode per (MAM mode, CMIP6 sector))
    logical, parameter, public :: use_sector_emissions = .true.
    ! Number of active PartMC emission modes. For the cflx pathway this stays 5;
    ! for the sector pathway it is overwritten at init from the discovered
    ! catalog: one mode per present (MAM-group, CMIP6 sector) pair plus the two
    ! natural sampled modes (sea salt, dust).
    integer, public :: n_emit_mode = 5
    ! Canonical anthropogenic sector list (CMIP6 / AeroCom). The sector
    ! catalog only registers a (MAM-group, sector) pair if the inventory
    ! actually carries that variable.
    integer, parameter :: n_canon_sectors = 8
    character(len=8), parameter :: canon_sectors(n_canon_sectors) = &
         (/ 'AGR     ', 'ENE     ', 'IND     ', 'RCO     ', &
            'SHP     ', 'SLV     ', 'TRA     ', 'WST     ' /)
    ! Per-mode catalog populated when use_sector_emissions = .true.
    type sector_mode_t
       character(len=32) :: name             ! e.g. emit_BCPOM_AGR
       character(len=8) :: sector           ! canonical sector
       integer :: parent_mam_mode  ! 1, 2, or 4
       real(kind=dp) :: sigma_g          ! geometric std dev
       integer :: n_mass           ! Number of species in the mass flux.
       character(len=16), allocatable :: mass_species(:)  ! e.g. bc_a4, pom_a4
       character(len=32), allocatable :: mass_sec_var(:)  ! sector var in that file
       integer, allocatable :: mass_pmc_idx(:)  ! PartMC aero_data species index
       integer :: n_num            ! Number of species in the number flux.
       character(len=16), allocatable :: num_species(:)   ! num_aN
       character(len=32), allocatable :: num_sec_var(:)   ! sector var in num file
       ! Sampled-mode metadata for natural sources (SEASALT, DUST). For
       ! anthropogenic modes is_sampled stays .false. and these fields are unused.
       logical :: is_sampled = .false.
       integer :: n_samples  = 0                       ! number of bins
       real(kind=dp), allocatable :: sample_radius(:)  ! n_samples+1 bin edge radii (m)
       integer :: sample_pmc_idx = 0                   ! single composition species index
                                                       ! (sampled modes have one vol_frac=1 species)
    end type sector_mode_t

    type(sector_mode_t), allocatable :: sector_modes(:)
    ! Max bins across sampled modes — used to size the per-step
    ! sample_num_conc buffer in partmc_mam_invoke.
    integer :: max_n_samples = 0
    ! Number of PartMC sub-bins per MAM dust bin. MAM has 2 wide dust bins
    ! (0.1-1 and 1-10 microns diameters); subdividing log-uniformly into K sub-bins
    ! per MAM bin gives PartMC finer size resolution while keeping the total
    ! mass per MAM bin equal to dust_emis output. Example: K=8 leads to
    ! 10^(1/8) = 1.33, putting mass error from log-uniform sampling within
    ! each sub-bin at the few-percent level.
    integer, parameter :: n_dust_subbins_per_mam_bin = 8
    character, allocatable :: buffer(:)
    integer :: buffer_size, max_buffer_size
    integer :: position
    logical :: do_init_equilibrate, aero_mode_type_exp_present
    character(len=PMC_MAX_FILENAME_LEN) :: restart_filename
    integer :: dummy_index, dummy_i_repeat
    integer :: nmodes,nspec_max_modes
    integer, parameter :: list_idx = 0  ! Climate list (0) vs. diagnostic list
    real(kind=dp), parameter :: third = 1.0d0 / 3.0d0
    real(kind=dp) :: n_part_ideal
    ! mam information
    character(len=256), allocatable :: mam_num_names(:)
    character(len=256), allocatable :: mam_species_names(:,:)
    integer, allocatable :: mam_spec_to_partmc_spec(:,:)
    ! Initial q values captured on first invoke (phys_state%q not valid at inti time)
    logical, allocatable :: q_init_saved(:)  ! (begchunk:endchunk)

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
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
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
  do n = 1,nmodes
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
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
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



  ! Loop over modes to compute properties
  geom_mean_diameter(:,:) = 0.0d0
  do n = 1, nmodes
    ! Initialize dry volume
    dryvol(:) = 0.0d0

    ! Get mode properties
    call rad_cnst_get_mode_props(list_idx, n, sigmag=sigmag)
    std_mam(n) = sigmag
    alnsg = log(sigmag)
    dumfac = exp(4.5d0 * alnsg**2) * pi / 6.0d0

    ! Get number flux index
    call rad_cnst_get_mode_num_idx(n, num_idx)

    ! Get the number of species in the mode
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    if (masterproc) then
      idx_chm = map2chm(num_idx)
      write(102,*) "sflxnam(", num_idx, "):", sflxnam(num_idx), "solsym : ", solsym(idx_chm)
    end if

    ! Compute number fluxes
    do icol = 1,ncol
      num_fluxes(icol, n) = cflx(icol, num_idx)
    end do

    ! Compute dry volume
    do ispec = 1, nspec
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)
      call rad_cnst_get_aer_props(list_idx, n, ispec, density_aer=specdens)
      dummwdens = 1.0d0 / specdens
      do icol = 1,ncol
        dryvol(icol) = dryvol(icol) + max(0.0d0, cflx(icol, spec_idx)) * dummwdens
      end do
    end do

    ! Compute geometric mean diameter
    do icol = 1,ncol
      if (num_fluxes(icol, n) /= 0) then
        geom_mean_diameter(icol, n) = (dryvol(icol) / (dumfac * num_fluxes(icol, n)))**third
      end if
    end do
    if (masterproc) then
            write(102,*)  trim(adjustl(mam_num_names(n))) //": geom_mean_diameter(", 1, ",", n, "):", geom_mean_diameter(1, n)
    end if
  end do

  ! Compute volume fractions
  volume_fractions(:, :, :) = 0.0d0
  sum_vf_per_mode(:,:) = 0.0d0
  do n = 1, nmodes
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    do ispec = 1, nspec
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)
      call rad_cnst_get_aer_props(list_idx, n, ispec, density_aer=specdens)
      dummwdens = 1.0d0 / specdens
      do icol = 1,ncol
        volume_fractions(icol, n, ispec) =  cflx(icol, spec_idx) * dummwdens
        sum_vf_per_mode(icol, n) = sum_vf_per_mode(icol, n) + volume_fractions(icol, n, ispec)
      end do
      if (masterproc) then
            write(102,*) "volume_fractions(", 1, n, ",", ispec, "):", volume_fractions(1,n, ispec)
      end if
    end do
  end do

  ! Normalize volume mixing ratio fractions
  do n = 1, nmodes
    call rad_cnst_get_info(list_idx, n, nspec=nspec)
    do ispec = 1, nspec
      call rad_cnst_get_mam_mmr_idx(n, ispec, spec_idx)
      idx_chm = map2chm(spec_idx)
        if (idx_chm > 0) then
          if (adv_mass(idx_chm) /= 0.0d0) then
            do icol = 1,ncol
              if (sum_vf_per_mode(icol, n) /= 0.0d0) then
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

  ! Sets properties of the PartMC run. For now, hardcoded.
  subroutine spec_file_read_run_part_eam(run_part_opt, &
       env_state_init, &
       n_part_ideal, rand_init)

    !> Monte Carlo options.
    type(run_part_opt_t), intent(inout) :: run_part_opt
    !> Initial environmental state.
    type(env_state_t), intent(inout) :: env_state_init
    !> Ideal number of computational particles.
    real(kind=dp), intent(inout) :: n_part_ideal
    !> Random number generator seed.
    integer, intent(out) :: rand_init

    integer :: dummy
    integer :: i_mode
    character(len=AERO_MODE_NAME_LEN) :: mode_name
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

    n_part_ideal = 250.0d0

    env_state_init%elapsed_time = 0d0

    ! run_part_opt general settings
    run_part_opt%output_prefix = "./partmc_output/urban_plume"
    run_part_opt%n_repeat = 1
    run_part_opt%t_output = 0
    run_part_opt%t_progress = 0

    ! run_part_opt process settings
    ! Coagulation disabled for now since it is slow and we want to
    ! focus on emissions and getting the interface working.
    ! Add it back later by changing to true.
    run_part_opt%do_coagulation = .false.
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
       if (use_sector_emissions) then
          mode_name = sector_modes(i_mode)%name
       else
          write(mode_name,'(a,i2.2)') 'emit_mode_', i_mode
       end if
       dummy = aero_data_source_by_name(aero_data, mode_name)
       weight_class_name = mode_name
       dummy = aero_data_weight_class_by_name(aero_data, &
            weight_class_name)
    end do

  end subroutine spec_file_read_run_part_eam

  ! Initializes the data structures of PartMC.
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

    type(physics_state), intent(in) :: phys_state(begchunk:endchunk)
    integer, dimension(:), intent(in) :: species_class

    integer :: i, i_spec, i_mode, nspec
    integer :: ncol, kk, icol, ichunk, idx_chm, num_idx
    integer :: n_gas_species
    integer :: gas_species_idx(pcnst)
    integer :: rank, ierr ! Remove when debugging of processor removed
    type(aero_dist_t) :: aero_dist_init
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

    integer :: naero, n_modes

    ! Uncomment for processor information for debugging
    !call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    !print*, 'MPI rank: ', rank, 'chunk start: ', begchunk, 'chunk end:', endchunk

    ! Allocate aero_state for each column of each chunk
    allocate(aero_state_array(begchunk:endchunk))
    do i = begchunk, endchunk
       ncol = phys_state(i)%ncol
       allocate(aero_state_array(i)%aero_state(ncol,pver))
    end do

    ! Allocate storage for initial q (captured on first invoke, not here,
    ! because phys_state%q is not yet populated at inti time)
    allocate(q_init_saved(begchunk:endchunk))
    q_init_saved(:) = .false.

    ! Count and collect the gas-phase constituents out of the
    ! full constituent list; gas_data/gas_state are sized to this count.
    n_gas_species = 0
    do i = 1,pcnst
       if (species_class(i) == spec_class_gas) then
          n_gas_species = n_gas_species + 1
          gas_species_idx(n_gas_species) = i
       end if
    end do

    if (masterproc) then
       write(102,*) 'Number of active gas species:', n_gas_species
    end if

    call ensure_string_array_size(gas_data%name, n_gas_species)
    call gas_state_set_size(gas_state, n_gas_species)

    do i = 1,n_gas_species
       gas_data%name(i) = solsym(map2chm(gas_species_idx(i)))
    end do

    if (masterproc) then
       write(102,*) 'PartMC gas_data names'
       do i = 1,gas_data_n_spec(gas_data)
          write(102,*) trim(gas_data%name(i))
       end do
    end if

    ! Initialization of aerosol data
    call rad_cnst_get_info(list_idx, nmodes=nmodes)
    call compute_nspec_max(nspec_max_modes)
    call aero_data_init(aero_data)

    ! Build the (MAM-group, sector) catalog and override n_emit_mode if the
    ! sector-resolved pathway is active. Must run before spec_file_read_run_part_eam
    ! because that routine registers one aero_data source/weight-class per emit mode.
    if (use_sector_emissions) then
       call partmc_build_sector_catalog()
    end if

    call spec_file_read_run_part_eam(run_part_opt, &
         env_state_init, n_part_ideal, rand_init)

    ! Make initial condition modes exist
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

    ! Set the weighting schemes and number of ideal particles for each aero_state
    do ichunk = begchunk,endchunk
       ncol = phys_state(ichunk)%ncol
       do kk = 1,pver
          do icol = 1,ncol
             call aero_state_zero(aero_state_array(ichunk)%aero_state(icol,kk))
             call aero_state_set_weight(aero_state_array(ichunk)%aero_state(icol,kk), aero_data, &
                  AERO_STATE_WEIGHT_FLAT_SOURCE)
             call aero_state_set_n_part_ideal(aero_state_array(ichunk)%aero_state(icol,kk), n_part_ideal)
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
    allocate(mam_num_names(nmodes))
    allocate(mam_species_names(nmodes, nspec_max_modes))
    call save_num_and_species_names(mam_num_names, mam_species_names)

    if (masterproc) then
       write(102,*) '-----------------------------------------'
       write(102,*) 'save_num_and_species_names'
       do i_mode = 1,nmodes
          write(102, "(A)", advance="no") "Mode " // trim(adjustl(mam_num_names(i_mode))) // ": "
          call rad_cnst_get_info(list_idx, i_mode, nspec=nspec)
          do i_spec = 1,nspec
             write(102, "(A)", advance="no") trim(adjustl(mam_species_names(i_mode, i_spec))) // " "
          end do
          write(102,*)
       end do
       write(102,*) '-----------------------------------------'

       write(102,*) '-----------------------------------------'
       write(102,*) 'aero_data sources (', size(aero_data%source_name), ')'
       do i = 1, size(aero_data%source_name)
          write(102,*) i, trim(aero_data%source_name(i))
       end do
       write(102,*) '-----------------------------------------'
       write(102,*) 'aero_data weight classes (', size(aero_data%weight_class_name), ')'
       do i = 1, size(aero_data%weight_class_name)
          write(102,*) i, trim(aero_data%weight_class_name(i))
       end do
       write(102,*) '-----------------------------------------'
       write(102,*) 'aero_state n_part_ideal = ', n_part_ideal
       write(102,*) '-----------------------------------------'
    end if

  end subroutine partmc_mam_inti

  ! Solves a time step dt of PartMC.
  subroutine partmc_mam_invoke(state, cam_in, cflx, dt)
    use physics_types,    only : physics_state
    use camsrfexch,       only : cam_in_t
    use cam_history,       only : outfld
    use constituents,     only: pcnst
    use mo_chem_utls,        only : get_spc_ndx
    use physconst,    only: spec_class_aerosol, spec_class_gas
    use ppgrid,           only : pver

    type(physics_state), intent(inout):: state
    ! cam_in carries sst, ocnfrac, dstflx for the natural-source pathway
    ! (sea salt, dust). cflx carries the emissions for MAM modes (but not sectors).
    ! TODO: once the natural-source field set is settled, decide whether to
    ! narrow this to specific arguments (sst, ocnfrac, dstflx, ...).
    type(cam_in_t),      intent(in) :: cam_in
    real(kind=dp),       intent(in) :: cflx(pcols,pcnst)  ! constituent surface flux (kg/m^2/s)
    real(kind=dp),            intent(in)    :: dt         ! time step

    integer :: i, icol, kk, lchnk, ncol
    real(kind=dp) ::  aero_particle_mass_out(pcols, pver,  n_part_max,n_aero_sp_max)
    real(kind=dp) ::  aero_num_conc_out(pcols, pver,  n_part_max)
    real(kind=dp) ::  number_conc_out(pcols, pver)
    ! cflx-pathway arrays (indexed by MAM mode)
    real(kind=dp) ::  geom_mean_diameter(pcols, nmodes)
    real(kind=dp) ::  sigma_mam(nmodes)
    real(kind=dp) ::  num_fluxes(pcols, nmodes)
    real(kind=dp) ::  volume_fractions(pcols, nmodes, nspec_max_modes)
    ! sector-pathway arrays (indexed by sector mode); allocatable so we only
    ! pay the storage when the sector pathway is active
    real(kind=dp), allocatable :: geom_mean_diameter_sec(:,:)
    real(kind=dp), allocatable :: sigma_emode(:)
    real(kind=dp), allocatable :: num_fluxes_sec(:,:)
    real(kind=dp), allocatable :: vol_frac_sec(:,:,:)
    ! Natural-source per-step buffers (sampled-mode pathway)
    real(kind=dp), allocatable :: u10cubed(:)              ! (pcols)
    real(kind=dp), allocatable :: sample_num_conc(:,:,:)   ! (pcols, n_emit_mode, max_n_samples)

    integer ::  n_samp, n_coag, i_time, n_time, n_emit
    integer :: i_mode
    real(kind=dp) :: emission_rate_scale, p
    real(kind=dp) :: characteristic_factor
    type(aero_dist_t) :: emissions

    !FIXME: we must pass a delta time factor
    n_time = 30
    run_part_opt%del_t = dt / n_time
    run_part_opt%t_max = dt
    run_part_opt%i_repeat = 1

    lchnk = state%lchnk
    ncol  = state%ncol

    ! Capture initial condition on first invocation for this chunk.
    ! (phys_state%q is not populated until d_p_coupling runs before the first
    ! timestep, which is after partmc_mam_inti is called during phys_init.)
    if (.not. q_init_saved(lchnk)) then
       call partmc_init_aero_dist(state)
    end if

    ! Output arrays
    aero_particle_mass_out(:,:,:,:)= -1000d0
    number_conc_out(:,:) = 0d0
    aero_num_conc_out(:,:,:) = 0d0

    if (masterproc) then
       write(102,*) '-----------------------------------------'
       write(102,*) 'Time step and Time max'
       write(102,*) 'run_part_opt%del_t: ', run_part_opt%del_t
       write(102,*) 'run_part_opt%t_max: ', run_part_opt%t_max
       write(102,*) '-----------------------------------------'
    end if

    ! FIXME: What time information does PartMC need here?
    env_state%start_time = 0d0
    env_state%start_day = 0d0
    env_state%elapsed_time = 0d0

    ! Emission inputs from E3SM
    if (use_sector_emissions) then
       allocate(geom_mean_diameter_sec(pcols, n_emit_mode))
       allocate(sigma_emode(n_emit_mode))
       allocate(num_fluxes_sec(pcols, n_emit_mode))
       allocate(vol_frac_sec(pcols, n_emit_mode, aero_data_n_spec(aero_data)))
       call compute_partmc_emission_inputs_sector(lchnk, ncol, &
            geom_mean_diameter_sec, sigma_emode, num_fluxes_sec, vol_frac_sec)
       ! Natural-source pseudo-modes (SEASALT, DUST): fills u10cubed and the
       ! bin-resolved sample_num_conc number fluxes.
       if ( max_n_samples > 0 ) then
          allocate(u10cubed(pcols))
          allocate(sample_num_conc(pcols, n_emit_mode, max_n_samples))
          call compute_partmc_natural_emission_inputs(state, cam_in, ncol, &
               u10cubed, sample_num_conc)
       end if
    else
       geom_mean_diameter(:,:) = 0d0
       num_fluxes(:,:) = 0d0
       call compute_partmc_emission_inputs(cflx, ncol, geom_mean_diameter, &
            sigma_mam, num_fluxes, volume_fractions)
    end if

    do kk = 1,pver
       do icol = 1,ncol
          ! Copy E3SM values to PartMC
          do i = 1,gas_data_n_spec(gas_data) !n_species
             ! TODO: Confirm units: PartMC is ppb. E3SM is vmr with units of mol/mol ?
             gas_state%mix_rat(i) = state%q(icol,kk,i) * 1d9
          end do ! species

          ! Debugging
          !if (masterproc) then
          !   write(102,*) state%q(icol,kk,:)
          !end if

          ! FIXME: Think about how to best do this scenario/env_state.
          ! scenario%temp(:)  = state%t(icol,kk)
          ! scenario%pressure = state%pmid(icol,kk)
          ! scenario%height(:) = state%zm(icol,kk)
          ! FIXME: we need to compute rel_humid
          !   See relhum array calculation in mo_gas_phase_chemdr.F90
          !!      call qsat(tfld(:ncol,:), pmid(:ncol,:), satv, satq)
          !!      relhum(:,k) = .622_r8 * h2ovmr(:,k) / satq(:,k)
          !!      relhum(:,k) = max( 0._r8,min( 1._r8,relhum(:,k) ) )
          env_state%rel_humid = 0.95d0

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
          ! Set the PartMC data structure for aerosol emissions
          if (use_sector_emissions) then
             ! TODO: we can refactor this when we are happy with the mixing of modal
             ! vs binned emissions.
             if ( allocated(sample_num_conc) ) then
                call partmc_interface_e3sm_emissions_sector(emissions, &
                     geom_mean_diameter_sec(icol,:), sigma_emode, &
                     num_fluxes_sec(icol,:), vol_frac_sec(icol,:,:), &
                     sample_num_conc(icol,:,:))
             else
                call partmc_interface_e3sm_emissions_sector(emissions, &
                     geom_mean_diameter_sec(icol,:), sigma_emode, &
                     num_fluxes_sec(icol,:), vol_frac_sec(icol,:,:))
             end if
          else
             call partmc_interface_e3sm_emissions(state, emissions, &
                  geom_mean_diameter(icol,:), sigma_mam, num_fluxes(icol,:), volume_fractions(icol,:,:))
          end if

          if (masterproc) then
             if (icol == 1) then
                write(102,*) '-----------------------------------------'
                write(102,*) 'grid cell | temperature | pressure | box height | altitude'
                write(102,*) kk, env_state%temp, env_state%pressure, env_state%height, env_state%altitude
                write(102,*) '-----------------------------------------'
             end if
             if (kk == pver) then
                write(102,*) '-----------------------------------------'
                write(102,*) 'i_mode | name | radius | log10sigma | number flux'
                do i_mode = 1,n_emit_mode
                   write(102,*) i_mode, trim(emissions%mode(i_mode)%name), &
                        emissions%mode(i_mode)%char_radius, &
                        emissions%mode(i_mode)%log10_std_dev_radius, &
                        emissions%mode(i_mode)%num_conc
                   do i = 1,aero_data_n_spec(aero_data)
                      if (emissions%mode(i_mode)%vol_frac(i) > 0.0d0) then
                         write(102,*) '   vol_frac ', trim(aero_data%name(i)), &
                              emissions%mode(i_mode)%vol_frac(i)
                      end if
                   end do
                end do
                write(102,*) '-----------------------------------------'
             end if
          end if

          ! Time stepping for PartMC processes
          do i_time = 1,n_time

             ! Aerosol emissions
             if (kk == pver) then
                emission_rate_scale = 1.0d0
                characteristic_factor = 3600.0d0 / run_part_opt%del_t
                p = emission_rate_scale * run_part_opt%del_t / env_state%height
                call aero_state_add_aero_dist_sample(aero_state_array(lchnk)%aero_state(icol,kk), &
                     aero_data, emissions, p, characteristic_factor, env_state%elapsed_time, &
                     run_part_opt%allow_doubling, run_part_opt%allow_halving, n_emit)
             end if

             ! Coagulation
             if (run_part_opt%do_coagulation) then
                call mc_coag(run_part_opt%coag_kernel_type, env_state, aero_data, &
                     aero_state_array(lchnk)%aero_state(icol,kk), run_part_opt%del_t, &
                     n_samp, n_coag)
             end if

             ! Rebalance
             call aero_state_rebalance(aero_state_array(lchnk)%aero_state(icol,kk), aero_data, &
                  run_part_opt%allow_doubling, &
                  run_part_opt%allow_halving, initial_state_warning=.false.)

          end do
          call write_nc_aero_state(aero_state_array(lchnk)%aero_state(icol,kk), &
               aero_particle_mass_out, aero_num_conc_out, number_conc_out, icol, kk)
       end do ! icol
    end do ! kk

    ! Output to E3SM
    do i = 1,aero_data_n_spec(aero_data)
       call outfld( 'aero_particle_mass_'// trim(aero_data%name(i)), &
            aero_particle_mass_out(:ncol, :, :, i), ncol, lchnk )
    end do

    call outfld( 'number_concentration', number_conc_out(:ncol, :), ncol, lchnk )
    call outfld( 'aero_num_conc', aero_num_conc_out(:ncol, :, :), ncol, lchnk )

    if (use_sector_emissions) then
       deallocate(geom_mean_diameter_sec, sigma_emode, num_fluxes_sec, vol_frac_sec)
       if ( allocated(u10cubed) )        deallocate(u10cubed)
       if ( allocated(sample_num_conc) ) deallocate(sample_num_conc)
    end if

  end subroutine partmc_mam_invoke

  ! Populates aero_state_array with the initial aerosol distribution for chunk
  ! lchnk from the first-timestep values of physics_state%q.
  subroutine partmc_init_aero_dist(state)
    use physics_types,    only : physics_state
    use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_mode_num_idx, rad_cnst_get_mam_mmr_idx
    use mo_gas_phase_chemdr, only : map2chm
    use modal_aero_data, only: ntot_amode, modename_amode, sigmag_amode, numptr_amode

    type(physics_state), intent(in) :: state

    integer :: lchnk, ncol
    type(aero_dist_t) :: aero_dist_init
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name
    integer :: i_mode, i_spec, kk, icol, pmc_aero_idx
    integer :: num_idx, idx_chm, spec_idx, nspec
    real(kind=dp) :: dryvol, dumfac, dummwdens, dgnum_dry, num_a

    lchnk = state%lchnk
    ncol  = state%ncol

    allocate(aero_dist_init%mode(ntot_amode))
    do i_mode = 1,ntot_amode
       aero_dist_init%mode(i_mode)%name = modename_amode(i_mode)
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

    do kk = 1,pver
       do icol = 1,ncol
          do i_mode = 1,ntot_amode
             ! Set number concentration of the mode
             call rad_cnst_get_mode_num_idx(i_mode, num_idx)
             idx_chm = map2chm(num_idx)
             if (masterproc) then
                if (kk == pver .and. icol == 1) then
                   write(102,*) "Initial number mixing ratio for mode ", i_mode, " is ", state%q(icol,kk,idx_chm), idx_chm, state%q(icol,kk,num_idx),num_idx
                end if
             end if
             num_a = state%q(icol,kk,num_idx)  ! number mixing ratio (#/kg_air)
             aero_dist_init%mode(i_mode)%vol_frac = 0.0d0
             ! Set volume fraction of the mode
             call rad_cnst_get_info(list_idx, i_mode, nspec=nspec)
             dryvol = 0.d0
             do i_spec = 1,nspec
                call rad_cnst_get_mam_mmr_idx(i_mode, i_spec, spec_idx)
                idx_chm = spec_idx !map2chm(spec_idx)
                pmc_aero_idx = mam_spec_to_partmc_spec(i_mode, i_spec)
                ! Convert from mass mixing ratio to volume fraction using density
                aero_dist_init%mode(i_mode)%vol_frac(pmc_aero_idx) = &
                     state%q(icol,kk,idx_chm) / aero_data%density(pmc_aero_idx)
                ! Compute dry volume of mode for diameter calculation
                dummwdens = 1.0d0 / aero_data%density(pmc_aero_idx)
                dryvol = dryvol + max(0.0d0, state%q(icol,kk,idx_chm))*dummwdens
             end do
             ! Normalize volume fractions
             if (sum(aero_dist_init%mode(i_mode)%vol_frac) > 0.0d0) then
                aero_dist_init%mode(i_mode)%vol_frac = aero_dist_init%mode(i_mode)%vol_frac / sum(aero_dist_init%mode(i_mode)%vol_frac)
             else
                aero_dist_init%mode(i_mode)%vol_frac = 1.0d0 / aero_data_n_spec(aero_data)
             end if
             ! Calculate geometric mean diameter using mixing ratios - conversions cancel
             dumfac = exp(4.5d0 * log(sigmag_amode(i_mode))**2) * const%pi / 6.0d0
             if (num_a > 0.0d0) then
                dgnum_dry = (dryvol / (dumfac * num_a))**third
             else
                dgnum_dry = 0.0d0
             end if
             aero_dist_init%mode(i_mode)%char_radius = dgnum_dry / 2.0d0
             ! Convert number mixing ratio (#/kg_air) to number concentration (#/m^3)
             aero_dist_init%mode(i_mode)%num_conc = num_a * state%pmid(icol,kk) &
                  / (const%univ_gas_const / const%air_molec_weight * state%t(icol,kk))
          end do
          if (masterproc) then
             if (icol == 1 .and. kk == pver) then
               do i_mode = 1,ntot_amode
                write(102,*) '-----------------------------------------'
                write(102,*) 'Initial condition for mode ', i_mode
                write(102,*) 'radius:', aero_dist_init%mode(i_mode)%char_radius
                write(102,*) 'number concentration:', aero_dist_init%mode(i_mode)%num_conc
                do i_spec = 1,aero_data_n_spec(aero_data)
                   write(102,*) 'volume fraction for ', trim(aero_data%name(i_spec)), ':', &
                        aero_dist_init%mode(i_mode)%vol_frac(i_spec)
                end do
               end do
             end if

          end if
          call aero_state_add_aero_dist_sample(aero_state_array(lchnk)%aero_state(icol,kk), &
               aero_data, aero_dist_init, 1d0, 1d0, 0d0, run_part_opt%allow_doubling, &
               run_part_opt%allow_halving)
       end do
    end do

    ! Flag to indicate the initial condition has been set for this chunk.
    q_init_saved(lchnk) = .true.

  end subroutine partmc_init_aero_dist

  ! Compute bulk statistics
  subroutine write_nc_aero_state(aero_state, &
                              aero_particle_mass_out, &
                              aero_num_conc_out, &
                              number_conc_out, &
                              icol, kk)

  !> aero_state to write.
  integer :: n_part, i_part, n_sp_aero
  type(aero_state_t), intent(in) :: aero_state
  real(kind=dp), intent(inout)  ::  aero_particle_mass_out(pcols,  pver, n_part_max,n_aero_sp_max)
  real(kind=dp), intent(inout)  ::  aero_num_conc_out(pcols, pver,  n_part_max)
  real(kind=dp), intent(inout)  ::  number_conc_out(pcols, pver)
  integer, intent(in) :: icol,kk

  real(kind=dp) :: aero_particle_mass(aero_state_n_part(aero_state), &
         aero_data_n_spec(aero_data))
  real(kind=dp) :: aero_num_conc(aero_state_n_part(aero_state))

  n_part=aero_state_n_part(aero_state)
  n_sp_aero=aero_data_n_spec(aero_data)
  if (n_part > 0) then
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

  ! Maps E3SM aerosol emissions to the PartMC emissions data structure.
  subroutine partmc_interface_e3sm_emissions(state, emissions, geom_mean_diam, &
      sigma, num_fluxes, vol_frac)
    use physics_types,    only : physics_state
    use rad_constituents, only: rad_cnst_get_info

    type(physics_state), intent(in):: state
    ! Emissions data structure to pass to PartMC.
    type(aero_dist_t), intent(inout) :: emissions

    real(kind=dp), intent(in) ::  geom_mean_diam(nmodes)
    real(kind=dp), intent(in) ::  sigma(nmodes)
    real(kind=dp), intent(in) ::  num_fluxes(nmodes)
    real(kind=dp), intent(in) ::  vol_frac(nmodes, nspec_max_modes)
    integer :: i_mode, i_spec, n_spec_emit, pmc_spec_idx
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
       emissions%mode(i_mode)%char_radius = geom_mean_diam(i_mode) / 2.0d0
       emissions%mode(i_mode)%log10_std_dev_radius = log10(sigma(i_mode))
       emissions%mode(i_mode)%num_conc = num_fluxes(i_mode)
       allocate(emissions%mode(i_mode)%vol_frac(aero_data_n_spec(aero_data)))
       allocate(emissions%mode(i_mode)%vol_frac_std(aero_data_n_spec(aero_data)))
       emissions%mode(i_mode)%vol_frac = 0.0d0
       ! Number of emitted species in this mode
       call rad_cnst_get_info(list_idx, i_mode, nspec=n_spec_emit)
       do i_spec = 1,n_spec_emit
          pmc_spec_idx = mam_spec_to_partmc_spec(i_mode, i_spec)
          emissions%mode(i_mode)%vol_frac(pmc_spec_idx) = vol_frac(i_mode, i_spec)
       end do
       emissions%mode(i_mode)%vol_frac_std = 0.0d0
       emissions%mode(i_mode)%sample_radius = [ real(kind=dp) :: ]
       emissions%mode(i_mode)%sample_num_conc = [ real(kind=dp) :: ]
    end do

  end subroutine partmc_interface_e3sm_emissions

  ! Initializes aero_data from E3SM aerosol scheme.
  subroutine aero_data_init(aero_data)
    use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_aer_props
    use modal_aero_data, only: &
        lspectype_amode, ntot_aspectype, specname_amode, specdens_amode, specmw_amode, spechygro

    !> Aerosol data.
    type(aero_data_t), intent(inout) :: aero_data

    integer :: n_aero_spec
    integer :: n_swbands
    integer :: i_spec
    integer :: m, l
    integer :: n_spec, n_modes
    real(kind=dp) :: density, hygro
    character(len=20):: aername
    integer :: i, j, n, unique_count, total_mam_vars, i_name
    logical :: is_unique
    character(len=20), dimension(:), allocatable :: input_array, unique_array
    real(kind=dp), dimension(:), allocatable :: density_array, kappa_array, mw_array
    real(kind=dp), dimension(:), allocatable :: unique_density_array, unique_kappa_array, unique_mw_array

    ! Notes:
    !   ntot_aspectype = overall number of aerosol chemical species defined (over all modes)
    !   specdens_amode(l) = dry density (kg/m^3) of aerosol chemical species type l
    !   specmw_amode(l) = molecular weight (kg/kmol) of aerosol chemical species type l
    !   specname_amode(l) = name of aerosol chemical species type l
    !   spechygro(l) = hygroscopicity of aerosol chemical species type l
    !   lspectype_amode(l,m) = index of species type l in mode m
    if (masterproc) then
      ! Number of aerosol chemical species defined (over all modes)
      print*, 'number of total aerosol species', ntot_aspectype
      call rad_cnst_get_info(list_idx, nmodes=n_modes)
      do m = 1,n_modes
         ! Properties of modal species
         call rad_cnst_get_info(list_idx, m, nspec=n_spec)
         do l = 1,n_spec
            call rad_cnst_get_aer_props(list_idx, m, l, &
               aername = aername, &
               density_aer = density, &
               hygro_aer   = hygro)
           write(102,*) m,l, trim(aername), density, hygro, lspectype_amode(l,m)
         end do
      end do
      do l=1,ntot_aspectype
         write(102,*) trim(specname_amode(l)), specdens_amode(l), specmw_amode(l), spechygro(l)
      end do

    end if

    ! Unique strings of the aername in the modes
    call rad_cnst_get_info(list_idx, nmodes=n_modes)
    total_mam_vars = 0
    do m =1,n_modes
       call rad_cnst_get_info(list_idx, m, nspec=n_spec)
       total_mam_vars = total_mam_vars + n_spec
    end do
    allocate(input_array(total_mam_vars))
    allocate(unique_array(total_mam_vars))
    allocate(density_array(total_mam_vars))
    allocate(kappa_array(total_mam_vars))
    allocate(mw_array(total_mam_vars))
    allocate(unique_density_array(total_mam_vars))
    allocate(unique_kappa_array(total_mam_vars))
    allocate(unique_mw_array(total_mam_vars))

    i_name = 0
    do m = 1,n_modes
       call rad_cnst_get_info(list_idx, m, nspec=n_spec)
       do l = 1,n_spec
          call rad_cnst_get_aer_props(list_idx, m, l, &
               aername = aername, &
               density_aer = density, &
               hygro_aer = hygro)
          i_name = i_name + 1
          input_array(i_name) = aername
          density_array(i_name) = density
          kappa_array(i_name) =  hygro
          mw_array(i_name) = specmw_amode(lspectype_amode(l,m))
      end do
    end do

   unique_count = 0
   do i = 1,total_mam_vars
    is_unique = .true.
    do j = 1, unique_count
      if (trim(input_array(i)) == trim(unique_array(j))) then
        is_unique = .false.
        exit
      end if
    end do
    if (is_unique) then
      unique_count = unique_count + 1
      unique_array(unique_count) = input_array(i)
      unique_density_array(unique_count) = density_array(i)
      unique_kappa_array(unique_count) = kappa_array(i)
      unique_mw_array(unique_count) = mw_array(i)
    end if
  end do

!    if(masterproc) then
!       do i=1,unique_count
!          write(102,*) unique_array(i)
!       end do
!    end if

    ! Option 1
    n_aero_spec = unique_count
    ! Option 2
    !n_aero_spec = ntot_aspectype

    n_swbands = 1
    call ensure_string_array_size(aero_data%name, n_aero_spec)
    call ensure_integer_array_size(aero_data%mosaic_index, n_aero_spec)
    call ensure_real_array_size(aero_data%wavelengths, n_swbands)
    call ensure_real_array_size(aero_data%density, n_aero_spec)
    call ensure_integer_array_size(aero_data%num_ions, n_aero_spec)
    call ensure_real_array_size(aero_data%molec_weight, n_aero_spec)
    call ensure_real_array_size(aero_data%kappa, n_aero_spec)

    do i_spec = 1,n_aero_spec
       ! Option 1 with minor issue of molecular weight
       aero_data%name(i_spec) = trim(unique_array(i_spec))
       aero_data%density(i_spec) = unique_density_array(i_spec)
       aero_data%kappa(i_spec) = unique_kappa_array(i_spec)
       aero_data%molec_weight(i_spec) = unique_mw_array(i_spec)

       ! Option 2
!       aero_data%name(i_spec) = specname_amode(i_spec)
!       aero_data%density(i_spec) = specdens_amode(i_spec)
!       aero_data%kappa(i_spec) = spechygro(i_spec)
!       aero_data%molec_weight(i_spec) = specmw_amode(i_spec)

       aero_data%num_ions(i_spec) = 0
    end do

    ! Set the optical wavelength (not used)
    aero_data%wavelengths = 550.0d0

    ! Set the index of water
    call aero_data_set_water_index(aero_data)
    ! Set MOSAIC map (not used)
    call aero_data_set_mosaic_map(aero_data)

    ! Set fractal properties
    call fractal_set_spherical(aero_data%fractal)

    ! Map MAM "species" to PartMC species
    allocate(mam_spec_to_partmc_spec(n_modes, nspec_max_modes))
    mam_spec_to_partmc_spec = 0
    do m = 1,n_modes
       call rad_cnst_get_info(list_idx, m, nspec=n_spec)
       do l = 1,n_spec
          call rad_cnst_get_aer_props(list_idx, m, l, aername = aername)
          ! Find the index
          mam_spec_to_partmc_spec(m,l) = aero_data_spec_by_name(aero_data, aername)
      end do
      if (masterproc) then
         write(102,*) mam_spec_to_partmc_spec(m,:n_spec)
      end if
    end do

    ! Print results
    if (masterproc) then
       write(102,*) 'Contents of aero_data'
       write(102,*) 'Name | Density | Molecular weight | kappa'
       do i_spec = 1,n_aero_spec
          write(102,*) trim(aero_data%name(i_spec)), aero_data%density(i_spec), &
             aero_data%molec_weight(i_spec), aero_data%kappa(i_spec)
       end do
    end if

  end subroutine aero_data_init

  !---------------------------------------------------------------------
  ! Sector-resolved emissions handling
  !---------------------------------------------------------------------

  ! Resolves a MAM constituent name (e.g. 'bc_a4', 'so4_a1') to the
  ! corresponding PartMC aero_data species index by walking MAM modes.
  ! Returns 0 if the constituent is not found in any mode.
  integer function pmc_idx_for_constituent(constituent_name)
    use rad_constituents, only : rad_cnst_get_info, rad_cnst_get_mam_mmr_idx, &
                                    rad_cnst_get_aer_props
    use mo_tracname, only : solsym
    use mo_gas_phase_chemdr, only : map2chm

    ! MAM species name to resolve (e.g. 'bc_a4', 'so4_a1')
    character(len=*), intent(in) :: constituent_name

    integer :: m, l, nspec, spec_idx, idx_chm
    character(len=20) :: aername

    pmc_idx_for_constituent = 0
    do m = 1,nmodes
       call rad_cnst_get_info(list_idx, m, nspec=nspec)
       do l = 1,nspec
          call rad_cnst_get_mam_mmr_idx(m, l, spec_idx)
          idx_chm = map2chm(spec_idx)
          if (idx_chm > 0) then
             if (trim(solsym(idx_chm)) == trim(constituent_name)) then
                call rad_cnst_get_aer_props(list_idx, m, l, aername=aername)
                pmc_idx_for_constituent = aero_data_spec_by_name(aero_data, aername)
                return
             end if
          end if
       end do
    end do

  end function pmc_idx_for_constituent

  ! Returns .true. if the given srf-emis species carries the named sector
  ! variable. Sector names match the raw NetCDF variable names exactly
  ! (e.g. 'AGR' for so4_a1, 'num_a1_BC_AGR' for num_a4). Note the possibility of
  ! a1/a4 issue in the inputs.
  logical function species_has_sector(species_name, sector_name)
    use mo_srf_emissions, only : has_srf_emis_species, &
                                 get_srf_emis_n_sectors, &
                                 get_srf_emis_sector_name

    ! Species name.
    character(len=*), intent(in) :: species_name
    ! Sector name.
    character(len=*), intent(in) :: sector_name

    integer :: nsec, isec
    character(len=32) :: sname

    species_has_sector = .false.
    if ( .not. has_srf_emis_species(species_name)) return
    nsec = get_srf_emis_n_sectors(species_name)
    do isec = 1,nsec
       call get_srf_emis_sector_name(species_name, isec, sname)
       if (trim(sname) == trim(sector_name) ) then
          species_has_sector = .true.
          return
       end if
    end do

  end function species_has_sector

  ! Populates a sector_mode_t entry for the SEASALT pseudo-mode.
  !
  ! TODO: Currently we are just sampling a single SEASALT mode but we may
  ! want to add organics later, either as another external mode or interally
  ! mixed with the sea salt.
  subroutine build_seasalt_sector_mode(sm)
    use sslt_sections,    only : nsections, rdry
    use modal_aero_data,  only : sigmag_amode

    type(sector_mode_t), intent(inout) :: sm
    integer :: i

    sm%name = 'emit_SEASALT'
    sm%sector = 'OCEAN'
    sm%parent_mam_mode = 0
    sm%sigma_g = sigmag_amode(1) ! unused for sampled mode
    sm%n_mass = 0 ! unused for sampled mode
    sm%n_num  = 0 ! unused for sampled mode

    sm%is_sampled = .true.
    sm%n_samples = nsections
    ! Any ncl constituent will work here.
    sm%sample_pmc_idx = pmc_idx_for_constituent('ncl_a1')

    ! The bin center radii in rdry are log-spaced; sample_radius
    ! holds n_samples+1 bin edges constructed by geometric midpoint of
    ! consecutive centers, with the first/last edges extrapolated using
    ! the same log-ratio so PartMC's sampled-mode implementation can be used
    ! without any modification.
    allocate(sm%sample_radius(nsections + 1))
    do i = 2,nsections
       sm%sample_radius(i) = sqrt(rdry(i-1) * rdry(i))
    end do
    ! Extrapolate first and last edges using the same log-ratio as the adjacent bins.
    sm%sample_radius(1) = rdry(1) * sqrt(rdry(1) / rdry(2))
    sm%sample_radius(nsections+1) = rdry(nsections) * sqrt(rdry(nsections) / rdry(nsections-1))

  end subroutine build_seasalt_sector_mode

  ! Populates a sector_mode_t entry for the DUST pseudo-mode. Subdivides
  ! each of MAM's dust_nbin wide bins into K = n_dust_subbins_per_mam_bin
  ! log-spaced sub-bins. The full sample_radius edge array spans all MAM
  ! bins and shares the inter-MAM-bin edge between adjacent sub-bins.
  subroutine build_dust_sector_mode(sm)
    use dust_model,       only : dust_nbin, dust_dmt_grd
    use modal_aero_data,  only : sigmag_amode

    type(sector_mode_t), intent(inout) :: sm
    integer :: m, k, isub
    real(kind=dp) :: r_lo, r_hi, log_ratio

    sm%name = 'emit_DUST'
    sm%sector = 'LAND'
    sm%parent_mam_mode = 0
    sm%sigma_g = sigmag_amode(3) ! unused for sampled mode
    sm%n_mass = 0 ! unused for sampled mode
    sm%n_num  = 0 ! unused for sampled mode

    sm%is_sampled = .true.
    sm%n_samples  = dust_nbin * n_dust_subbins_per_mam_bin
    ! Any dst constituent will work here.
    sm%sample_pmc_idx = pmc_idx_for_constituent('dst_a1')

    ! Build sub-bin edges: K-1 interior edges log-spaced within each MAM
    ! bin, sharing edges with the next MAM bin. Total edges = n_samples + 1.
    allocate(sm%sample_radius(sm%n_samples + 1))
    isub = 0
    do m = 1, dust_nbin
       r_lo = dust_dmt_grd(m)   / 2.0d0
       r_hi = dust_dmt_grd(m+1) / 2.0d0
       log_ratio = log(r_hi / r_lo)
       do k = 0, n_dust_subbins_per_mam_bin - 1
          isub = isub + 1
          sm%sample_radius(isub) = r_lo * exp( real(k, kind=dp) &
               / real(n_dust_subbins_per_mam_bin, kind=dp) * log_ratio )
       end do
    end do
    ! Final edge: top of last MAM bin.
    sm%sample_radius(sm%n_samples + 1) = dust_dmt_grd(dust_nbin + 1) / 2.0d0

  end subroutine build_dust_sector_mode

  ! Walks the canonical CMIP6 anthropogenic sector list and builds one
  ! sector_modes entry per (MAM-group, sector) pair that is actually present
  ! in the inventory. Sets module-level n_emit_mode to the discovered count.
  !
  ! Example MAM-groups handled (MAM4 + CMIP6):
  !   * BCPOM  (parent mode 4) — bc_a4 + pom_a4 + num_a4
  !   * SO4a1  (parent mode 1) — so4_a1 + num_a1
  !   * SO4a2  (parent mode 2) — so4_a2 + num_a2
  !
  ! The MAM split between so4_a1/so4_a2 (and the corresponding number)
  ! is inherited as-is. SEASALT and DUST natural modes are appended
  ! to the catalog via build_seasalt_sector_mode / build_dust_sector_mode.
  subroutine partmc_build_sector_catalog()
    use modal_aero_data, only : sigmag_amode

    integer :: i_sec, i_out, i_pass
    character(len=8) :: sector
    logical :: have_bcpom, have_so4a1, have_so4a2

    ! Pass 1: Count the number of emission modes based on sectors.
    ! Pass 2: Populate sector information.
    do i_pass = 1,2
       i_out = 0
       do i_sec = 1,n_canon_sectors
          sector = canon_sectors(i_sec)

          have_bcpom = species_has_sector('bc_a4',  trim(sector)) .and. &
                       species_has_sector('pom_a4', trim(sector))
          have_so4a1 = species_has_sector('so4_a1', trim(sector))
          have_so4a2 = species_has_sector('so4_a2', trim(sector))

          ! Handle Hydrophobic BCPOM sectors in a4 (Accumulation) mode.
          if (have_bcpom) then
             i_out = i_out + 1
             if (i_pass == 2) then
                sector_modes(i_out)%name = 'emit_BCPOM_'//trim(sector)
                sector_modes(i_out)%sector = sector
                sector_modes(i_out)%parent_mam_mode = 4
                sector_modes(i_out)%sigma_g = sigmag_amode(4)
                sector_modes(i_out)%n_mass = 2
                allocate(sector_modes(i_out)%mass_species(2))
                allocate(sector_modes(i_out)%mass_sec_var(2))
                allocate(sector_modes(i_out)%mass_pmc_idx(2))
                sector_modes(i_out)%mass_species(1) = 'bc_a4'
                sector_modes(i_out)%mass_sec_var(1) = trim(sector)
                sector_modes(i_out)%mass_pmc_idx(1) = pmc_idx_for_constituent('bc_a4')
                sector_modes(i_out)%mass_species(2) = 'pom_a4'
                sector_modes(i_out)%mass_sec_var(2) = trim(sector)
                sector_modes(i_out)%mass_pmc_idx(2) = pmc_idx_for_constituent('pom_a4')
                sector_modes(i_out)%n_num = 2
                allocate(sector_modes(i_out)%num_species(2))
                allocate(sector_modes(i_out)%num_sec_var(2))
                ! WARNING: num_a4 file uses 'num_a1_BC_*' / 'num_a1_POM_*' variable names
                ! (CMIP6 inventory artifact — naming follows the chemistry, not
                ! the destination MAM mode).
                sector_modes(i_out)%num_species(1) = 'num_a4'
                sector_modes(i_out)%num_sec_var(1) = 'num_a1_BC_'//trim(sector)
                sector_modes(i_out)%num_species(2) = 'num_a4'
                sector_modes(i_out)%num_sec_var(2) = 'num_a1_POM_'//trim(sector)
             end if
          end if

          ! Handle SO4 mode a1 (Accumulation) sectors.
          if (have_so4a1) then
             i_out = i_out + 1
             if (i_pass == 2) then
                sector_modes(i_out)%name = 'emit_SO4a1_'//trim(sector)
                sector_modes(i_out)%sector = sector
                sector_modes(i_out)%parent_mam_mode = 1
                sector_modes(i_out)%sigma_g = sigmag_amode(1)
                sector_modes(i_out)%n_mass = 1
                allocate(sector_modes(i_out)%mass_species(1))
                allocate(sector_modes(i_out)%mass_sec_var(1))
                allocate(sector_modes(i_out)%mass_pmc_idx(1))
                sector_modes(i_out)%mass_species(1) = 'so4_a1'
                sector_modes(i_out)%mass_sec_var(1) = trim(sector)
                sector_modes(i_out)%mass_pmc_idx(1) = pmc_idx_for_constituent('so4_a1')
                sector_modes(i_out)%n_num = 1
                allocate(sector_modes(i_out)%num_species(1))
                allocate(sector_modes(i_out)%num_sec_var(1))
                sector_modes(i_out)%num_species(1) = 'num_a1'
                sector_modes(i_out)%num_sec_var(1) = 'num_a1_SO4_'//trim(sector)
             end if
          end if

          ! Handle SO4 mode a2 (Aitken) sectors.
          if (have_so4a2) then
             i_out = i_out + 1
             if (i_pass == 2) then
                sector_modes(i_out)%name = 'emit_SO4a2_'//trim(sector)
                sector_modes(i_out)%sector = sector
                sector_modes(i_out)%parent_mam_mode = 2
                sector_modes(i_out)%sigma_g = sigmag_amode(2)
                sector_modes(i_out)%n_mass = 1
                allocate(sector_modes(i_out)%mass_species(1))
                allocate(sector_modes(i_out)%mass_sec_var(1))
                allocate(sector_modes(i_out)%mass_pmc_idx(1))
                sector_modes(i_out)%mass_species(1) = 'so4_a2'
                sector_modes(i_out)%mass_sec_var(1) = trim(sector)
                sector_modes(i_out)%mass_pmc_idx(1) = pmc_idx_for_constituent('so4_a2')
                sector_modes(i_out)%n_num = 1
                allocate(sector_modes(i_out)%num_species(1))
                allocate(sector_modes(i_out)%num_sec_var(1))
                sector_modes(i_out)%num_species(1) = 'num_a2'
                sector_modes(i_out)%num_sec_var(1) = 'num_a2_SO4_'//trim(sector)
             end if
          end if
       end do

       ! Natural-sources: register modes for natural source so aero_data knows about
       ! the source/weight-class names. Populate mode sample_radius edges here while
       ! per-step sample_num_conc is computed by compute_partmc_natural_emission_inputs.
       i_out = i_out + 1
       if (i_pass == 2) then
          call build_seasalt_sector_mode(sector_modes(i_out))
       end if

       i_out = i_out + 1
       if (i_pass == 2) then
          call build_dust_sector_mode(sector_modes(i_out))
       end if

       if (i_pass == 1) then
          n_emit_mode = i_out
          allocate(sector_modes(n_emit_mode))
       end if
    end do

    ! Determine max_n_samples for the sampled mode array to be properly sized
    max_n_samples = 0
    do i_out = 1, n_emit_mode
       if (sector_modes(i_out)%n_samples > max_n_samples) then
          max_n_samples = sector_modes(i_out)%n_samples
       end if
    end do

    if (masterproc) then
       write(102,*) '-----------------------------------------'
       write(102,*) 'PartMC sector emission catalog'
       write(102,*) 'n_emit_mode = ', n_emit_mode
       do i_out = 1,n_emit_mode
          write(102,*) i_out, ' ', trim(sector_modes(i_out)%name), &
               ' parent_mam=', sector_modes(i_out)%parent_mam_mode, &
               ' sigma_g=', sector_modes(i_out)%sigma_g
       end do
       write(102,*) '-----------------------------------------'
    end if

  end subroutine partmc_build_sector_catalog

  ! Sector-resolved versions of compute_partmc_emission_inputs.
  ! Produces per-(column, sector_mode) inputs for PartMC. vol_frac is indexed
  ! directly by PartMC aero_data species index (not the per-mode species index)
  ! because each sector_mode lists its constituents.
  subroutine compute_partmc_emission_inputs_sector(lchnk, ncol, &
       geom_mean_diameter, sigma_emode, num_fluxes, vol_frac)
    use mo_srf_emissions, only : get_srf_emis_sector_flux
    use physconst, only : pi

    ! Chunk index.
    integer, intent(in)  :: lchnk
    ! Number of columns in chunk.
    integer, intent(in)  :: ncol
    ! Geometric mean diameter of each sector mode.
    real(kind=dp), intent(out) :: geom_mean_diameter(:,:) ! (pcols, n_emit_mode)
    ! Geometric standard deviation of each sector mode.
    real(kind=dp), intent(out) :: sigma_emode(:) ! (n_emit_mode)
    ! Number flux of each sector mode.
    real(kind=dp), intent(out) :: num_fluxes(:,:) ! (pcols, n_emit_mode)
    ! Volume fraction of each PartMC species in each sector mode.
    real(kind=dp), intent(out) :: vol_frac(:,:,:) ! (pcols, n_emit_mode, n_aero_spec)

    integer  :: i_mode, j, icol, pmc_idx
    real(kind=dp) :: alnsg, dumfac, specdens, dummwdens
    real(kind=dp) :: tmp(pcols)
    real(kind=dp) :: dryvol(pcols), sum_vf(pcols)

    geom_mean_diameter(:,:) = 0.0d0
    num_fluxes(:,:) = 0.0d0
    vol_frac(:,:,:) = 0.0d0

    do i_mode = 1,n_emit_mode
       sigma_emode(i_mode) = sector_modes(i_mode)%sigma_g
       alnsg  = log(sector_modes(i_mode)%sigma_g)
       dumfac = exp(4.5d0 * alnsg**2) * pi / 6.0d0

       ! Sum number flux across all num sources for this sector mode
       do j = 1,sector_modes(i_mode)%n_num
          call get_srf_emis_sector_flux( &
               sector_modes(i_mode)%num_species(j), &
               sector_modes(i_mode)%num_sec_var(j), &
               lchnk, ncol, tmp)
          do icol = 1, ncol
             num_fluxes(icol, i_mode) = num_fluxes(icol, i_mode) + tmp(icol)
          end do
       end do

       ! Accumulate dry volume flux per species; unnormalized vol_frac stored in place
       dryvol(:) = 0.0d0
       sum_vf(:) = 0.0d0
       do j = 1,sector_modes(i_mode)%n_mass
          pmc_idx = sector_modes(i_mode)%mass_pmc_idx(j)
          if (pmc_idx <= 0) cycle
          specdens  = aero_data%density(pmc_idx)
          dummwdens = 1.0d0 / specdens
          call get_srf_emis_sector_flux( &
               sector_modes(i_mode)%mass_species(j), &
               sector_modes(i_mode)%mass_sec_var(j), &
               lchnk, ncol, tmp)
          do icol = 1,ncol
             vol_frac(icol, i_mode, pmc_idx) = max(0.0d0, tmp(icol)) * dummwdens
             dryvol(icol) = dryvol(icol) + vol_frac(icol, i_mode, pmc_idx)
             sum_vf(icol) = sum_vf(icol) + vol_frac(icol, i_mode, pmc_idx)
          end do
       end do

       do icol = 1,ncol
          if (num_fluxes(icol, i_mode) > 0.0d0) then
             geom_mean_diameter(icol, i_mode) = &
                  (dryvol(icol) / (dumfac * num_fluxes(icol, i_mode)))**third
          end if
       end do

       ! Normalize vol_frac.
       do j = 1,sector_modes(i_mode)%n_mass
          pmc_idx = sector_modes(i_mode)%mass_pmc_idx(j)
          if (pmc_idx <= 0) cycle
          do icol = 1,ncol
             if (sum_vf(icol) > 0.0d0) then
                vol_frac(icol, i_mode, pmc_idx) = vol_frac(icol, i_mode, pmc_idx) / sum_vf(icol)
             end if
          end do
       end do
    end do

  end subroutine compute_partmc_emission_inputs_sector

  ! Per-step inputs for the natural-source pseudo-modes (SEASALT, DUST).
  !
  !   * Seasalt: per-bin number flux from sslt_sections::fluxes(sst, u10cubed),
  !              scaled by ocnfrac and seasalt_emis_scale.
  !   * Dust:    per-MAM-bin mass from dust_model.F90:dust_emis logic
  !              (cam_in%dstflx, dust_emis_sclfctr, soil_erodibility, soil_erod_fact),
  !              split equally across n_dust_subbins_per_mam_bin sub-bins (uniform
  !              mass per log(r) within each MAM bin), then converted to per-sub-bin
  !              number via the volume-weighted moment over each sub-bin's edges.
  subroutine compute_partmc_natural_emission_inputs(state, cam_in, ncol, &
       u10cubed, sample_num_conc)
    use physics_types, only : physics_state
    use camsrfexch,    only : cam_in_t
    use ppgrid,        only : pver
    use sslt_sections, only : nsections, fluxes
    use aero_model,    only : seasalt_emis_scale
    use dust_model,    only : dust_nbin, dust_emis_sclfctr, dust_dmt_vwr, dust_indices
    use shr_dust_mod,  only : dust_emis_scheme
    use soil_erod_mod, only : soil_erodibility, soil_erod_fact
    use mo_constants,  only : dust_density
    use physconst,     only : pi

    ! Current physics state.
    type(physics_state), intent(in)  :: state
    ! cam_in: passed to provide access to surface variables.
    type(cam_in_t), intent(in)  :: cam_in
    ! Number of columns in chunk.
    integer, intent(in)  :: ncol
    ! Wind at 10 m raised to the 3.41 power, indexed by column.
    real(kind=dp), intent(out) :: u10cubed(:)
    ! Sampled per-bin number flux indexed by (column, sector_mode, bin).
    ! Only sea salt and dust modes will have non-zero entries here and
    ! log-normal modes will ignore this array.
    real(kind=dp), intent(out) :: sample_num_conc(:,:,:)

    real(kind=dp), parameter :: z0 = 1.0d-4 ! ocean roughness length (m); matches aero_model.F90
    real(kind=dp), parameter :: soil_erod_threshold = 0.1d0  ! matches dust_emis
    real(kind=dp) :: u10(pcols)
    real(kind=dp) :: fi_seasalt(pcols, nsections)
    real(kind=dp) :: soil_erod_val, mass_flux, x_mton
    real(kind=dp) :: mass_subbin, r_lo, r_hi, r3_vw_subbin
    integer :: lchnk
    integer :: icol, ibin, i_mode, isub, isub_in_bin, dust_icol

    sample_num_conc(:,:,:) = 0.0d0
    u10cubed(:) = 0.0d0
    lchnk = state%lchnk

    ! Wind at 10 m, raised to the 3.41 power per Gong et al. (1997).
    ! Same code path as aero_model.F90:2880-2887.
    do icol = 1,ncol
       u10(icol) = sqrt(state%u(icol,pver)**2 + state%v(icol,pver)**2)
       u10cubed(icol) = u10(icol) * log(10.0d0 / z0) / log(state%zm(icol,pver) / z0)
       u10cubed(icol) = u10cubed(icol)**3.41d0
    end do

    ! Sea salt: bin-resolved number flux density (m^-2 s^-1) per column from
    ! Martensson/Monahan-style polynomials in sslt_sections. Multiplied by
    ! ocean fraction so over-land columns (ocnfrac=0) emit zero naturally.
    fi_seasalt(:ncol, :) = fluxes(cam_in%sst(:ncol), u10cubed(:ncol), ncol)

    do i_mode = 1, n_emit_mode
       if ( .not. sector_modes(i_mode)%is_sampled ) cycle
       if ( trim(sector_modes(i_mode)%name) == 'emit_SEASALT' ) then
          do icol = 1, ncol
             do ibin = 1, nsections
                sample_num_conc(icol, i_mode, ibin) = &
                     max(0.0d0, fi_seasalt(icol, ibin)) &
                     * cam_in%ocnfrac(icol) * seasalt_emis_scale
             end do
          end do

          if ( masterproc ) then
             write(102,*) '-----------------------------------------'
             write(102,*) 'Sea salt sample_num_conc (i_mode=', i_mode, ')'
             write(102,*) 'seasalt_emis_scale (from aero_model namelist) = ', seasalt_emis_scale
             write(102,*) 'icol | sst | ocnfrac | u10cubed | total num flux (m^-2 s^-1)'
             do icol = 1, ncol
                write(102,*) icol, cam_in%sst(icol), cam_in%ocnfrac(icol), &
                     u10cubed(icol), sum(sample_num_conc(icol, i_mode, 1:nsections))
             end do
             write(102,*) 'Per-bin breakdown (icol = 1):'
             write(102,*) 'ibin | radius (m) | num_conc (m^-2 s^-1)'
             do ibin = 1, nsections
                write(102,*) ibin, sector_modes(i_mode)%sample_radius(ibin), &
                     sample_num_conc(1, i_mode, ibin)
             end do
             write(102,*) '-----------------------------------------'
          end if
       end if
       if ( trim(sector_modes(i_mode)%name) == 'emit_DUST' ) then
          ! Per-column dust mass per MAM bin (same scaling as dust_model.F90
          ! dust_emis lines 143-171), then split equally across
          ! n_dust_subbins_per_mam_bin sub-bins (uniform mass per log(r)
          ! within each MAM bin). Per-sub-bin number is derived from
          ! x_mton_sub = 6/(π·ρ·D_vw_sub³) where D_vw_sub³ = 8 × <r³> with
          ! <r³>_sub = (r_hi³ - r_lo³) / (3·ln(r_hi/r_lo)) — the volume-
          ! weighted moment for the assumed mass-per-log(r)-uniform shape.
          ! This conserves the MAM-bin total mass exactly (the equal-mass
          ! split summed over sub-bins recovers M_mam_bin) while letting
          ! PartMC sample particles across the bin's full size range.
          !
          ! NOTE: aero_model.F90:2851-2868 caps total dust mass flux against
          ! dstemislimit and rescales the per-bin distribution if the cap is
          ! hit. That cap is not yet applied here but will not compare against
          ! MAM's cflx unless added. Cap in theory rarely triggers in
          ! practice, so deferred for now.
          do icol = 1, ncol
             soil_erod_val = soil_erodibility(icol, lchnk)
             if ( dust_emis_scheme == 2 ) soil_erod_val = 1.0d0
             if ( soil_erod_val < soil_erod_threshold ) soil_erod_val = 0.0d0

             do ibin = 1, dust_nbin
                mass_flux = sum(-cam_in%dstflx(icol, :)) * 0.73d0 / 0.87d0 &
                     * dust_emis_sclfctr(ibin) * soil_erod_val / soil_erod_fact * 1.15d0
                ! TODO: switch from uniform-mass-per-log(r) to Kok11 brittle
                ! fragmentation. Replace this equal split with
                !     mass_subbin = mass_flux * w_kok11(isub_in_bin, ibin)
                ! where w_kok11(:,:) is a precomputed weight array from integrate
                ! Kok11's dV/dlogD over each sub-bin's edges, normalized so the
                ! per-MAM-bin weights sum to 1.
                mass_subbin = mass_flux / real(n_dust_subbins_per_mam_bin, kind=dp)
                do isub_in_bin = 1, n_dust_subbins_per_mam_bin
                   isub = (ibin - 1) * n_dust_subbins_per_mam_bin + isub_in_bin
                   r_lo = sector_modes(i_mode)%sample_radius(isub)
                   r_hi = sector_modes(i_mode)%sample_radius(isub + 1)
                   r3_vw_subbin = (r_hi**3 - r_lo**3) / (3.0d0 * log(r_hi / r_lo))
                   x_mton = 6.0d0 / (pi * dust_density * 8.0d0 * r3_vw_subbin)
                   sample_num_conc(icol, i_mode, isub) = mass_subbin * x_mton
                end do
             end do
          end do

          if ( masterproc ) then
             write(102,*) '-----------------------------------------'
             write(102,*) 'Dust sample_num_conc (i_mode=', i_mode, ')'
             write(102,*) 'soil_erod_fact (from soil_erod_mod) = ', soil_erod_fact
             write(102,*) 'dust_emis_scheme (from shr_dust_mod) = ', dust_emis_scheme
             write(102,*) 'n_dust_subbins_per_mam_bin = ', n_dust_subbins_per_mam_bin
             write(102,*) 'icol | soil_erod | dst_total | total num flux (m^-2 s^-1)'
             do icol = 1, ncol
                soil_erod_val = soil_erodibility(icol, lchnk)
                if ( dust_emis_scheme == 2 ) soil_erod_val = 1.0d0
                if ( soil_erod_val < soil_erod_threshold ) soil_erod_val = 0.0d0
                write(102,*) icol, soil_erod_val, sum(-cam_in%dstflx(icol, :)), &
                     sum(sample_num_conc(icol, i_mode, 1:sector_modes(i_mode)%n_samples))
             end do
             ! Pick a column that actually has dust emissions for the per-bin
             ! breakdown — column 1 is often ocean/non-erodible, giving zeros.
             dust_icol = 1
             do icol = 1, ncol
                if ( sum(sample_num_conc(icol, i_mode, &
                         1:sector_modes(i_mode)%n_samples)) > 0.0d0 ) then
                   dust_icol = icol
                   exit
                end if
             end do

             write(102,*) 'Per-MAM-bin aggregate (icol = ', dust_icol, '):'
             write(102,*) 'ibin | mass PartMC | mass cflx(dst_aN) | num PartMC | num MAM-eqv | num cflx(num_aN)'
             soil_erod_val = soil_erodibility(dust_icol, lchnk)
             if ( dust_emis_scheme == 2 ) soil_erod_val = 1.0d0
             if ( soil_erod_val < soil_erod_threshold ) soil_erod_val = 0.0d0
             do ibin = 1, dust_nbin
                ! Mass flux for this MAM bin (same formula as the per-step compute above).
                mass_flux = sum(-cam_in%dstflx(dust_icol, :)) * 0.73d0 / 0.87d0 &
                     * dust_emis_sclfctr(ibin) * soil_erod_val / soil_erod_fact * 1.15d0
                ! cflx(dst_aN) is natural-only (anthro doesn't write to dst slots),
                ! so the mass comparison is clean. cflx(num_aN) mixes natural dust +
                ! anthropogenic + seasalt by the time partmc_mam_invoke runs, so the
                ! "num cflx" column won't match "num MAM-eqv" unless this is a pure
                ! dust-only column.
                x_mton = 6.0d0 / (pi * dust_density * dust_dmt_vwr(ibin)**3)
                write(102,*) ibin, mass_flux, &
                     cam_in%cflx(dust_icol, dust_indices(ibin)), &
                     sum(sample_num_conc(dust_icol, i_mode, &
                         (ibin-1)*n_dust_subbins_per_mam_bin + 1 : &
                         ibin*n_dust_subbins_per_mam_bin)), &
                     mass_flux * x_mton, &
                     cam_in%cflx(dust_icol, dust_indices(ibin + dust_nbin))
             end do

             write(102,*) 'Per-sub-bin breakdown (icol = ', dust_icol, '):'
             write(102,*) 'isub | r_lo (m) | r_hi (m) | num_conc (m^-2 s^-1)'
             do isub = 1, sector_modes(i_mode)%n_samples
                write(102,*) isub, sector_modes(i_mode)%sample_radius(isub), &
                     sector_modes(i_mode)%sample_radius(isub + 1), &
                     sample_num_conc(dust_icol, i_mode, isub)
             end do
             write(102,*) '-----------------------------------------'
          end if
       end if

    end do

  end subroutine compute_partmc_natural_emission_inputs

  ! Sector-resolved version of partmc_interface_e3sm_emissions. Branches per
  ! mode: anthropogenic modes get AERO_MODE_TYPE_LOG_NORMAL with the usual
  ! (char_radius, sigma, num_conc, vol_frac); natural sampled modes
  ! (sector_modes(i)%is_sampled == .true.) get AERO_MODE_TYPE_SAMPLED with
  ! per-bin (sample_radius, sample_num_conc) and a single-species vol_frac.
  ! Returns the emissions data structure to be passed to PartMC for the current step.
  subroutine partmc_interface_e3sm_emissions_sector(emissions, geom_mean_diam, &
       sigma_emode, num_fluxes, vol_frac, sample_num_conc)

    ! Emissions data structure to pass to PartMC for sampling.
    type(aero_dist_t), intent(inout) :: emissions
    ! Geometric mean diameter of each sector mode. Not necesarily the same
    ! as the parent MAM mode diameter.
    real(kind=dp), intent(in) :: geom_mean_diam(:)   ! (n_emit_mode)
    ! Geometric standard deviation of each sector mode.
    real(kind=dp), intent(in) :: sigma_emode(:)      ! (n_emit_mode)
    ! Number flux of each sector mode.
    real(kind=dp), intent(in) :: num_fluxes(:)       ! (n_emit_mode)
    ! Volume fraction of each PartMC species in each sector mode. Indexed by
    ! PartMC species index, not per-mode species index so no per-mode species
    ! mapping needed.
    real(kind=dp), intent(in) :: vol_frac(:,:)       ! (n_emit_mode, n_aero_spec)
    ! Per-bin number flux for sampled modes; ignored for log-normal modes.
    ! Currently may be unallocated when no sampled modes are present.
    real(kind=dp), intent(in), optional :: sample_num_conc(:,:)  ! (n_emit_mode, max_n_samples)

    integer :: i_mode, n_aero_spec, n_smp, pmc_idx
    character(len=AERO_MODE_NAME_LEN) :: mode_name
    character(len=SPEC_LINE_MAX_VAR_LEN) :: weight_class_name

    if (allocated(emissions%mode)) deallocate(emissions%mode)
    allocate(emissions%mode(n_emit_mode))

    n_aero_spec = aero_data_n_spec(aero_data)

    do i_mode = 1, n_emit_mode
       mode_name = sector_modes(i_mode)%name
       emissions%mode(i_mode)%name = mode_name
       emissions%mode(i_mode)%source = aero_data_source_by_name(aero_data, mode_name)
       weight_class_name = mode_name
       emissions%mode(i_mode)%weight_class = &
            aero_data_weight_class_by_name(aero_data, weight_class_name)

       allocate(emissions%mode(i_mode)%vol_frac(n_aero_spec))
       allocate(emissions%mode(i_mode)%vol_frac_std(n_aero_spec))
       emissions%mode(i_mode)%vol_frac_std(:) = 0.0d0

       if ( sector_modes(i_mode)%is_sampled ) then
          ! Sampled-mode (binned) pathway for natural sources.
          emissions%mode(i_mode)%type = AERO_MODE_TYPE_SAMPLED
          ! char_radius and log10_std_dev_radius unused by sampled mode
          emissions%mode(i_mode)%char_radius = 0.0d0
          emissions%mode(i_mode)%log10_std_dev_radius = log10(sigma_emode(i_mode))

          n_smp = sector_modes(i_mode)%n_samples
          emissions%mode(i_mode)%sample_radius   = sector_modes(i_mode)%sample_radius(:)
          if ( present(sample_num_conc) ) then
             emissions%mode(i_mode)%sample_num_conc = sample_num_conc(i_mode, 1:n_smp)
          else
             emissions%mode(i_mode)%sample_num_conc = spread(0.0d0, 1, n_smp)
          end if
          emissions%mode(i_mode)%num_conc = sum(emissions%mode(i_mode)%sample_num_conc)

          ! Single-species composition: put all volume fraction into
          ! sample_pmc_idx (e.g. ncl_a1 for SEASALT, dst_a1 for DUST).
          ! TODO: If we add organics to be internally mixed with sea salt,
          ! we will need to split the vol_frac between sea salt and organics.
          emissions%mode(i_mode)%vol_frac(:) = 0.0d0
          pmc_idx = sector_modes(i_mode)%sample_pmc_idx
          if (pmc_idx > 0) emissions%mode(i_mode)%vol_frac(pmc_idx) = 1.0d0
       else
          ! Anthropogenic log-normal pathway.
          emissions%mode(i_mode)%type = AERO_MODE_TYPE_LOG_NORMAL
          emissions%mode(i_mode)%char_radius          = geom_mean_diam(i_mode) / 2.0d0
          emissions%mode(i_mode)%log10_std_dev_radius = log10(sigma_emode(i_mode))
          emissions%mode(i_mode)%num_conc             = num_fluxes(i_mode)
          emissions%mode(i_mode)%vol_frac(:)          = vol_frac(i_mode, :)
          emissions%mode(i_mode)%sample_radius        = [ real(kind=dp) :: ]
          emissions%mode(i_mode)%sample_num_conc      = [ real(kind=dp) :: ]
       end if
    end do

  end subroutine partmc_interface_e3sm_emissions_sector

end module mo_partmc_interface

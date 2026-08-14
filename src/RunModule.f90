! module for executing Noah-OWP-Modular model in a streamlined way

module RunModule
  
  use NamelistRead
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use AsciiReadModule
  use OutputModule
  use UtilitiesModule
  use ForcingModule
  use InterceptionModule
  use EnergyModule
  use WaterModule
  use DateTimeUtilsModule
  use LoggingModule
  use StateSerialization
  use messagepack
  use iso_fortran_env

  implicit none
  type :: noahowp_type
    type(namelist_type)   :: namelist
    type(levels_type)     :: levels
    type(domain_type)     :: domain
    type(options_type)    :: options
    type(parameters_type) :: parameters
    type(water_type)      :: water
    type(forcing_type)    :: forcing
    type(energy_type)     :: energy
    ! Size in bytes of any state snapshot of this model, measured once at
    ! initialization. Snapshots are fixed size for a given configuration, so this
    ! answers "how big is a snapshot" before one has been taken -- which restore
    ! has to ask, since it happens before any snapshot is created.
    integer :: serialization_nbytes
    integer, dimension(:), allocatable :: serialization_buffer
    ! How much of a snapshot a restore should apply; see StateSerialization
    integer :: restore_mode
  end type noahowp_type
contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_filename)
    implicit none
    
    type(noahowp_type), target, intent(out) :: model
    character(len=*), intent (in)           :: config_filename    ! config file from command line argument
    integer             :: forcing_timestep         ! integer time step (set to dt) for some subroutine calls
    
    associate(namelist   => model%namelist,   &
              levels     => model%levels,     &
              domain     => model%domain,     &
              options    => model%options,    &
              parameters => model%parameters, &
              water      => model%water,      &
              forcing    => model%forcing,    &
              energy     => model%energy)
        
      !---------------------------------------------------------------------
      !  initialize
      !---------------------------------------------------------------------
      call namelist%ReadNamelist(config_filename)

      call levels%Init
      call levels%InitTransfer(namelist)

      call domain%Init(namelist)
      call domain%InitTransfer(namelist)

      call options%Init()
      call options%InitTransfer(namelist)

      call parameters%Init(namelist)
      call parameters%paramRead(namelist)

      call forcing%Init(namelist)
      call forcing%InitTransfer(namelist)

      call energy%Init(namelist)
      call energy%InitTransfer(namelist)

      call water%Init(namelist)
      call water%InitTransfer(namelist)

      ! Initializations
      ! for soil water
      !water%zwt       = -100.0       ! should only be needed for run=1
      water%smcwtd    = 0.0          ! should only be needed for run=5
      water%deeprech  = 0.0          ! should only be needed for run=5
      water%qinsur    = 0.0          !
      water%runsrf    = 0.0          !
      water%runsub    = 0.0          !
      water%qdrain    = 0.0          !
      water%wcnd      = 0.0          !
      water%fcrmax    = 0.0          !
      water%snoflow   = 0.0          ! glacier outflow for all RUNSUB options, [mm/s]
      water%qseva     = 0.0          ! soil evaporation [mm/s]
      water%etrani    = 0.0          ! transpiration from each level[mm/s]
      water%btrani    = 0.0          ! soil water transpiration factor (0 to 1) by soil layer
      water%btran     = 0.0          ! soil water transpiration factor (0 to 1)
  
      ! for canopy water
      water%RAIN      = 0.0          ! rainfall mm/s
      water%SNOW      = 0.0          ! snowfall mm/s
      water%BDFALL    = 0.0        ! bulk density of snowfall (kg/m3)
      water%FB_snow   = 0.0          ! canopy fraction buried by snow (computed from phenology)
      water%FP        = 1.0          ! fraction of the gridcell that receives precipitation
      water%CANLIQ    = 0.0          ! canopy liquid water [mm]
      water%CANICE    = 0.0          ! canopy frozen water [mm]
      water%FWET      = 0.0          ! canopy fraction wet or snow
      water%CMC       = 0.0          ! intercepted water per ground area (mm)
      water%QINTR    = 0.0           ! interception rate for rain (mm/s)
      water%QDRIPR   = 0.0           ! drip rate for rain (mm/s)
      water%QTHROR   = 0.0           ! throughfall for rain (mm/s)
      water%QINTS    = 0.0           ! interception (loading) rate for snowfall (mm/s)
      water%QDRIPS   = 0.0           ! drip (unloading) rate for intercepted snow (mm/s)
      water%QTHROS   = 0.0           ! throughfall of snowfall (mm/s)
      water%QRAIN    = 0.0           ! rain at ground srf (mm/s) [+]
      water%QSNOW    = 0.0           ! snow at ground srf (mm/s) [+]
      water%SNOWHIN  = 0.0           ! snow depth increasing rate (m/s)
      water%ECAN     = 0.0           ! evap of intercepted water (mm/s) [+]
      water%ETRAN    = 0.0           ! transpiration rate (mm/s) [+]
  
      ! for snow water
      water%QVAP     = 0.0           ! evaporation/sublimation rate mm/s 
      water%ISNOW    = 0
      water%SNOWH    = 0.0
      water%SNEQV    = 0.0
      water%SNEQVO   = 0.0
      water%BDSNO    = 0.0
      water%PONDING  = 0.0
      water%PONDING1 = 0.0
      water%PONDING2 = 0.0
      water%QSNBOT   = 0.0
      water%QSNFRO   = 0.0
      water%QSNSUB   = 0.0
      water%QDEW     = 0.0
      water%QSDEW    = 0.0
      water%SNICE    = 0.0
      water%SNLIQ    = 0.0
      water%FICEOLD  = 0.0
      water%FSNO     = 0.0
  
      ! for energy-related variable
      energy%TV      = 298.0        ! leaf temperature [K]
      energy%TG      = 298.0        ! ground temperature [K]
      energy%CM      = 0.0          ! momentum drag coefficient
      energy%CH      = 0.0          ! heat drag coefficient
      energy%FCEV    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
      energy%FCTR    = 5.0          ! constant transpiration (w/m2) [+ to atm]
      energy%IMELT   = 1 ! freeze
      energy%STC     = 298.0
      energy%COSZ    = 0.7        ! cosine of solar zenith angle
      energy%ICE     = 0          ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
      energy%ALB     = 0.6        ! initialize snow albedo in CLASS routine
      energy%ALBOLD  = 0.6        ! initialize snow albedo in CLASS routine
      energy%FROZEN_CANOPY = .false. ! used to define latent heat pathway
      energy%FROZEN_GROUND = .false. 

      ! -- forcings 
      ! these are initially set to huge(1) -- to trap errors may want to set to a recognizable flag if they are
      !   supposed to be assigned below (eg -9999)
      !forcing%UU       = 0.0        ! wind speed in u direction (m s-1)
      !forcing%VV       = 0.0        ! wind speed in v direction (m s-1)
      !forcing%SFCPRS   = 0.0        ! pressure (pa)
      !forcing%SFCTMP   = 0.0        ! surface air temperature [k]
      !forcing%Q2       = 0.0        ! mixing ratio (kg/kg)
      !forcing%PRCP     = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      !forcing%SOLDN    = 0.0        ! downward shortwave radiation (w/m2)
      !forcing%LWDN     = 0.0        ! downward longwave radiation (w/m2)
      
      ! forcing-related variables
      forcing%PRCPCONV = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      forcing%PRCPNONC = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
      forcing%PRCPSHCV = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
      forcing%PRCPSNOW = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
      forcing%PRCPGRPL = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
      forcing%PRCPHAIL = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
      forcing%THAIR    = 0.0        ! potential temperature (k)
      forcing%QAIR     = 0.0        ! specific humidity (kg/kg) (q2/(1+q2))
      forcing%EAIR     = 0.0        ! vapor pressure air (pa)
      forcing%RHOAIR   = 0.0        ! density air (kg/m3)
      forcing%SWDOWN   = 0.0        ! downward solar filtered by sun angle [w/m2]
      forcing%FPICE    = 0.0        ! fraction of ice                AJN
      forcing%JULIAN   = 0.0        ! Setting arbitrary julian day
      forcing%YEARLEN  = 365        ! Setting year to be normal (i.e. not a leap year)  
      forcing%FOLN     = 1.0        ! foliage nitrogen concentration (%); for now, set to nitrogen saturation
      forcing%TBOT     = 285.0      ! bottom condition for soil temperature [K]

      ! domain variables
      domain%zsnso(-namelist%nsnow+1:0) = 0.0
      domain%zsnso(1:namelist%nsoil)    = namelist%zsoil
     
      ! time variables
      forcing_timestep = domain%dt        ! integer timestep for some subroutine calls
      domain%itime     = 1                ! initialize the time loop counter at 1
      domain%time_dbl  = 0.d0             ! start model run at t = 0
      
      !---------------------------------------------------------------------
      !--- set a time vector for simulation ---
      !---------------------------------------------------------------------
      ! --- AWW:  calculate start and end utimes & records for requested station data read period ---
      call get_utime_list (domain%start_datetime, domain%end_datetime, domain%dt, domain%sim_datetimes)  ! makes unix-time list for desired records (end-of-timestep)
      domain%ntime = size (domain%sim_datetimes)   
      !print *, "---------"; 
      !print *, 'Simulation startdate = ', domain%startdate, ' enddate = ', domain%enddate, ' dt(sec) = ', domain%dt, ' ntimes = ', domain%ntime  ! YYYYMMDD dates
      !print *, "---------"
      
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Code adapted from the ASCII_IO from NOAH-MP V1.1
      ! Compiler directive NGEN_FORCING_ACTIVE to be defined if 
      ! Nextgen forcing is being used (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call open_forcing_file(namelist%forcing_filename)
#endif
      
      !---------------------------------------------------------------------
      ! create output file and add initial values
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call initialize_output(namelist%output_filename, domain%ntime, levels%nsoil, levels%nsnow)
#endif
      
    end associate ! terminate the associate block

    ! TODO: only a hotstart restore is driven at present; when a resume is also
    ! driven, this needs to become selectable by the caller
    model%restore_mode = NOAHOWP_RESTORE_HOTSTART

    ! No snapshot size known yet; measure_serialization is what establishes it
    model%serialization_nbytes = 0

    call measure_serialization(model)

  END SUBROUTINE initialize_from_file

  !== Finalize the model ================================================================================

  SUBROUTINE cleanup(model)
    implicit none
    type(noahowp_type), intent(inout) :: model
      
      !---------------------------------------------------------------------
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call finalize_output()
#endif

      call free_serialization(model)

  END SUBROUTINE cleanup

  !== Move the model ahead one time step ================================================================

  SUBROUTINE advance_in_time(model)
    type (noahowp_type), intent (inout) :: model

    call solve_noahowp(model)

    model%domain%itime    = model%domain%itime + 1 ! increment the integer time by 1
    model%domain%time_dbl = dble(model%domain%time_dbl + model%domain%dt) ! increment model time in seconds by DT
  END SUBROUTINE advance_in_time
  
  !== Run one time step of the model ================================================================

  SUBROUTINE solve_noahowp(model)
    type (noahowp_type), intent (inout) :: model
    integer, parameter :: iunit        = 10 ! Fortran unit number to attach to the opened file
    integer            :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    integer            :: ierr              ! error code for reading forcing data
    integer            :: read_yr, read_mo, read_dy, read_hr, read_mi ! date components at forcing read time

    associate(namelist => model%namelist, &
              levels     => model%levels, &
              domain     => model%domain, &
              options    => model%options, &
              parameters => model%parameters, &
              water      => model%water, &
              forcing    => model%forcing, &
              energy     => model%energy)
    
    ! Compute the current UNIX datetime
    domain%curr_datetime = domain%sim_datetimes(domain%itime)     ! use end-of-timestep datetimes  because initial var values are being written

    !---------------------------------------------------------------------
    ! Read in the forcing data
    ! Compiler directive NGEN_FORCING_ACTIVE to be defined if 
    ! Nextgen forcing is being used (https://github.com/NOAA-OWP/ngen)
    ! If it is defined, Nextgen MUST provide forcing
    !---------------------------------------------------------------------
    forcing_timestep = domain%dt
#ifndef NGEN_FORCING_ACTIVE
    ! Forcing is read at the beginning-of-timestep date, start + (itime-1)*dt,
    ! while UtilitiesMain below computes solar geometry at the
    ! end-of-timestep date, start + itime*dt.
    call advance_datetime(domain%start_year, domain%start_month, domain%start_day, &
                          domain%start_hour, domain%start_minute,                  &
                          int((domain%itime - 1) * (domain%dt / 60)),              &
                          read_yr, read_mo, read_dy, read_hr, read_mi)
    call read_forcing_text(iunit, read_yr, read_mo, read_dy, read_hr, read_mi, forcing_timestep, &
         forcing%UU, forcing%VV, forcing%SFCTMP, forcing%Q2, forcing%SFCPRS, forcing%SOLDN, forcing%LWDN, forcing%PRCP, ierr)
#endif
   
    !---------------------------------------------------------------------
    ! call the main utility routines
    !---------------------------------------------------------------------
    call UtilitiesMain (domain%itime, domain, forcing, energy)

    !---------------------------------------------------------------------
    ! call the main forcing routines
    !---------------------------------------------------------------------

    call ForcingMain (options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main interception routines
    !---------------------------------------------------------------------

    call InterceptionMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main energy balance routines
    !---------------------------------------------------------------------

    call EnergyMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main water routines (canopy + snow + soil water components)
    !---------------------------------------------------------------------

    call WaterMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! add to output file
    ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
    ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
    !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    call add_to_output(domain, water, energy, forcing, domain%itime, levels%nsoil,levels%nsnow)
#endif
    
    end associate ! terminate associate block
  END SUBROUTINE solve_noahowp

  !== State serialization ===============================================================================

  SUBROUTINE measure_serialization(model)

    ! Record how large a snapshot of this model is. Called once from
    ! initialization, before any caller can ask.

    type(noahowp_type), intent(inout) :: model
    integer :: exec_status

    call create_serialization(model, exec_status)

    if (exec_status == 0) then
      model%serialization_nbytes = buffer_nbytes(model%serialization_buffer)
    else
      model%serialization_nbytes = -1
      call write_log("Could not determine serialized state size", LOG_LEVEL_SEVERE)
    end if

    call free_serialization(model)

  END SUBROUTINE measure_serialization

  SUBROUTINE create_serialization (model, exec_status)

    ! Capture current state into the model's snapshot buffer. The buffer is an
    ! integer array because that is the widest type the Fortran BMI bindings can
    ! carry: its first element is the MessagePack byte count, and the packed
    ! bytes follow, padded out to a whole number of integers.

    type(noahowp_type), intent(inout) :: model
    integer, intent(out) :: exec_status
    class(msgpack), allocatable :: mp
    class(mp_arr_type), allocatable :: mp_header_arr, mp_forcing_arr, mp_domain_arr
    class(mp_arr_type), allocatable :: mp_energy_arr, mp_water_arr, mp_parameters_arr
    type(mp_arr_type) :: mp_arr
    byte, dimension(:), allocatable :: packed
    integer :: packed_bytes, packed_ints

    exec_status = 1
    mp = msgpack()
    mp_arr = mp_arr_type(SERIALIZATION_PAYLOAD_ELEMENTS)

    call header_serialization(model%domain%curr_datetime, mp_header_arr)
    allocate(mp_arr%values(1)%obj, source = mp_header_arr)

    call forcing_serialization(model%forcing, mp_forcing_arr)
    allocate(mp_arr%values(2)%obj, source = mp_forcing_arr)

    call energy_serialization(model%energy, mp_energy_arr)
    allocate(mp_arr%values(3)%obj, source = mp_energy_arr)

    call domain_serialization(model%domain, mp_domain_arr)
    allocate(mp_arr%values(4)%obj, source = mp_domain_arr)

    call water_serialization(model%water, mp_water_arr)
    allocate(mp_arr%values(5)%obj, source = mp_water_arr)

    call parameters_serialization(model%parameters, mp_parameters_arr)
    allocate(mp_arr%values(6)%obj, source = mp_parameters_arr)

    call mp%pack_alloc(mp_arr, packed)
    if (mp%failed()) then
      call write_log("Serialization using messagepack failed. Error:"//mp%error_message, LOG_LEVEL_SEVERE)
      return
    end if

    call free_serialization(model)

    packed_bytes = size(packed)
    packed_ints  = CEILING(real(packed_bytes) / (storage_size(packed_ints) / 8))
    allocate(model%serialization_buffer(packed_ints + 1))
    model%serialization_buffer(1)  = packed_bytes
    model%serialization_buffer(2:) = transfer(packed, model%serialization_buffer(2:))

    ! A snapshot that is not the size initialization measured means something in
    ! StateSerialization stopped encoding to a fixed width, which would silently
    ! truncate a restore. Refuse rather than hand back a buffer of the wrong size.
    if (model%serialization_nbytes > 0 .and. &
        buffer_nbytes(model%serialization_buffer) /= model%serialization_nbytes) then
      call write_log("Serialized state size is not stable; refusing to serialize", LOG_LEVEL_SEVERE)
      call free_serialization(model)
      return
    end if

    exec_status = 0
    call write_log("Serialization using messagepack successful", LOG_LEVEL_DEBUG)

  END SUBROUTINE create_serialization

  SUBROUTINE free_serialization(model)
    type(noahowp_type), intent(inout) :: model

    if (allocated(model%serialization_buffer)) deallocate(model%serialization_buffer)

  END SUBROUTINE free_serialization

  SUBROUTINE restore_serialization (model, serialized_data, exec_status)

    ! Apply a snapshot to the model, as much of it as model%restore_mode calls
    ! for. See StateSerialization for what each mode takes.

    type(noahowp_type), intent(inout) :: model
    integer, intent(in) :: serialized_data(:)
    integer, intent(out) :: exec_status
    byte, allocatable :: packed(:)
    class(mp_value_type), allocatable :: mpv
    class(msgpack), allocatable :: mp
    class(mp_arr_type), allocatable :: arr_all, arr
    logical :: status
    integer(kind=int64) :: index
    real(kind=real64) :: save_datetime

    exec_status = 1

    if (size(serialized_data) < 2) then
      call write_log("Serialized state is too short to be a snapshot", LOG_LEVEL_SEVERE)
      return
    end if
    if (serialized_data(1) < 1 .or. &
        serialized_data(1) > buffer_nbytes(serialized_data) ) then
      call write_log("Serialized state has an implausible length header", LOG_LEVEL_SEVERE)
      return
    end if

    mp = msgpack()
    ! Trailing bytes are not our error signal -- the payload element count and
    ! the header say whether this buffer is ours. Leaving this on also routes a
    ! foreign buffer into a MessagePack error path whose message is built with a
    ! malformed format string, which aborts instead of reporting.
    call mp%extra_bytes_is_error(.false.)

    allocate(packed(serialized_data(1)))
    packed = TRANSFER(serialized_data(2:), packed, size=serialized_data(1))

    call mp%unpack(packed, mpv)
    ! A truncated or malformed buffer leaves mpv unallocated without necessarily
    ! setting the failure flag, so check the value itself before touching it
    if (.not. allocated(mpv)) then
      call write_log("Serialized state could not be unpacked. Error:"//mp%error_message, LOG_LEVEL_SEVERE)
      return
    end if
    if (.not. is_arr(mpv)) then
      call write_log("Serialized state is not a MessagePack array", LOG_LEVEL_SEVERE)
      return
    end if

    call get_arr_ref(mpv, arr_all, status)
    if (.not. status) then
      call write_log("Deserialization using messagepack failed. Error:"//mp%error_message, LOG_LEVEL_SEVERE)
      return
    end if

    if (arr_all%numelements() /= SERIALIZATION_PAYLOAD_ELEMENTS) then
      call write_log("Serialized state does not contain all state information", LOG_LEVEL_SEVERE)
      return
    end if

    call get_arr_ref(arr_all%values(1)%obj, arr, status)
    if (.not. status) then
      call write_log("Serialized state has no readable header", LOG_LEVEL_SEVERE)
      return
    end if
    call header_deserialization(arr, save_datetime, status)
    if (.not. status) then
      call write_log("Serialized state is not a Noah-OWP-Modular snapshot this build can read", &
                     LOG_LEVEL_SEVERE)
      return
    end if
    if (save_datetime < model%domain%start_datetime .or. &
        save_datetime > model%domain%end_datetime) then
      call write_log("Restoring state saved outside this run's simulation period", LOG_LEVEL_WARNING)
    end if

    do index = 2, SERIALIZATION_PAYLOAD_ELEMENTS
      call get_arr_ref(arr_all%values(index)%obj, arr, status)
      if (.not. status) then
        call write_log("Deserialization using messagepack failed. Error:"//mp%error_message, LOG_LEVEL_SEVERE)
        return
      end if
      select case(index)
        case(2)
          call forcing_deserialization (arr, model%forcing)
        case(3)
          call energy_deserialization (arr, model%energy)
        case(4)
          call domain_deserialization (arr, model%domain, model%restore_mode)
        case(5)
          call water_deserialization (arr, model%water)
        case(6)
          call parameters_deserialization (arr, model%parameters)
      end select
    end do

    exec_status = 0
    call write_log("Deserialization using messagepack successful", LOG_LEVEL_DEBUG)

  END SUBROUTINE restore_serialization

  FUNCTION buffer_nbytes (buffer) RESULT (nbytes)
    integer, dimension(:), intent(in) :: buffer
    integer :: nbytes

    nbytes = size(buffer) * (storage_size(buffer) / 8)

  END FUNCTION buffer_nbytes

end module RunModule

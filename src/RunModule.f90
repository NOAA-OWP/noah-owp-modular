! module for executing Noah-OWP-Modular model in a streamlined way

module RunModule
  
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use LevelsGridType
  use DomainGridType
  use OptionsGridType
  use ParametersGridType
  use WaterGridType
  use ForcingGridType
  use EnergyGridType
  use AsciiReadModule
  use OutputModule
  use UtilitiesModule
  use ForcingModule
  use InterceptionModule
  use EnergyModule
  use WaterModule
  use DateTimeUtilsModule
  use NoahowpType
  
  implicit none

  type :: noahowp_type

    type(levels_type)     :: levels
    type(domain_type)     :: domain
    type(options_type)    :: options
    type(parameters_type) :: parameters
    type(water_type)      :: water
    type(forcing_type)    :: forcing
    type(energy_type)     :: energy
    
  end type noahowp_type

  type :: noahowpgrid_type

    type(namelist)                :: namelist
    type(levelsgrid_type)         :: levels_grid
    type(domaingrid_type)         :: domain_grid
    type(optionsgrid_type)        :: options_grid
    type(parametersgrid_type)     :: parameters_grid
    type(watergrid_type)          :: water_grid
    type(forcinggrid_type)        :: forcing_grid
    type(energygrid_type)         :: energy_grid
  
  end type

contains
  
  SUBROUTINE initialize_from_file (noahowpgrid, config_filename)

    implicit none
    
    type (noahowpgrid_type), intent (inout) :: noahowpgrid
    character(len=*), intent (in)           :: config_filename    ! config file from command line argument
    type(namelist_type)                     :: namelist
    integer                                 :: forcing_timestep         ! integer time step (set to dt) for some subroutine calls
    integer                                 :: ii
        
    associate(namelist   => noahowpgrid%namelist,   &
      levelsgrid     => noahowpgrid%levelsgrid,     &
      domaingrid     => noahowpgrid%domaingrid,     &
      optionsgrid    => noahowpgrid%optionsgrid,    &
      parametersgrid => noahowpgrid%parametersgrid, &
      watergrid      => noahowpgrid%watergrid,      &
      forcinggrid    => noahowpgrid%forcinggrid,    &
      energygrid     => noahowpgrid%energygrid)

      !---------------------------------------------------------------------
      !  initialize
      !---------------------------------------------------------------------
      call namelist%ReadNamelist(config_filename)

      call levelsgrid%Init(namelist)
      call levelsgrid%InitTransfer(namelist)

      call domaingrid%Init(namelist)
      call domaingrid%InitTransfer(namelist)

      call optionsgrid%Init()
      call optionsgrid%InitTransfer(namelist)

      call parametersgrid%Init(namelist)
      call parametersgrid%paramRead(namelist)

      call forcinggrid%Init(namelist)
      call forcinggrid%InitTransfer(namelist)

      call energygrid%Init(namelist)
      call energygrid%InitTransfer(namelist)

      call watergrid%Init(namelist)
      call watergrid%InitTransfer(namelist)

      ! Initializations
      ! for soil water
      !water%zwt       = -100.0       ! should only be needed for run=1
      watergrid%smcwtd    = 0.0          ! should only be needed for run=5
      watergrid%deeprech  = 0.0          ! should only be needed for run=5
      watergrid%qinsur    = 0.0          !
      watergrid%runsrf    = 0.0          !
      watergrid%runsub    = 0.0          !
      watergrid%qdrain    = 0.0          !
      watergrid%wcnd      = 0.0          !
      watergrid%fcrmax    = 0.0          !
      watergrid%snoflow   = 0.0          ! glacier outflow for all RUNSUB options, [mm/s]
      watergrid%qseva     = 0.0          ! soil evaporation [mm/s]
      watergrid%etrani    = 0.0          ! transpiration from each level[mm/s]
      watergrid%btrani    = 0.0          ! soil water transpiration factor (0 to 1) by soil layer
      watergrid%btran     = 0.0          ! soil water transpiration factor (0 to 1)
  
      ! for canopy water
      watergrid%RAIN      = 0.0          ! rainfall mm/s
      watergrid%SNOW      = 0.0          ! snowfall mm/s
      watergrid%BDFALL    = 0.0        ! bulk density of snowfall (kg/m3)
      watergrid%FB_snow   = 0.0          ! canopy fraction buried by snow (computed from phenology)
      watergrid%FP        = 1.0          ! fraction of the gridcell that receives precipitation
      watergrid%CANLIQ    = 0.0          ! canopy liquid water [mm]
      watergrid%CANICE    = 0.0          ! canopy frozen water [mm]
      watergrid%FWET      = 0.0          ! canopy fraction wet or snow
      watergrid%CMC       = 0.0          ! intercepted water per ground area (mm)
      watergrid%QINTR    = 0.0           ! interception rate for rain (mm/s)
      watergrid%QDRIPR   = 0.0           ! drip rate for rain (mm/s)
      watergrid%QTHROR   = 0.0           ! throughfall for rain (mm/s)
      watergrid%QINTS    = 0.0           ! interception (loading) rate for snowfall (mm/s)
      watergrid%QDRIPS   = 0.0           ! drip (unloading) rate for intercepted snow (mm/s)
      watergrid%QTHROS   = 0.0           ! throughfall of snowfall (mm/s)
      watergrid%QRAIN    = 0.0           ! rain at ground srf (mm/s) [+]
      watergrid%QSNOW    = 0.0           ! snow at ground srf (mm/s) [+]
      watergrid%SNOWHIN  = 0.0           ! snow depth increasing rate (m/s)
      watergrid%ECAN     = 0.0           ! evap of intercepted water (mm/s) [+]
      watergrid%ETRAN    = 0.0           ! transpiration rate (mm/s) [+]
  
      ! for snow water
      watergrid%QVAP     = 0.0           ! evaporation/sublimation rate mm/s 
      watergrid%ISNOW    = 0
      watergrid%SNOWH    = 0.0
      watergrid%SNEQV    = 0.0
      watergrid%SNEQVO   = 0.0
      watergrid%BDSNO    = 0.0
      watergrid%PONDING  = 0.0
      watergrid%PONDING1 = 0.0
      watergrid%PONDING2 = 0.0
      watergrid%QSNBOT   = 0.0
      watergrid%QSNFRO   = 0.0
      watergrid%QSNSUB   = 0.0
      watergrid%QDEW     = 0.0
      watergrid%QSDEW    = 0.0
      watergrid%SNICE    = 0.0
      watergrid%SNLIQ    = 0.0
      watergrid%FICEOLD  = 0.0
      watergrid%FSNO     = 0.0
  
      ! for energy-related variable
      energygrid%TV      = 298.0        ! leaf temperature [K]
      energygrid%TG      = 298.0        ! ground temperature [K]
      energygrid%CM      = 0.0          ! momentum drag coefficient
      energygrid%CH      = 0.0          ! heat drag coefficient
      energygrid%FCEV    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
      energygrid%FCTR    = 5.0          ! constant transpiration (w/m2) [+ to atm]
      energygrid%IMELT   = 1 ! freeze
      energygrid%STC     = 298.0
      energygrid%COSZ    = 0.7        ! cosine of solar zenith angle
      energygrid%ICE     = 0          ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
      energygrid%ALB     = 0.6        ! initialize snow albedo in CLASS routine
      energygrid%ALBOLD  = 0.6        ! initialize snow albedo in CLASS routine
      energygrid%FROZEN_CANOPY = .false. ! used to define latent heat pathway
      energygrid%FROZEN_GROUND = .false. 

      ! -- forcings 
      ! these are initially set to huge(1) -- to trap errors may want to set to a recognizable flag if they are
      !   supposed to be assigned below (eg -9999)
      !forcinggrid%UU       = 0.0        ! wind speed in u direction (m s-1)
      !forcinggrid%VV       = 0.0        ! wind speed in v direction (m s-1)
      !forcinggrid%SFCPRS   = 0.0        ! pressure (pa)
      !forcinggrid%SFCTMP   = 0.0        ! surface air temperature [k]
      !forcinggrid%Q2       = 0.0        ! mixing ratio (kg/kg)
      !forcinggrid%PRCP     = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      !forcinggrid%SOLDN    = 0.0        ! downward shortwave radiation (w/m2)
      !forcinggrid%LWDN     = 0.0        ! downward longwave radiation (w/m2)
      
      ! forcing-related variables
      forcinggrid%PRCPCONV = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      forcinggrid%PRCPNONC = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
      forcinggrid%PRCPSHCV = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
      forcinggrid%PRCPSNOW = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
      forcinggrid%PRCPGRPL = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
      forcinggrid%PRCPHAIL = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
      forcinggrid%THAIR    = 0.0        ! potential temperature (k)
      forcinggrid%QAIR     = 0.0        ! specific humidity (kg/kg) (q2/(1+q2))
      forcinggrid%EAIR     = 0.0        ! vapor pressure air (pa)
      forcinggrid%RHOAIR   = 0.0        ! density air (kg/m3)
      forcinggrid%SWDOWN   = 0.0        ! downward solar filtered by sun angle [w/m2]
      forcinggrid%FPICE    = 0.0        ! fraction of ice                AJN
      forcinggrid%JULIAN   = 0.0        ! Setting arbitrary julian day
      forcinggrid%YEARLEN  = 365        ! Setting year to be normal (i.e. not a leap year)  
      forcinggrid%FOLN     = 1.0        ! foliage nitrogen concentration (%); for now, set to nitrogen saturation
      forcinggrid%TBOT     = 285.0      ! bottom condition for soil temperature [K]

      ! domain variables
      do ii = -namelist%nsnow+1, 0
        noahowpgrid%zsnso(:,:,ii) = 0.0
      end do
      do ii = 1, namelist%nsoil
        noahowpgrid%zsnso(:,:,ii) = namelist%zsoil
      end do

      ! time variables
      noahowpgrid%nowdate   = noahowpgrid%startdate ! start the model with nowdate = startdate
      forcing_timestep      = noahowpgrid%dt        ! integer timestep for some subroutine calls
      noahowpgrid%itime     = 1                     ! initialize the time loop counter at 1
      noahowpgrid%time_dbl  = 0.d0                  ! start model run at t = 0
      
      !---------------------------------------------------------------------
      !--- set a time vector for simulation ---
      !---------------------------------------------------------------------
      ! --- AWW:  calculate start and end utimes & records for requested station data read period ---
      call get_utime_list (noahowpgrid%start_datetime, noahowpgrid%end_datetime, noahowpgrid%dt, noahowpgrid%sim_datetimes)  ! makes unix-time list for desired records (end-of-timestep)
      noahowpgrid%ntime = size (noahowpgrid%sim_datetimes)   
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
      call initialize_output(namelist%output_filename, noahowpgrid%ntime, noahowpgrid%nsoil, noahowpgrid%nsnow)
#endif

  END SUBROUTINE initialize_from_file   

  SUBROUTINE cleanup()
    implicit none
      
    !---------------------------------------------------------------------
    ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
    ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
    !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    call finalize_output()
#endif
    
  END SUBROUTINE cleanup

  SUBROUTINE advance_in_time(noahowpgrid)
    type (noahowpgrid_type), intent (inout) :: noahowpgrid

    call solve_noahowp_grid(noahowpgrid)

    model%domain%itime    = model%domain%itime + 1 ! increment the integer time by 1
    model%domain%time_dbl = dble(model%domain%time_dbl + model%domain%dt) ! increment model time in seconds by DT

  END SUBROUTINE advance_in_time

  SUBROUTINE solve_noahowp_grid(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type)                    :: noahowp                                             !local instance 
    integer                               :: ix, iy, ierr
    integer                               :: iunit = 10
    real                                  :: read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS !to read in forcing
    real                                  :: read_SOLDN, read_LWDN, read_PRCP                    !to read in forcing
    integer                               :: idt                                                 !to iterate nowdate

  #ifndef NGEN_FORCING_ACTIVE

    !ifndef NGEN_FORCING_ACTIVE, read forcings for time step, else then 
    !forcings must be updated/set via bmi function calls prior to call 
    !to bmi_update/NoahowpGridDriverMain

    !Update noahowpgrid%nowdate 
    !I think this needs to be done here rather than in UtilitiesMain if NGEN isn't doing forcing
    !idt = noahowpgrid%itime * (noahowpgrid%dt / 60)
    !call geth_newdate(noahowpgrid%startdate, idt, noahowpgrid%nowdate)

    !Read for forcings for nowdate
    call read_forcing_text(iunit, noahowpgrid%nowdate, int(noahowpgrid%dt), &
          read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS, read_SOLDN, read_LWDN, read_PRCP, ierr)

    !Give read-in forcings to all grid cells
    noahowpgrid%UU(:,:)     = read_UU
    noahowpgrid%VV(:,:)     = read_VV
    noahowpgrid%SFCTMP(:,:) = read_SFCTMP
    noahowpgrid%Q2(:,:)     = read_Q2
    noahowpgrid%SFCPRS(:,:) = read_SFCPRS
    noahowpgrid%SOLDN(:,:)  = read_SOLDN
    noahowpgrid%LWDN(:,:)   = read_LWDN
    noahowpgrid%PRCP(:,:)   = read_PRCP
    noahowpgrid%UU(:,:)     = read_UU

  #endif

    !Iterate over x and y dimensions
    do ix = 1, noahowpgrid%n_x
      do iy = 1, noahowpgrid%n_y

        

        !Transfer variable values from noahowpgrid_type to noahowp_type
        call DomainVarInTransfer       (noahowp, noahowpgrid)
        call LevelsVarInTransfer       (noahowp, noahowpgrid)
        call EnergyVarInTransfer       (noahowp, noahowpgrid)
        call ForcingVarInTransfer      (noahowp, noahowpgrid)
        call OptionsVarInTransfer      (noahowp, noahowpgrid)
        call ParametersVarInTransfer   (noahowp, noahowpgrid)
        call WaterVarInTransfer        (noahowp, noahowpgrid)
        
        !Execute column model
        call solve_noahowp             (noahowp)

        !Transfer variable values from noahowp_type back to noahowpgrid_type
        call DomainVarOutTransfer      (noahowp, noahowpgrid)
        call LevelsVarOutTransfer      (noahowp, noahowpgrid)
        call EnergyVarOutTransfer      (noahowp, noahowpgrid)
        call ForcingVarOutTransfer     (noahowp, noahowpgrid)
        call OptionsVarOutTransfer     (noahowp, noahowpgrid)
        call ParametersVarOutTransfer  (noahowp, noahowpgrid)
        call WaterVarOutTransfer       (noahowp, noahowpgrid)

      end do
    end do

  END SUBROUTINE solve_noahowp_grid

  SUBROUTINE solve_noahowp(noahowp)

    type (noahowp_type), intent (inout) :: noahowp
    integer, parameter                  :: iunit        = 10 ! Fortran unit number to attach to the opened file
    integer                             :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    integer                             :: ierr              ! error code for reading forcing data
    integer                             :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  ! current UNIX timestep details

    associate(levels     => noahowp%levels, &
              domain     => noahowp%domain, &
              options    => noahowp%options, &
              parameters => noahowp%parameters, &
              water      => noahowp%water, &
              forcing    => noahowp%forcing, &
              energy     => noahowp%energy)
    
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

  SUBROUTINE cleanup()
    implicit none
      
      !---------------------------------------------------------------------
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
  
#ifndef NGEN_OUTPUT_ACTIVE
      call finalize_output()
#endif
    
  END SUBROUTINE cleanup

end module RunModule

! module for executing the NOAH-MP model in a streamlined way
! this adapts code from the old NOAH-MP driver 
module RunModule
  
  use NamelistRead
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use NoahMPAsciiRead
  use NoahMPOutput
  use UtilitiesModule
  use ForcingModule
  use InterceptionModule
  use EnergyModule
  use WaterModule
  use DateTimeUtilsModule
  
  implicit none
  type :: noahmp_type
    type(namelist_type)   :: namelist
    type(levels_type)     :: levels
    type(domain_type)     :: domain
    type(options_type)    :: options
    type(parameters_type) :: parameters
    type(water_type)      :: water
    type(forcing_type)    :: forcing
    type(energy_type)     :: energy
  end type noahmp_type
contains

!== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(noahmp_type), target, intent(out) :: model
    character(len=*), intent (in) :: config_file ! config file from command line argument
    
    integer             :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    
    associate(namelist   => model%namelist, &
              levels     => model%levels, &
              domain     => model%domain, &
              options    => model%options, &
              parameters => model%parameters, &
              water      => model%water, &
              forcing    => model%forcing, &
              energy     => model%energy)
        
    !---------------------------------------------------------------------
    !  initialize
    !---------------------------------------------------------------------

      call namelist%ReadNamelist(config_file)

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
      water%zwt       = -100.0       ! should only be needed for run=1
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
      energy%FROZEN_CANOPY = .false. ! used to define latent heat pathway
      energy%IMELT = 1 ! freeze
      energy%FROZEN_GROUND = .false. 
      energy%STC      = 298.0
      energy%COSZ     = 0.7        ! cosine of solar zenith angle
      energy%ICE      = 0          ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
      energy%ALB      = 0.6        ! initialize snow albedo in CLASS routine
      energy%ALBOLD   = 0.6        ! initialize snow albedo in CLASS routine
  
      ! forcing-related variables
      forcing%UU       = 0.0        ! wind speed in u direction (m s-1)
      forcing%VV       = 0.0        ! wind speed in v direction (m s-1)
      forcing%SFCPRS   = 0.0        ! pressure (pa)
      forcing%SFCTMP   = 0.0        ! surface air temperature [k]
      forcing%Q2       = 0.0        ! mixing ratio (kg/kg)
      forcing%PRCPCONV = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      forcing%PRCPNONC = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
      forcing%PRCPSHCV = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
      forcing%PRCPSNOW = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
      forcing%PRCPGRPL = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
      forcing%PRCPHAIL = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
      forcing%SOLDN    = 0.0        ! downward shortwave radiation (w/m2)
      forcing%LWDN     = 0.0        ! downward longwave radiation (w/m2)
      forcing%THAIR    = 0.0        ! potential temperature (k)
      forcing%QAIR     = 0.0        ! specific humidity (kg/kg) (q2/(1+q2))
      forcing%EAIR     = 0.0        ! vapor pressure air (pa)
      forcing%RHOAIR   = 0.0        ! density air (kg/m3)
      forcing%SWDOWN   = 0.0        ! downward solar filtered by sun angle [w/m2]
      forcing%FPICE    = 0.0        ! fraction of ice                AJN
      forcing%JULIAN   = 0.0       ! Setting arbitrary julian day
      forcing%YEARLEN  = 365        ! Setting year to be normal (i.e. not a leap year)  
      forcing%FOLN     = 1.0        ! foliage nitrogen concentration (%); for now, set to nitrogen saturation
      forcing%TBOT     = 285.0      ! bottom condition for soil temperature [K]

      ! domain variables
      domain%IST = 1
      domain%zsnso(-namelist%nsnow+1:0) = 0.0
      domain%zsnso(1:namelist%nsoil) = namelist%zsoil
     
      ! time variables
      domain%nowdate   = domain%startdate ! start the model with nowdate = startdate
      forcing_timestep = domain%dt        ! integer timestep for some subroutine calls
      domain%itime     = 1                ! initialize the time loop counter at 1
      domain%time_dbl  = 0.d0             ! start model run at t = 0
      
      !---------------------------------------------------------------------
      !--- set a time vector for simulation ---
      !---------------------------------------------------------------------
      ! --- AWW:  calculate start and end utimes & records for requested station data read period ---
      call get_utime_list (domain%start_datetime, domain%end_datetime, domain%dt, domain%sim_datetimes)  ! makes unix-time list for desired records (end-of-timestep)
      domain%ntime = size (domain%sim_datetimes)   
      print *, "---------"; 
      print *, 'Simulation startdate = ', domain%startdate, ' enddate = ', domain%enddate, ' dt(sec) = ', domain%dt, ' ntimes = ', domain%ntime  ! YYYYMMDD dates
      print *, "---------"
      
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Code adapted from the ASCII_IO from NOAH-MP V1.1
      !---------------------------------------------------------------------
      call open_forcing_file(namelist%input_filename)
      
      !---------------------------------------------------------------------
      ! create output file and add initial values
      !---------------------------------------------------------------------
      call initialize_output(namelist%output_filename, domain%ntime, levels%nsoil, levels%nsnow)
      
    end associate ! terminate the associate block

  END SUBROUTINE initialize_from_file   
  
!== Finalize the model ================================================================================

  SUBROUTINE cleanup(model)
    implicit none
    type(noahmp_type), intent(inout) :: model

      call finalize_output()
  
  END SUBROUTINE cleanup

!== Move the model ahead one time step ================================================================

  SUBROUTINE advance_in_time(model)
    type (noahmp_type), intent (inout) :: model

    call solve_noahmp(model)

    model%domain%itime    = model%domain%itime + 1 ! increment the integer time by 1
    model%domain%time_dbl = dble(model%domain%time_dbl + model%domain%dt) ! increment model time in seconds by DT
  END SUBROUTINE advance_in_time
  
!== Run one time step of the model ================================================================

  SUBROUTINE solve_noahmp(model)
    type (noahmp_type), intent (inout) :: model
    integer, parameter :: iunit        = 10 ! Fortran unit number to attach to the opened file
    integer            :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    integer            :: ierr              ! error code for reading forcing data
    real               :: QV_CURR           ! water vapor mixing ratio (kg/kg)
    integer            :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  ! current UNIX timestep details

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
    call unix_to_date (domain%curr_datetime, curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec)
    ! print '(2x,I4,1x,I2,1x,I2,1x,I2,1x,I2)', curr_yr, curr_mo, curr_dy, curr_hr, curr_min ! time check for debugging
    
    !---------------------------------------------------------------------
    ! Read in the forcing data
    !---------------------------------------------------------------------
    forcing_timestep = domain%dt
    call read_forcing_text(iunit, domain%nowdate, forcing_timestep, &
         forcing%UU, forcing%VV, forcing%SFCTMP, forcing%Q2, forcing%SFCPRS, forcing%SOLDN, forcing%LWDN, forcing%PRCPNONC, ierr)

    !---------------------------------------------------------------------
    ! there is a need for a derived variables routine here
    !---------------------------------------------------------------------
    ! it would handle the following plus a lot of other conversions, reassignments, settings
    forcing%P_ML     = forcing%SFCPRS              ! surf press estimated at model level [Pa], can avg multi-level nwp
    forcing%O2PP     = parameters%O2 * forcing%P_ML        ! atmospheric co2 concentration partial pressure (Pa)
    forcing%CO2PP    = parameters%CO2 * forcing%P_ML       ! atmospheric o2 concentration partial pressure (Pa)

    energy%TAH = forcing%SFCTMP                         ! assign canopy temp with forcing air temp (K)
    QV_CURR    = forcing%Q2 / (1 - forcing%Q2)          ! mixing ratio, assuming input forcing Q2 is specific hum.
    energy%EAH = forcing%SFCPRS*QV_CURR/(0.622+QV_CURR) ! Initial guess only. (Pa)

    !---------------------------------------------------------------------
    ! call the main utility routines
    !---------------------------------------------------------------------
    call UtilitiesMain (domain%itime, domain, forcing, energy)

    !---------------------------------------------------------------------
    ! call the main forcing routines
    !---------------------------------------------------------------------

    call ForcingMain (domain, levels, options, parameters, forcing, energy, water)

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
    !---------------------------------------------------------------------

    call add_to_output(domain, water, energy, domain%itime, levels%nsoil,levels%nsnow)
    
    end associate ! terminate associate block
  END SUBROUTINE solve_noahmp

end module RunModule

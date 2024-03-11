! module for executing Noah-OWP-Modular model in a streamlined way

module RunModule
  
  use LevelsType
  use LevelsGridType
  use DomainType
  use DomainGridType
  use OptionsType
  use OptionsGridType
  use ParametersType
  use ParametersGridType
  use WaterType
  use WaterGridType
  use ForcingType
  use ForcingGridType
  use EnergyType
  use EnergyGridType
  use AsciiReadModule
  use OutputModule
  use UtilitiesModule
  use ForcingModule
  use InterceptionModule
  use EnergyModule
  use WaterModule
  use DateTimeUtilsModule
  use NamelistRead
  use AttributesType
  use bmi_grid

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

    type(namelist_type)       :: namelist
    type(attributes_type)     :: attributes
    type(levelsgrid_type)     :: levelsgrid
    type(domaingrid_type)     :: domaingrid
    type(optionsgrid_type)    :: optionsgrid
    type(parametersgrid_type) :: parametersgrid
    type(watergrid_type)      :: watergrid
    type(forcinggrid_type)    :: forcinggrid
    type(energygrid_type)     :: energygrid
  
  end type

contains

  ! This subroutine is a temporary way to connect BMI grid definitions to the NOAH-OWP-Modular domain
  ! I think we can further refine/refactor the domain structure to take advantage of the same information/data stucture
  subroutine set_grid_from_model(model, grid)
    implicit none
    ! A fully initialized model instance with domain information already set
    type(noahowpgrid_type), intent (in) :: model
    type(GridType), intent(inout) :: grid
    ! note these are in y, x order
    !TODO align model%domaingrid with GridType and reduce this redundancy
    if(grid%rank==2) grid%shape = (/model%domaingrid%n_y, model%domaingrid%n_x/)
    if(grid%rank==3) grid%shape = (/model%levelsgrid%nsnow,model%domaingrid%n_y, model%domaingrid%n_x/)
    grid%spacing = (/model%domaingrid%dy, model%domaingrid%dx/)
    grid%origin = (/model%domaingrid%lat(1,1), model%domaingrid%lon(1,1)/) 
    ! in a projected system, it is possible that grid spacing has units, for now just use none
    grid%units = none
  
  end subroutine

  SUBROUTINE initialize_from_file(model, config_filename)

    implicit none
    
    type (noahowpgrid_type), intent (out)   :: model
    character(len=*), intent (in)           :: config_filename    ! config file from command line argument
    integer                                 :: forcing_timestep         ! integer time step (set to dt) for some subroutine calls
    integer                                 :: ii, ix, iy
        
    associate(namelist       => model%namelist,       &
              attributes     => model%attributes,     &
              levelsgrid     => model%levelsgrid,     &
              domaingrid     => model%domaingrid,     &
              optionsgrid    => model%optionsgrid,    &
              parametersgrid => model%parametersgrid, &
              watergrid      => model%watergrid,      &
              forcinggrid    => model%forcinggrid,    &
              energygrid     => model%energygrid)

      !---------------------------------------------------------------------
      !  initialize
      !---------------------------------------------------------------------
      call namelist%ReadNamelist(config_filename)
      call attributes%Init(namelist)
      
      call levelsgrid%Init(namelist)
      call levelsgrid%InitTransfer(namelist)

      call domaingrid%Init(namelist,attributes)
      call domaingrid%InitTransfer(namelist,attributes)

      call optionsgrid%Init(namelist)
      call optionsgrid%InitTransfer(namelist)

      call parametersgrid%Init(namelist,attributes)
      call parametersgrid%paramRead(namelist,domaingrid)

      call forcinggrid%Init(attributes)
      call forcinggrid%InitTransfer(namelist,attributes)

      call energygrid%Init(namelist,attributes)
      call energygrid%InitTransfer(namelist)

      call watergrid%Init(namelist,attributes)
      call watergrid%InitTransfer(namelist,attributes)

      ! Initializations
      ! for soil water
      !water%zwt       = -100.0       ! should only be needed for run=1
      watergrid%smcwtd(:,:)    = 0.0          ! should only be needed for run=5
      watergrid%deeprech(:,:)  = 0.0          ! should only be needed for run=5
      watergrid%qinsur(:,:)    = 0.0          !
      watergrid%runsrf(:,:)    = 0.0          !
      watergrid%runsub(:,:)    = 0.0          !
      watergrid%qdrain(:,:)    = 0.0          !
      watergrid%wcnd(:,:,:)    = 0.0          !
      watergrid%fcrmax(:,:)    = 0.0          !
      watergrid%snoflow(:,:)   = 0.0          ! glacier outflow for all RUNSUB options, [mm/s]
      watergrid%qseva(:,:)     = 0.0          ! soil evaporation [mm/s]
      watergrid%etrani(:,:,:)  = 0.0          ! transpiration from each level[mm/s]
      watergrid%btrani(:,:,:)  = 0.0          ! soil water transpiration factor (0 to 1) by soil layer
      watergrid%btran(:,:)     = 0.0          ! soil water transpiration factor (0 to 1)
  
      ! for canopy water
      watergrid%RAIN(:,:)      = 0.0          ! rainfall mm/s
      watergrid%SNOW(:,:)      = 0.0          ! snowfall mm/s
      watergrid%BDFALL(:,:)    = 0.0        ! bulk density of snowfall (kg/m3)
      watergrid%FB_snow(:,:)   = 0.0          ! canopy fraction buried by snow (computed from phenology)
      watergrid%FP(:,:)        = 1.0          ! fraction of the gridcell that receives precipitation
      watergrid%CANLIQ(:,:)    = 0.0          ! canopy liquid water [mm]
      watergrid%CANICE(:,:)    = 0.0          ! canopy frozen water [mm]
      watergrid%FWET(:,:)      = 0.0          ! canopy fraction wet or snow
      watergrid%CMC(:,:)       = 0.0          ! intercepted water per ground area (mm)
      watergrid%QINTR(:,:)    = 0.0           ! interception rate for rain (mm/s)
      watergrid%QDRIPR(:,:)   = 0.0           ! drip rate for rain (mm/s)
      watergrid%QTHROR(:,:)   = 0.0           ! throughfall for rain (mm/s)
      watergrid%QINTS(:,:)    = 0.0           ! interception (loading) rate for snowfall (mm/s)
      watergrid%QDRIPS(:,:)   = 0.0           ! drip (unloading) rate for intercepted snow (mm/s)
      watergrid%QTHROS(:,:)   = 0.0           ! throughfall of snowfall (mm/s)
      watergrid%QRAIN(:,:)    = 0.0           ! rain at ground srf (mm/s) [+]
      watergrid%QSNOW(:,:)    = 0.0           ! snow at ground srf (mm/s) [+]
      watergrid%SNOWHIN(:,:)  = 0.0           ! snow depth increasing rate (m/s)
      watergrid%ECAN(:,:)     = 0.0           ! evap of intercepted water (mm/s) [+]
      watergrid%ETRAN(:,:)    = 0.0           ! transpiration rate (mm/s) [+]
  
      ! for snow water
      watergrid%QVAP(:,:)     = 0.0           ! evaporation/sublimation rate mm/s 
      watergrid%ISNOW(:,:)    = 0
      watergrid%SNOWH(:,:)    = 0.0
      watergrid%SNEQV(:,:)    = 0.0
      watergrid%SNEQVO(:,:)   = 0.0
      watergrid%BDSNO(:,:)    = 0.0
      watergrid%PONDING(:,:)  = 0.0
      watergrid%PONDING1(:,:) = 0.0
      watergrid%PONDING2(:,:) = 0.0
      watergrid%QSNBOT(:,:)   = 0.0
      watergrid%QSNFRO(:,:)   = 0.0
      watergrid%QSNSUB(:,:)   = 0.0
      watergrid%QDEW(:,:)     = 0.0
      watergrid%QSDEW(:,:)    = 0.0
      watergrid%SNICE(:,:,:)    = 0.0
      watergrid%SNLIQ(:,:,:)    = 0.0
      watergrid%FICEOLD(:,:,:)  = 0.0
      watergrid%FSNO(:,:)     = 0.0
  
      ! for energy-related variable
      energygrid%TV(:,:)      = 298.0        ! leaf temperature [K]
      energygrid%TG(:,:)      = 298.0        ! ground temperature [K]
      energygrid%CM(:,:)      = 0.0          ! momentum drag coefficient
      energygrid%CH(:,:)      = 0.0          ! heat drag coefficient
      energygrid%FCEV(:,:)    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
      energygrid%FCTR(:,:)    = 5.0          ! constant transpiration (w/m2) [+ to atm]
      energygrid%IMELT(:,:,:) = 1 ! freeze
      energygrid%STC(:,:,:)   = 298.0
      energygrid%COSZ(:,:)    = 0.7        ! cosine of solar zenith angle
      energygrid%ICE(:,:)     = 0          ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
      energygrid%ALB(:,:)     = 0.6        ! initialize snow albedo in CLASS routine
      energygrid%ALBOLD(:,:)  = 0.6        ! initialize snow albedo in CLASS routine
      energygrid%FROZEN_CANOPY(:,:) = .false. ! used to define latent heat pathway
      energygrid%FROZEN_GROUND(:,:) = .false. 

      ! -- forcings 
      ! these are initially set to huge(1) -- to trap errors may want to set to a recognizable flag if they are
      !   supposed to be assigned below (eg -9999)
      !forcinggrid%UU(:,:)       = 0.0        ! wind speed in u direction (m s-1)
      !forcinggrid%VV(:,:)       = 0.0        ! wind speed in v direction (m s-1)
      !forcinggrid%SFCPRS(:,:)   = 0.0        ! pressure (pa)
      !forcinggrid%SFCTMP(:,:)   = 0.0        ! surface air temperature [k]
      !forcinggrid%Q2(:,:)       = 0.0        ! mixing ratio (kg/kg)
      !forcinggrid%PRCP(:,:)     = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      !forcinggrid%SOLDN(:,:)    = 0.0        ! downward shortwave radiation (w/m2)
      !forcinggrid%LWDN(:,:)     = 0.0        ! downward longwave radiation (w/m2)
      
      ! forcing-related variables
      forcinggrid%PRCPCONV(:,:) = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      forcinggrid%PRCPNONC(:,:) = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
      forcinggrid%PRCPSHCV(:,:) = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
      forcinggrid%PRCPSNOW(:,:) = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
      forcinggrid%PRCPGRPL(:,:) = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
      forcinggrid%PRCPHAIL(:,:) = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
      forcinggrid%THAIR(:,:)    = 0.0        ! potential temperature (k)
      forcinggrid%QAIR(:,:)     = 0.0        ! specific humidity (kg/kg) (q2/(1+q2))
      forcinggrid%EAIR(:,:)     = 0.0        ! vapor pressure air (pa)
      forcinggrid%RHOAIR(:,:)   = 0.0        ! density air (kg/m3)
      forcinggrid%SWDOWN(:,:)   = 0.0        ! downward solar filtered by sun angle [w/m2]
      forcinggrid%FPICE(:,:)    = 0.0        ! fraction of ice                AJN
      forcinggrid%JULIAN        = 0.0        ! Setting arbitrary julian day
      forcinggrid%YEARLEN       = 365        ! Setting year to be normal (i.e. not a leap year)  
      forcinggrid%FOLN(:,:)     = 1.0        ! foliage nitrogen concentration (%); for now, set to nitrogen saturation
      forcinggrid%TBOT(:,:)     = 285.0      ! bottom condition for soil temperature [K]

      ! domain variables
      domaingrid%zsnso(:,:,-namelist%nsnow+1:0) = 0.0
      do ii = 1, namelist%nsoil
        domaingrid%zsnso(:,:,ii) = namelist%zsoil(ii)
      end do
      domaingrid%IST(:,:) = 1                                                ! 1 = soil
      where (domaingrid%vegtyp == parametersgrid%ISWATER) domaingrid%IST = 2 ! 2 = lake

      ! time variables
      domaingrid%nowdate   = domaingrid%startdate ! start the model with nowdate = startdate
      forcing_timestep     = domaingrid%dt        ! integer timestep for some subroutine calls
      domaingrid%itime     = 1                     ! initialize the time loop counter at 1
      domaingrid%time_dbl  = 0.d0                  ! start model run at t = 0
      
      !---------------------------------------------------------------------
      !--- set a time vector for simulation ---
      !---------------------------------------------------------------------
      ! --- AWW:  calculate start and end utimes & records for requested station data read period ---
      call get_utime_list (domaingrid%start_datetime, domaingrid%end_datetime, domaingrid%dt, domaingrid%sim_datetimes)  ! makes unix-time list for desired records (end-of-timestep)
      domaingrid%ntime = size (domaingrid%sim_datetimes)   
      domaingrid%curr_datetime = domaingrid%start_datetime 
      !print *, "---------"; 
      !print *, 'Simulation startdate = ', domain%startdate, ' enddate = ', domain%enddate, ' dt(sec) = ', domain%dt, ' ntimes = ', domain%ntime  ! YYYYMMDD dates
      !print *, "---------"
      
      !---------------------------------------------------------------------
      ! Open/read the first forcing file
      ! Compiler directive NGEN_FORCING_ACTIVE to be defined if 
      ! Nextgen forcing is being used (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
call forcinggrid%ReadForcings(domaingrid%start_datetime/60.,domaingrid%startdate)
#endif
      
      !---------------------------------------------------------------------
      ! create output file and add initial values
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call initialize_output(namelist%output_filename, domaingrid%ntime, levelsgrid%nsoil, levelsgrid%nsnow, domaingrid%n_x, domaingrid%n_y)
#endif

      end associate

  END SUBROUTINE initialize_from_file   

  SUBROUTINE cleanup(model)
    implicit none
    type(noahowpgrid_type), intent(inout) :: model
      
      !---------------------------------------------------------------------
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call finalize_output()
#endif
  
  END SUBROUTINE cleanup

  SUBROUTINE advance_in_time(model)
    
    implicit none
    type (noahowpgrid_type), intent (inout) :: model
    integer                                 :: idt    ! change in time since beginning of run (in minutes)

    ! execute model
    call solve_noahowp_grid(model)

    !advance/set time variables for next time step
    associate(domaingrid => model%domaingrid)
    domaingrid%itime    = domaingrid%itime + 1                                                                     ! increment itime by 1
    domaingrid%time_dbl = dble(domaingrid%time_dbl + domaingrid%dt)                                                ! increment model time in seconds by DT
    idt = (domaingrid%itime-1) * (domaingrid%dt / 60)                                                              ! calculate change in time since beginning of run (in minutes)
    call geth_newdate(domaingrid%startdate, idt, domaingrid%nowdate)                                               ! update nowdate
    if(domaingrid%itime <= domaingrid%ntime) domaingrid%curr_datetime = domaingrid%sim_datetimes(domaingrid%itime) ! update curr_datetime 
    end associate

  END SUBROUTINE advance_in_time

  SUBROUTINE solve_noahowp_grid(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type)                    :: noahowp       
    integer                               :: ix, iy, ierr

    associate(namelist       => noahowpgrid%namelist,       &
              domain         => noahowp%domain,             &
              domaingrid     => noahowpgrid%domaingrid,     &
              levels         => noahowp%levels,             &
              levelsgrid     => noahowpgrid%levelsgrid,     &
              energy         => noahowp%energy,             &
              energygrid     => noahowpgrid%energygrid,     &
              forcing        => noahowp%forcing,            &
              forcinggrid    => noahowpgrid%forcinggrid,    &
              options        => noahowp%options,            &
              optionsgrid    => noahowpgrid%optionsgrid,    &
              parameters     => noahowp%parameters,         &
              parametersgrid => noahowpgrid%parametersgrid, &
              water          => noahowp%water,              &
              watergrid      => noahowpgrid%watergrid)

  !---------------------------------------------------------------------
  ! Set forcings if not NGEN_FORCING_ACTIVE
  !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
    call forcinggrid%SetForcings(domaingrid%curr_datetime/60.,domaingrid%nowdate)
#endif

    !---------------------------------------------------------------------
    ! Initialize noahowp_type subtypes
    !---------------------------------------------------------------------
    call levels%Init      (namelist,  levelsgrid    )
    call domain%Init      (namelist,  domaingrid    )
    call options%Init     (namelist,  optionsgrid   )
    call parameters%Init  (namelist,  parametersgrid)
    call forcing%Init     (namelist,  forcinggrid   )
    call energy%Init      (namelist,  energygrid    )
    call water%Init       (namelist,  watergrid     )

    !---------------------------------------------------------------------
    ! Iterate over x and y dimensions
    !---------------------------------------------------------------------
    do ix = 1, noahowpgrid%domaingrid%n_x
      do iy = 1, noahowpgrid%domaingrid%n_y

        !---------------------------------------------------------------------
        ! If (x,y) is within the model domain
        !---------------------------------------------------------------------
        if(domaingrid%mask(ix,iy) == 1) then

          !---------------------------------------------------------------------
          ! Transfer all other variable values from noahowpgrid_type to noahowp_type
          !---------------------------------------------------------------------
          call domain%TransferIn       (domaingrid,     ix, iy)
          call levels%TransferIn       (levelsgrid,     ix, iy)
          call energy%TransferIn       (energygrid,     ix, iy)
          call forcing%TransferIn      (forcinggrid,    ix, iy)
          call options%TransferIn      (optionsgrid,    ix, iy)
          call parameters%TransferIn   (parametersgrid, ix, iy)
          call water%TransferIn        (watergrid,      ix, iy)
          
          !---------------------------------------------------------------------
          ! Execute the column model
          !---------------------------------------------------------------------
          call solve_noahowp           (noahowp)

          !---------------------------------------------------------------------
          ! Transfer variable values from noahowp_type back to noahowpgrid_type
          !---------------------------------------------------------------------
          call domain%TransferOut      (domaingrid,     ix, iy)
          call levels%TransferOut      (levelsgrid,     ix, iy)
          call energy%TransferOut      (energygrid,     ix, iy)
          call forcing%TransferOut     (forcinggrid,    ix, iy)
          call options%TransferOut     (optionsgrid,    ix, iy)
          call parameters%TransferOut  (parametersgrid, ix, iy)
          call water%TransferOut       (watergrid,      ix, iy)

        end if
        
      end do
    end do

    end associate

  END SUBROUTINE solve_noahowp_grid

  SUBROUTINE solve_noahowp(noahowp)

    type (noahowp_type), intent (inout) :: noahowp

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
    call UtilitiesMain (domain, forcing, energy)

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
    call add_to_output(domain, water, energy, forcing, domain%itime, levels%nsoil, levels%nsnow)
#endif
    
    end associate ! terminate associate block

  END SUBROUTINE solve_noahowp

end module RunModule

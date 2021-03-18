!
! compile: 
!

program water_driver

  use WaterOutput
  use LevelsType
  use DomainType
  use NamelistRead
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use WaterModule

  implicit none

!---------------------------------------------------------------------
!  types
!---------------------------------------------------------------------

  type (namelist_type)     :: namelist
  type (levels_type)       :: levels
  type (domain_type)       :: domain
  type (parameters_type)   :: parameters
  type (options_type)      :: options
  type (water_type)        :: water
  type (forcing_type)      :: forcing 
  type (energy_type)       :: energy

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz          ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: rain_steps = 0     ! number of timesteps in rain event
  integer :: snow_steps = 0     ! number of timesteps in snow event
  integer :: dry_steps  = 0     ! number of timesteps between rain events
  integer :: rain_step  = 0     ! number of timesteps in current event
  integer :: snow_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0     ! number of timesteps in current event
  logical :: raining            ! .true. if raining
  logical :: snowing            ! .true. if snowing

!---------------------------------------------------------------------
!  initialize
!---------------------------------------------------------------------

  call namelist%ReadNamelist()

  call levels%Init
  call levels%InitTransfer(namelist)

  call domain%Init(namelist)
  call domain%InitTransfer(namelist)

  call options%Init()
  call options%InitTransfer(namelist)

  call parameters%Init(namelist)
  call parameters%InitTransfer(namelist)

  call forcing%Init(namelist)
  call forcing%InitTransfer(namelist)

  call energy%Init(namelist)
  call energy%InitTransfer(namelist)

  call water%Init(namelist)
  call water%InitTransfer(namelist)

! for soil water
!    water%zwt       = (25.0 + 2.0) - 4900.0/1000/0.2 ! cenlin for run=1
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
    water%btrani    = 0.0
! for canopy water
    water%RAIN      = 0.0          ! rainfall mm/s
    water%SNOW      = 0.0          ! snowfall mm/s
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
! for energy-related variable
    energy%TV      = 298.0        ! leaf temperature [K]
    energy%TG      = 298.0        ! ground temperature [K]
    energy%SFCTMP  = 298.0        ! model-level temperature (k)
    energy%FCEV    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
    energy%FCTR    = 5.0          ! constant transpiration (w/m2) [+ to atm]
    energy%FROZEN_CANOPY = .false. ! used to define latent heat pathway
    energy%IMELT = 1 ! freeze
    energy%FROZEN_GROUND = .false. 
    energy%STC      = 298.0
! for other variables
    ntime      =  nint(namelist%maxtime * 3600.0 / namelist%dt)
    rain_steps = namelist%rain_duration * 3600.0 / namelist%dt
    dry_steps  =  namelist%dry_duration * 3600.0 / namelist%dt
    raining    = namelist%raining
    snow_steps = namelist%snow_duration * 3600.0 / namelist%dt
    snowing    = namelist%snowing

    domain%IST = 1
    domain%zsnso(-namelist%nsnow+1:0) = 0.0
    domain%zsnso(1:namelist%nsoil) = namelist%zsoil

! additional assignment for testing
    water%qseva     = 0.005/3600.0
    water%etrani    = 0.005/3600.0
    water%QVAP      = 0.000005

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(namelist%output_filename, ntime+1, levels%soil, levels%snow)
  call add_to_output(0,levels%soil,levels%snow,domain%dzsnso,domain%dt,domain%zsnso,water,energy)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
   
  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------
  
  ! below statements create an artificial rain event
!    if(raining) then
!      water%rain    = namelist%rainrate/3600.0    ! input water [m/s]
!      rain_step = rain_step + 1
!      if(rain_step == rain_steps) then            ! event length met
!        rain_step = 0
!        raining   = .false.
!      end if
!    else
!      water%rain   = 0.0                        ! stop water input [m/s]
!      dry_step = dry_step + 1
!      if(dry_step == dry_steps) then              ! between event length met
!        dry_step = 0
!        raining  = .true.
!      end if
!    end if
	
    ! below statements create an artificial snow event
    if(snowing) then
      water%snow    = namelist%snowrate/3600.0    ! input water [m/s]
      snow_step = snow_step + 1
      if(snow_step == snow_steps) then            ! event length met
        snow_step = 0
        snowing   = .false.
      end if
    else
      water%snow   = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if(dry_step == dry_steps) then              ! between event length met
        dry_step = 0
        snowing  = .true.
      end if
    end if



  !---------------------------------------------------------------------
  ! call the main water routines (canopy + snow + soil water components
  !--------------------------------------------------------------------- 

    call WaterMain (domain, levels, options, parameters, forcing, energy, water)

  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

    call add_to_output(itime,levels%soil,levels%snow,domain%dzsnso,domain%dt,domain%zsnso,water,energy)
   
  end do ! time loop

  call finalize_output()
   
end program

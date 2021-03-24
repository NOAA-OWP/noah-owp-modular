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

  integer :: itime, iz            ! some loop counters
  integer :: ntime      = 0       ! number of timesteps to run
  integer :: precip_steps = 0     ! number of timesteps in rain event
  integer :: dry_steps  = 0       ! number of timesteps between rain events
  integer :: precip_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0       ! number of timesteps in current event
  logical :: precipitating        ! .true. if precipitating

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
    energy%FCEV    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
    energy%FCTR    = 5.0          ! constant transpiration (w/m2) [+ to atm]
    energy%FROZEN_CANOPY = .false. ! used to define latent heat pathway
    energy%IMELT = 1 ! freeze
    energy%FROZEN_GROUND = .false. 
    energy%STC      = 298.0
! for forcing-related variables
    forcing%uwind    = 0.0        ! wind speed in u direction (m s-1)
    forcing%vwind    = 0.0        ! wind speed in v direction (m s-1)
    forcing%SFCPRS   = 100000.0   !pressure (pa)
    forcing%SFCTMP   = 273.0      !surface air temperature [k]
    forcing%Q2       = 0.0        !mixing ratio (kg/kg)
    forcing%PRCPCONV = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
    forcing%PRCPNONC = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
    forcing%PRCPSHCV = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
    forcing%PRCPSNOW = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
    forcing%PRCPGRPL = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
    forcing%PRCPHAIL = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
    !forcing%SOLDN    = 0.0        ! downward shortwave radiation (w/m2)
    !forcing%COSZ     = 0.0        ! cosine solar zenith angle [0-1]
    forcing%THAIR    = 0.0        !potential temperature (k)
    forcing%QAIR     = 0.0        !specific humidity (kg/kg) (q2/(1+q2))
    forcing%EAIR     = 0.0        !vapor pressure air (pa)
    forcing%RHOAIR   = 0.0        !density air (kg/m3)
    forcing%QPRECC   = 0.0        !convective precipitation (mm/s)
    forcing%QPRECL   = 0.0        !large-scale precipitation (mm/s)
    !REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
    !REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
    !forcing%SWDOWN   = 0.0        ! downward solar filtered by sun angle [w/m2]
    forcing%FP       = 0.0        ! fraction of area receiving precipitation  AJN
    forcing%FPICE    = 0.0        ! fraction of ice                AJN

! for other variables
    ntime         =  nint(namelist%maxtime * 3600.0 / namelist%dt)
    precip_steps  =  namelist%precip_duration * 3600.0 / namelist%dt
    dry_steps     =  namelist%dry_duration * 3600.0 / namelist%dt
    precipitating =  namelist%precipitating

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
  ! calculate the input water by simulating a synthetic precip event
  !---------------------------------------------------------------------

    if(precipitating) then
      forcing%PRCPNONC    = namelist%preciprate/3600.0    ! input water [m/s]
      precip_step = precip_step + 1
      if(precip_step == precip_steps) then            ! event length met
        precip_step = 0
        precipitating   = .false.
      end if
    else
      forcing%PRCPNONC   = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if(dry_step == dry_steps) then              ! between event length met
        dry_step = 0
        precipitating  = .true.
      end if
    end if

  !---------------------------------------------------------------------
  ! call the main forcing routines 
  !--------------------------------------------------------------------- 

    call ForcingMain (domain, levels, options, parameters, forcing, energy, water)

  !---------------------------------------------------------------------
  ! call the main water routines (canopy + snow + soil water components)
  !--------------------------------------------------------------------- 

    call WaterMain (domain, levels, options, parameters, forcing, energy, water)

  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

    call add_to_output(itime,levels%soil,levels%snow,domain%dzsnso,domain%dt,domain%zsnso,water,energy)
   
  end do ! time loop

  call finalize_output()
   
end program

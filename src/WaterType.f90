module WaterType

use NamelistRead,  only: namelist_type
use WaterGridType, only: watergrid_type

implicit none
private

type, public :: water_type

  real                            :: qinsur      ! water input on soil surface [m/s]
  real                            :: qseva       ! soil surface evap rate [m/s]
  real                            :: EVAPOTRANS  ! evapotranspiration, sum of QSEVA + ETRAN [m/s]
  real                            :: runsrf      ! surface runoff [mm/s] 
  real                            :: runsub      ! baseflow (sturation excess) [mm/s]
  real                            :: qdrain      ! soil-bottom free drainage [mm/s] 
  real                            :: zwt         ! the depth to water table [m]
  real                            :: smcwtd      ! soil water content between bottom of the soil and water table [m3/m3]
  real                            :: deeprech    ! recharge to or from the water table when deep [m]
  real                            :: fcrmax      ! maximum of fcr (-)
  real                            :: snoflow     ! glacier outflow, added to RUNSUB
  real                            :: pddum       ! infiltration rate at surface (m/s)
  real                            :: FACC        ! accumulated infiltration rate (m/s) in dynamic vic option
  real                            :: sicemax     ! maximum soil ice content (m3/m3)
  real                            :: FB_snow     ! canopy fraction buried by snow
  real                            :: rain        ! rainfall (mm/s)
  real                            :: snow        ! snowfall (mm/s)
  real                            :: bdfall      ! bulk density of new snowfall (kg/m3)
  real                            :: FP          ! fraction of the gridcell that receives precipitation
  real                            :: canliq      ! intercepted liquid water (mm)
  real                            :: canice      ! intercepted ice mass (mm)
  real                            :: FWET        ! wetted or snowed fraction of the canopy (-)
  real                            :: CMC         ! total canopy moisture content (CANLIQ + CANICE) (mm)
  real                            :: QINTR       ! interception rate for rain (mm/s)
  real                            :: QDRIPR      ! drip rate for rain (mm/s)
  real                            :: QTHROR      ! throughfall for rain (mm/s)
  real                            :: QINTS       ! interception (loading) rate for snowfall (mm/s)
  real                            :: QDRIPS      ! drip (unloading) rate for intercepted snow (mm/s)
  real                            :: QTHROS      ! throughfall of snowfall (mm/s)
  real                            :: QRAIN       ! rain at ground surface (mm/s) [+]
  real                            :: QSNOW       ! snow at ground surface (mm/s) [+]
  real                            :: SNOWHIN     ! snow depth increasing rate (m/s)
  real                            :: ECAN        ! evaporation of intercepted water (mm/s) [+]
  real                            :: ETRAN       ! transpiration rate (mm/s) [+]
  real                            :: QSNFRO      ! snow surface frost rate[mm/s]
  real                            :: QSNSUB      ! snow surface sublimation rate[mm/s]
  real                            :: SNOWH       ! snow height [m]
  real                            :: SNEQV       ! snow water eqv. [mm]
  real                            :: SNEQVO      ! snow water eqv. of previous time step [mm]
  real                            :: BDSNO       ! bulk density of snowpack (kg/m3)
  real                            :: QSNBOT      ! melting water out of snow bottom [mm/s]
  real                            :: PONDING
  real                            :: PONDING1
  real                            :: PONDING2
  real                            :: QVAP        ! ground surface evaporation/sublimation rate mm/s
  real                            :: QDEW        ! ground surface dew rate [mm/s]
  real                            :: QSDEW       ! soil surface dew rate [mm/s]
  real                            :: WSLAKE      ! water storage in lake (can be -) (mm)
  real                            :: runsrf_dt   ! temporal time step for surface runoff calculations
  real                            :: ASAT        ! accumulated saturation in VIC runoff scheme
  
  integer                         :: ISNOW       ! actual no. of snow layers 
  real, allocatable, dimension(:) :: smc         ! total soil water content [m3/m3]
  real, allocatable, dimension(:) :: smc_init    ! initial total soil water content [m3/m3]
  real, allocatable, dimension(:) :: sice        ! total soil ice content [m3/m3]
  real, allocatable, dimension(:) :: sh2o        ! total soil liquid content [m3/m3]
  real, allocatable, dimension(:) :: etrani      ! transpiration rate (mm/s) [+]
  real, allocatable, dimension(:) :: BTRANI      ! Soil water transpiration factor (0 - 1)
  real, allocatable, dimension(:) :: wcnd        ! hydraulic conductivity (m/s)
  real, allocatable, dimension(:) :: fcr         ! impermeable fraction due to frozen soil 
  real, allocatable, dimension(:) :: FICEOLD     ! ice fraction at last timestep
  real, allocatable, dimension(:) :: SNICE       ! snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ       ! snow layer liquid water [mm] 
  real, allocatable, dimension(:) :: SNICEV      ! snow layer partial volume of ice [m3/m3]
  real, allocatable, dimension(:) :: SNLIQV      ! snow layer partial volume of liquid water [m3/m3]
  real, allocatable, dimension(:) :: FICE        ! fraction of ice at current time step
  real, allocatable, dimension(:) :: EPORE       ! snow layer effective porosity [m3/m3]
  
  real                            :: FSNO        ! fraction of grid cell with snow cover
  real                            :: BTRAN       ! soil water transpiration factor (0 to 1)  

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault
    procedure, private :: InitTransfer     

end type water_type

contains   

  subroutine Init(this, namelist, watergrid)

    class(water_type),    intent(inout) :: this
    type(namelist_type),  intent(in)    :: namelist
    type(watergrid_type), intent(in)    :: watergrid

    call this%InitAllocate(namelist)
    call this%InitDefault()
    call this%InitTransfer(watergrid)

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(water_type),   intent(inout) :: this
    type(namelist_type), intent(in)    :: namelist

    associate(nsoil => namelist%nsoil, &
              nsnow => namelist%nsnow)

    if(.NOT.allocated(this%smc))      allocate(this%smc     (nsoil))
    if(.NOT.allocated(this%smc_init)) allocate(this%smc_init(nsoil))
    if(.NOT.allocated(this%sice))     allocate(this%sice    (nsoil))
    if(.NOT.allocated(this%sh2o))     allocate(this%sh2o    (nsoil))
    if(.NOT.allocated(this%etrani))   allocate(this%etrani  (nsoil))
    if(.NOT.allocated(this%btrani))   allocate(this%btrani  (nsoil))
    if(.NOT.allocated(this%wcnd))     allocate(this%wcnd    (nsoil))
    if(.NOT.allocated(this%fcr))      allocate(this%fcr     (nsoil))
    if(.NOT.allocated(this%FICEOLD))  allocate(this%FICEOLD (-nsnow+1:0))
    if(.NOT.allocated(this%SNICE))    allocate(this%SNICE   (-nsnow+1:0))
    if(.NOT.allocated(this%SNLIQ))    allocate(this%SNLIQ   (-nsnow+1:0))
    if(.NOT.allocated(this%SNICEV))   allocate(this%SNICEV  (-nsnow+1:0))
    if(.NOT.allocated(this%SNLIQV))   allocate(this%SNLIQV  (-nsnow+1:0))
    if(.NOT.allocated(this%FICE))     allocate(this%FICE    (-nsnow+1:0))
    if(.NOT.allocated(this%EPORE))    allocate(this%EPORE   (-nsnow+1:0))

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(water_type), intent(inout) :: this

    this%qinsur      = huge(1.0)
    this%qseva       = huge(1.0)
    this%EVAPOTRANS  = huge(1.0)
    this%runsrf      = huge(1.0)
    this%runsub      = huge(1.0)
    this%qdrain      = huge(1.0)
    this%zwt         = huge(1.0)
    this%smcwtd      = huge(1.0)
    this%deeprech    = huge(1.0)
    this%fcrmax      = huge(1.0)
    this%snoflow     = huge(1.0)
    this%pddum       = huge(1.0)
    this%FACC        = huge(1.0)
    this%sicemax     = huge(1.0)
    this%FB_snow     = huge(1.0)
    this%rain        = huge(1.0)
    this%snow        = huge(1.0)
    this%bdfall      = huge(1.0)
    this%FP          = huge(1.0)
    this%canliq      = huge(1.0)
    this%canice      = huge(1.0)
    this%FWET        = huge(1.0)
    this%CMC         = huge(1.0)
    this%QINTR       = huge(1.0)
    this%QDRIPR      = huge(1.0)
    this%QTHROR      = huge(1.0)
    this%QINTS       = huge(1.0)
    this%QDRIPS      = huge(1.0)
    this%QTHROS      = huge(1.0)
    this%QRAIN       = huge(1.0)
    this%QSNOW       = huge(1.0)
    this%SNOWHIN     = huge(1.0)
    this%ECAN        = huge(1.0)
    this%ETRAN       = huge(1.0)
    this%QSNFRO      = huge(1.0)
    this%QSNSUB      = huge(1.0)
    this%SNOWH       = huge(1.0)
    this%SNEQV       = huge(1.0)
    this%SNEQVO      = huge(1.0)
    this%BDSNO       = huge(1.0)
    this%QSNBOT      = huge(1.0)
    this%PONDING     = huge(1.0)
    this%PONDING1    = huge(1.0)
    this%PONDING2    = huge(1.0)
    this%QVAP        = huge(1.0)
    this%QDEW        = huge(1.0)
    this%QSDEW       = huge(1.0)
    this%WSLAKE      = huge(1.0)
    this%runsrf_dt   = huge(1.0)
    this%ASAT        = huge(1.0)
    this%ISNOW       = huge(1)
    this%smc(:)      = huge(1.0)
    this%smc_init(:) = huge(1.0)
    this%sice(:)     = huge(1.0)
    this%sh2o(:)     = huge(1.0)
    this%etrani(:)   = huge(1.0)
    this%BTRANI(:)   = huge(1.0)
    this%wcnd(:)     = huge(1.0)
    this%fcr(:)      = huge(1.0)
    this%FICEOLD(:)  = huge(1.0)
    this%SNICE(:)    = huge(1.0)
    this%SNLIQ(:)    = huge(1.0)
    this%SNICEV(:)   = huge(1.0)
    this%SNLIQV(:)   = huge(1.0)
    this%FICE(:)     = huge(1.0)
    this%EPORE(:)    = huge(1.0)
    this%FSNO        = huge(1.0)
    this%BTRAN       = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this,watergrid)

    class(water_type),    intent(inout) :: this
    type(watergrid_type), intent(in)    :: watergrid

    ! Nothing to do

  end subroutine InitTransfer


end module WaterType

module WaterType

use NamelistRead

implicit none
save
private

type, public :: water_type

  real                            :: qinsur      ! water input on soil surface [m/s]
  real                            :: qseva       ! soil surface evap rate [mm/s]
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
  real                            :: CMC         ! intercepted water (mm)
  real                            :: QINTR       ! interception rate for rain (mm/s)
  real                            :: QDRIPR      ! drip rate for rain (mm/s)
  real                            :: QTHROR      ! throughfall for rain (mm/s)
  real                            :: QINTS       ! interception (loading) rate for snowfall (mm/s)
  real                            :: QDRIPS      ! drip (unloading) rate for intercepted snow (mm/s)
  real                            :: QTHROS      ! throughfall of snowfall (mm/s)
  real                            :: QRAIN       ! rain at ground srf (mm/s) [+]
  real                            :: QSNOW       ! snow at ground srf (mm/s) [+]
  real                            :: SNOWHIN     ! snow depth increasing rate (m/s)
  real                            :: ECAN        ! evaporation of intercepted water (mm/s) [+]
  real                            :: ETRAN       ! transpiration rate (mm/s) [+]
  real                            :: QSNFRO      ! snow surface frost rate[mm/s]
  real                            :: QSNSUB      ! snow surface sublimation rate[mm/s]
  real                            :: SNOWH       ! snow height [m]
  real                            :: SNEQV       ! snow water eqv. [mm]
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
    procedure, public  :: InitTransfer

end type water_type

contains   

  subroutine Init(this, namelist)

    class(water_type) :: this
    type(namelist_type) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(water_type) :: this
    type(namelist_type) :: namelist

    allocate(this%smc   (namelist%nsoil))  ; this%smc   (:) = huge(1.0)
    allocate(this%sice  (namelist%nsoil))  ; this%sice  (:) = huge(1.0)
    allocate(this%sh2o  (namelist%nsoil))  ; this%sh2o  (:) = huge(1.0)
    allocate(this%etrani(namelist%nsoil))  ; this%etrani(:) = huge(1.0)
    allocate(this%btrani(namelist%nsoil))  ; this%btrani(:) = huge(1.0)
    allocate(this%wcnd  (namelist%nsoil))  ; this%wcnd  (:) = huge(1.0)
    allocate(this%fcr   (namelist%nsoil))  ; this%fcr   (:) = huge(1.0)
    allocate(this%FICEOLD(-namelist%nsnow+1:0)); this%FICEOLD (:) = huge(1.0)
    allocate(this%SNICE  (-namelist%nsnow+1:0)); this%SNICE   (:) = huge(1.0)
    allocate(this%SNLIQ  (-namelist%nsnow+1:0)); this%SNLIQ   (:) = huge(1.0)
    allocate(this%SNICEV (-namelist%nsnow+1:0)); this%SNICEV  (:) = huge(1.0)
    allocate(this%SNLIQV (-namelist%nsnow+1:0)); this%SNLIQV  (:) = huge(1.0)
    allocate(this%FICE   (-namelist%nsnow+1:0)); this%FICE    (:) = huge(1.0)
    allocate(this%EPORE  (-namelist%nsnow+1:0)); this%EPORE   (:) = huge(1.0)

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(water_type) :: this

    this%qinsur   = huge(1.0)
    this%qseva    = huge(1.0)
    this%runsrf   = huge(1.0)
    this%runsub   = huge(1.0)
    this%qdrain   = huge(1.0)
    this%zwt      = huge(1.0)
    this%smcwtd   = huge(1.0)
    this%deeprech = huge(1.0)
    this%fcrmax   = huge(1.0)
    this%snoflow  = huge(1.0)
    this%pddum    = huge(1.0)
    this%FACC     = huge(1.0)
    this%sicemax  = huge(1.0)
    this%FB_snow  = huge(1.0)
    this%rain     = huge(1.0)
    this%snow     = huge(1.0)
    this%bdfall   = huge(1.0)
    this%FP       = huge(1.0)
    this%canliq   = huge(1.0)
    this%canice   = huge(1.0)
    this%FWET     = huge(1.0)
    this%CMC      = huge(1.0)
    this%QINTR    = huge(1.0)
    this%QDRIPR   = huge(1.0)
    this%QTHROR   = huge(1.0)
    this%QINTS    = huge(1.0)
    this%QDRIPS   = huge(1.0)
    this%QTHROS   = huge(1.0)
    this%QRAIN    = huge(1.0)
    this%QSNOW    = huge(1.0)
    this%SNOWHIN  = huge(1.0)
    this%ECAN     = huge(1.0)
    this%ETRAN    = huge(1.0)
    this%BTRAN    = huge(1.0)
    this%QSNFRO   = huge(1.0)
    this%QSNSUB   = huge(1.0)
    this%SNOWH    = huge(1.0)
    this%SNEQV    = huge(1.0)
    this%BDSNO    = huge(1.0)
    this%QSNBOT   = huge(1.0)
    this%PONDING  = huge(1.0)
    this%PONDING1 = huge(1.0)
    this%PONDING2 = huge(1.0)
    this%QVAP     = huge(1.0)
    this%QDEW     = huge(1.0)
    this%QSDEW    = huge(1.0)
    this%WSLAKE   = huge(1.0)
    this%runsrf_dt= huge(1.0)
    this%ASAT     = huge(1.0)
    this%ISNOW    = huge(1)

    this%FSNO     = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(water_type) :: this
    type(namelist_type) :: namelist

    if(namelist%initial_uniform) then
      this%sh2o = namelist%initial_sh2o_value
      this%sice = namelist%initial_sice_value
    else
      this%sh2o = namelist%sh2o
      this%sice = namelist%sice
    end if

    this%smc = this%sh2o + this%sice  ! initial volumetric soil water

  end subroutine InitTransfer

end module WaterType

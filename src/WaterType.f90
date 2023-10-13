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
  real                            :: ACSNOM      ! Accumulated meltwater from bottom snow layer [mm] (NWM 3.0)
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
    procedure, public  :: TransferIn
    procedure, public  :: TransferOut     

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

  subroutine TransferIn(this, watergrid, ix, iy)

    implicit none

    class(water_type),    intent(inout) :: this
    type(watergrid_type), intent(in)    :: watergrid
    integer,              intent(in)    :: ix
    integer,              intent(in)    :: iy

    this%qinsur = watergrid%qinsur(ix,iy)
    this%qseva = watergrid%qseva(ix,iy)
    this%EVAPOTRANS = watergrid%EVAPOTRANS(ix,iy)
    this%runsrf = watergrid%runsrf(ix,iy)
    this%runsub = watergrid%runsub(ix,iy)
    this%qdrain = watergrid%qdrain(ix,iy)
    this%zwt = watergrid%zwt(ix,iy)
    this%smcwtd = watergrid%smcwtd(ix,iy)
    this%deeprech = watergrid%deeprech(ix,iy)
    this%fcrmax = watergrid%fcrmax(ix,iy)
    this%snoflow = watergrid%snoflow(ix,iy)
    this%pddum = watergrid%pddum(ix,iy)
    this%FACC = watergrid%FACC(ix,iy)
    this%sicemax = watergrid%sicemax(ix,iy)
    this%FB_snow = watergrid%FB_snow(ix,iy)
    this%rain = watergrid%rain(ix,iy)
    this%snow = watergrid%snow(ix,iy)
    this%bdfall = watergrid%bdfall(ix,iy)
    this%FP = watergrid%FP(ix,iy)
    this%canliq = watergrid%canliq(ix,iy)
    this%canice = watergrid%canice(ix,iy)
    this%FWET = watergrid%FWET(ix,iy)
    this%CMC = watergrid%CMC(ix,iy)
    this%QINTR = watergrid%QINTR(ix,iy)
    this%QDRIPR = watergrid%QDRIPR(ix,iy)
    this%QTHROR = watergrid%QTHROR(ix,iy)
    this%QINTS = watergrid%QINTS(ix,iy)
    this%QDRIPS = watergrid%QDRIPS(ix,iy)
    this%QTHROS = watergrid%QTHROS(ix,iy)
    this%QRAIN = watergrid%QRAIN(ix,iy)
    this%QSNOW = watergrid%QSNOW(ix,iy)
    this%SNOWHIN = watergrid%SNOWHIN(ix,iy)
    this%ECAN = watergrid%ECAN(ix,iy)
    this%ETRAN = watergrid%ETRAN(ix,iy)
    this%QSNFRO = watergrid%QSNFRO(ix,iy)
    this%QSNSUB = watergrid%QSNSUB(ix,iy)
    this%SNOWH = watergrid%SNOWH(ix,iy)
    this%SNEQV = watergrid%SNEQV(ix,iy)
    this%SNEQVO = watergrid%SNEQVO(ix,iy)
    this%BDSNO = watergrid%BDSNO(ix,iy)
    this%QSNBOT = watergrid%QSNBOT(ix,iy)
    this%PONDING = watergrid%PONDING(ix,iy)
    this%PONDING1 = watergrid%PONDING1(ix,iy)
    this%PONDING2 = watergrid%PONDING2(ix,iy)
    this%QVAP = watergrid%QVAP(ix,iy)
    this%QDEW = watergrid%QDEW(ix,iy)
    this%QSDEW = watergrid%QSDEW(ix,iy)
    this%WSLAKE = watergrid%WSLAKE(ix,iy)
    this%runsrf_dt = watergrid%runsrf_dt(ix,iy)
    this%ASAT = watergrid%ASAT(ix,iy)
    this%ISNOW = watergrid%ISNOW(ix,iy)
    this%smc(:) = watergrid%smc(ix,iy,:)
    this%smc_init(:) = watergrid%smc_init(ix,iy,:)
    this%sice(:) = watergrid%sice(ix,iy,:)
    this%sh2o(:) = watergrid%sh2o(ix,iy,:)
    this%etrani(:) = watergrid%etrani(ix,iy,:)
    this%BTRANI(:) = watergrid%BTRANI(ix,iy,:)
    this%wcnd(:) = watergrid%wcnd(ix,iy,:)
    this%fcr(:) = watergrid%fcr(ix,iy,:)
    this%FICEOLD(:) = watergrid%FICEOLD(ix,iy,:)
    this%SNICE(:) = watergrid%SNICE(ix,iy,:)
    this%SNLIQ(:) = watergrid%SNLIQ(ix,iy,:)
    this%SNICEV(:) = watergrid%SNICEV(ix,iy,:)
    this%SNLIQV(:) = watergrid%SNLIQV(ix,iy,:)
    this%FICE(:) = watergrid%FICE(ix,iy,:)
    this%EPORE(:) = watergrid%EPORE(ix,iy,:)
    this%FSNO = watergrid%FSNO(ix,iy)
    this%BTRAN = watergrid%BTRAN(ix,iy)

  end subroutine TransferIn

  subroutine TransferOut(this, watergrid, ix, iy)

    implicit none

    class(water_type),    intent(in)    :: this
    type(watergrid_type), intent(inout) :: watergrid
    integer,              intent(in)    :: ix
    integer,              intent(in)    :: iy

    watergrid%qinsur(ix,iy) = this%qinsur
    watergrid%qseva(ix,iy) = this%qseva
    watergrid%EVAPOTRANS(ix,iy) = this%EVAPOTRANS
    watergrid%runsrf(ix,iy) = this%runsrf
    watergrid%runsub(ix,iy) = this%runsub
    watergrid%qdrain(ix,iy) = this%qdrain
    watergrid%zwt(ix,iy) = this%zwt
    watergrid%smcwtd(ix,iy) = this%smcwtd
    watergrid%deeprech(ix,iy) = this%deeprech
    watergrid%fcrmax(ix,iy) = this%fcrmax
    watergrid%snoflow(ix,iy) = this%snoflow
    watergrid%pddum(ix,iy) = this%pddum
    watergrid%FACC(ix,iy) = this%FACC
    watergrid%sicemax(ix,iy) = this%sicemax
    watergrid%FB_snow(ix,iy) = this%FB_snow
    watergrid%rain(ix,iy) = this%rain
    watergrid%snow(ix,iy) = this%snow
    watergrid%bdfall(ix,iy) = this%bdfall
    watergrid%FP(ix,iy) = this%FP
    watergrid%canliq(ix,iy) = this%canliq
    watergrid%canice(ix,iy) = this%canice
    watergrid%FWET(ix,iy) = this%FWET
    watergrid%CMC(ix,iy) = this%CMC
    watergrid%QINTR(ix,iy) = this%QINTR
    watergrid%QDRIPR(ix,iy) = this%QDRIPR
    watergrid%QTHROR(ix,iy) = this%QTHROR
    watergrid%QINTS(ix,iy) = this%QINTS
    watergrid%QDRIPS(ix,iy) = this%QDRIPS
    watergrid%QTHROS(ix,iy) = this%QTHROS
    watergrid%QRAIN(ix,iy) = this%QRAIN
    watergrid%QSNOW(ix,iy) = this%QSNOW
    watergrid%SNOWHIN(ix,iy) = this%SNOWHIN
    watergrid%ECAN(ix,iy) = this%ECAN
    watergrid%ETRAN(ix,iy) = this%ETRAN
    watergrid%QSNFRO(ix,iy) = this%QSNFRO
    watergrid%QSNSUB(ix,iy) = this%QSNSUB
    watergrid%SNOWH(ix,iy) = this%SNOWH
    watergrid%SNEQV(ix,iy) = this%SNEQV
    watergrid%SNEQVO(ix,iy) = this%SNEQVO
    watergrid%BDSNO(ix,iy) = this%BDSNO
    watergrid%QSNBOT(ix,iy) = this%QSNBOT
    watergrid%PONDING(ix,iy) = this%PONDING
    watergrid%PONDING1(ix,iy) = this%PONDING1
    watergrid%PONDING2(ix,iy) = this%PONDING2
    watergrid%QVAP(ix,iy) = this%QVAP
    watergrid%QDEW(ix,iy) = this%QDEW
    watergrid%QSDEW(ix,iy) = this%QSDEW
    watergrid%WSLAKE(ix,iy) = this%WSLAKE
    watergrid%runsrf_dt(ix,iy) = this%runsrf_dt
    watergrid%ASAT(ix,iy) = this%ASAT
    watergrid%ISNOW(ix,iy) = this%ISNOW
    watergrid%smc(ix,iy,:) = this%smc(:)
    watergrid%smc_init(ix,iy,:) = this%smc_init(:)
    watergrid%sice(ix,iy,:) = this%sice(:)
    watergrid%sh2o(ix,iy,:) = this%sh2o(:)
    watergrid%etrani(ix,iy,:) = this%etrani(:)
    watergrid%BTRANI(ix,iy,:) = this%BTRANI(:)
    watergrid%wcnd(ix,iy,:) = this%wcnd(:)
    watergrid%fcr(ix,iy,:) = this%fcr(:)
    watergrid%FICEOLD(ix,iy,:) = this%FICEOLD(:)
    watergrid%SNICE(ix,iy,:) = this%SNICE(:)
    watergrid%SNLIQ(ix,iy,:) = this%SNLIQ(:)
    watergrid%SNICEV(ix,iy,:) = this%SNICEV(:)
    watergrid%SNLIQV(ix,iy,:) = this%SNLIQV(:)
    watergrid%FICE(ix,iy,:) = this%FICE(:)
    watergrid%EPORE(ix,iy,:) = this%EPORE(:)
    watergrid%FSNO(ix,iy) = this%FSNO
    watergrid%BTRAN(ix,iy) = this%BTRAN

  end subroutine TransferOut

end module WaterType

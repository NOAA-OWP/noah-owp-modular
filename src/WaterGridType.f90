module WaterGridType

use NamelistRead,   only: namelist_type
use NetCDFVarsType, only: netcdfvars_type

implicit none
private

type, public :: watergrid_type

  real,allocatable,dimension(:,:)                             :: qinsur      ! water input on soil surface [m/s]
  real,allocatable,dimension(:,:)                             :: qseva       ! soil surface evap rate [m/s]
  real,allocatable,dimension(:,:)                             :: EVAPOTRANS  ! evapotranspiration, sum of QSEVA + ETRAN [m/s]
  real,allocatable,dimension(:,:)                             :: runsrf      ! surface runoff [mm/s] 
  real,allocatable,dimension(:,:)                             :: runsub      ! baseflow (sturation excess) [mm/s]
  real,allocatable,dimension(:,:)                             :: qdrain      ! soil-bottom free drainage [mm/s] 
  real,allocatable,dimension(:,:)                             :: zwt         ! the depth to water table [m]
  real,allocatable,dimension(:,:)                             :: smcwtd      ! soil water content between bottom of the soil and water table [m3/m3]
  real,allocatable,dimension(:,:)                             :: deeprech    ! recharge to or from the water table when deep [m]
  real,allocatable,dimension(:,:)                             :: fcrmax      ! maximum of fcr (-)
  real,allocatable,dimension(:,:)                             :: snoflow     ! glacier outflow, added to RUNSUB
  real,allocatable,dimension(:,:)                             :: pddum       ! infiltration rate at surface (m/s)
  real,allocatable,dimension(:,:)                             :: FACC        ! accumulated infiltration rate (m/s) in dynamic vic option
  real,allocatable,dimension(:,:)                             :: sicemax     ! maximum soil ice content (m3/m3)
  real,allocatable,dimension(:,:)                             :: FB_snow     ! canopy fraction buried by snow
  real,allocatable,dimension(:,:)                             :: rain        ! rainfall (mm/s)
  real,allocatable,dimension(:,:)                             :: snow        ! snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: bdfall      ! bulk density of new snowfall (kg/m3)
  real,allocatable,dimension(:,:)                             :: FP          ! fraction of the gridcell that receives precipitation
  real,allocatable,dimension(:,:)                             :: canliq      ! intercepted liquid water (mm)
  real,allocatable,dimension(:,:)                             :: canice      ! intercepted ice mass (mm)
  real,allocatable,dimension(:,:)                             :: FWET        ! wetted or snowed fraction of the canopy (-)
  real,allocatable,dimension(:,:)                             :: CMC         ! total canopy moisture content (CANLIQ + CANICE) (mm)
  real,allocatable,dimension(:,:)                             :: QINTR       ! interception rate for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QDRIPR      ! drip rate for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QTHROR      ! throughfall for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QINTS       ! interception (loading) rate for snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: QDRIPS      ! drip (unloading) rate for intercepted snow (mm/s)
  real,allocatable,dimension(:,:)                             :: QTHROS      ! throughfall of snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: QRAIN       ! rain at ground surface (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: QSNOW       ! snow at ground surface (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: SNOWHIN     ! snow depth increasing rate (m/s)
  real,allocatable,dimension(:,:)                             :: ECAN        ! evaporation of intercepted water (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: ETRAN       ! transpiration rate (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: QSNFRO      ! snow surface frost rate[mm/s]
  real,allocatable,dimension(:,:)                             :: QSNSUB      ! snow surface sublimation rate[mm/s]
  real,allocatable,dimension(:,:)                             :: SNOWH       ! snow height [m]
  real,allocatable,dimension(:,:)                             :: SNEQV       ! snow water eqv. [mm]
  real,allocatable,dimension(:,:)                             :: SNEQVO      ! snow water eqv. of previous time step [mm]
  real,allocatable,dimension(:,:)                             :: BDSNO       ! bulk density of snowpack (kg/m3)
  real,allocatable,dimension(:,:)                             :: QSNBOT      ! melting water out of snow bottom [mm/s]
  real,allocatable,dimension(:,:)                             :: PONDING
  real,allocatable,dimension(:,:)                             :: PONDING1
  real,allocatable,dimension(:,:)                             :: PONDING2
  real,allocatable,dimension(:,:)                             :: QVAP        ! ground surface evaporation/sublimation rate mm/s
  real,allocatable,dimension(:,:)                             :: QDEW        ! ground surface dew rate [mm/s]
  real,allocatable,dimension(:,:)                             :: QSDEW       ! soil surface dew rate [mm/s]
  real,allocatable,dimension(:,:)                             :: WSLAKE      ! water storage in lake (can be -) (mm)
  real,allocatable,dimension(:,:)                             :: runsrf_dt   ! temporal time step for surface runoff calculations
  real,allocatable,dimension(:,:)                             :: ASAT        ! accumulated saturation in VIC runoff scheme

  integer,allocatable,dimension(:,:)                          :: ISNOW       ! actual no. of snow layers 
  real, allocatable, dimension(:,:,:)                         :: smc         ! total soil water content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: smc_init    ! initial total soil water content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: sice        ! total soil ice content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: sh2o        ! total soil liquid content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: etrani      ! transpiration rate (mm/s) [+]
  real, allocatable, dimension(:,:,:)                         :: BTRANI      ! Soil water transpiration factor (0 - 1)
  real, allocatable, dimension(:,:,:)                         :: wcnd        ! hydraulic conductivity (m/s)
  real, allocatable, dimension(:,:,:)                         :: fcr         ! impermeable fraction due to frozen soil 
  real, allocatable, dimension(:,:,:)                         :: FICEOLD     ! ice fraction at last timestep
  real, allocatable, dimension(:,:,:)                         :: SNICE       ! snow layer ice [mm]
  real, allocatable, dimension(:,:,:)                         :: SNLIQ       ! snow layer liquid water [mm] 
  real, allocatable, dimension(:,:,:)                         :: SNICEV      ! snow layer partial volume of ice [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: SNLIQV      ! snow layer partial volume of liquid water [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: FICE        ! fraction of ice at current time step
  real, allocatable, dimension(:,:,:)                         :: EPORE       ! snow layer effective porosity [m3/m3]

  real,allocatable,dimension(:,:)                             :: FSNO        ! fraction of grid cell with snow cover
  real,allocatable,dimension(:,:)                             :: BTRAN       ! soil water transpiration factor (0 to 1)  

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type watergrid_type

contains   

  subroutine Init(this, namelist, netcdfvars)

    class(watergrid_type), intent(inout) :: this
    type(namelist_type),   intent(in)    :: namelist
    type(netcdfvars_type), intent(in)    :: netcdfvars

    call this%InitAllocate(namelist,netcdfvars)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist, netcdfvars)

    class(watergrid_type), intent(inout) :: this
    type(namelist_type),   intent(in)    :: namelist
    type(netcdfvars_type), intent(in)    :: netcdfvars

    associate(n_x   => netcdfvars%metadata%n_x,   &
              n_y   => netcdfvars%metadata%n_y,   &
              nsoil => namelist%nsoil, &
              nsnow => namelist%nsnow)

    allocate(this%qinsur(n_x,n_y))
    allocate(this%qseva(n_x,n_y))
    allocate(this%EVAPOTRANS(n_x,n_y))
    allocate(this%runsrf(n_x,n_y))
    allocate(this%runsub(n_x,n_y))
    allocate(this%qdrain(n_x,n_y))
    allocate(this%zwt(n_x,n_y))
    allocate(this%smcwtd(n_x,n_y))
    allocate(this%deeprech(n_x,n_y))
    allocate(this%fcrmax(n_x,n_y))
    allocate(this%snoflow(n_x,n_y))
    allocate(this%pddum(n_x,n_y))
    allocate(this%FACC(n_x,n_y))
    allocate(this%sicemax(n_x,n_y))
    allocate(this%FB_snow(n_x,n_y))
    allocate(this%rain(n_x,n_y))
    allocate(this%snow(n_x,n_y))
    allocate(this%bdfall(n_x,n_y))
    allocate(this%FP(n_x,n_y))
    allocate(this%canliq(n_x,n_y))
    allocate(this%canice(n_x,n_y))
    allocate(this%FWET(n_x,n_y))
    allocate(this%CMC(n_x,n_y))
    allocate(this%QINTR(n_x,n_y))
    allocate(this%QDRIPR(n_x,n_y))
    allocate(this%QTHROR(n_x,n_y))
    allocate(this%QINTS(n_x,n_y))
    allocate(this%QDRIPS(n_x,n_y))
    allocate(this%QTHROS(n_x,n_y))
    allocate(this%QRAIN(n_x,n_y))
    allocate(this%QSNOW(n_x,n_y))
    allocate(this%SNOWHIN(n_x,n_y))
    allocate(this%ECAN(n_x,n_y))
    allocate(this%ETRAN(n_x,n_y))
    allocate(this%QSNFRO(n_x,n_y))
    allocate(this%QSNSUB(n_x,n_y))
    allocate(this%SNOWH(n_x,n_y))
    allocate(this%SNEQV(n_x,n_y))
    allocate(this%SNEQVO(n_x,n_y))
    allocate(this%BDSNO(n_x,n_y))
    allocate(this%QSNBOT(n_x,n_y))
    allocate(this%PONDING(n_x,n_y))
    allocate(this%PONDING1(n_x,n_y))
    allocate(this%PONDING2(n_x,n_y))
    allocate(this%QVAP(n_x,n_y))
    allocate(this%QDEW(n_x,n_y))
    allocate(this%QSDEW(n_x,n_y))
    allocate(this%WSLAKE(n_x,n_y))
    allocate(this%runsrf_dt(n_x,n_y))
    allocate(this%ASAT(n_x,n_y))
    allocate(this%ISNOW(n_x,n_y))
    allocate(this%smc(n_x,n_y,nsoil))
    allocate(this%smc_init(n_x,n_y,nsoil))
    allocate(this%sice(n_x,n_y,nsoil))
    allocate(this%sh2o(n_x,n_y,nsoil))
    allocate(this%etrani(n_x,n_y,nsoil))
    allocate(this%BTRANI(n_x,n_y,nsoil))
    allocate(this%wcnd(n_x,n_y,nsoil))
    allocate(this%fcr(n_x,n_y,nsoil))
    allocate(this%FICEOLD(n_x,n_y,-nsnow+1:0))
    allocate(this%SNICE(n_x,n_y,-nsnow+1:0))
    allocate(this%SNLIQ(n_x,n_y,-nsnow+1:0))
    allocate(this%SNICEV(n_x,n_y,-nsnow+1:0))
    allocate(this%SNLIQV(n_x,n_y,-nsnow+1:0))
    allocate(this%FICE(n_x,n_y,-nsnow+1:0))
    allocate(this%EPORE(n_x,n_y,-nsnow+1:0))
    allocate(this%FSNO(n_x,n_y))
    allocate(this%BTRAN(n_x,n_y))

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(watergrid_type) :: this

    this%qinsur(:,:) = huge(1.0)
    this%qseva(:,:) = huge(1.0)
    this%EVAPOTRANS(:,:) = huge(1.0)
    this%runsrf(:,:) = huge(1.0)
    this%runsub(:,:) = huge(1.0)
    this%qdrain(:,:) = huge(1.0)
    this%zwt(:,:) = huge(1.0)
    this%smcwtd(:,:) = huge(1.0)
    this%deeprech(:,:) = huge(1.0)
    this%fcrmax(:,:) = huge(1.0)
    this%snoflow(:,:) = huge(1.0)
    this%pddum(:,:) = huge(1.0)
    this%FACC(:,:) = huge(1.0)
    this%sicemax(:,:) = huge(1.0)
    this%FB_snow(:,:) = huge(1.0)
    this%rain(:,:) = huge(1.0)
    this%snow(:,:) = huge(1.0)
    this%bdfall(:,:) = huge(1.0)
    this%FP(:,:) = huge(1.0)
    this%canliq(:,:) = huge(1.0)
    this%canice(:,:) = huge(1.0)
    this%FWET(:,:) = huge(1.0)
    this%CMC(:,:) = huge(1.0)
    this%QINTR(:,:) = huge(1.0)
    this%QDRIPR(:,:) = huge(1.0)
    this%QTHROR(:,:) = huge(1.0)
    this%QINTS(:,:) = huge(1.0)
    this%QDRIPS(:,:) = huge(1.0)
    this%QTHROS(:,:) = huge(1.0)
    this%QRAIN(:,:) = huge(1.0)
    this%QSNOW(:,:) = huge(1.0)
    this%SNOWHIN(:,:) = huge(1.0)
    this%ECAN(:,:) = huge(1.0)
    this%ETRAN(:,:) = huge(1.0)
    this%QSNFRO(:,:) = huge(1.0)
    this%QSNSUB(:,:) = huge(1.0)
    this%SNOWH(:,:) = huge(1.0)
    this%SNEQV(:,:) = huge(1.0)
    this%SNEQVO(:,:) = huge(1.0)
    this%BDSNO(:,:) = huge(1.0)
    this%QSNBOT(:,:) = huge(1.0)
    this%PONDING(:,:) = huge(1.0)
    this%PONDING1(:,:) = huge(1.0)
    this%PONDING2(:,:) = huge(1.0)
    this%QVAP(:,:) = huge(1.0)
    this%QDEW(:,:) = huge(1.0)
    this%QSDEW(:,:) = huge(1.0)
    this%WSLAKE(:,:) = huge(1.0)
    this%runsrf_dt(:,:) = huge(1.0)
    this%ASAT(:,:) = huge(1.0)
    this%ISNOW(:,:) = huge(1)
    this%smc(:,:,:) = huge(1.0)
    this%smc_init(:,:,:) = huge(1.0)
    this%sice(:,:,:) = huge(1.0)
    this%sh2o(:,:,:) = huge(1.0)
    this%etrani(:,:,:) = huge(1.0)
    this%BTRANI(:,:,:) = huge(1.0)
    this%wcnd(:,:,:) = huge(1.0)
    this%fcr(:,:,:) = huge(1.0)
    this%FICEOLD(:,:,:) = huge(1.0)
    this%SNICE(:,:,:) = huge(1.0)
    this%SNLIQ(:,:,:) = huge(1.0)
    this%SNICEV(:,:,:) = huge(1.0)
    this%SNLIQV(:,:,:) = huge(1.0)
    this%FICE(:,:,:) = huge(1.0)
    this%EPORE(:,:,:) = huge(1.0)
    this%FSNO(:,:) = huge(1.0)
    this%BTRAN(:,:) = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist, netcdfvars)

    class(watergrid_type), intent(inout) :: this
    type(namelist_type),   intent(in)    :: namelist
    type(netcdfvars_type), intent(in)    :: netcdfvars
    integer                              :: ix, iy

    do ix = 1, netcdfvars%metadata%n_x
      do iy = 1, netcdfvars%metadata%n_y
        this%sh2o(ix,iy,:)     = namelist%sh2o(:)
        this%sice(ix,iy,:)     = namelist%sice(:)
        this%smc(ix,iy,:)      = this%sh2o(ix,iy,:) + this%sice(ix,iy,:)  ! volumetric soil water
        this%smc_init(ix,iy,:) = this%smc(ix,iy,:)               ! initial SMC
      end do
    end do
    this%zwt(:,:) = namelist%zwt                ! initialize zwt
    
  end subroutine InitTransfer

end module WaterGridType

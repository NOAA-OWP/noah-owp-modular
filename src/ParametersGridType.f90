module ParametersGridType

  use NamelistRead,   only: namelist_type
  use AttributesType, only: attributes_type
  use ParametersRead
  use DomainGridType

  implicit none
  private

  type, public :: parametersgrid_type

    real, allocatable, dimension(:,:,:)               :: bexp                      ! b parameter
    real, allocatable, dimension(:,:,:)               :: smcmax                    ! porosity (volumetric)
    real, allocatable, dimension(:,:,:)               :: smcwlt                    ! wilting point
    real, allocatable, dimension(:,:,:)               :: smcref                    ! field capacity
    real, allocatable, dimension(:,:,:)               :: dksat                     ! saturated conductivity
    real, allocatable, dimension(:,:,:)               :: dwsat                     ! saturated diffusivity
    real, allocatable, dimension(:,:,:)               :: psisat                    ! saturated matric potential
    real ,allocatable,dimension(:,:)                  :: bvic                      ! VIC or DVIC model infiltration parameter
    real,allocatable,dimension(:,:)                   :: AXAJ                      ! Xinanjiang: Tension water distribution inflection parameter [-]
    real,allocatable,dimension(:,:)                   :: BXAJ                      ! Xinanjiang: Tension water distribution shape parameter [-]
    real,allocatable,dimension(:,:)                   :: XXAJ                      ! Xinanjiang: Free water distribution shape parameter [-]
    real,allocatable,dimension(:,:)                   :: BBVIC                     ! DVIC heterogeniety parameter for infiltration
    real,allocatable,dimension(:,:)                   :: G                         ! Mean Capillary Drive (m) for infiltration models
    real,allocatable,dimension(:,:)                   :: QUARTZ                    ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
    real,allocatable,dimension(:,:)                   :: kdt                       !
    real,allocatable,dimension(:,:)                   :: refkdt                    !
    real,allocatable,dimension(:,:)                   :: refdk                     !
    real,allocatable,dimension(:,:)                   :: csoil                     ! volumetric soil heat capacity [j/m3/K]
    real,allocatable,dimension(:,:)                   :: Z0                        ! bare soil roughness length (m)
    real,allocatable,dimension(:,:)                   :: CZIL                      ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
    real,allocatable,dimension(:,:)                   :: ZBOT                      ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL
    real,allocatable,dimension(:,:)                   :: frzx                      !
    real,allocatable,dimension(:,:)                   :: slope                     ! drainage parameter
    real,allocatable,dimension(:,:)                   :: timean
    real,allocatable,dimension(:,:)                   :: fsatmx
    real,allocatable,dimension(:,:)                   :: ZWT_INIT                  ! initial water table depth below surface [m]
    logical,allocatable,dimension(:,:)                :: urban_flag
    real,allocatable,dimension(:,:,:)                 :: LAIM                      ! monthly LAI
    real,allocatable,dimension(:,:,:)                 :: SAIM                      ! monthly SAI
    real,allocatable,dimension(:,:)                   :: LAI
    real,allocatable,dimension(:,:)                   :: SAI
    real,allocatable,dimension(:,:)                   :: CH2OP                     ! maximum intercepted h2o per unit lai+sai (mm)
    integer,allocatable,dimension(:,:)                :: NROOT                     ! vegetation root level
    real,allocatable,dimension(:,:)                   :: HVT                       ! canopy top height (m)
    real,allocatable,dimension(:,:)                   :: HVB                       ! canopy bottom height (m)
    real,allocatable,dimension(:,:)                   :: TMIN                      ! minimum temperature for photosynthesis (k)
    real,allocatable,dimension(:,:)                   :: SHDFAC                    ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real,allocatable,dimension(:,:)                   :: SHDMAX                    ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real,allocatable,dimension(:,:)                   :: Z0MVT                     ! momentum roughness length (m)
    real,allocatable,dimension(:,:)                   :: RC                        ! tree crown radius (m)
    real,allocatable,dimension(:,:)                   :: XL                        ! leaf/stem orientation index
    real,allocatable,dimension(:,:)                   :: BP                        ! minimum leaf conductance (umol/m**2/s)
    real,allocatable,dimension(:,:)                   :: FOLNMX                    ! foliage nitrogen concentration when f(n)=1 (%)
    real,allocatable,dimension(:,:)                   :: QE25                      ! quantum efficiency at 25c (umol co2 / umol photon)
    real,allocatable,dimension(:,:)                   :: VCMX25                    ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real,allocatable,dimension(:,:)                   :: MP                        ! slope of conductance-to-photosynthesis relationship
    real,allocatable,dimension(:,:)                   :: RGL                       ! Parameter used in radiation stress function
    real,allocatable,dimension(:,:)                   :: RSMIN                     ! Minimum stomatal resistance [s m-1]
    real,allocatable,dimension(:,:)                   :: HS                        ! Parameter used in vapor pressure deficit function
    real,allocatable,dimension(:,:)                   :: AKC                       ! q10 for kc25
    real,allocatable,dimension(:,:)                   :: AKO                       ! q10 for ko25
    real,allocatable,dimension(:,:)                   :: AVCMX                     ! q10 for vcmx25
    real,allocatable,dimension(:,:)                   :: RSMAX                     ! Maximal stomatal resistance [s m-1]
    real,allocatable,dimension(:,:)                   :: CWP                       ! canopy wind absorption coefficient (formerly CWPVT)
    real,allocatable,dimension(:,:)                   :: C3PSN                     ! photosynth. pathway: 0. = c4, 1. = c3 [by vegtype]
    real,allocatable,dimension(:,:)                   :: DLEAF                     ! characteristic leaf dimension (m)
    real,allocatable,dimension(:,:)                   :: KC25                      ! co2 michaelis-menten constant at 25c (pa)
    real,allocatable,dimension(:,:)                   :: KO25                      ! o2 michaelis-menten constant at 25c (pa)
    real,allocatable,dimension(:,:)                   :: ELAI
    real,allocatable,dimension(:,:)                   :: ESAI
    real,allocatable,dimension(:,:)                   :: VAI                       ! sum of ELAI + ESAI
    logical,allocatable,dimension(:,:)                :: VEG                       ! grid cell is vegetated (true) or not (false)
    real,allocatable,dimension(:,:)                   :: FVEG                      ! vegetation fraction
    real,allocatable,dimension(:,:,:)                 :: RHOL                      ! leaf reflectance (1 = vis, 2 = NIR)
    real,allocatable,dimension(:,:,:)                 :: RHOS                      ! stem reflectance (1 = vis, 2 = NIR)
    real,allocatable,dimension(:,:,:)                 :: TAUL                      ! leaf transmittance (1 = vis, 2 = NIR)
    real,allocatable,dimension(:,:,:)                 :: TAUS                      ! stem transmittance (1 = vis, 2 = NIR)
    real                                              :: TFRZ                      ! freezing/melting point (k)
    real                                              :: HSUB                      ! latent heat of sublimation (j/kg)
    real                                              :: HVAP                      ! latent heat of vaporization (j/kg)
    real                                              :: HFUS                      ! latent heat of fusion (j/kg)
    real                                              :: CWAT                      ! specific heat capacity of water (j/m3/k)
    real                                              :: CICE                      ! specific heat capacity of ice (j/m3/k)
    real                                              :: CPAIR                     ! heat capacity dry air at const pres (j/kg/k)
    real                                              :: TKWAT                     ! thermal conductivity of water (w/m/k)
    real                                              :: TKICE                     ! thermal conductivity of ice (w/m/k)
    real                                              :: TKAIR                     ! thermal conductivity of air (w/m/k) (not used MB: 20140718)
    real                                              :: RAIR                      ! gas constant for dry air (j/kg/k)
    real                                              :: RW                        ! gas constant for  water vapor (j/kg/k)
    real                                              :: DENH2O                    ! density of water (kg/m3)
    real                                              :: DENICE                    ! density of ice (kg/m3)
    real                                              :: THKW                      ! thermal conductivity of water in soil module (W/m/K)
    real                                              :: THKO                      ! thermal conductivity of for other soil components in soil module (W/m/K)
    real                                              :: THKQTZ                    ! thermal conductivity of quartz in soil module (W/m/K)
    real,allocatable,dimension(:,:)                   :: SSI                       ! liquid water holding capacity for snowpack (m3/m3)
    real,allocatable,dimension(:,:)                   :: MFSNO                     ! fractional snow covered area (FSNO) curve parameter
    real,allocatable,dimension(:,:)                   :: Z0SNO                     ! snow surface roughness length (m)
    real                                              :: SWEMX                     ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
    real                                              :: TAU0                      ! tau0 from Yang97 eqn. 10a
    real                                              :: GRAIN_GROWTH              ! growth from vapor diffusion Yang97 eqn. 10b
    real                                              :: EXTRA_GROWTH              ! extra growth near freezing Yang97 eqn. 10c
    real                                              :: DIRT_SOOT                 ! dirt and soot term Yang97 eqn. 10d
    real                                              :: BATS_COSZ                 ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real                                              :: BATS_VIS_NEW              ! new snow visible albedo
    real                                              :: BATS_NIR_NEW              ! new snow NIR albedo
    real                                              :: BATS_VIS_AGE              ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real                                              :: BATS_NIR_AGE              ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real                                              :: BATS_VIS_DIR              ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real                                              :: BATS_NIR_DIR              ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real,allocatable,dimension(:,:)                   :: RSURF_SNOW                ! surface resistence for snow [s/m]
    real,allocatable,dimension(:,:)                   :: RSURF_EXP                 ! exponent in the shape parameter for soil resistance option 1
    real,allocatable,dimension(:,:,:)                 :: ALBSAT                    ! saturated soil albedo (1=vis, 2=nir)
    real,allocatable,dimension(:,:,:)                 :: ALBDRY                    ! dry soil albedo (1=vis, 2=nir)
    real,dimension(2)                                 :: ALBICE                    ! Land ice albedo (1=vis, 2=nir)
    real,dimension(2)                                 :: ALBLAK                    ! Lake ice albedo (1=vis, 2=nir)
    real,dimension(2)                                 :: OMEGAS                    ! two-stream parameter omega for snow (1=vis, 2=nir)
    real                                              :: BETADS                    ! two-stream parameter betad for snow
    real                                              :: BETAIS                    ! two-stream parameter betaI for snow
    real,allocatable,dimension(:,:,:)                 :: EG                        ! emissivity of land surface (1=soil,2=lake)
    real,allocatable,dimension(:,:)                   :: WSLMAX                    ! maximum lake water storage (mm)
    real,allocatable,dimension(:,:)                   :: max_liq_mass_fraction     ! For snow water retention
    real,allocatable,dimension(:,:)                   :: SNOW_RET_FAC              ! snowpack water release timescale factor (1/s)
    real,allocatable,dimension(:,:)                   :: TOPT                      ! Optimum transpiration air temperature [K]
    real                                              :: O2                        ! o2 partial pressure, from MPTABLE.TBL
    real                                              :: CO2                       ! co2 partial pressure, from MPTABLE.TBL
    real,allocatable,dimension(:,:)                   :: PSIWLT                    ! matric potential for wilting point (m)  (orig a fixed param.)
    real,allocatable,dimension(:,:)                   :: TBOT                      ! bottom condition for soil temp. (k)
    real                                              :: GRAV                      ! acceleration due to gravity (m/s2)
    real,allocatable,dimension(:,:)                   :: rain_snow_thresh          ! user-defined rain-snow temperature threshold (°C)
    integer                                           :: ISURBAN                   ! vegtype code for urban land cover
    integer                                           :: ISWATER                   ! vegtype code for water
    integer                                           :: ISBARREN                  ! vegtype code for barren land cover
    integer                                           :: ISICE                     ! vegtype code for ice/snow land cover
    integer                                           :: ISCROP                    ! vegtype code for crop land cover
    integer                                           :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
    integer                                           :: NATURAL                   ! vegtype code for cropland/grassland mosaic
    integer                                           :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
    integer                                           :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
    integer                                           :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial
    real                                              :: SB                        ! Stefan-Boltzmann constant (w/m2/k4)
    real                                              :: VKC                       ! von Karman constant
    integer                                           :: NBAND                     ! Number of shortwave bands (2, visible and NIR)
    real                                              :: MPE                       ! MPE is nominally small to prevent dividing by zero error

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate
    procedure, private :: InitDefault
    procedure, public  :: paramRead

  end type

  contains

  subroutine Init(this, namelist, attributes)

    implicit none
    class(parametersgrid_type), intent(inout) :: this
    type(namelist_type),        intent(in)    :: namelist
    type(attributes_type),      intent(in)    :: attributes

    call this%InitAllocate(namelist,attributes)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist, attributes)

    implicit none
    class(parametersgrid_type), intent(inout) :: this
    type(namelist_type),        intent(in)    :: namelist
    type(attributes_type),      intent(in)    :: attributes

    associate(n_x   => attributes%metadata%n_x,  &
              n_y   => attributes%metadata%n_y,  &
              nsoil => namelist%nsoil)

    allocate(this%bexp(n_x,n_y,nsoil))
    allocate(this%smcmax(n_x,n_y,nsoil))
    allocate(this%smcwlt(n_x,n_y,nsoil))
    allocate(this%smcref(n_x,n_y,nsoil))
    allocate(this%dksat(n_x,n_y,nsoil))
    allocate(this%dwsat(n_x,n_y,nsoil))
    allocate(this%psisat(n_x,n_y,nsoil))
    allocate(this%bvic(n_x,n_y))
    allocate(this%AXAJ(n_x,n_y))
    allocate(this%BXAJ(n_x,n_y))
    allocate(this%XXAJ(n_x,n_y))
    allocate(this%BBVIC(n_x,n_y))
    allocate(this%G(n_x,n_y))
    allocate(this%QUARTZ(n_x,n_y))
    allocate(this%kdt(n_x,n_y))
    allocate(this%refkdt(n_x,n_y))
    allocate(this%refdk(n_x,n_y))
    allocate(this%csoil(n_x,n_y))
    allocate(this%Z0(n_x,n_y))
    allocate(this%CZIL(n_x,n_y))
    allocate(this%ZBOT(n_x,n_y))
    allocate(this%frzx(n_x,n_y))
    allocate(this%slope(n_x,n_y))
    allocate(this%timean(n_x,n_y))
    allocate(this%fsatmx(n_x,n_y))
    allocate(this%ZWT_INIT(n_x,n_y))
    allocate(this%urban_flag(n_x,n_y))
    allocate(this%LAIM(n_x,n_y,12))
    allocate(this%SAIM(n_x,n_y,12))
    allocate(this%LAI(n_x,n_y))
    allocate(this%SAI(n_x,n_y))
    allocate(this%CH2OP(n_x,n_y))
    allocate(this%NROOT(n_x,n_y))
    allocate(this%HVT(n_x,n_y))
    allocate(this%HVB(n_x,n_y))
    allocate(this%TMIN(n_x,n_y))
    allocate(this%SHDFAC(n_x,n_y))
    allocate(this%SHDMAX(n_x,n_y))
    allocate(this%Z0MVT(n_x,n_y))
    allocate(this%RC(n_x,n_y))
    allocate(this%XL(n_x,n_y))
    allocate(this%BP(n_x,n_y))
    allocate(this%FOLNMX(n_x,n_y))
    allocate(this%QE25(n_x,n_y))
    allocate(this%VCMX25(n_x,n_y))
    allocate(this%MP(n_x,n_y))
    allocate(this%RGL(n_x,n_y))
    allocate(this%RSMIN(n_x,n_y))
    allocate(this%HS(n_x,n_y))
    allocate(this%AKC(n_x,n_y))
    allocate(this%AKO(n_x,n_y))
    allocate(this%AVCMX(n_x,n_y))
    allocate(this%RSMAX(n_x,n_y))
    allocate(this%CWP(n_x,n_y))
    allocate(this%C3PSN(n_x,n_y))
    allocate(this%DLEAF(n_x,n_y))
    allocate(this%KC25(n_x,n_y))
    allocate(this%KO25(n_x,n_y))
    allocate(this%ELAI(n_x,n_y))
    allocate(this%ESAI(n_x,n_y))
    allocate(this%VAI(n_x,n_y))
    allocate(this%VEG(n_x,n_y))
    allocate(this%FVEG(n_x,n_y))
    allocate(this%RHOL(n_x,n_y,2))
    allocate(this%RHOS(n_x,n_y,2))
    allocate(this%TAUL(n_x,n_y,2))
    allocate(this%TAUS(n_x,n_y,2))
    allocate(this%SSI(n_x,n_y))
    allocate(this%MFSNO(n_x,n_y))
    allocate(this%Z0SNO(n_x,n_y))
    allocate(this%RSURF_SNOW(n_x,n_y))
    allocate(this%RSURF_EXP(n_x,n_y))
    allocate(this%ALBSAT(n_x,n_y,2))
    allocate(this%ALBDRY(n_x,n_y,2))
    allocate(this%EG(n_x,n_y,2))
    allocate(this%WSLMAX(n_x,n_y))
    allocate(this%max_liq_mass_fraction(n_x,n_y))
    allocate(this%SNOW_RET_FAC(n_x,n_y))
    allocate(this%TOPT(n_x,n_y))
    allocate(this%PSIWLT(n_x,n_y))
    allocate(this%TBOT(n_x,n_y))
    allocate(this%rain_snow_thresh(n_x,n_y))

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    implicit none
    class(parametersgrid_type) :: this

    this%bexp(:,:,:) = huge(1.0)
    this%smcmax(:,:,:) = huge(1.0)
    this%smcwlt(:,:,:) = huge(1.0)
    this%smcref(:,:,:) = huge(1.0)
    this%dksat(:,:,:) = huge(1.0)
    this%dwsat(:,:,:) = huge(1.0)
    this%psisat(:,:,:) = huge(1.0)
    this%bvic(:,:) = huge(1.0)
    this%AXAJ(:,:) = huge(1.0)
    this%BXAJ(:,:) = huge(1.0)
    this%XXAJ(:,:) = huge(1.0)
    this%BBVIC(:,:) = huge(1.0)
    this%G(:,:) = huge(1.0)
    this%QUARTZ(:,:) = huge(1.0)
    this%kdt(:,:) = huge(1.0)
    this%refkdt(:,:) = huge(1.0)
    this%refdk(:,:) = huge(1.0)
    this%csoil(:,:) = huge(1.0)
    this%Z0(:,:) = huge(1.0)
    this%CZIL(:,:) = huge(1.0)
    this%ZBOT(:,:) = huge(1.0)
    this%frzx(:,:) = huge(1.0)
    this%slope(:,:) = huge(1.0)
    this%timean(:,:) = huge(1.0)
    this%fsatmx(:,:) = huge(1.0)
    this%ZWT_INIT(:,:) = huge(1.0)
    this%urban_flag(:,:) = .FALSE.
    this%LAIM(:,:,:) = huge(1.0)
    this%SAIM(:,:,:) = huge(1.0)
    this%LAI(:,:) = huge(1.0)
    this%SAI(:,:) = huge(1.0)
    this%CH2OP(:,:) = huge(1.0)
    this%NROOT(:,:) = huge(1)
    this%HVT(:,:) = huge(1.0)
    this%HVB(:,:) = huge(1.0)
    this%TMIN(:,:) = huge(1.0)
    this%SHDFAC(:,:) = huge(1.0)
    this%SHDMAX(:,:) = huge(1.0)
    this%Z0MVT(:,:) = huge(1.0)
    this%RC(:,:) = huge(1.0)
    this%XL(:,:) = huge(1.0)
    this%BP(:,:) = huge(1.0)
    this%FOLNMX(:,:) = huge(1.0)
    this%QE25(:,:) = huge(1.0)
    this%VCMX25(:,:) = huge(1.0)
    this%MP(:,:) = huge(1.0)
    this%RGL(:,:) = huge(1.0)
    this%RSMIN(:,:) = huge(1.0)
    this%HS(:,:) = huge(1.0)
    this%AKC(:,:) = huge(1.0)
    this%AKO(:,:) = huge(1.0)
    this%AVCMX(:,:) = huge(1.0)
    this%RSMAX(:,:) = huge(1.0)
    this%CWP(:,:) = huge(1.0)
    this%C3PSN(:,:) = huge(1.0)
    this%DLEAF(:,:) = huge(1.0)
    this%KC25(:,:) = huge(1.0)
    this%KO25(:,:) = huge(1.0)
    this%ELAI(:,:) = huge(1.0)
    this%ESAI(:,:) = huge(1.0)
    this%VAI(:,:) = huge(1.0)
    this%VEG(:,:) = .FALSE.
    this%FVEG(:,:) = huge(1.0)
    this%RHOL(:,:,:) = huge(1.0)
    this%RHOS(:,:,:) = huge(1.0)
    this%TAUL(:,:,:) = huge(1.0)
    this%TAUS(:,:,:) = huge(1.0)
    this%TFRZ = huge(1.0)
    this%HSUB = huge(1.0)
    this%HVAP = huge(1.0)
    this%HFUS = huge(1.0)
    this%CWAT = huge(1.0)
    this%CICE = huge(1.0)
    this%CPAIR = huge(1.0)
    this%TKWAT = huge(1.0)
    this%TKICE = huge(1.0)
    this%TKAIR = huge(1.0)
    this%RAIR = huge(1.0)
    this%RW = huge(1.0)
    this%DENH2O = huge(1.0)
    this%DENICE = huge(1.0)
    this%THKW = huge(1.0)
    this%THKO = huge(1.0)
    this%THKQTZ = huge(1.0)
    this%SSI(:,:) = huge(1.0)
    this%MFSNO(:,:) = huge(1.0)
    this%Z0SNO(:,:) = huge(1.0)
    this%SWEMX = huge(1.0)
    this%TAU0 = huge(1.0)
    this%GRAIN_GROWTH = huge(1.0)
    this%EXTRA_GROWTH = huge(1.0)
    this%DIRT_SOOT = huge(1.0)
    this%BATS_COSZ = huge(1.0)
    this%BATS_VIS_NEW = huge(1.0)
    this%BATS_NIR_NEW = huge(1.0)
    this%BATS_VIS_AGE = huge(1.0)
    this%BATS_NIR_AGE = huge(1.0)
    this%BATS_VIS_DIR = huge(1.0)
    this%BATS_NIR_DIR = huge(1.0)
    this%RSURF_SNOW(:,:) = huge(1.0)
    this%RSURF_EXP(:,:) = huge(1.0)
    this%ALBSAT(:,:,:) = huge(1.0)
    this%ALBDRY(:,:,:) = huge(1.0)
    this%ALBICE(:) = huge(1.0)
    this%ALBLAK(:) = huge(1.0)
    this%OMEGAS(:) = huge(1.0)
    this%BETADS = huge(1.0)
    this%BETAIS = huge(1.0)
    this%EG(:,:,:) = huge(1.0)
    this%WSLMAX(:,:) = huge(1.0)
    this%max_liq_mass_fraction(:,:) = huge(1.0)
    this%SNOW_RET_FAC(:,:) = huge(1.0)
    this%TOPT(:,:) = huge(1.0)
    this%O2 = huge(1.0)
    this%CO2 = huge(1.0)
    this%PSIWLT(:,:) = huge(1.0)
    this%TBOT(:,:) = huge(1.0)
    this%GRAV = huge(1.0)
    this%rain_snow_thresh(:,:) = huge(1.0)

    this%ISURBAN = huge(1)
    this%ISWATER = huge(1)
    this%ISBARREN = huge(1)
    this%ISICE = huge(1)
    this%ISCROP = huge(1)
    this%EBLFOREST = huge(1)
    this%NATURAL = huge(1)
    this%LOW_DENSITY_RESIDENTIAL = huge(1)
    this%HIGH_DENSITY_RESIDENTIAL = huge(1)
    this%HIGH_INTENSITY_INDUSTRIAL = huge(1)
    this%SB = huge(1.0)
    this%VKC = huge(1.0)
    this%NBAND = huge(1)
    this%MPE = huge(1.0)

  end subroutine InitDefault

  subroutine paramRead(this, namelist, domaingrid)

    implicit none
    class(parametersgrid_type)             :: this
    type(namelist_type), intent(in)        :: namelist
    type(domaingrid_type), intent(in)      :: domaingrid
    ! local variables
    integer                          :: ix, iy, ii
    character(len=50)                :: dataset_identifier

    !dataset_identifier = "MODIFIED_IGBP_MODIS_NOAH"   ! This can be in namelist
    !call read_veg_parameters(namelist%parameter_dir, namelist%noahowp_table, dataset_identifier)
    call read_soil_parameters(namelist%parameter_dir, namelist%soil_table, namelist%general_table, namelist%soil_class_name)
    call read_veg_parameters(namelist%parameter_dir, namelist%noahowp_table, namelist%veg_class_name)
    !call read_soil_parameters(namelist%parameter_dir, namelist%soil_table, namelist%general_table)

    call read_rad_parameters(namelist%parameter_dir, namelist%noahowp_table)
    call read_global_parameters(namelist%parameter_dir, namelist%noahowp_table)

!---------------------------------------------------------------------
!  transfer to structure
!---------------------------------------------------------------------
      
    this%refkdt(:,:)               = REFKDT_TABLE
    this%refdk(:,:)                = REFDK_TABLE
    this%csoil(:,:)                = CSOIL_TABLE
    this%Z0(:,:)                   = Z0_TABLE     ! bare soil roughness length (m). in GENPARM.TBL.  NOTE: This is hard-coded in hrldas version of noah-mp
    this%CZIL(:,:)                 = CZIL_TABLE
    this%ZBOT(:,:)                 = ZBOT_TABLE
    this%SSI(:,:)                  = SSI_TABLE
    this%Z0SNO(:,:)                = Z0SNO_TABLE
    this%SWEMX                     = SWEMX_TABLE
    this%TAU0                      = TAU0_TABLE
    this%GRAIN_GROWTH              = GRAIN_GROWTH_TABLE
    this%EXTRA_GROWTH              = EXTRA_GROWTH_TABLE
    this%DIRT_SOOT                 = DIRT_SOOT_TABLE
    this%BATS_COSZ                 = BATS_COSZ_TABLE
    this%BATS_VIS_NEW              = BATS_VIS_NEW_TABLE
    this%BATS_NIR_NEW              = BATS_NIR_NEW_TABLE
    this%BATS_VIS_AGE              = BATS_VIS_AGE_TABLE
    this%BATS_NIR_AGE              = BATS_NIR_AGE_TABLE
    this%BATS_VIS_DIR              = BATS_VIS_DIR_TABLE
    this%BATS_NIR_DIR              = BATS_NIR_DIR_TABLE
    this%RSURF_SNOW(:,:)           = RSURF_SNOW_TABLE
    this%RSURF_EXP(:,:)            = RSURF_EXP_TABLE
    this%BETADS                    = BETADS_TABLE
    this%BETAIS                    = BETAIS_TABLE
    this%slope(:,:)                = SLOPE_TABLE(1)
    this%ISURBAN                   = ISURBAN_TABLE
    this%ISWATER                   = ISWATER_TABLE
    this%ISBARREN                  = ISBARREN_TABLE
    this%ISICE                     = ISICE_TABLE
    this%ISCROP                    = ISCROP_TABLE
    this%EBLFOREST                 = EBLFOREST_TABLE
    this%NATURAL                   = NATURAL_TABLE
    this%LOW_DENSITY_RESIDENTIAL   = LCZ_1_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_1
    this%HIGH_DENSITY_RESIDENTIAL  = LCZ_2_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_2
    this%HIGH_INTENSITY_INDUSTRIAL = LCZ_3_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_3

    this%urban_flag(:,:)           = .false.
    this%timean(:,:)               = 10.5
    this%fsatmx(:,:)               = 0.38
    this%GRAV                      = 9.80616
    this%SB                        = 5.67E-08
    this%VKC                       = 0.40
    this%TFRZ                      = 273.16
    this%HSUB                      = 2.8440E06
    this%HVAP                      = 2.5104E06
    this%HFUS                      = 0.3336E06
    this%CWAT                      = 4.188E06
    this%CICE                      = 2.094E06
    this%CPAIR                     = 1004.64
    this%TKWAT                     = 0.6
    this%TKICE                     = 2.2
    this%TKAIR                     = 0.023
    this%RAIR                      = 287.04
    this%RW                        = 461.269
    this%DENH2O                    = 1000.0
    this%DENICE                    = 917.0
    this%THKW                      = 0.57
    this%THKO                      = 2.0
    this%THKQTZ                    = 7.7
    this%WSLMAX(:,:)               = 5000.0
    this%max_liq_mass_fraction(:,:) = 0.4
    this%SNOW_RET_FAC(:,:)         = 5.e-5
    this%NBAND                     = 2           ! do not change
    this%MPE                       = 1.E-06      ! do not change ! need to make this a parameter
    this%TOPT(:,:)                 = 1.E-06      ! Optimum transpiration air temperature [K]
    this%CO2                       = 395.e-06    ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    this%O2                        = 0.209       ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    this%PSIWLT(:,:)               = -150.0      ! originally a fixed parameter set in ENERGY()
    this%TBOT(:,:)                 = 263.0       ! (K) can be updated depending on option OPT_TBOT
    
    do ix = 1, domaingrid%n_x
      do iy = 1, domaingrid%n_y

        associate(isltyp    => domaingrid%isltyp(ix,iy), &
                  vegtyp    => domaingrid%vegtyp(ix,iy), &
                  soilcolor => domaingrid%soilcolor(ix,iy))

        this%bexp(ix,iy,:) = BEXP_TABLE(isltyp)
        this%smcmax(ix,iy,:)  = SMCMAX_TABLE(isltyp)
        this%smcwlt(ix,iy,:)  = SMCWLT_TABLE(isltyp)
        this%smcref(ix,iy,:)  = SMCREF_TABLE(isltyp)
        this%dksat(ix,iy,:)   = DKSAT_TABLE(isltyp)
        this%dwsat(ix,iy,:)   = DWSAT_TABLE(isltyp)
        this%psisat(ix,iy,:)  = PSISAT_TABLE(isltyp)
        this%bvic(ix,iy)    = BVIC_table(isltyp)
        this%AXAJ(ix,iy)    = AXAJ_table(isltyp)
        this%BXAJ(ix,iy)    = BXAJ_table(isltyp)
        this%XXAJ(ix,iy)    = XXAJ_table(isltyp)
        this%BBVIC(ix,iy)   = BBVIC_table(isltyp)
        this%G(ix,iy)       = GDVIC_table(isltyp)
        this%QUARTZ(ix,iy)  = QUARTZ_table(isltyp)
        do ii = 1,12
          this%LAIM(ix,iy,ii) = LAIM_TABLE(vegtyp, ii)
          this%SAIM(ix,iy,ii) = SAIM_TABLE(vegtyp, ii)
        end do
        this%CH2OP(ix,iy)   = CH2OP_TABLE(vegtyp)
        this%NROOT(ix,iy)   = NROOT_TABLE(vegtyp)
        this%HVT(ix,iy)     = HVT_TABLE(vegtyp)
        this%HVB(ix,iy)     = HVB_TABLE(vegtyp)
        this%TMIN(ix,iy)    = TMIN_TABLE(vegtyp)
        this%SHDFAC(ix,iy)  = SHDFAC_TABLE(vegtyp) ! this used to be in VEGPARM.TBL, but now somewhere else for hrldas. this is temporarily in MPTABLE.TBL.
        this%SHDMAX(ix,iy)  = SHDFAC_TABLE(vegtyp)
        this%Z0MVT(ix,iy)   = Z0MVT_TABLE(vegtyp)
        this%RC(ix,iy)      = RC_TABLE(vegtyp)
        this%XL(ix,iy)      = XL_TABLE(vegtyp)
        this%BP(ix,iy)      = BP_TABLE(vegtyp)
        this%FOLNMX(ix,iy)  = FOLNMX_TABLE(vegtyp)
        this%QE25(ix,iy)    = QE25_TABLE(vegtyp)
        this%VCMX25(ix,iy)  = VCMX25_TABLE(vegtyp)
        this%MP(ix,iy)      = MP_TABLE(vegtyp)
        this%RGL(ix,iy)     = RGL_TABLE(vegtyp)
        this%RSMIN(ix,iy)   = RS_TABLE(vegtyp)
        this%HS(ix,iy)      = HS_TABLE(vegtyp)
        this%AKC(ix,iy)     = AKC_TABLE(vegtyp)
        this%AKO(ix,iy)     = AKO_TABLE(vegtyp)
        this%AVCMX(ix,iy)   = AVCMX_TABLE(vegtyp)
        this%RSMAX(ix,iy)   = RSMAX_TABLE(vegtyp)
        this%CWP(ix,iy)     = CWPVT_TABLE(vegtyp)
        this%C3PSN(ix,iy)   = C3PSN_TABLE(vegtyp)
        this%DLEAF(ix,iy)   = DLEAF_TABLE(vegtyp)
        this%KC25(ix,iy)    = KC25_TABLE(vegtyp)
        this%KO25(ix,iy)    = KO25_TABLE(vegtyp)
        this%RHOL(ix,iy,1) = RHOL_TABLE(vegtyp, 1)
        this%RHOL(ix,iy,2) = RHOL_TABLE(vegtyp, 2)
        this%RHOS(ix,iy,1) = RHOS_TABLE(vegtyp, 1)
        this%RHOS(ix,iy,2) = RHOS_TABLE(vegtyp, 2)
        this%TAUL(ix,iy,1) = TAUL_TABLE(vegtyp, 1)
        this%TAUL(ix,iy,2) = TAUL_TABLE(vegtyp, 2)
        this%TAUS(ix,iy,1) = TAUS_TABLE(vegtyp, 1)
        this%TAUS(ix,iy,2) = TAUS_TABLE(vegtyp, 2)
        this%kdt(ix,iy)      = this%refkdt(ix,iy) * this%dksat(ix,iy,1) / this%refdk(ix,iy)
        this%frzx(ix,iy)     = 0.15 * (this%smcmax(ix,iy,1) / this%smcref(ix,iy,1)) * (0.412 / 0.468)
        this%MFSNO(ix,iy)    = MFSNO_TABLE(vegtyp)
        this%ALBSAT(ix,iy,1) = ALBSAT_TABLE(soilcolor, 1)
        this%ALBSAT(ix,iy,2) = ALBSAT_TABLE(soilcolor, 2)
        this%ALBDRY(ix,iy,1) = ALBDRY_TABLE(soilcolor, 1)
        this%ALBDRY(ix,iy,2) = ALBDRY_TABLE(soilcolor, 2)
        this%ALBICE(:) = ALBICE_TABLE
        this%ALBLAK(:) = ALBLAK_TABLE
        this%OMEGAS(:) = OMEGAS_TABLE
        this%EG(ix,iy,:)     = EG_TABLE

        ! Assign rain-snow threshold based on option
        IF(namelist%precip_phase_option == 2) THEN
          this%rain_snow_thresh(ix,iy) = this%TFRZ + 2.2
        ELSE IF(namelist%precip_phase_option == 3) THEN
          this%rain_snow_thresh(ix,iy) = this%TFRZ
        ELSE IF(namelist%precip_phase_option == 5 .or. namelist%precip_phase_option == 6) THEN
          this%rain_snow_thresh(ix,iy) = this%TFRZ + namelist%rain_snow_thresh
        ELSE 
          this%rain_snow_thresh(ix,iy) = this%TFRZ ! set to TFRZ as a backup
        ENDIF
        
        end associate
      end do
    end do

    ! Assign initial soil moisture based on variable or uniform initial conditions
    this%zwt_init(:,:) = namelist%zwt
  
  end subroutine paramRead

end module ParametersGridType

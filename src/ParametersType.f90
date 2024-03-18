module ParametersType

  use NamelistRead,       only: namelist_type
  use ParametersGridType, only: parametersgrid_type
  
  implicit none
  private
  
  type, public :: parameters_type
  
    real, allocatable, dimension(:) :: bexp                      ! b parameter
    real, allocatable, dimension(:) :: smcmax                    ! porosity (volumetric)
    real, allocatable, dimension(:) :: smcwlt                    ! wilting point
    real, allocatable, dimension(:) :: smcref                    ! field capacity
    real, allocatable, dimension(:) :: dksat                     ! saturated conductivity
    real, allocatable, dimension(:) :: dwsat                     ! saturated diffusivity
    real, allocatable, dimension(:) :: psisat                    ! saturated matric potential
    real                            :: bvic                      ! VIC or DVIC model infiltration parameter
    real                            :: AXAJ                      ! Xinanjiang: Tension water distribution inflection parameter [-]
    real                            :: BXAJ                      ! Xinanjiang: Tension water distribution shape parameter [-]
    real                            :: XXAJ                      ! Xinanjiang: Free water distribution shape parameter [-]
    real                            :: BBVIC                     ! DVIC heterogeniety parameter for infiltration
    real                            :: G                         ! Mean Capillary Drive (m) for infiltration models
    real                            :: QUARTZ                    ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
    real                            :: kdt                       !
    real                            :: refkdt                    !
    real                            :: refdk                     !
    real                            :: csoil                     ! volumetric soil heat capacity [j/m3/K]
    real                            :: Z0                        ! bare soil roughness length (m)
    real                            :: CZIL                      ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
    real                            :: ZBOT                      ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL
    real                            :: frzx                      !
    real                            :: slope                     ! drainage parameter
    real                            :: timean
    real                            :: fsatmx
    real                            :: ZWT_INIT                  ! initial water table depth below surface [m]
    logical                         :: urban_flag
    real, dimension(12)             :: LAIM                      ! monthly LAI
    real, dimension(12)             :: SAIM                      ! monthly SAI
    real                            :: LAI
    real                            :: SAI
    real                            :: CH2OP                     ! maximum intercepted h2o per unit lai+sai (mm)
    integer                         :: NROOT                     ! vegetation root level
    real                            :: HVT                       ! canopy top height (m)
    real                            :: HVB                       ! canopy bottom height (m)
    real                            :: TMIN                      ! minimum temperature for photosynthesis (k)
    real                            :: SHDFAC                    ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real                            :: SHDMAX                    ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real                            :: Z0MVT                     ! momentum roughness length (m)
    real                            :: RC                        ! tree crown radius (m)
    real                            :: XL                        ! leaf/stem orientation index
    real                            :: BP                        ! minimum leaf conductance (umol/m**2/s)
    real                            :: FOLNMX                    ! foliage nitrogen concentration when f(n)=1 (%)
    real                            :: QE25                      ! quantum efficiency at 25c (umol co2 / umol photon)
    real                            :: VCMX25                    ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real                            :: MP                        ! slope of conductance-to-photosynthesis relationship
    real                            :: RGL                       ! Parameter used in radiation stress function
    real                            :: RSMIN                     ! Minimum stomatal resistance [s m-1]
    real                            :: HS                        ! Parameter used in vapor pressure deficit function
    real                            :: AKC                       ! q10 for kc25
    real                            :: AKO                       ! q10 for ko25
    real                            :: AVCMX                     ! q10 for vcmx25
    real                            :: RSMAX                     ! Maximal stomatal resistance [s m-1]
    real                            :: CWP                       ! canopy wind absorption coefficient (formerly CWPVT)
    real                            :: C3PSN                     ! photosynth. pathway: 0. = c4, 1. = c3 [by vegtype]
    real                            :: DLEAF                     ! characteristic leaf dimension (m)
    real                            :: KC25                      ! co2 michaelis-menten constant at 25c (pa)
    real                            :: KO25                      ! o2 michaelis-menten constant at 25c (pa)
    real                            :: ELAI
    real                            :: ESAI
    real                            :: VAI                       ! sum of ELAI + ESAI
    logical                         :: VEG                       ! grid cell is vegetated (true) or not (false)
    real                            :: FVEG                      ! vegetation fraction
    real, dimension(2)              :: RHOL                      ! leaf reflectance (1 = vis, 2 = NIR)
    real, dimension(2)              :: RHOS                      ! stem reflectance (1 = vis, 2 = NIR)
    real, dimension(2)              :: TAUL                      ! leaf transmittance (1 = vis, 2 = NIR)
    real, dimension(2)              :: TAUS                      ! stem transmittance (1 = vis, 2 = NIR)
    integer                         :: ISURBAN                   ! vegtype code for urban land cover
    integer                         :: ISWATER                   ! vegtype code for water
    integer                         :: ISBARREN                  ! vegtype code for barren land cover
    integer                         :: ISICE                     ! vegtype code for ice/snow land cover
    integer                         :: ISCROP                    ! vegtype code for crop land cover
    integer                         :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
    integer                         :: NATURAL                   ! vegtype code for cropland/grassland mosaic
    integer                         :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
    integer                         :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
    integer                         :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial
    real                            :: SB                        ! Stefan-Boltzmann constant (w/m2/k4)
    real                            :: VKC                       ! von Karman constant
    real                            :: TFRZ                      ! freezing/melting point (k)
    real                            :: HSUB                      ! latent heat of sublimation (j/kg)
    real                            :: HVAP                      ! latent heat of vaporization (j/kg)
    real                            :: HFUS                      ! latent heat of fusion (j/kg)
    real                            :: CWAT                      ! specific heat capacity of water (j/m3/k)
    real                            :: CICE                      ! specific heat capacity of ice (j/m3/k)
    real                            :: CPAIR                     ! heat capacity dry air at const pres (j/kg/k)
    real                            :: TKWAT                     ! thermal conductivity of water (w/m/k)
    real                            :: TKICE                     ! thermal conductivity of ice (w/m/k)
    real                            :: TKAIR                     ! thermal conductivity of air (w/m/k) (not used MB: 20140718)
    real                            :: RAIR                      ! gas constant for dry air (j/kg/k)
    real                            :: RW                        ! gas constant for  water vapor (j/kg/k)
    real                            :: DENH2O                    ! density of water (kg/m3)
    real                            :: DENICE                    ! density of ice (kg/m3)
    real                            :: THKW                      ! thermal conductivity of water in soil module (W/m/K)
    real                            :: THKO                      ! thermal conductivity of for other soil components in soil module (W/m/K)
    real                            :: THKQTZ                    ! thermal conductivity of quartz in soil module (W/m/K)
    real                            :: SSI                       ! liquid water holding capacity for snowpack (m3/m3)
    real                            :: MFSNO                     ! fractional snow covered area (FSNO) curve parameter
    real                            :: Z0SNO                     ! snow surface roughness length (m)
    real                            :: SWEMX                     ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
    real                            :: TAU0                      ! tau0 from Yang97 eqn. 10a
    real                            :: GRAIN_GROWTH              ! growth from vapor diffusion Yang97 eqn. 10b
    real                            :: EXTRA_GROWTH              ! extra growth near freezing Yang97 eqn. 10c
    real                            :: DIRT_SOOT                 ! dirt and soot term Yang97 eqn. 10d
    real                            :: BATS_COSZ                 ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real                            :: BATS_VIS_NEW              ! new snow visible albedo
    real                            :: BATS_NIR_NEW              ! new snow NIR albedo
    real                            :: BATS_VIS_AGE              ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real                            :: BATS_NIR_AGE              ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real                            :: BATS_VIS_DIR              ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real                            :: BATS_NIR_DIR              ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real                            :: RSURF_SNOW                ! surface resistence for snow [s/m]
    real                            :: RSURF_EXP                 ! exponent in the shape parameter for soil resistance option 1
    real, dimension(2)              :: ALBSAT                    ! saturated soil albedo (1=vis, 2=nir)
    real, dimension(2)              :: ALBDRY                    ! dry soil albedo (1=vis, 2=nir)
    real, dimension(2)              :: ALBICE                    ! Land ice albedo (1=vis, 2=nir)
    real, dimension(2)              :: ALBLAK                    ! Lake ice albedo (1=vis, 2=nir)
    real, dimension(2)              :: OMEGAS                    ! two-stream parameter omega for snow (1=vis, 2=nir)
    real                            :: BETADS                    ! two-stream parameter betad for snow
    real                            :: BETAIS                    ! two-stream parameter betaI for snow
    real, dimension(2)              :: EG                        ! emissivity of land surface (1=soil,2=lake)
    real                            :: WSLMAX                    ! maximum lake water storage (mm)
    real                            :: max_liq_mass_fraction     ! For snow water retention
    real                            :: SNOW_RET_FAC              ! snowpack water release timescale factor (1/s)
    integer                         :: NBAND                     ! Number of shortwave bands (2, visible and NIR)
    real                            :: MPE                       ! MPE is nominally small to prevent dividing by zero error
    real                            :: TOPT                      ! Optimum transpiration air temperature [K]
    real                            :: O2                        ! o2 partial pressure, from MPTABLE.TBL
    real                            :: CO2                       ! co2 partial pressure, from MPTABLE.TBL
    real                            :: PSIWLT                    ! matric potential for wilting point (m)  (orig a fixed param.)
    real                            :: TBOT                      ! bottom condition for soil temp. (k)
    real                            :: GRAV                      ! acceleration due to gravity (m/s2)
    real                            :: rain_snow_thresh          ! user-defined rain-snow temperature threshold (°C)
    real                            :: SCAMAX                    ! maximum fractional snow-covered area
  
    contains
  
      procedure, public  :: Init
      procedure, private :: InitAllocate
      procedure, private :: InitDefault
      procedure, private :: InitTransfer
      procedure, public  :: TransferIn
      procedure, public  :: TransferOut
  
  end type parameters_type
  
  contains
  
    subroutine Init(this, namelist, parametersgrid)
  
      implicit none
      class(parameters_type),    intent(inout) :: this
      type(namelist_type),       intent(in)    :: namelist
      type(parametersgrid_type), intent(in)    :: parametersgrid
  
      call this%InitAllocate(namelist)
      call this%InitDefault()
      call this%InitTransfer(parametersgrid)
  
    end subroutine Init
  
    subroutine InitAllocate(this, namelist)
  
      implicit none
      class(parameters_type), intent(inout) :: this
      type(namelist_type),    intent(in)    :: namelist
  
      associate(nsoil => namelist%nsoil)

      if(.NOT.allocated(this%bexp))   allocate(this%bexp   (nsoil))
      if(.NOT.allocated(this%smcmax)) allocate(this%smcmax (nsoil))
      if(.NOT.allocated(this%smcwlt)) allocate(this%smcwlt (nsoil))
      if(.NOT.allocated(this%smcref)) allocate(this%smcref (nsoil))
      if(.NOT.allocated(this%dksat))  allocate(this%dksat  (nsoil))
      if(.NOT.allocated(this%dwsat))  allocate(this%dwsat  (nsoil))
      if(.NOT.allocated(this%psisat)) allocate(this%psisat (nsoil))
      
      end associate

    end subroutine InitAllocate
  
    subroutine InitDefault(this)
  
      implicit none
      class(parameters_type) :: this
  
      this%LAI        = huge(1.0)
      this%SAI        = huge(1.0)
      this%ELAI       = huge(1.0)
      this%ESAI       = huge(1.0)
      this%VAI        = huge(1.0)
      this%VEG        = .true.
      this%FVEG       = huge(1.0)
      this%bexp(:)    = huge(1.0)
      this%smcmax(:)  = huge(1.0)
      this%smcwlt(:)  = huge(1.0)
      this%smcref(:)  = huge(1.0)
      this%dksat(:)   = huge(1.0)
      this%dwsat(:)   = huge(1.0)
      this%psisat(:)  = huge(1.0)
      this%SCAMAX     = huge(1.0)
  
    end subroutine InitDefault
  
    subroutine InitTransfer(this,parametersgrid)
  
      implicit none
      class(parameters_type),    intent(inout) :: this
      type(parametersgrid_type), intent(in)    :: parametersgrid
  
      this%ISURBAN = parametersgrid%ISURBAN
      this%ISWATER = parametersgrid%ISWATER
      this%ISBARREN = parametersgrid%ISBARREN
      this%ISICE = parametersgrid%ISICE
      this%ISCROP = parametersgrid%ISCROP
      this%EBLFOREST = parametersgrid%EBLFOREST
      this%NATURAL = parametersgrid%NATURAL
      this%LOW_DENSITY_RESIDENTIAL = parametersgrid%LOW_DENSITY_RESIDENTIAL
      this%HIGH_DENSITY_RESIDENTIAL = parametersgrid%HIGH_DENSITY_RESIDENTIAL
      this%HIGH_INTENSITY_INDUSTRIAL = parametersgrid%HIGH_INTENSITY_INDUSTRIAL
      this%SB = parametersgrid%SB
      this%VKC = parametersgrid%VKC
      this%TFRZ = parametersgrid%TFRZ 
      this%HSUB = parametersgrid%HSUB 
      this%HVAP = parametersgrid%HVAP 
      this%HFUS = parametersgrid%HFUS 
      this%CWAT = parametersgrid%CWAT 
      this%CICE = parametersgrid%CICE 
      this%CPAIR = parametersgrid%CPAIR 
      this%TKWAT = parametersgrid%TKWAT 
      this%TKICE = parametersgrid%TKICE 
      this%TKAIR = parametersgrid%TKAIR 
      this%RAIR = parametersgrid%RAIR
      this%RW = parametersgrid%RW 
      this%DENH2O = parametersgrid%DENH2O 
      this%DENICE = parametersgrid%DENICE 
      this%THKW = parametersgrid%THKW 
      this%THKO = parametersgrid%THKO 
      this%THKQTZ = parametersgrid%THKQTZ 
      this%SWEMX = parametersgrid%SWEMX 
      this%TAU0 = parametersgrid%TAU0 
      this%GRAIN_GROWTH = parametersgrid%GRAIN_GROWTH
      this%EXTRA_GROWTH = parametersgrid%EXTRA_GROWTH
      this%DIRT_SOOT = parametersgrid%DIRT_SOOT 
      this%BATS_COSZ = parametersgrid%BATS_COSZ 
      this%BATS_VIS_NEW = parametersgrid%BATS_VIS_NEW 
      this%BATS_NIR_NEW = parametersgrid%BATS_NIR_NEW 
      this%BATS_VIS_AGE = parametersgrid%BATS_VIS_AGE 
      this%BATS_NIR_AGE = parametersgrid%BATS_NIR_AGE 
      this%BATS_VIS_DIR = parametersgrid%BATS_VIS_DIR
      this%BATS_NIR_DIR = parametersgrid%BATS_NIR_DIR
      this%BETADS = parametersgrid%BETADS
      this%BETAIS = parametersgrid%BETAIS
      this%NBAND = parametersgrid%NBAND
      this%MPE = parametersgrid%MPE
      this%O2 = parametersgrid%O2
      this%CO2 = parametersgrid%CO2
      this%GRAV = parametersgrid%GRAV

    end subroutine InitTransfer

    subroutine TransferIn(this, parametersgrid, ix, iy)

      implicit none
  
      class(parameters_type),     intent(inout) :: this
      type(parametersgrid_type),  intent(in)    :: parametersgrid
      integer,                    intent(in)    :: ix
      integer,                    intent(in)    :: iy
  
      this%bexp(:) = parametersgrid%bexp(ix,iy,:)
      this%smcmax(:) = parametersgrid%smcmax(ix,iy,:)
      this%smcwlt(:) = parametersgrid%smcwlt(ix,iy,:)
      this%smcref(:) = parametersgrid%smcref(ix,iy,:)
      this%dksat(:) = parametersgrid%dksat(ix,iy,:)
      this%dwsat(:) = parametersgrid%dwsat(ix,iy,:)
      this%psisat(:) = parametersgrid%psisat(ix,iy,:)
      this%bvic = parametersgrid%bvic(ix,iy)
      this%AXAJ = parametersgrid%AXAJ(ix,iy)
      this%BXAJ = parametersgrid%BXAJ(ix,iy)
      this%XXAJ = parametersgrid%XXAJ(ix,iy)
      this%BBVIC = parametersgrid%BBVIC(ix,iy)
      this%G = parametersgrid%G(ix,iy)
      this%QUARTZ = parametersgrid%QUARTZ(ix,iy)
      this%kdt = parametersgrid%kdt(ix,iy)
      this%refkdt = parametersgrid%refkdt(ix,iy)
      this%refdk = parametersgrid%refdk(ix,iy)
      this%csoil = parametersgrid%csoil(ix,iy)
      this%Z0 = parametersgrid%Z0(ix,iy)
      this%CZIL = parametersgrid%CZIL(ix,iy)
      this%ZBOT = parametersgrid%ZBOT(ix,iy)
      this%frzx = parametersgrid%frzx(ix,iy)
      this%slope = parametersgrid%slope(ix,iy)
      this%timean = parametersgrid%timean(ix,iy)
      this%fsatmx = parametersgrid%fsatmx(ix,iy)
      this%ZWT_INIT = parametersgrid%ZWT_INIT(ix,iy)
      this%urban_flag = parametersgrid%urban_flag(ix,iy)
      this%LAIM(:) = parametersgrid%LAIM(ix,iy,:)
      this%SAIM(:) = parametersgrid%SAIM(ix,iy,:)
      this%LAI = parametersgrid%LAI(ix,iy)
      this%SAI = parametersgrid%SAI(ix,iy)
      this%CH2OP = parametersgrid%CH2OP(ix,iy)
      this%NROOT = parametersgrid%NROOT(ix,iy)
      this%HVT = parametersgrid%HVT(ix,iy)
      this%HVB = parametersgrid%HVB(ix,iy)
      this%TMIN = parametersgrid%TMIN(ix,iy)
      this%SHDFAC = parametersgrid%SHDFAC(ix,iy)
      this%SHDMAX = parametersgrid%SHDMAX(ix,iy)
      this%Z0MVT = parametersgrid%Z0MVT(ix,iy)
      this%RC = parametersgrid%RC(ix,iy)
      this%XL = parametersgrid%XL(ix,iy)
      this%BP = parametersgrid%BP(ix,iy)
      this%FOLNMX = parametersgrid%FOLNMX(ix,iy)
      this%QE25 = parametersgrid%QE25(ix,iy)
      this%VCMX25 = parametersgrid%VCMX25(ix,iy)
      this%MP = parametersgrid%MP(ix,iy)
      this%RGL = parametersgrid%RGL(ix,iy)
      this%RSMIN = parametersgrid%RSMIN(ix,iy)
      this%HS = parametersgrid%HS(ix,iy)
      this%AKC = parametersgrid%AKC(ix,iy)
      this%AKO = parametersgrid%AKO(ix,iy)
      this%AVCMX = parametersgrid%AVCMX(ix,iy)
      this%RSMAX = parametersgrid%RSMAX(ix,iy)
      this%CWP = parametersgrid%CWP(ix,iy)
      this%C3PSN = parametersgrid%C3PSN(ix,iy)
      this%DLEAF = parametersgrid%DLEAF(ix,iy)
      this%KC25 = parametersgrid%KC25(ix,iy)
      this%KO25 = parametersgrid%KO25(ix,iy)
      this%ELAI = parametersgrid%ELAI(ix,iy)
      this%ESAI = parametersgrid%ESAI(ix,iy)
      this%VAI = parametersgrid%VAI(ix,iy)
      this%VEG = parametersgrid%VEG(ix,iy)
      this%FVEG = parametersgrid%FVEG(ix,iy)
      this%RHOL(:) = parametersgrid%RHOL(ix,iy,:)
      this%RHOS(:) = parametersgrid%RHOS(ix,iy,:)
      this%TAUL(:) = parametersgrid%TAUL(ix,iy,:)
      this%TAUS(:) = parametersgrid%TAUS(ix,iy,:)
      this%SSI = parametersgrid%SSI(ix,iy)
      this%MFSNO = parametersgrid%MFSNO(ix,iy)
      this%Z0SNO = parametersgrid%Z0SNO(ix,iy)
      this%RSURF_SNOW = parametersgrid%RSURF_SNOW(ix,iy)
      this%RSURF_EXP = parametersgrid%RSURF_EXP(ix,iy)
      this%ALBSAT(:) = parametersgrid%ALBSAT(ix,iy,:)
      this%ALBDRY(:) = parametersgrid%ALBDRY(ix,iy,:)
      this%ALBICE(:) = parametersgrid%ALBICE(:)
      this%ALBLAK(:) = parametersgrid%ALBLAK(:)
      this%OMEGAS(:) = parametersgrid%OMEGAS(:)
      this%EG(:) = parametersgrid%EG(ix,iy,:)
      this%WSLMAX = parametersgrid%WSLMAX(ix,iy)
      this%max_liq_mass_fraction = parametersgrid%max_liq_mass_fraction(ix,iy)
      this%SNOW_RET_FAC = parametersgrid%SNOW_RET_FAC(ix,iy)
      this%TOPT = parametersgrid%TOPT(ix,iy)
      this%PSIWLT = parametersgrid%PSIWLT(ix,iy)
      this%TBOT = parametersgrid%TBOT(ix,iy)
      this%rain_snow_thresh = parametersgrid%rain_snow_thresh(ix,iy)
      this%SCAMAX = parametersgrid%SCAMAX(ix,iy)
  
    end subroutine TransferIn
  
    subroutine TransferOut(this, parametersgrid, ix, iy)
  
      implicit none
  
      class(parameters_type),     intent(in)    :: this
      type(parametersgrid_type),  intent(inout) :: parametersgrid
      integer,                    intent(in)    :: ix
      integer,                    intent(in)    :: iy
  
      parametersgrid%LAI(ix,iy) = this%LAI
      parametersgrid%SAI(ix,iy) = this%SAI
      parametersgrid%ELAI(ix,iy) = this%ELAI
      parametersgrid%ESAI(ix,iy) = this%ESAI
      parametersgrid%VAI(ix,iy) = this%VAI
      parametersgrid%VEG(ix,iy) = this%VEG
      parametersgrid%FVEG(ix,iy) = this%FVEG
  
    end subroutine TransferOut

  end module ParametersType
  
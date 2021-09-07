module ParametersType

use modelConfigRead, only: modelConfig_type
use parametersRead,  only: parametersNML

implicit none
save
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

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type parameters_type

contains   

  subroutine Init(this, modelConfig)

    implicit none
    class(parameters_type)              :: this
    class(modelConfig_type), intent(in) :: modelConfig

    call this%InitAllocate(modelConfig)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, modelConfig)

    implicit none
    class(parameters_type)               :: this
    class(modelConfig_type), intent(in)  :: modelConfig

    allocate(this%bexp   (modelConfig%nsoil))  ; this%bexp   (:) = huge(1.0)
    allocate(this%smcmax (modelConfig%nsoil))  ; this%smcmax (:) = huge(1.0)
    allocate(this%smcwlt (modelConfig%nsoil))  ; this%smcwlt (:) = huge(1.0)
    allocate(this%smcref (modelConfig%nsoil))  ; this%smcref (:) = huge(1.0)
    allocate(this%dksat  (modelConfig%nsoil))  ; this%dksat  (:) = huge(1.0)
    allocate(this%dwsat  (modelConfig%nsoil))  ; this%dwsat  (:) = huge(1.0)
    allocate(this%psisat (modelConfig%nsoil))  ; this%psisat (:) = huge(1.0)

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

  end subroutine InitDefault

  subroutine InitTransfer(this, modelConfig, paramNML)

    ! transfer pameters read from parameter namelist to parameter datatype

    implicit none
    class(parameters_type)               :: this
    class(modelConfig_type),  intent(in) :: modelConfig
    class(parametersNML),     intent(in) :: paramNML

    this%bexp   = paramNML%bb        (modelConfig%isltyp)
    this%smcmax = paramNML%maxsmc    (modelConfig%isltyp)
    this%smcwlt = paramNML%wltsmc    (modelConfig%isltyp)
    this%smcref = paramNML%refsmc    (modelConfig%isltyp)
    this%dksat  = paramNML%satdk     (modelConfig%isltyp)
    this%dwsat  = paramNML%satdw     (modelConfig%isltyp)
    this%psisat = paramNML%satpsi    (modelConfig%isltyp)
    this%bvic   = paramNML%bvic      (modelConfig%isltyp)
    this%AXAJ   = paramNML%AXAJ      (modelConfig%isltyp)
    this%BXAJ   = paramNML%BXAJ      (modelConfig%isltyp)
    this%XXAJ   = paramNML%XXAJ      (modelConfig%isltyp)
    this%BBVIC  = paramNML%BBVIC     (modelConfig%isltyp)
    this%G      = paramNML%G         (modelConfig%isltyp)
    this%QUARTZ = paramNML%QUARTZ    (modelConfig%isltyp)
    this%LAIM   = paramNML%LAIM_TABLE(modelConfig%vegtyp, :)
    this%SAIM   = paramNML%SAIM_TABLE(modelConfig%vegtyp, :)
    this%CH2OP  = paramNML%CH2OP     (modelConfig%vegtyp)
    this%NROOT  = paramNML%NROOT     (modelConfig%vegtyp)
    this%HVT    = paramNML%HVT       (modelConfig%vegtyp)
    this%HVB    = paramNML%HVB       (modelConfig%vegtyp)
    this%TMIN   = paramNML%TMIN      (modelConfig%vegtyp)
    this%SHDFAC = paramNML%SHDFAC    (modelConfig%vegtyp)
    this%SHDMAX = paramNML%SHDFAC    (modelConfig%vegtyp)
    this%Z0MVT  = paramNML%Z0MVT     (modelConfig%vegtyp)
    this%RC     = paramNML%RC        (modelConfig%vegtyp)
    this%XL     = paramNML%XL        (modelConfig%vegtyp)
    this%BP     = paramNML%BP        (modelConfig%vegtyp)
    this%FOLNMX = paramNML%FOLNMX    (modelConfig%vegtyp)
    this%QE25   = paramNML%QE25      (modelConfig%vegtyp)
    this%VCMX25 = paramNML%VCMX25    (modelConfig%vegtyp)
    this%MP     = paramNML%MP        (modelConfig%vegtyp)
    this%RGL    = paramNML%RGL       (modelConfig%vegtyp)
    this%RSMIN  = paramNML%RSMIN     (modelConfig%vegtyp)
    this%HS     = paramNML%HS        (modelConfig%vegtyp)
    this%AKC    = paramNML%AKC       (modelConfig%vegtyp)
    this%AKO    = paramNML%AKO       (modelConfig%vegtyp)
    this%AVCMX  = paramNML%AVCMX     (modelConfig%vegtyp)
    this%RSMAX  = paramNML%RSMAX     (modelConfig%vegtyp)
    this%CWP    = paramNML%CWP
    this%c3psn  = paramNML%c3psn
    this%DLEAF  = paramNML%DLEAF
    this%KC25   = paramNML%KC25
    this%KO25   = paramNML%KO25
    this%RHOL   = paramNML%RHOL_TABLE(modelConfig%vegtyp, :)
    this%RHOS   = paramNML%RHOS_TABLE(modelConfig%vegtyp, :)
    this%TAUL   = paramNML%TAUL_TABLE(modelConfig%vegtyp, :)
    this%TAUS   = paramNML%TAUS_TABLE(modelConfig%vegtyp, :)
    this%refkdt = paramNML%refkdt
    this%refdk  = paramNML%refdk
    this%kdt    = this%refkdt * this%dksat(1) / this%refdk
    this%csoil  = paramNML%csoil
    this%Z0     = paramNML%Z0     ! orig = 0.002 in energy() as a fixed parameter
    this%CZIL   = paramNML%CZIL
    this%ZBOT   = paramNML%ZBOT
    this%frzx   = 0.15 * (this%smcmax(1) / this%smcref(1)) * (0.412 / 0.468)
    this%SSI    = paramNML%SSI
    this%MFSNO  = paramNML%MFSNO
    this%Z0SNO  = paramNML%Z0SNO
    this%SWEMX        = paramNML%SWEMX
    this%TAU0         = paramNML%TAU0
    this%GRAIN_GROWTH = paramNML%GRAIN_GROWTH
    this%EXTRA_GROWTH = paramNML%EXTRA_GROWTH
    this%DIRT_SOOT    = paramNML%DIRT_SOOT
    this%BATS_COSZ    = paramNML%BATS_COSZ
    this%BATS_VIS_NEW = paramNML%BATS_VIS_NEW
    this%BATS_NIR_NEW = paramNML%BATS_NIR_NEW
    this%BATS_VIS_AGE = paramNML%BATS_VIS_AGE
    this%BATS_NIR_AGE = paramNML%BATS_NIR_AGE
    this%BATS_VIS_DIR = paramNML%BATS_VIS_DIR
    this%BATS_NIR_DIR = paramNML%BATS_NIR_DIR
    this%RSURF_SNOW   = paramNML%RSURF_SNOW
    this%RSURF_EXP    = paramNML%RSURF_EXP
    this%ALBSAT       = paramNML%ALBSAT_TABLE  (modelConfig%soilcolor, :)
    this%ALBDRY       = paramNML%ALBDRY_TABLE  (modelConfig%soilcolor, :)
    this%ALBICE       = paramNML%ALBICE
    this%ALBLAK       = paramNML%ALBLAK
    this%OMEGAS       = paramNML%OMEGAS
    this%BETADS       = paramNML%BETADS
    this%BETAIS       = paramNML%BETAIS
    this%EG           = paramNML%EG
    this%slope        = paramNML%slope
    this%ISURBAN                   = paramNML%ISURBAN
    this%ISWATER                   = paramNML%ISWATER
    this%ISBARREN                  = paramNML%ISBARREN
    this%ISICE                     = paramNML%ISICE
    this%ISCROP                    = paramNML%ISCROP
    this%EBLFOREST                 = paramNML%EBLFOREST
    this%NATURAL                   = paramNML%NATURAL
    this%LOW_DENSITY_RESIDENTIAL   = paramNML%LOW_DENSITY_RESIDENTIAL
    this%HIGH_DENSITY_RESIDENTIAL  = paramNML%HIGH_DENSITY_RESIDENTIAL
    this%HIGH_INTENSITY_INDUSTRIAL = paramNML%HIGH_INTENSITY_INDUSTRIAL
    this%urban_flag = .false.
    this%timean     = 10.5
    this%fsatmx     = 0.38 
    this%GRAV       = 9.80616
    this%SB         = 5.67E-08
    this%VKC        = 0.40 
    this%TFRZ       = 273.16 
    this%HSUB       = 2.8440E06
    this%HVAP       = 2.5104E06
    this%HFUS       = 0.3336E06
    this%CWAT       = 4.188E06
    this%CICE       = 2.094E06
    this%CPAIR      = 1004.64
    this%TKWAT      = 0.6
    this%TKICE      = 2.2
    this%TKAIR      = 0.023
    this%RAIR       = 287.04
    this%RW         = 461.269 
    this%DENH2O     = 1000.0
    this%DENICE     = 917.0
    this%THKW       = 0.57
    this%THKO       = 2.0
    this%THKQTZ     = 7.7
    this%WSLMAX     = 5000.0 
    this%max_liq_mass_fraction = 0.4
    this%SNOW_RET_FAC = 5.e-5
    this%NBAND        = 2  ! do not change 
    this%MPE          = 1.E-06  ! do not change ! need to make this a parameter
    this%TOPT         = 1.E-06  ! Optimum transpiration air temperature [K]   
    
    this%CO2       =  395.e-06   ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    this%O2        =  0.209      ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    this%PSIWLT    = -150.0      ! originally a fixed parameter set in ENERGY()
    this%TBOT      = 263.0       ! (K) can be updated depending on option OPT_TBOT

  end subroutine InitTransfer

end module ParametersType

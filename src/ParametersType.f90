module ParametersType

use NoahowpGridTypeModule

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

  contains

  procedure, public  :: Init
  procedure, private :: InitAllocate
  procedure, private :: InitDefault

end type parameters_type

contains

  subroutine Init(this, noahowpgrid)

    implicit none
    class(parameters_type)                :: this
    type(noahowpgrid_type), intent(in)    :: noahowpgrid

    call this%InitAllocate(noahowpgrid)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, noahowpgrid)

    implicit none
    class(parameters_type)                :: this
    type(noahowpgrid_type), intent(in)    :: noahowpgrid

    allocate(this%bexp   (noahowpgrid%nsoil))
    allocate(this%smcmax (noahowpgrid%nsoil)) 
    allocate(this%smcwlt (noahowpgrid%nsoil))
    allocate(this%smcref (noahowpgrid%nsoil)) 
    allocate(this%dksat  (noahowpgrid%nsoil)) 
    allocate(this%dwsat  (noahowpgrid%nsoil))  
    allocate(this%psisat (noahowpgrid%nsoil))

  end subroutine InitAllocate

  subroutine InitDefault(this)

    implicit none
    class(parameters_type) :: this

    this%bexp(:)                   = huge(1.0)
    this%smcmax(:)                 = huge(1.0)
    this%smcwlt(:)                 = huge(1.0)
    this%smcref(:)                 = huge(1.0)
    this%dksat(:)                  = huge(1.0)
    this%dwsat(:)                  = huge(1.0)
    this%psisat(:)                 = huge(1.0)
    this%bvic                      = huge(1.0)
    this%AXAJ                      = huge(1.0)
    this%BXAJ                      = huge(1.0)
    this%XXAJ                      = huge(1.0)
    this%BBVIC                     = huge(1.0)
    this%G                         = huge(1.0)
    this%QUARTZ                    = huge(1.0)
    this%kdt                       = huge(1.0)
    this%refkdt                    = huge(1.0)
    this%refdk                     = huge(1.0)
    this%csoil                     = huge(1.0)
    this%Z0                        = huge(1.0)
    this%CZIL                      = huge(1.0)
    this%ZBOT                      = huge(1.0)
    this%frzx                      = huge(1.0)
    this%slope                     = huge(1.0)
    this%timean                    = huge(1.0)
    this%fsatmx                    = huge(1.0)
    this%ZWT_INIT                  = huge(1.0)
    this%urban_flag                = .FALSE.
    this%LAIM(:)                   = huge(1.0)
    this%SAIM(:)                   = huge(1.0)
    this%LAI                       = huge(1.0)
    this%SAI                       = huge(1.0)
    this%CH2OP                     = huge(1.0)
    this%NROOT                     = huge(1)
    this%HVT                       = huge(1.0)
    this%HVB                       = huge(1.0)
    this%TMIN                      = huge(1.0)
    this%SHDFAC                    = huge(1.0)
    this%SHDMAX                    = huge(1.0)
    this%Z0MVT                     = huge(1.0)
    this%RC                        = huge(1.0)
    this%XL                        = huge(1.0)
    this%BP                        = huge(1.0)
    this%FOLNMX                    = huge(1.0)
    this%QE25                      = huge(1.0)
    this%VCMX25                    = huge(1.0)
    this%MP                        = huge(1.0)
    this%RGL                       = huge(1.0)
    this%RSMIN                     = huge(1.0)
    this%HS                        = huge(1.0)
    this%AKC                       = huge(1.0)
    this%AKO                       = huge(1.0)
    this%AVCMX                     = huge(1.0)
    this%RSMAX                     = huge(1.0)
    this%CWP                       = huge(1.0)
    this%C3PSN                     = huge(1.0)
    this%DLEAF                     = huge(1.0)
    this%KC25                      = huge(1.0)
    this%KO25                      = huge(1.0)
    this%ELAI                      = huge(1.0)
    this%ESAI                      = huge(1.0)
    this%VAI                       = huge(1.0)
    this%VEG                       = .FALSE.
    this%FVEG                      = huge(1.0)
    this%RHOL(:)                   = huge(1.0)
    this%RHOS(:)                   = huge(1.0)
    this%TAUL(:)                   = huge(1.0)
    this%TAUS(:)                   = huge(1.0)
    this%ISURBAN                   = huge(1)
    this%ISWATER                   = huge(1)
    this%ISBARREN                  = huge(1)
    this%ISICE                     = huge(1)
    this%ISCROP                    = huge(1)
    this%EBLFOREST                 = huge(1)
    this%NATURAL                   = huge(1)
    this%LOW_DENSITY_RESIDENTIAL   = huge(1)
    this%HIGH_DENSITY_RESIDENTIAL  = huge(1)
    this%HIGH_INTENSITY_INDUSTRIAL = huge(1)
    this%SB                        = huge(1.0)
    this%VKC                       = huge(1.0)
    this%TFRZ                      = huge(1.0)
    this%HSUB                      = huge(1.0)
    this%HVAP                      = huge(1.0)
    this%HFUS                      = huge(1.0)
    this%CWAT                      = huge(1.0)
    this%CICE                      = huge(1.0)
    this%CPAIR                     = huge(1.0)
    this%TKWAT                     = huge(1.0)
    this%TKICE                     = huge(1.0)
    this%TKAIR                     = huge(1.0)
    this%RAIR                      = huge(1.0)
    this%RW                        = huge(1.0)
    this%DENH2O                    = huge(1.0)
    this%DENICE                    = huge(1.0)
    this%THKW                      = huge(1.0)
    this%THKO                      = huge(1.0)
    this%THKQTZ                    = huge(1.0)
    this%SSI                       = huge(1.0)
    this%MFSNO                     = huge(1.0)
    this%Z0SNO                     = huge(1.0)
    this%SWEMX                     = huge(1.0)
    this%TAU0                      = huge(1.0)
    this%GRAIN_GROWTH              = huge(1.0)
    this%EXTRA_GROWTH              = huge(1.0)
    this%DIRT_SOOT                 = huge(1.0)
    this%BATS_COSZ                 = huge(1.0)
    this%BATS_VIS_NEW              = huge(1.0)
    this%BATS_NIR_NEW              = huge(1.0)
    this%BATS_VIS_AGE              = huge(1.0)
    this%BATS_NIR_AGE              = huge(1.0)
    this%BATS_VIS_DIR              = huge(1.0)
    this%BATS_NIR_DIR              = huge(1.0)
    this%RSURF_SNOW                = huge(1.0)
    this%RSURF_EXP                 = huge(1.0)
    this%ALBSAT(:)                 = huge(1.0)
    this%ALBDRY(:)                 = huge(1.0)
    this%ALBICE(:)                 = huge(1.0)
    this%ALBLAK(:)                 = huge(1.0)
    this%OMEGAS(:)                 = huge(1.0)
    this%BETADS                    = huge(1.0)
    this%BETAIS                    = huge(1.0)
    this%EG(:)                     = huge(1.0)
    this%WSLMAX                    = huge(1.0)
    this%max_liq_mass_fraction     = huge(1.0)
    this%SNOW_RET_FAC              = huge(1.0)
    this%NBAND                     = huge(1)
    this%MPE                       = huge(1.0)
    this%TOPT                      = huge(1.0)
    this%O2                        = huge(1.0)
    this%CO2                       = huge(1.0)
    this%PSIWLT                    = huge(1.0)
    this%TBOT                      = huge(1.0)
    this%GRAV                      = huge(1.0)
    this%rain_snow_thresh          = huge(1.0)

  end subroutine InitDefault

end module ParametersType

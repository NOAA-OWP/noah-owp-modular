module ParametersType

use NamelistRead, only: namelist_type

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
    procedure, public  :: paramRead

end type parameters_type

contains

  subroutine Init(this, namelist)

    implicit none
    class(parameters_type)           :: this
    class(namelist_type), intent(in) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    implicit none
    class(parameters_type)            :: this
    class(namelist_type), intent(in)  :: namelist

    allocate(this%bexp   (namelist%nsoil))  ; this%bexp   (:) = huge(1.0)
    allocate(this%smcmax (namelist%nsoil))  ; this%smcmax (:) = huge(1.0)
    allocate(this%smcwlt (namelist%nsoil))  ; this%smcwlt (:) = huge(1.0)
    allocate(this%smcref (namelist%nsoil))  ; this%smcref (:) = huge(1.0)
    allocate(this%dksat  (namelist%nsoil))  ; this%dksat  (:) = huge(1.0)
    allocate(this%dwsat  (namelist%nsoil))  ; this%dwsat  (:) = huge(1.0)
    allocate(this%psisat (namelist%nsoil))  ; this%psisat (:) = huge(1.0)

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

  subroutine paramRead(this, param_file, namelist)

    class(parameters_type)           :: this
    character(len=256),   intent(in) :: param_file
    class(namelist_type), intent(in) :: namelist

    !--------------------!
    !  soil parameters   !
    !--------------------!
    real, dimension(12)     ::      bb  ! b parameter
    real, dimension(12)     ::   satdk  ! conductivity at saturation
    real, dimension(12)     ::   satdw  ! diffusivity at saturation
    real, dimension(12)     ::  maxsmc  ! porosity
    real, dimension(12)     ::  satpsi  ! matric potential at saturation
    real, dimension(12)     ::  wltsmc  ! wilting point
    real, dimension(12)     ::  refsmc  ! field capacity
    real, dimension(12)     :: pctsand  ! percent sand
    real, dimension(12)     :: pctclay  ! percent clay
    real, dimension(12)     ::   bvic   ! VIC or DVIC model infiltration parameter
    real, dimension(12)     ::   AXAJ   ! Xinanjiang: Tension water distribution inflection parameter [-]
    real, dimension(12)     ::   BXAJ   ! Xinanjiang: Tension water distribution shape parameter [-]
    real, dimension(12)     ::   XXAJ   ! Xinanjiang: Free water distribution shape parameter [-]
    real, dimension(12)     ::   G      ! Mean Capillary Drive (m) for infiltration models
    real, dimension(12)     ::   BBVIC  ! DVIC heterogeniety parameter for infiltration
    real, dimension(12)     ::   QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
    real                    ::   slope  ! free drainage parameter
    real                    ::  refkdt  ! infiltration parameter for Schaake scheme
    real                    ::   refdk  ! reference diffusivity for Schaake scheme
    real                    ::   csoil  ! volumetric soil heat capacity [j/m3/K]
    real                    ::   Z0     ! bare soil roughness length (m)
    real                    ::   CZIL   ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
    real                    ::   ZBOT   ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL

    !--------------------------!
    !  Vegetation parameters   !
    !--------------------------!
    real, dimension(20)     ::   LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                 LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(20)     ::   SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                 SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(20)     ::   CH2OP
    real, dimension(20)     ::   NROOT
    real, dimension(20)     ::   HVT            ! canopy top height (m)
    real, dimension(20)     ::   HVB            ! canopy bottom height (m)
    real, dimension(20)     ::   TMIN           ! minimum temperature for photosynthesis (k)
    real, dimension(20)     ::   SHDFAC         ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   SHDMAX         ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   Z0MVT          ! momentum roughness length (m)
    real, dimension(20)     ::   RC             ! tree crown radius (m)
    real, dimension(20)     ::   XL             ! leaf/stem orientation index
    real, dimension(20)     ::   BP             ! minimum leaf conductance (umol/m**2/s)
    real, dimension(20)     ::   FOLNMX         ! foliage nitrogen concentration when f(n)=1 (%)
    real, dimension(20)     ::   QE25           ! quantum efficiency at 25c (umol co2 / umol photon)
    real, dimension(20)     ::   VCMX25         ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real, dimension(20)     ::   MP             ! slope of conductance-to-photosynthesis relationship
    real, dimension(20)     ::   RGL            ! Parameter used in radiation stress function
    real, dimension(20)     ::   RSMIN          ! Minimum stomatal resistance [s m-1]
    real, dimension(20)     ::   HS             ! Parameter used in vapor pressure deficit function
    real, dimension(20)     ::   AKC            ! q10 for kc25
    real, dimension(20)     ::   AKO            ! q10 for ko25
    real, dimension(20)     ::   AVCMX          ! q10 for vcmx25
    real, dimension(20)     ::   RSMAX          ! Maximal stomatal resistance [s m-1]
    real                    ::   CWP            ! canopy wind absorption coefficient (formerly CWPVT)
    real                    ::   C3PSN          ! photosynth. pathway: 0. = c4, 1. = c3
    real                    ::   DLEAF          ! characteristic leaf dimension (m)
    real                    ::   KC25           ! co2 michaelis-menten constant at 25c (pa)
    real                    ::   KO25           ! o2 michaelis-menten constant at 25c (pa)
    real, dimension(20)     ::   RHOL_VIS       ! leaf reflectance in visible
    real, dimension(20)     ::   RHOL_NIR       ! leaf reflectance in near infrared
    real, dimension(20)     ::   RHOS_VIS       ! stem reflectance in visible
    real, dimension(20)     ::   RHOS_NIR       ! stem reflectance in near infrared
    real, dimension(20)     ::   TAUL_VIS       ! leaf transmittance in visible
    real, dimension(20)     ::   TAUL_NIR       ! leaf transmittance in near infrared
    real, dimension(20)     ::   TAUS_VIS       ! stem transmittance in visible
    real, dimension(20)     ::   TAUS_NIR       ! stem transmittance in near infrared
    real, dimension(20,2)   ::   RHOL_TABLE     ! leaf reflectance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   RHOS_TABLE     ! stem reflectance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   TAUL_TABLE     ! leaf transmittance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   TAUS_TABLE     ! stem transmittance table (1 = vis, 2 = NIR)
    real, dimension(20,12)  ::   LAIM_TABLE     ! monthly leaf area index, one-sided
    real, dimension(20,12)  ::   SAIM_TABLE     ! monthly stem area index, one-sided

    !--------------------!
    !  snow parameters   !
    !--------------------!
    real                    ::   SSI            ! liquid water holding capacity of snowpack (m3/m3)
    real                    ::   MFSNO          ! fractional snow covered area (FSNO) curve parameter
    real                    ::   Z0SNO          ! snow surface roughness length (m)
    real                    ::   SWEMX          ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
    real                    ::   TAU0           ! tau0 from Yang97 eqn. 10a
    real                    ::   GRAIN_GROWTH   ! growth from vapor diffusion Yang97 eqn. 10b
    real                    ::   EXTRA_GROWTH   ! extra growth near freezing Yang97 eqn. 10c
    real                    ::   DIRT_SOOT      ! dirt and soot term Yang97 eqn. 10d
    real                    ::   BATS_COSZ      ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real                    ::   BATS_VIS_NEW   ! new snow visible albedo
    real                    ::   BATS_NIR_NEW   ! new snow NIR albedo
    real                    ::   BATS_VIS_AGE   ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real                    ::   BATS_NIR_AGE   ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real                    ::   BATS_VIS_DIR   ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real                    ::   BATS_NIR_DIR   ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real                    ::   RSURF_SNOW     ! surface resistence for snow [s/m]
    real                    ::   RSURF_EXP      ! exponent in the shape parameter for soil resistance option 1

    !-----------------------!
    ! radiation parameters  !
    !-----------------------!
    real, dimension(8)      ::   ALBSAT_VIS    ! saturated soil VIS albedo per SOILCOLOR
    real, dimension(8)      ::   ALBSAT_NIR    ! saturated soil NIR albedo per SOILCOLOR
    real, dimension(8)      ::   ALBDRY_VIS    ! dry soil VIS albedo per SOILCOLOR
    real, dimension(8)      ::   ALBDRY_NIR    ! dry soil NIR albedo per SOILCOLOR
    real, dimension(8,2)    ::   ALBSAT_TABLE  ! saturated soil albedo table per SOILCOLOR (1=vis, 2=nir)
    real, dimension(8,2)    ::   ALBDRY_TABLE  ! dry soil albedo table per SOILCOLOR (1=vis, 2=nir)
    real, dimension(2)      ::   ALBICE        ! Land ice albedo (1=vis, 2=nir)
    real, dimension(2)      ::   ALBLAK        ! Lake ice albedo (1=vis, 2=nir)
    real, dimension(2)      ::   OMEGAS        ! two-stream parameter omega for snow (1=vis, 2=nir)
    real                    ::   BETADS        ! two-stream parameter betad for snow
    real                    ::   BETAIS        ! two-stream parameter betaI for snow
    real, dimension(2)      ::   EG            ! emissivity of land surface (1=soil,2=lake)

    !--------------------!
    !  land parameters   !
    !--------------------!
    integer       :: ISURBAN                   ! vegtype code for urban land cover
    integer       :: ISWATER                   ! vegtype code for water
    integer       :: ISBARREN                  ! vegtype code for barren land cover
    integer       :: ISICE                     ! vegtype code for ice/snow land cover
    integer       :: ISCROP                    ! vegtype code for crop land cover
    integer       :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
    integer       :: NATURAL                   ! vegtype code for cropland/grassland mosaic
    integer       :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
    integer       :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
    integer       :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial

    namelist / soil_parameters / bb,satdk,satdw,maxsmc,satpsi,wltsmc, &
                                 refsmc,pctsand,pctclay,bvic,AXAJ,BXAJ,XXAJ,&
                                 BBVIC,G,QUARTZ,slope,refkdt,refdk,CSOIL,Z0,CZIL,ZBOT

    namelist / snow_parameters / SSI,MFSNO,Z0SNO,SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
                                 BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
                                 RSURF_SNOW,RSURF_EXP

    namelist / veg_parameters  / CH2OP,NROOT,HVT,HVB,TMIN,SHDFAC,SHDMAX,Z0MVT,RC,XL,&
                                 BP,FOLNMX,QE25,VCMX25,MP,RGL,RSMIN,HS,AKC,AKO,AVCMX,RSMAX,&
                                 CWP,C3PSN,DLEAF,KC25,KO25,&
                                 RHOL_VIS,RHOL_NIR,RHOS_VIS,RHOS_NIR,TAUL_VIS,TAUL_NIR,TAUS_VIS,TAUS_NIR,&
                                 LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN,&
                                 LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC,&
                                 SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN,&
                                 SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC

    namelist / radiation_parameters / ALBSAT_VIS,ALBSAT_NIR,ALBDRY_VIS,ALBDRY_NIR,ALBICE,ALBLAK,OMEGAS,BETADS,BETAIS,EG

    namelist / land_parameters / ISURBAN,ISWATER,ISBARREN,ISICE,ISCROP,EBLFOREST,NATURAL,LOW_DENSITY_RESIDENTIAL,&
                                 HIGH_DENSITY_RESIDENTIAL,HIGH_INTENSITY_INDUSTRIAL

!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file=trim(param_file), form="formatted")
      read(30, soil_parameters)
      read(30, snow_parameters)
      read(30, veg_parameters)
      read(30, radiation_parameters)
      read(30, land_parameters)
    close(30)

!---------------------------------------------------------------------
!  transfer to structure
!---------------------------------------------------------------------

    this%bexp    = bb(namelist%isltyp)
    this%smcmax  = maxsmc(namelist%isltyp)
    this%smcwlt  = wltsmc(namelist%isltyp)
    this%smcref  = refsmc(namelist%isltyp)
    this%dksat   = satdk(namelist%isltyp)
    this%dwsat   = satdw(namelist%isltyp)
    this%psisat  = satpsi(namelist%isltyp)
    this%bvic    = bvic(namelist%isltyp)
    this%AXAJ    = AXAJ(namelist%isltyp)
    this%BXAJ    = BXAJ(namelist%isltyp)
    this%XXAJ    = XXAJ(namelist%isltyp)
    this%BBVIC   = BBVIC(namelist%isltyp)
    this%G       = G(namelist%isltyp)
    this%QUARTZ  = QUARTZ(namelist%isltyp)

    this%LAIM( 1) = LAI_JAN(namelist%vegtyp)
    this%LAIM( 2) = LAI_FEB(namelist%vegtyp)
    this%LAIM( 3) = LAI_MAR(namelist%vegtyp)
    this%LAIM( 4) = LAI_APR(namelist%vegtyp)
    this%LAIM( 5) = LAI_MAY(namelist%vegtyp)
    this%LAIM( 6) = LAI_JUN(namelist%vegtyp)
    this%LAIM( 7) = LAI_JUL(namelist%vegtyp)
    this%LAIM( 8) = LAI_AUG(namelist%vegtyp)
    this%LAIM( 9) = LAI_SEP(namelist%vegtyp)
    this%LAIM(10) = LAI_OCT(namelist%vegtyp)
    this%LAIM(11) = LAI_NOV(namelist%vegtyp)
    this%LAIM(12) = LAI_DEC(namelist%vegtyp)

    this%SAIM( 1) = SAI_JAN(namelist%vegtyp)
    this%SAIM( 2) = SAI_FEB(namelist%vegtyp)
    this%SAIM( 3) = SAI_MAR(namelist%vegtyp)
    this%SAIM( 4) = SAI_APR(namelist%vegtyp)
    this%SAIM( 5) = SAI_MAY(namelist%vegtyp)
    this%SAIM( 6) = SAI_JUN(namelist%vegtyp)
    this%SAIM( 7) = SAI_JUL(namelist%vegtyp)
    this%SAIM( 8) = SAI_AUG(namelist%vegtyp)
    this%SAIM( 9) = SAI_SEP(namelist%vegtyp)
    this%SAIM(10) = SAI_OCT(namelist%vegtyp)
    this%SAIM(11) = SAI_NOV(namelist%vegtyp)
    this%SAIM(12) = SAI_DEC(namelist%vegtyp)

    this%CH2OP   = CH2OP(namelist%vegtyp)
    this%NROOT   = NROOT(namelist%vegtyp)
    this%HVT     = HVT(namelist%vegtyp)
    this%HVB     = HVB(namelist%vegtyp)
    this%TMIN    = TMIN(namelist%vegtyp)
    this%SHDFAC  = SHDFAC(namelist%vegtyp)
    this%SHDMAX  = SHDFAC(namelist%vegtyp)  ! this should be SHDMAX(namelist%vegtyp)
    this%Z0MVT   = Z0MVT(namelist%vegtyp)
    this%RC      = RC(namelist%vegtyp)
    this%XL      = XL(namelist%vegtyp)
    this%BP      = BP(namelist%vegtyp)
    this%FOLNMX  = FOLNMX(namelist%vegtyp)
    this%QE25    = QE25(namelist%vegtyp)
    this%VCMX25  = VCMX25(namelist%vegtyp)
    this%MP      = MP(namelist%vegtyp)
    this%RGL     = RGL(namelist%vegtyp)
    this%RSMIN   = RSMIN(namelist%vegtyp)
    this%HS      = HS(namelist%vegtyp)
    this%AKC     = AKC(namelist%vegtyp)
    this%AKO     = AKO(namelist%vegtyp)
    this%AVCMX   = AVCMX(namelist%vegtyp)
    this%RSMAX   = RSMAX(namelist%vegtyp)
    this%CWP     = CWP
    this%C3PSN   = C3PSN
    this%DLEAF   = DLEAF
    this%KC25    = KC25
    this%KO25    = KO25

    this%RHOL(1) = RHOL_VIS(namelist%vegtyp)
    this%RHOL(2) = RHOL_NIR(namelist%vegtyp)
    this%RHOS(1) = RHOS_VIS(namelist%vegtyp)
    this%RHOS(2) = RHOS_NIR(namelist%vegtyp)
    this%TAUL(1) = TAUL_VIS(namelist%vegtyp)
    this%TAUL(2) = TAUL_NIR(namelist%vegtyp)
    this%TAUS(1) = TAUS_VIS(namelist%vegtyp)
    this%TAUS(2) = TAUS_NIR(namelist%vegtyp)

    this%refkdt  = refkdt
    this%refdk   = refdk
    this%kdt     = this%refkdt * this%dksat(1) / this%refdk
    this%csoil   = csoil
    this%Z0      = Z0
    this%CZIL    = CZIL
    this%ZBOT    = ZBOT
    this%frzx    = 0.15 * (this%smcmax(1) / this%smcref(1)) * (0.412 / 0.468)
    this%SSI          = SSI
    this%MFSNO        = MFSNO
    this%Z0SNO        = Z0SNO
    this%SWEMX        = SWEMX
    this%TAU0         = TAU0
    this%GRAIN_GROWTH = GRAIN_GROWTH
    this%EXTRA_GROWTH = EXTRA_GROWTH
    this%DIRT_SOOT    = DIRT_SOOT
    this%BATS_COSZ    = BATS_COSZ
    this%BATS_VIS_NEW = BATS_VIS_NEW
    this%BATS_NIR_NEW = BATS_NIR_NEW
    this%BATS_VIS_AGE = BATS_VIS_AGE
    this%BATS_NIR_AGE = BATS_NIR_AGE
    this%BATS_VIS_DIR = BATS_VIS_DIR
    this%BATS_NIR_DIR = BATS_NIR_DIR
    this%RSURF_SNOW   = RSURF_SNOW
    this%RSURF_EXP    = RSURF_EXP

    this%ALBSAT(1) = ALBSAT_VIS(namelist%soilcolor)
    this%ALBSAT(2) = ALBSAT_NIR(namelist%soilcolor)
    this%ALBDRY(1) = ALBDRY_VIS(namelist%soilcolor)
    this%ALBDRY(2) = ALBDRY_NIR(namelist%soilcolor)
    this%ALBICE               = ALBICE
    this%ALBLAK               = ALBLAK
    this%OMEGAS               = OMEGAS
    this%BETADS               = BETADS
    this%BETAIS               = BETAIS
    this%EG                   = EG
    this%slope   = slope

    this%ISURBAN                   = ISURBAN
    this%ISWATER                   = ISWATER
    this%ISBARREN                  = ISBARREN
    this%ISICE                     = ISICE
    this%ISCROP                    = ISCROP
    this%EBLFOREST                 = EBLFOREST
    this%NATURAL                   = NATURAL
    this%LOW_DENSITY_RESIDENTIAL   = LOW_DENSITY_RESIDENTIAL
    this%HIGH_DENSITY_RESIDENTIAL  = HIGH_DENSITY_RESIDENTIAL
    this%HIGH_INTENSITY_INDUSTRIAL = HIGH_INTENSITY_INDUSTRIAL

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

    !this%pctsand = pctsand
    !this%pctclay = pctclay

  end subroutine paramRead

end module ParametersType

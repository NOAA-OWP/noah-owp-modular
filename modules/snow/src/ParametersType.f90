module ParametersType

use NamelistRead

implicit none
save
private

type, public :: parameters_type

  real, allocatable, dimension(:) :: bexp   ! b parameter
  real, allocatable, dimension(:) :: smcmax ! porosity (volumetric)
  real, allocatable, dimension(:) :: smcwlt ! wilting point
  real, allocatable, dimension(:) :: smcref ! field capacity
  real, allocatable, dimension(:) :: dksat  ! saturated conductivity
  real, allocatable, dimension(:) :: dwsat  ! saturated diffusivity
  real, allocatable, dimension(:) :: psisat ! saturated matric potential
  real                            :: bvic   ! VIC or DVIC model infiltration parameter
  real                            :: AXAJ   ! Xinanjiang: Tension water distribution inflection parameter [-]
  real                            :: BXAJ   ! Xinanjiang: Tension water distribution shape parameter [-]
  real                            :: XXAJ   ! Xinanjiang: Free water distribution shape parameter [-]
  real                            :: BBVIC  ! DVIC heterogeniety parameter for infiltration 
  real                            :: G      ! Mean Capillary Drive (m) for infiltration models
  real                            :: QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
  real                            :: kdt    !
  real                            :: refkdt !
  real                            :: refdk  !
  real                            :: csoil  ! volumetric soil heat capacity [j/m3/K]
  real                            :: Z0     ! bare soil roughness length (m)
  real                            :: frzx   !
  real                            :: slope  ! drainage parameter
  real                            :: timean
  real                            :: fsatmx
  logical                         :: urban_flag
  real, dimension(12)             :: LAIM ! monthly LAI
  real, dimension(12)             :: SAIM ! monthly SAI
  real                            :: LAI
  real                            :: SAI
  real                            :: CH2OP !maximum intercepted h2o per unit lai+sai (mm)
  integer                         :: NROOT    !vegetation root level
  real                            :: HVT    ! canopy top height (m)
  real                            :: HVB    ! canopy bottom height (m)
  real                            :: TMIN   ! minimum temperature for photosynthesis (k)
  real                            :: SHDFAC ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real                            :: SHDMAX ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real                            :: Z0MVT  ! momentum roughness length (m)
  real                            :: CWP    ! canopy wind absorption coefficient (formerly CWPVT)
  real                            :: ELAI
  real                            :: ESAI
  real                            :: FVEG ! vegetation fraction
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
  real                            :: GRAV     !acceleration due to gravity (m/s2)
  real                            :: SB       !Stefan-Boltzmann constant (w/m2/k4)
  real                            :: VKC      !von Karman constant
  real                            :: TFRZ     !freezing/melting point (k)
  real                            :: HSUB     !latent heat of sublimation (j/kg)
  real                            :: HVAP     !latent heat of vaporization (j/kg)
  real                            :: HFUS     !latent heat of fusion (j/kg)
  real                            :: CWAT     !specific heat capacity of water (j/m3/k)
  real                            :: CICE     !specific heat capacity of ice (j/m3/k)
  real                            :: CPAIR    !heat capacity dry air at const pres (j/kg/k)
  real                            :: TKWAT    !thermal conductivity of water (w/m/k)
  real                            :: TKICE    !thermal conductivity of ice (w/m/k)
  real                            :: TKAIR    !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  real                            :: RAIR     !gas constant for dry air (j/kg/k)
  real                            :: RW       !gas constant for  water vapor (j/kg/k)
  real                            :: DENH2O   !density of water (kg/m3)
  real                            :: DENICE   !density of ice (kg/m3)
  real                            :: THKW     ! thermal conductivity of water in soil module (W/m/K)
  real                            :: THKO     ! thermal conductivity of for other soil components in soil module (W/m/K)
  real                            :: THKQTZ   ! thermal conductivity of quartz in soil module (W/m/K)
  real                            :: SSI      !liquid water holding capacity for snowpack (m3/m3)
  real                            :: MFSNO    ! fractional snow covered area (FSNO) curve parameter
  real                            :: Z0SNO    ! snow surface roughness length (m)
  real                            :: WSLMAX   !maximum lake water storage (mm)
  real                            :: max_liq_mass_fraction !For snow water retention
  real                            :: SNOW_RET_FAC !snowpack water release timescale factor (1/s)

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type parameters_type

contains   

  subroutine Init(this, namelist)

    class(parameters_type) :: this
    type(namelist_type) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(parameters_type) :: this
    type(namelist_type) :: namelist

    allocate(this%bexp   (namelist%nsoil))  ; this%bexp   (:) = huge(1.0)
    allocate(this%smcmax (namelist%nsoil))  ; this%smcmax (:) = huge(1.0)
    allocate(this%smcwlt (namelist%nsoil))  ; this%smcwlt (:) = huge(1.0)
    allocate(this%smcref (namelist%nsoil))  ; this%smcref (:) = huge(1.0)
    allocate(this%dksat  (namelist%nsoil))  ; this%dksat  (:) = huge(1.0)
    allocate(this%dwsat  (namelist%nsoil))  ; this%dwsat  (:) = huge(1.0)
    allocate(this%psisat (namelist%nsoil))  ; this%psisat (:) = huge(1.0)

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(parameters_type) :: this

    this%LAI        = huge(1.0)
    this%SAI        = huge(1.0)
    this%ELAI       = huge(1.0)
    this%ESAI       = huge(1.0)
    this%FVEG       = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(parameters_type) :: this
    type(namelist_type) :: namelist

    this%bexp   = namelist%bb     (namelist%isltyp)
    this%smcmax = namelist%maxsmc (namelist%isltyp)
    this%smcwlt = namelist%wltsmc (namelist%isltyp)
    this%smcref = namelist%refsmc (namelist%isltyp)
    this%dksat  = namelist%satdk  (namelist%isltyp)
    this%dwsat  = namelist%satdw  (namelist%isltyp)
    this%psisat = namelist%satpsi (namelist%isltyp)
    this%bvic   = namelist%bvic   (namelist%isltyp)
    this%AXAJ   = namelist%AXAJ   (namelist%isltyp)
    this%BXAJ   = namelist%BXAJ   (namelist%isltyp)
    this%XXAJ   = namelist%XXAJ   (namelist%isltyp)
    this%BBVIC  = namelist%BBVIC  (namelist%isltyp)
    this%G      = namelist%G      (namelist%isltyp)
    this%QUARTZ = namelist%QUARTZ (namelist%isltyp)
    this%LAIM   = namelist%LAIM_TABLE  (namelist%vegtyp, :)
    this%SAIM   = namelist%SAIM_TABLE  (namelist%vegtyp, :)
    this%CH2OP  = namelist%CH2OP (namelist%vegtyp)
    this%NROOT  = namelist%NROOT (namelist%vegtyp)
    this%HVT    = namelist%HVT (namelist%vegtyp)
    this%HVB    = namelist%HVB (namelist%vegtyp)
    this%TMIN   = namelist%TMIN (namelist%vegtyp)
    this%SHDFAC = namelist%SHDFAC (namelist%vegtyp)
    this%SHDMAX = namelist%SHDFAC (namelist%vegtyp)
    this%Z0MVT  = namelist%Z0MVT (namelist%vegtyp)
    this%CWP    = namelist%CWP
    this%refkdt = namelist%refkdt
    this%refdk  = namelist%refdk
    this%kdt    = this%refkdt * this%dksat(1) / this%refdk
    this%csoil  = namelist%csoil
    this%Z0     = namelist%Z0
    this%frzx   = 0.15 * (this%smcmax(1) / this%smcref(1)) * (0.412 / 0.468)
    this%SSI    = namelist%SSI  
    this%MFSNO  = namelist%MFSNO  
    this%Z0SNO  = namelist%Z0SNO  
    this%slope      = namelist%slope
    this%ISURBAN                   = namelist%ISURBAN
    this%ISWATER                   = namelist%ISWATER
    this%ISBARREN                  = namelist%ISBARREN
    this%ISICE                     = namelist%ISICE
    this%ISCROP                    = namelist%ISCROP
    this%EBLFOREST                 = namelist%EBLFOREST
    this%NATURAL                   = namelist%NATURAL
    this%LOW_DENSITY_RESIDENTIAL   = namelist%LOW_DENSITY_RESIDENTIAL
    this%HIGH_DENSITY_RESIDENTIAL  = namelist%HIGH_DENSITY_RESIDENTIAL
    this%HIGH_INTENSITY_INDUSTRIAL = namelist%HIGH_INTENSITY_INDUSTRIAL
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

  end subroutine InitTransfer

end module ParametersType

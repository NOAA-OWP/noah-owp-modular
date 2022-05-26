module EnergyType

use NamelistRead, only: namelist_type

implicit none
save
private

type, public :: energy_type

  real    :: TV                                ! vegetation temperature (k)
  real    :: TG                                ! ground temperature (k)
  real    :: FCEV                              ! canopy evaporation energy flux (w/m2)
  real    :: FCTR                              ! canopy transpiration energy flux (w/m2)
  real    :: IGS                               ! growing season index (0=off, 1=on)
  logical :: FROZEN_CANOPY                     ! binary frozen canopy status (true when TV <= parameters%TFRZ)
  logical :: FROZEN_GROUND                     ! binary frozen ground status (true when TG <= parameters%TFRZ)
  integer, allocatable, dimension(:) :: IMELT  ! snow layer melting state index [0-no melt;1-melt]
  real,    allocatable, dimension(:) :: STC    ! snow/soil layer temperature [k]
  real,    allocatable, dimension(:) :: DF     ! snow and soil layer thermal conductivity [w/m/k]
  real,    allocatable, dimension(:) :: HCPCT  ! snow and soil layer heat capacity [j/m3/k]
  real,    allocatable, dimension(:) :: FACT   ! temporary variable used in phase change [s/j/m2/k]
  
  ! Heat advected by precipitation
  real    :: PAHV                              ! precipitation advected heat - vegetation net (W/m2)
  real    :: PAHG                              ! precipitation advected heat - under canopy net (W/m2)
  real    :: PAHB                              ! precipitation advected heat - bare ground net (W/m2)
  REAL    :: PAH                               ! precipitation advected heat - total (W/m2)
  
  ! Albedo
  real                 :: TAUSS  ! non-dimensional snow age
  real                 :: FAGE   ! snow age (0 = new snow)
  real                 :: ALB    ! broadband albedo in CLASS scheme
  real                 :: ALBOLD ! broadband albedo at previous timestep 
  
  real, allocatable, dimension(:) :: ALBD   ! surface albedo (direct)
  real, allocatable, dimension(:) :: ALBI   ! surface albedo (diffuse)
  real, allocatable, dimension(:) :: ALBGRD ! ground albedo (direct)
  real, allocatable, dimension(:) :: ALBGRI ! ground albedo (diffuse)
  real, allocatable, dimension(:) :: ALBSND ! snow albedo for direct(1=vis, 2=nir)
  real, allocatable, dimension(:) :: ALBSNI ! snow albedo for diffuse
  
  real, allocatable, dimension(:) :: FABD   ! flux abs by veg (per unit direct flux)
  real, allocatable, dimension(:) :: FABI   ! flux abs by veg (per unit diffuse flux)
  real, allocatable, dimension(:) :: FTDD   ! down direct flux below veg (per unit dir flux)
  real, allocatable, dimension(:) :: FTDI   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:) :: FTID   ! down diffuse flux below veg (per unit dir flux)
  real, allocatable, dimension(:) :: FTII   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:) :: FREVD  ! direct flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREVI  ! diffuse flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREGD  ! direct flux reflected by ground (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREGI  ! direct flux reflected by ground (per unit incoming flux)
  real, allocatable, dimension(:) :: RHO    ! leaf/stem reflectance weighted by fraction LAI and SAI
  real, allocatable, dimension(:) :: TAU    ! leaf/stem transmittance weighted by fraction LAI and SAI

  
  ! Shortwave radiation
  real                 :: COSZ    ! cosine solar zenith angle [0-1]
  real                 :: COSZ_HORIZ ! cosine solar zenith angle for flat ground [0-1]
  real                 :: BGAP    ! between canopy gap fraction for beam (-)
  real                 :: WGAP    ! within canopy gap fraction for beam (-)
  REAL                 :: FSUN    ! sunlit fraction of canopy (-)
  REAL                 :: FSHA    ! shaded fraction of canopy (-)
  REAL                 :: LAISUN  ! sunlit leaf area (-)
  REAL                 :: LAISHA  ! shaded leaf area (-)
  REAL                 :: PARSUN  ! average absorbed par for sunlit leaves (w/m2)
  REAL                 :: PARSHA  ! average absorbed par for shaded leaves (w/m2)
  REAL                 :: SAV     ! solar radiation absorbed by vegetation (w/m2)
  REAL                 :: SAG     ! solar radiation absorbed by ground (w/m2)
  REAL                 :: FSA     ! total absorbed solar radiation (w/m2)
  REAL                 :: FSR     ! total reflected solar radiation (w/m2)
  REAL                 :: FSRV    ! reflected solar radiation by vegetation
  REAL                 :: FSRG    ! reflected solar radiation by ground

  ! Other, uncategorized
  REAL                 :: TAH     ! canopy air tmeperature (K)
  REAL                 :: EAH     ! canopy water vapor pressure (Pa)  
  REAL                 :: ZPD     ! zero plane displacement, ground (m)
  REAL                 :: Z0MG    ! z0 momentum, ground (m)
  REAL                 :: Z0M     ! roughness length, momentum (m)
  REAL                 :: ZLVL    ! reference height (m)  
  REAL                 :: CMV     ! momentum drag coefficient (vegetated surface)  
  REAL                 :: CMB     ! momentum drag coefficient (bare ground)  
  REAL                 :: CM      ! momentum drag coefficient (weighted version of CMV + CMB by FVEG)  
  REAL                 :: CH      ! drag coefficient for heat
  REAL                 :: TGB     ! ground temperature (K)
  REAL                 :: QSFC    ! mixing ratio at lowest model layer (g/g)
  REAL                 :: EMV     ! vegetation emissivity (-)
  REAL                 :: EMG     ! ground emissivity (-)
  REAL                 :: GAMMAV  ! psychrometric constant (Pa/K)
  REAL                 :: GAMMAG  ! psychrometric constant (Pa/K)  
!  REAL                 :: GAMMA   ! psychrometric constant (Pa/K)  NOT USED IN CURRENT VERSION
  REAL                 :: EVC     ! evaporation heat flux (w/m2)  [+= to atm]  
  REAL                 :: IRC     ! net longwave radiation (w/m2) [+= to atm]
  REAL                 :: IRG     ! net longwave radiation (w/m2) [+= to atm]
  REAL                 :: SHC     ! sensible heat flux (w/m2)     [+= to atm]
  REAL                 :: SHG     ! sensible heat flux (w/m2)     [+= to atm]
  REAL                 :: SHB     ! sensible heat flux (w/m2) [+ to atm] 
  REAL                 :: EVG     ! evaporation heat flux (w/m2)  [+= to atm]
  REAL                 :: EVB     ! latent heat flux (w/m2)   [+ to atm]
  REAL                 :: TR      ! transpiration heat flux (w/m2)[+= to atm]
  REAL                 :: GH      ! ground heat (w/m2) [+ = to soil]
  REAL                 :: GHB     ! ground heat flux (w/m2)  [+ to soil] 
  REAL                 :: GHV     ! ground heat flux [w/m2]  [+ to soil]
  REAL                 :: T2MV    ! 2 m height air temperature (k)
  REAL                 :: CHLEAF  ! leaf exchange coefficient
  REAL                 :: CHUC    ! under canopy exchange coefficient
  REAL                 :: CHV2    ! sensible heat exch. coeff. over vegetated fraction (m/s)  
  REAL                 :: CHB2    ! sensible heat exch. coeff. bare-ground (m/s)
  REAL                 :: Q2V     ! check
  REAL                 :: LATHEAV ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL                 :: LATHEAG ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL                 :: LATHEA  ! latent heat of vaporization/subli (j/kg) (bare ground version)
  REAL                 :: RSURF   ! ground surface resistance (s/m)
  REAL                 :: RHSUR   ! relative humidity in surface soil/snow air space (-)  
  REAL                 :: TAUXV   ! wind stress: e-w (n/m2) vegetation
  REAL                 :: TAUYV   ! wind stress: n-s (n/m2) vegetation
  REAL                 :: TAUXB   ! wind stress: e-w (n/m2) bare ground
  REAL                 :: TAUYB   ! wind stress: n-s (n/m2) bare ground
  REAL                 :: TAUX    ! wind stress: e-w (n/m2)
  REAL                 :: TAUY    ! wind stress: n-s (n/m2)  
  REAL                 :: CAH2    ! sensible heat conductance for diagnostics
  REAL                 :: EHB2    ! sensible heat conductance for diagnostics (bare ground)
  REAL                 :: T2MB    ! 2 m height air temperature (K)  
  REAL                 :: Q2B     ! bare ground heat conductance
  REAL                 :: TGV     ! ground surface temp. [k]
  REAL                 :: CHV     ! sensible heat exchange coefficient
  REAL                 :: RSSUN   ! sunlit leaf stomatal resistance (s/m)
  REAL                 :: RSSHA   ! shaded leaf stomatal resistance (s/m)
  REAL                 :: RB      ! leaf boundary layer resistance (s/m)
  REAL                 :: FIRA    ! total net LW. rad (w/m2)   [+ to atm]
  REAL                 :: FSH     ! total sensible heat (w/m2) [+ to atm]
  REAL                 :: FGEV    ! ground evaporation (w/m2)  [+ to atm]
  REAL                 :: TRAD    ! radiative temperature (k)
  REAL                 :: IRB     ! net longwave rad. [w/m2] [+ to atm]
  REAL                 :: SSOIL   ! ground heat flux (w/m2)   [+ to soil]
  REAL                 :: T2M     ! 2-meter air temperature (k)
  REAL                 :: TS      ! surface temperature (k)
  REAL                 :: CHB     ! sensible heat exchange coefficient
  REAL                 :: Q1
  REAL                 :: Q2E
  REAL                 :: Z0WRF   ! combined z0 sent to coupled model
  REAL                 :: EMISSI  ! net surface emissivity  
  REAL                 :: PSN     ! total photosyn. (umolco2/m2/s) [+]
  REAL                 :: PSNSUN  ! sunlit photosynthesis (umolco2/m2/s)  
  REAL                 :: PSNSHA  ! shaded photosynthesis (umolco2/m2/s)    
  REAL                 :: APAR    ! total photosyn. active energy (w/m2)
  REAL                 :: QMELT   ! snowmelt [mm/s]
  
  REAL                 :: LH      ! latent heat (total) flux [W m-2]
  REAL                 :: TGS     ! ground surface temperature (K, takes value of TG when SNOWH <= 0.05 and STC[0] when SNOWH > 0.05) 
                                  ! this is a temporary fix for when coupled to subsurface modules
                                  ! TODO: further investigation
  

  integer              :: ICE     ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
  
  
  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate        
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type energy_type

contains   

  subroutine Init(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

    allocate(this%IMELT (-namelist%nsnow+1:namelist%nsoil)); this%IMELT(:) = huge(1)
    allocate(this%STC   (-namelist%nsnow+1:namelist%nsoil)); this%STC(:)   = huge(1.0)
    allocate(this%DF    (-namelist%nsnow+1:namelist%nsoil)); this%DF(:)    = huge(1.0)
    allocate(this%HCPCT (-namelist%nsnow+1:namelist%nsoil)); this%HCPCT(:) = huge(1.0)
    allocate(this%FACT  (-namelist%nsnow+1:namelist%nsoil)); this%FACT(:)  = huge(1.0)
    
    allocate(this%ALBD (1:2)); this%ALBD(:)      = huge(1.0) 
    allocate(this%ALBI (1:2)); this%ALBI(:)      = huge(1.0) 
    allocate(this%ALBGRD (1:2)); this%ALBGRD(:)  = huge(1.0) 
    allocate(this%ALBGRI (1:2)); this%ALBGRI(:)  = huge(1.0) 
    allocate(this%ALBSND (1:2)); this%ALBSND(:)  = huge(1.0) 
    allocate(this%ALBSNI (1:2)); this%ALBSNI(:)  = huge(1.0) 
    
    allocate(this%FABD (1:2)); this%FABD(:)      = huge(1.0) 
    allocate(this%FABI (1:2)); this%FABI(:)      = huge(1.0) 
    allocate(this%FTDD (1:2)); this%FTDD(:)      = huge(1.0) 
    allocate(this%FTDI (1:2)); this%FTDI(:)      = huge(1.0) 
    allocate(this%FTID (1:2)); this%FTID(:)      = huge(1.0) 
    allocate(this%FTII (1:2)); this%FTII(:)      = huge(1.0) 
    allocate(this%FREVD (1:2)); this%FREVD(:)    = huge(1.0) 
    allocate(this%FREVI (1:2)); this%FREVI(:)    = huge(1.0) 
    allocate(this%FREGD (1:2)); this%FREGD(:)    = huge(1.0) 
    allocate(this%FREGI (1:2)); this%FREGI(:)    = huge(1.0) 
    allocate(this%RHO (1:2)); this%RHO(:)        = huge(1.0) 
    allocate(this%TAU (1:2)); this%TAU(:)        = huge(1.0) 

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(energy_type) :: this

    this%TV        = huge(1.0)
    this%TG        = huge(1.0)
    this%FCEV      = huge(1.0)
    this%FCTR      = huge(1.0)
    this%IGS       = huge(1.0)
    this%FROZEN_CANOPY = .false.
    this%FROZEN_GROUND = .false.
    this%PAHV      = huge(1.0)
    this%PAHG      = huge(1.0)
    this%PAHB      = huge(1.0)
    this%PAH       = huge(1.0)
    
    this%TAUSS     = huge(1.0)
    this%FAGE      = huge(1.0)
    this%ALB       = huge(1.0)
    this%ALBOLD    = huge(1.0)
    
    this%COSZ      = huge(1.0)
    this%COSZ_HORIZ= huge(1.0)
    this%BGAP      = huge(1.0)
    this%WGAP      = huge(1.0)
    this%FSUN      = huge(1.0)
    this%FSHA      = huge(1.0)
    this%LAISUN    = huge(1.0)
    this%LAISHA    = huge(1.0)
    this%PARSUN    = huge(1.0)
    this%PARSHA    = huge(1.0)
    this%SAV       = huge(1.0)
    this%SAG       = huge(1.0)
    this%FSA       = huge(1.0)
    this%FSR       = huge(1.0)
    this%FSRV      = huge(1.0)
    this%FSRG      = huge(1.0)
    
    this%TAH       = huge(1.0)
    this%EAH       = huge(1.0)
    this%ZPD       = huge(1.0)    
    this%Z0MG      = huge(1.0)    
    this%Z0M       = huge(1.0)      
    this%ZLVL      = huge(1.0)
    this%CMV       = huge(1.0)  
    this%CMB       = huge(1.0)  
    this%CM        = huge(1.0)  
    this%CH        = huge(1.0)  
    this%TGB       = huge(1.0)    
    this%QSFC      = huge(1.0)  
    this%EMV       = huge(1.0)  
    this%EMG       = huge(1.0)  
    this%GAMMAV    = huge(1.0)  
    this%GAMMAG    = huge(1.0)  
!    this%GAMMA     = huge(1.0)  ! NOT USED IN CURRENT VERSION
    
    this%EVC       = huge(1.0)  
    this%IRC       = huge(1.0)  
    this%SHC       = huge(1.0)  
    this%IRG       = huge(1.0)  
    this%SHG       = huge(1.0)  
    this%EVG       = huge(1.0)  
    this%TR        = huge(1.0)  
    this%GH        = huge(1.0) 
    this%GHB       = huge(1.0)   
    this%GHV       = huge(1.0)     
    this%T2MV      = huge(1.0)  
    this%CHLEAF    = huge(1.0)  
    this%CHUC      = huge(1.0)  
    this%CHV2      = huge(1.0)
    this%CHB2      = huge(1.0)
    this%Q2V       = huge(1.0)  
    this%LATHEAV   = huge(1.0)      
    this%LATHEAG   = huge(1.0)  
    this%LATHEA    = huge(1.0)     
    this%RSURF     = huge(1.0)    
    this%RHSUR     = huge(1.0)    
    this%TAUXV     = huge(1.0) 
    this%TAUYV     = huge(1.0) 
    this%TAUXB     = huge(1.0) 
    this%TAUYB     = huge(1.0) 
    this%TAUX      = huge(1.0) 
    this%TAUY      = huge(1.0) 
    this%CAH2      = huge(1.0) 
    this%EHB2      = huge(1.0)
    this%T2MB      = huge(1.0)
    this%Q2B       = huge(1.0)
    this%SHB       = huge(1.0)
    this%EVB       = huge(1.0)
    this%TGB       = huge(1.0)
    this%CHV       = huge(1.0)
    this%RSSUN     = huge(1.0)     
    this%RSSHA     = huge(1.0)
    this%RB        = huge(1.0)

    this%FIRA      = huge(1.0)
    this%FSH       = huge(1.0)
    this%FGEV      = huge(1.0)
    this%TRAD      = huge(1.0)
    this%IRB       = huge(1.0)
    this%SSOIL     = huge(1.0)
    this%T2M       = huge(1.0)
    this%TS        = huge(1.0)
    this%CHB       = huge(1.0)
    this%Q1        = huge(1.0)
    this%Q2E       = huge(1.0)
    this%Z0WRF     = huge(1.0)
    this%EMISSI    = huge(1.0)
    this%PSN       = huge(1.0)
    this%PSNSUN    = huge(1.0)
    this%PSNSHA    = huge(1.0)    
    this%APAR      = huge(1.0)
    this%QMELT     = huge(1.0)

    this%LH        = huge(1.0)    
    this%TGS       = huge(1.0)    
    
    this%ICE       = huge(1)    
    
    
    
  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module EnergyType

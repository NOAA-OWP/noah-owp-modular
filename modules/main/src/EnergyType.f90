module EnergyType

use NamelistRead

implicit none
save
private

type, public :: energy_type

  real    :: TV
  real    :: TG
  real    :: FCEV 
  real    :: FCTR
  real    :: IGS                               ! growing season index (0=off, 1=on)
  logical :: FROZEN_CANOPY
  logical :: FROZEN_GROUND
  integer, allocatable, dimension(:) :: IMELT  ! snow layer melting state index [0-no melt;1-melt]
  real,    allocatable, dimension(:) :: STC    ! snow/soil layer temperature [k]
  real,    allocatable, dimension(:) :: DF     ! snow and soil layer thermal conductivity [w/m/k]
  real,    allocatable, dimension(:) :: HCPCT  ! snow and soil layer heat capacity [j/m3/k]
  real,    allocatable, dimension(:) :: FACT   ! temporary variable used in phase change [s/j/m2/k]
  
  ! Heat advected by precipitation
  real    :: PAHV                              ! precipitation advected heat - vegetation net (W/m2)
  real    :: PAHG                              ! precipitation advected heat - under canopy net (W/m2)
  real    :: PAHB                              ! precipitation advected heat - bare ground net (W/m2)
  
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
  real, allocatable, dimension(:) :: FREVD   ! direct flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREVI   ! diffuse flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREGD   ! direct flux reflected by ground (per unit incoming flux) 
  real, allocatable, dimension(:) :: FREGI   ! direct flux reflected by ground (per unit incoming flux)
  real, allocatable, dimension(:) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  real, allocatable, dimension(:) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI

  
  ! Shortwave radiation
  real                 :: COSZ    ! cosine solar zenith angle [0-1]
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
  REAL                 :: TAH     ! canopy air tmeperature (k)
  REAL                 :: ZPD     ! zero plane displacement, ground (m)
  REAL                 :: Z0MG    ! z0 momentum, ground (m)
  REAL                 :: Z0M     ! roughness length, momentum (m)
  REAL                 :: ZLVL    ! reference height (m)  
  REAL                 :: CM      ! momentum drag coefficient  
  REAL                 :: CH      ! drag coefficient for heat
  
  
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

    allocate(this%IMELT (-namelist%nsnow+1:0)); this%IMELT(:)              = huge(1)
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
    
    this%TAUSS     = huge(1.0)
    this%FAGE      = huge(1.0)
    this%ALB       = huge(1.0)
    this%ALBOLD    = huge(1.0)
    
    this%COSZ      = huge(1.0)
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
    this%ZPD       = huge(1.0)    
    this%Z0MG      = huge(1.0)    
    this%Z0M       = huge(1.0)      
    this%ZLVL      = huge(1.0)
    this%CM        = huge(1.0)  
    this%CH        = huge(1.0)  

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module EnergyType

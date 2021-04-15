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
  real    :: TAUSS                             ! non-dimensional snow age
  real    :: FAGE                              ! snow age (0 = new snow)
  REAL, DIMENSION(1:2) :: ALBD   ! surface albedo (direct)
  REAL, DIMENSION(1:2) :: ALBI   ! surface albedo (diffuse)
  REAL, DIMENSION(1:2) :: ALBGRD ! ground albedo (direct)
  REAL, DIMENSION(1:2) :: ALBGRI  !ground albedo (diffuse)
  REAL, DIMENSION(1:2) :: ALBSND ! snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2) :: ALBSNI ! snow albedo for diffuse
  real    :: ALB                 ! broadband albedo in CLASS scheme
  real    :: ALBOLD              ! broadband albedo at previous timestep 
  
  REAL, DIMENSION(1:2) :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:2) :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:2) :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2) :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2) :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL                 :: FSUN   !sunlit fraction of canopy (-)
  
  ! Shortwave radiation
  real    :: COSZ         ! cosine solar zenith angle [0-1]
  real    :: BGAP                              ! between canopy gap fraction for beam (-)
  real    :: WGAP                              ! within canopy gap fraction for beam (-)
  REAL, DIMENSION(1:2) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI
  
  
  
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
    
  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module EnergyType

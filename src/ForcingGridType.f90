module ForcingGridType

use NamelistRead,   only: namelist_type
use AttributesType, only: attributes_type

implicit none
private

type, public :: forcinggrid_type

  ! atmospheric inputs (meteorology, chemistry)
real,allocatable,dimension(:,:)                   :: SFCPRS       ! surface pressure (pa)
real,allocatable,dimension(:,:)                   :: SFCTMP       ! surface air temperature [K]
real,allocatable,dimension(:,:)                   :: Q2           ! specific humidity (note: in some Noah-MP versions Q2 is mixing ratio)
real,allocatable,dimension(:,:)                   :: PRCP         ! total input precipitation[mm/s]
real,allocatable,dimension(:,:)                   :: PRCPCONV     ! convective precipitation entering  [mm/s]
real,allocatable,dimension(:,:)                   :: PRCPNONC     ! non-convective precipitation entering [mm/s]
real,allocatable,dimension(:,:)                   :: PRCPSHCV     ! shallow convective precip entering  [mm/s]
real,allocatable,dimension(:,:)                   :: PRCPSNOW     ! snow entering land model [mm/s] 
real,allocatable,dimension(:,:)                   :: PRCPGRPL     ! graupel entering land model [mm/s]
real,allocatable,dimension(:,:)                   :: PRCPHAIL     ! hail entering land model [mm/s]             
real,allocatable,dimension(:,:)                   :: SOLDN        ! downward shortwave radiation (w/m2)
real,allocatable,dimension(:,:)                   :: LWDN         ! atmospheric longwave radiation (w/m2)
real,allocatable,dimension(:,:)                   :: FOLN         ! foliage nitrogen concentration (%)
real,allocatable,dimension(:,:)                   :: O2PP         ! atmospheric co2 concentration partial pressure (pa)
real,allocatable,dimension(:,:)                   :: CO2PP        ! atmospheric o2 concentration partial pressure (pa) 
real,allocatable,dimension(:,:)                   :: UU           ! wind speed in eastward dir (m/s)  
real,allocatable,dimension(:,:)                   :: VV           ! wind speed in northward dir (m/s)  

! surface inputs
real,allocatable,dimension(:,:)                   :: TBOT         ! bottom condition for soil temperature [K]

! outputs
real,allocatable,dimension(:,:)                   :: UR           ! wind speed at reference height (m/s)
real,allocatable,dimension(:,:)                   :: THAIR        ! potential temperature (k)
real,allocatable,dimension(:,:)                   :: QAIR         ! specific humidity (kg/kg) (q2/(1+q2))
real,allocatable,dimension(:,:)                   :: EAIR         ! vapor pressure air (pa)
real,allocatable,dimension(:,:)                   :: RHOAIR       ! density air (kg/m3)
real,allocatable,dimension(:,:)                   :: FPICE        ! fraction of ice in precipitation (-)
real,allocatable,dimension(:,:)                   :: SWDOWN       ! downward solar filtered by sun angle [w/m2]
real                                              :: JULIAN       ! julian day of year
integer                                           :: YEARLEN      ! year length (days)
             
real, allocatable, dimension(:,:,:)               :: SOLAD  !incoming direct solar radiation (w/m2)
real, allocatable, dimension(:,:,:)               :: SOLAI  !incoming diffuse solar radiation (w/m2)

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate        
    procedure, private :: InitDefault
    procedure, public  :: InitTransfer     

end type

contains   

  subroutine Init(this,attributes)

    class(forcinggrid_type), intent(inout) :: this
    type(attributes_type),   intent(in)    :: attributes

    call this%InitAllocate(attributes)
    call this%InitDefault()

  end subroutine Init
  
  subroutine InitAllocate(this, attributes)

    class(forcinggrid_type), intent(inout) :: this
    type(attributes_type),   intent(in)    :: attributes

    associate(n_x => attributes%metadata%n_x, &
              n_y => attributes%metadata%n_y)

    allocate(this%SFCPRS(n_x,n_y))
    allocate(this%SFCTMP(n_x,n_y))
    allocate(this%Q2(n_x,n_y))
    allocate(this%PRCP(n_x,n_y))
    allocate(this%PRCPCONV(n_x,n_y))
    allocate(this%PRCPNONC(n_x,n_y))
    allocate(this%PRCPSHCV(n_x,n_y))
    allocate(this%PRCPSNOW(n_x,n_y))
    allocate(this%PRCPGRPL(n_x,n_y))
    allocate(this%PRCPHAIL(n_x,n_y))
    allocate(this%SOLDN(n_x,n_y))
    allocate(this%LWDN(n_x,n_y))
    allocate(this%FOLN(n_x,n_y))
    allocate(this%O2PP(n_x,n_y))
    allocate(this%CO2PP(n_x,n_y))
    allocate(this%UU(n_x,n_y))
    allocate(this%VV(n_x,n_y))
    allocate(this%TBOT(n_x,n_y))
    allocate(this%UR(n_x,n_y))
    allocate(this%THAIR(n_x,n_y))
    allocate(this%QAIR(n_x,n_y))
    allocate(this%EAIR(n_x,n_y))
    allocate(this%RHOAIR(n_x,n_y))
    allocate(this%FPICE(n_x,n_y))
    allocate(this%SWDOWN(n_x,n_y))
    allocate(this%SOLAD(n_x,n_y,2))
    allocate(this%SOLAI(n_x,n_y,2))

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(forcinggrid_type) :: this

    this%SFCPRS(:,:)    = huge(1.0)
    this%SFCTMP(:,:)    = huge(1.0)
    this%Q2(:,:)        = huge(1.0)
    this%PRCP(:,:)      = huge(1.0)
    this%PRCPCONV(:,:)  = huge(1.0)
    this%PRCPNONC(:,:)  = huge(1.0)
    this%PRCPSHCV(:,:)  = huge(1.0)
    this%PRCPSNOW(:,:)  = huge(1.0)
    this%PRCPGRPL(:,:)  = huge(1.0)
    this%PRCPHAIL(:,:)  = huge(1.0)
    this%SOLDN(:,:)     = huge(1.0)
    this%LWDN(:,:)      = huge(1.0) 
    this%FOLN(:,:)      = huge(1.0) 
    this%O2PP(:,:)      = huge(1.0) 
    this%CO2PP(:,:)     = huge(1.0) 
    this%UU(:,:)        = huge(1.0)
    this%VV(:,:)        = huge(1.0)
    this%TBOT(:,:)      = huge(1.0)
    this%UR(:,:)        = huge(1.0)
    this%THAIR(:,:)     = huge(1.0)
    this%QAIR(:,:)      = huge(1.0)
    this%EAIR(:,:)      = huge(1.0)
    this%RHOAIR(:,:)    = huge(1.0)
    this%FPICE(:,:)     = huge(1.0)
    this%SWDOWN(:,:)    = huge(1.0)
    this%JULIAN         = huge(1.0)
    this%YEARLEN        = huge(1)
    this%SOLAD(:,:,:)   = huge(1.0)
    this%SOLAI(:,:,:)   = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(forcinggrid_type) :: this
    type(namelist_type)     :: namelist

    ! Nothing to do.

  end subroutine InitTransfer

end module ForcingGridType

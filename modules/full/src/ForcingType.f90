module ForcingType

use NamelistRead, only: namelist_type

implicit none
save
private

type, public :: forcing_type

  ! atmospheric inputs (meteorology, chemistry)
  real    :: SFCPRS       ! surface pressure (pa)
  real    :: SFCTMP       ! surface air temperature [K]
  real    :: Q2           ! specific humidity (note: in some Noah-MP versions Q2 is mixing ratio)
  real    :: PRCP         ! total input precipitation[mm/s]
  real    :: PRCPCONV     ! convective precipitation entering  [mm/s]
  real    :: PRCPNONC     ! non-convective precipitation entering [mm/s]
  real    :: PRCPSHCV     ! shallow convective precip entering  [mm/s]
  real    :: PRCPSNOW     ! snow entering land model [mm/s] 
  real    :: PRCPGRPL     ! graupel entering land model [mm/s]
  real    :: PRCPHAIL     ! hail entering land model [mm/s]             
  real    :: SOLDN        ! downward shortwave radiation (w/m2)
  real    :: LWDN         ! atmospheric longwave radiation (w/m2)
  real    :: FOLN         ! foliage nitrogen concentration (%)
  real    :: O2PP         ! atmospheric co2 concentration partial pressure (pa)
  real    :: CO2PP        ! atmospheric o2 concentration partial pressure (pa) 
  real    :: UU           ! wind speed in eastward dir (m/s)  
  real    :: VV           ! wind speed in northward dir (m/s)  
  
  ! surface inputs
  real    :: TBOT         ! bottom condition for soil temperature [K]

  ! outputs
  real    :: UR           ! wind speed at reference height (m/s)
  real    :: THAIR        ! potential temperature (k)
  real    :: QAIR         ! specific humidity (kg/kg) (q2/(1+q2))
  real    :: EAIR         ! vapor pressure air (pa)
  real    :: RHOAIR       ! density air (kg/m3)
  real    :: FPICE        ! fraction of ice in precipitation (-)
  real    :: SWDOWN       ! downward solar filtered by sun angle [w/m2]
  real    :: JULIAN       ! julian day of year
  integer :: YEARLEN      ! year length (days)

  real, allocatable, dimension(:) :: SOLAD  !incoming direct solar radiation (w/m2)
  real, allocatable, dimension(:) :: SOLAI  !incoming diffuse solar radiation (w/m2)

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate        
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type forcing_type

contains   

  subroutine Init(this, namelist)

    class(forcing_type)    :: this
    type(namelist_type) :: namelist

    call this%InitAllocate()
    call this%InitDefault()

  end subroutine Init
  
  subroutine InitAllocate(this)

    class(forcing_type) :: this

    allocate(this%SOLAD (1:2)); this%SOLAD(:) = huge(1.0) 
    allocate(this%SOLAI (1:2)); this%SOLAI(:) = huge(1.0) 

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(forcing_type) :: this

    this%SFCPRS    = huge(1.0)
    this%SFCPRS    = huge(1.0)
    this%SFCTMP    = huge(1.0)
    this%Q2        = huge(1.0)
    this%PRCP      = huge(1.0)
    this%PRCPCONV  = huge(1.0)
    this%PRCPNONC  = huge(1.0)
    this%PRCPSHCV  = huge(1.0)
    this%PRCPSNOW  = huge(1.0)
    this%PRCPGRPL  = huge(1.0)
    this%PRCPHAIL  = huge(1.0)
    this%SOLDN     = huge(1.0)
    this%LWDN      = huge(1.0)  
    this%UU        = huge(1.0)
    this%VV        = huge(1.0)

    this%TBOT      = huge(1.0)

    this%UR        = huge(1.0)
    this%THAIR     = huge(1.0)
    this%QAIR      = huge(1.0)
    this%EAIR      = huge(1.0)
    this%RHOAIR    = huge(1.0)
    this%FPICE     = huge(1.0)
    this%JULIAN    = huge(1.0)
    this%YEARLEN   = huge(1)
    this%FOLN      = huge(1.0)
    this%O2PP      = huge(1.0)
    this%CO2PP     = huge(1.0)
        
  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(forcing_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module ForcingType

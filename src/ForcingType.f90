module ForcingType

use NamelistRead,    only: namelist_type
use ForcingGridType, only: forcinggrid_type
implicit none
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
    procedure, private :: InitTransfer
    procedure, public  :: TransferIn
    procedure, public  :: TransferOut

end type forcing_type

contains   

  subroutine Init(this,namelist,forcinggrid)

    class(forcing_type),    intent(inout) :: this
    type(namelist_type),    intent(in)    :: namelist
    type(forcinggrid_type), intent(in)    :: forcinggrid

    call this%InitAllocate()
    call this%InitDefault()
    call this%InitTransfer(forcinggrid)

  end subroutine Init
  
  subroutine InitAllocate(this)

    class(forcing_type), intent(inout) :: this

    if(.NOT.allocated(this%SOLAD)) allocate(this%SOLAD (2))
    if(.NOT.allocated(this%SOLAI)) allocate(this%SOLAI (2))

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(forcing_type), intent(inout) :: this

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
        
    this%UR        = huge(1.0)     
    this%THAIR     = huge(1.0) 
    this%QAIR      = huge(1.0)   
    this%EAIR      = huge(1.0)    
    this%RHOAIR    = huge(1.0)   
    this%FPICE     = huge(1.0)  
    this%SWDOWN    = huge(1.0)   
    this%JULIAN    = huge(1.0) 
    this%YEARLEN   = huge(1) 
  
    this%SOLAD(:)  = huge(1.0)
    this%SOLAI(:)  = huge(1.0) 

  end subroutine InitDefault

  subroutine InitTransfer(this,forcinggrid)

    class(forcing_type),    intent(inout) :: this
    type(forcinggrid_type), intent(in)    :: forcinggrid

    this%JULIAN = forcinggrid%JULIAN
    this%YEARLEN = forcinggrid%YEARLEN

  end subroutine InitTransfer

  subroutine TransferIn(this, forcinggrid, ix, iy)

    implicit none

    class(forcing_type),    intent(inout) :: this
    type(forcinggrid_type), intent(in)    :: forcinggrid
    integer,                intent(in)    :: ix
    integer,                intent(in)    :: iy

    this%SFCPRS = forcinggrid%SFCPRS(ix,iy)
    this%SFCTMP = forcinggrid%SFCTMP(ix,iy)
    this%Q2 = forcinggrid%Q2(ix,iy)
    this%PRCP = forcinggrid%PRCP(ix,iy)
    this%PRCPCONV = forcinggrid%PRCPCONV(ix,iy)
    this%PRCPNONC = forcinggrid%PRCPNONC(ix,iy)
    this%PRCPSHCV = forcinggrid%PRCPSHCV(ix,iy)
    this%PRCPSNOW = forcinggrid%PRCPSNOW(ix,iy)
    this%PRCPGRPL = forcinggrid%PRCPGRPL(ix,iy)
    this%PRCPHAIL = forcinggrid%PRCPHAIL(ix,iy)            
    this%SOLDN = forcinggrid%SOLDN(ix,iy)
    this%LWDN = forcinggrid%LWDN(ix,iy)
    this%FOLN = forcinggrid%FOLN(ix,iy)
    this%O2PP = forcinggrid%O2PP(ix,iy)
    this%CO2PP = forcinggrid%CO2PP(ix,iy) 
    this%UU = forcinggrid%UU(ix,iy)
    this%VV = forcinggrid%VV(ix,iy)
    this%TBOT = forcinggrid%TBOT(ix,iy)
    this%UR = forcinggrid%UR(ix,iy)
    this%THAIR = forcinggrid%THAIR(ix,iy)
    this%QAIR = forcinggrid%QAIR(ix,iy)
    this%EAIR = forcinggrid%EAIR(ix,iy)
    this%RHOAIR = forcinggrid%RHOAIR(ix,iy)
    this%FPICE = forcinggrid%FPICE(ix,iy)
    this%SWDOWN = forcinggrid%SWDOWN(ix,iy)
    this%SOLAD(:) = forcinggrid%SOLAD(ix,iy,:)
    this%SOLAI(:) = forcinggrid%SOLAI(ix,iy,:)

  end subroutine TransferIn

  subroutine TransferOut(this, forcinggrid, ix, iy)

    implicit none

    class(forcing_type),    intent(in)    :: this
    type(forcinggrid_type), intent(inout) :: forcinggrid
    integer,                intent(in)    :: ix
    integer,                intent(in)    :: iy

    forcinggrid%SFCPRS(ix,iy) = this%SFCPRS
    forcinggrid%SFCTMP(ix,iy) = this%SFCTMP
    forcinggrid%Q2(ix,iy) = this%Q2
    forcinggrid%PRCP(ix,iy) = this%PRCP
    forcinggrid%PRCPCONV(ix,iy) = this%PRCPCONV
    forcinggrid%PRCPNONC(ix,iy) = this%PRCPNONC
    forcinggrid%PRCPSHCV(ix,iy) = this%PRCPSHCV
    forcinggrid%PRCPSNOW(ix,iy) = this%PRCPSNOW
    forcinggrid%PRCPGRPL(ix,iy) = this%PRCPGRPL
    forcinggrid%PRCPHAIL(ix,iy) = this%PRCPHAIL
    forcinggrid%SOLDN(ix,iy) = this%SOLDN
    forcinggrid%LWDN(ix,iy) = this%LWDN
    forcinggrid%FOLN(ix,iy) = this%FOLN
    forcinggrid%O2PP(ix,iy) = this%O2PP
    forcinggrid%CO2PP(ix,iy) = this%CO2PP
    forcinggrid%UU(ix,iy) = this%UU
    forcinggrid%VV(ix,iy) = this%VV
    forcinggrid%TBOT(ix,iy) = this%TBOT
    forcinggrid%UR(ix,iy) = this%UR
    forcinggrid%THAIR(ix,iy) = this%THAIR
    forcinggrid%QAIR(ix,iy) = this%QAIR
    forcinggrid%EAIR(ix,iy) = this%EAIR
    forcinggrid%RHOAIR(ix,iy) = this%RHOAIR
    forcinggrid%FPICE(ix,iy) = this%FPICE
    forcinggrid%SWDOWN(ix,iy) = this%SWDOWN
    forcinggrid%SOLAD(ix,iy,:) = this%SOLAD(:)
    forcinggrid%SOLAI(ix,iy,:) = this%SOLAI(:)

  end subroutine TransferOut


end module ForcingType

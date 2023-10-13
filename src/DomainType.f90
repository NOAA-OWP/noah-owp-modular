module DomainType

  use NamelistRead,   only: namelist_type
  use DomainGridType, only: domaingrid_type
  implicit none
  private
  
  type, public :: domain_type
  
    integer             :: ix                ! i index in grid  
    integer             :: iy                ! j index in grid
    integer             :: n_x               ! 
    integer             :: n_y               !
    real                :: dx
    real                :: dy
    real                :: DT                ! run timestep (s)
    character(len=12)   :: startdate         ! Start date of the model run ( YYYYMMDDHHmm ) 
    character(len=12)   :: enddate           ! End date of the model run ( YYYYMMDDHHmm ) 
    character(len=12)   :: nowdate           ! Current date of the model run ( YYYYMMDDHHmm ) 
    real*8              :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
    real*8              :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
    real*8              :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
    integer             :: itime             ! current integer time step of model run
    integer             :: ntime             ! total number of integer time steps in model run
    double precision    :: time_dbl          ! current time of model run in seconds from beginning
    real                :: lat               ! latitude (°)
    real                :: lon               ! longitude (°)
    real                :: ZREF              ! measurement height of wind speed (m)
    real                :: terrain_slope     ! terrain slope (°)
    real                :: azimuth           ! terrain azimuth or aspect (° clockwise from north)
    integer             :: vegtyp            ! land cover type
    integer             :: croptype          ! crop type
    integer             :: isltyp            ! soil type
    integer             :: IST               ! surface type 1-soil; 2-lake
    real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
    real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
    real, allocatable, dimension(:) :: zsnso   ! depth of snow/soil layer-bottom
    integer                         :: soilcolor

    contains
  
      procedure, public  :: Init         
      procedure, private :: InitAllocate 
      procedure, private :: InitDefault     
      procedure, private :: InitTransfer
      procedure, public  :: TransferIn
      procedure, public  :: TransferOut
  
  end type domain_type
  
  contains   
  
    subroutine Init(this, namelist, domaingrid)
  
      class(domain_type),    intent(inout) :: this
      type(namelist_type),   intent(in)    :: namelist
      type(domaingrid_type), intent(in)    :: domaingrid
  
      call this%InitAllocate(namelist)
      call this%InitDefault()
      call this%InitTransfer(domaingrid)
  
    end subroutine Init
  
    subroutine InitAllocate(this, namelist)
  
      class(domain_type) :: this
      type(namelist_type) :: namelist
  
      associate(nsoil => namelist%nsoil, &
                nsnow => namelist%nsnow)

      if(.NOT.allocated(this%zsoil))  allocate(this%zsoil  (nsoil))
      if(.NOT.allocated(this%dzsnso)) allocate(this%dzsnso (-nsnow+1:nsoil))
      if(.NOT.allocated(this%zsnso))  allocate(this%zsnso  (-nsnow+1:nsoil))
  
      end associate

    end subroutine InitAllocate
  
    subroutine InitDefault(this)
  
      class(domain_type) :: this
  
      this%ix             = huge(1)
      this%iy             = huge(1)
      this%n_x            = huge(1)
      this%n_y            = huge(1)
      this%dt             = huge(1.0)
      this%startdate      = 'EMPTYDATE999'
      this%enddate        = 'EMPTYDATE999'
      this%nowdate        = 'EMPTYDATE999'
      this%start_datetime = huge(1)
      this%end_datetime   = huge(1)
      this%curr_datetime  = huge(1)
      this%itime          = huge(1) 
      this%ntime          = huge(1) 
      this%time_dbl       = huge(1.d0)
      this%lat            = huge(1.0)
      this%lon            = huge(1.0)
      this%terrain_slope  = huge(1.0)
      this%azimuth        = huge(1.0)
      this%ZREF           = huge(1.0)
      this%vegtyp         = huge(1)
      this%croptype       = huge(1)
      this%isltyp         = huge(1)
      this%IST            = huge(1)
      this%soilcolor      = huge(1)
      this%zsoil(:)       = huge(1.0)
      this%dzsnso(:)      = huge(1.0)
      this%zsnso(:)       = huge(1.0)
  
    end subroutine InitDefault
  
    subroutine InitTransfer(this,domaingrid)
  
      class(domain_type),    intent(inout) :: this
      type(domaingrid_type), intent(in)    :: domaingrid
  
      this%DT = domaingrid%DT
      this%dx = domaingrid%dx
      this%dy = domaingrid%dy
      this%n_x = domaingrid%n_x
      this%n_y = domaingrid%n_y
      this%startdate = domaingrid%startdate 
      this%enddate = domaingrid%enddate    
      this%nowdate = domaingrid%nowdate           
      this%start_datetime = domaingrid%start_datetime  
      this%end_datetime = domaingrid%end_datetime  
      this%curr_datetime = domaingrid%sim_datetimes(domaingrid%itime)    
      this%itime = domaingrid%itime          
      this%ntime = domaingrid%ntime          
      this%time_dbl = domaingrid%time_dbl
      this%ZREF = domaingrid%ZREF        
  
    end subroutine InitTransfer

    subroutine TransferIn(this, domaingrid, ix, iy)

      implicit none
  
      class(domain_type),    intent(inout) :: this
      type(domaingrid_type), intent(in)    :: domaingrid
      integer,               intent(in)    :: ix
      integer,               intent(in)    :: iy
  
      this%ix = ix 
      this%iy = iy 
      this%curr_datetime = domaingrid%sim_datetimes(domaingrid%itime)    
      this%lat = domaingrid%lat(ix,iy)  
      this%lon = domaingrid%lon(ix,iy)           
      this%terrain_slope = domaingrid%terrain_slope(ix,iy) 
      this%azimuth = domaingrid%azimuth(ix,iy)          
      this%vegtyp = domaingrid%vegtyp(ix,iy)    
      this%croptype = domaingrid%croptype(ix,iy)        
      this%isltyp = domaingrid%isltyp(ix,iy)       
      this%IST = domaingrid%IST(ix,iy)      
      this%zsoil(:) = domaingrid%zsoil(ix,iy,:)    
      this%dzsnso(:) = domaingrid%dzsnso(ix,iy,:)   
      this%zsnso(:) = domaingrid%zsnso(ix,iy,:)
      this%soilcolor = domaingrid%soilcolor(ix,iy)    
  
    end subroutine

    subroutine TransferOut(this, domaingrid, ix, iy)

      implicit none
  
      class(domain_type),       intent(in) :: this
      type(domaingrid_type), intent(inout) :: domaingrid
      integer, intent(in)                  :: ix
      integer, intent(in)                  :: iy
  
      domaingrid%zsoil(ix,iy,:) = this%zsoil(:) 
      domaingrid%dzsnso(ix,iy,:) = this%dzsnso(:)  
      domaingrid%zsnso(ix,iy,:) = this%zsnso(:)  
      domaingrid%nowdate = this%nowdate 
  
    end subroutine

  end module DomainType
  
  
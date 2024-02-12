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

! standalone forcings
character(len=256)                                :: forcings_dir         ! name of the forcings directory
character(len=256)                                :: forcings_file_prefix ! prefix for forcings files
character(len=256)                                :: forcings_file_name   ! name of the currently read-in forcings file
character(len=7)                                  :: forcing_file_type    ! type of forcing file ( HOURLY, DAILY, MONTHLY, or YEARLY )
integer                                           :: iread
integer                                           :: iread_step
integer                                           :: ncid
integer                                           :: status
integer                                           :: n_x
integer                                           :: n_y
real,allocatable,dimension(:,:,:)                 :: read_UU
real,allocatable,dimension(:,:,:)                 :: read_VV
real,allocatable,dimension(:,:,:)                 :: read_sfctmp
real,allocatable,dimension(:,:,:)                 :: read_sfcprs
real,allocatable,dimension(:,:,:)                 :: read_rhf
real,allocatable,dimension(:,:,:)                 :: read_swrad
real,allocatable,dimension(:,:,:)                 :: read_lwrad
real,allocatable,dimension(:,:,:)                 :: read_pcprate
real,allocatable,dimension(:)                     :: read_time
real*8                                            :: file_min_time
real*8                                            :: file_max_time

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate        
    procedure, private :: InitDefault
    procedure, public  :: InitTransfer     
    procedure, public  :: ReadForcings     
    procedure, public  :: SetForcings    

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

  subroutine InitTransfer(this, namelist, attributes)

    class(forcinggrid_type) :: this
    type(namelist_type)     :: namelist
    type(attributes_type)   :: attributes

    this%forcings_dir         = namelist%forcings_dir
    this%forcings_file_prefix = namelist%forcings_file_prefix
    this%forcings_file_type   = namelist%forcings_file_type
    this%n_x                  = attributes%metadata%n_x
    this%n_y                  = attributes%metadata%n_y

  end subroutine InitTransfer

  subroutine ReadForcings(this,datetime_unix,datetime_str)

    class(forcinggrid_type), intent(inout) :: this
    real*8,intent(in)                      :: datetime_unit                                     ! unix datetime (s since 1970-01-01 00:00:00) ?UTC? 
    character(len=12),intent(in)           :: datetime_str                                      ! character date ( YYYYMMDDHHmm )
    character(len=14)                      :: datetime_long_str                                 ! character date ( YYYYMMDDHHmmss )
    character(len=4)                       :: year_str                                          ! date string
    character(len=2)                       :: month_str,day_str,minute_str,second_str           ! more date strings
    integer                                :: year_int,month_int,day_int,minute_int,second_int  ! date ints
    integer                                :: time_dim_len,ndays
    real*8,allocatable,dimension(:)        :: time_dif
    real*8                                 :: next_datetime_unix

    !---------------------------------------------------------------------
    ! Determine expected file name
    !---------------------------------------------------------------------
    year_str = datetime_str(1:4); month_str = datetime_str(5:6); day_str = datetime_str(7:8); hour_str = datetime_str(9:10)
    select case(this%forcing_file_type)
    case('YEARLY')
      this%forcing_filename = this%forcings_dir//this%forcings_file_prefix//'.'//year_str//'.nc'
    case('MONTHLY')
      this%forcing_filename = this%forcings_dir//this%forcings_file_prefix//'.'//year_str//month_str//'.nc'
    case('DAILY')
      this%forcing_filename = this%forcings_dir//this%forcings_file_prefix//'.'//year_str//month_str//day_str//'.nc'
    case('HOURLY')
      this%forcing_filename = this%forcings_dir//this%forcings_file_prefix//'.'//yea_str//month_str//day_str//hour_str//'.nc'
    case default
      write(*,*) 'ERROR Unrecognized forcing file type ''',trim(this%forcing_file_type),''' -- but must be HOURLY, DAILY, MONTHLY, or YEARLY'; stop ":  ERROR EXIT"
    end select

    !---------------------------------------------------------------------
    ! Determine unix time bounds for file (i.e., file_min_time, file_max_time)
    !---------------------------------------------------------------------
    select case(this%forcing_file_type)
    case('YEARLY')
      date_long(1:4) = year_str; date_long(5:6) = '01'; date_long(7:8) = '01'; date_long(9:10) = '00'; date_long(11:12) = '01'; date_long(13:14) = '01'
      this%file_min_time = date_to_unix (date_long)
      date_long(1:4) = year_str; date_long(5:6) = '12'; date_long(7:8) = '31'; date_long(9:10) = '23'; date_long(11:12) = '59'; date_long(13:14) = '59' 
      this%file_max_time = date_to_unix (date_long)
    case('MONTHLY')
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = '01'; date_long(9:10) = '00'; date_long(11:12) = '01', date_long(13:14) = '01' 
      this%file_min_time = date_to_unix (date_long)
      call unix_to_date (file_min_time+1, year_int, month_int, day_int, hour_int, minute_int, second_int)
      ndays = days_in_month(month_int,year_int,days_int)
      write(day_str,'(i2)') ndays; if(ndays < 10) day(1:1) = '0'
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = day_str; date_long(9:10) = '23'; date_long(11:12) = '59', date_long(13:14) = '59' 
      this%file_max_time = date_to_unix (date_long)
    case('DAILY')
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = day_str; date_long(9:10) = '00'; date_long(11:12) = '01'; date_long(13:14) = '01'
      this%file_min_time = date_to_unix (date_long)
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = day_str; date_long(9:10) = '23'; date_long(11:12) = '59'; date_long(13:14) = '59' 
      this%file_max_time = date_to_unix (date_long)
    case('HOURLY')
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = day_str; date_long(9:10) = hour_str; date_long(11:12) = '01'; date_long(13:14) = '01'
      this%file_min_time = date_to_unix (date_long)
      date_long(1:4) = year_str; date_long(5:6) = month_str; date_long(7:8) = day_str; date_long(9:10) = hour_str; date_long(11:12) = '59'; date_long(13:14) = '59' 
      this%file_max_time = date_to_unix (date_long)
    case default
      write(*,*) 'ERROR Unrecognized forcing file type ''',trim(this%forcing_file_type),''' -- but must be HOURLY, DAILY, MONTHLY, or YEARLY'; stop ":  ERROR EXIT"
    end select

    !---------------------------------------------------------------------
    ! Check that file exists
    !---------------------------------------------------------------------
    inquire(file = trim(this%forcing_filename), exist = lexist)
    if (.not. lexist) then; write(*,*) 'ERROR Could not find forcings file ''',trim(this%forcing_filename),''' for datetime ''',trim(datetime_str),'''';stop ":  ERROR EXIT";endif

    !---------------------------------------------------------------------
    ! Open file
    !---------------------------------------------------------------------
    status = nf90_open(path = trim(this%forcing_filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then; write(*,*) 'ERROR Could not open ''',trim(this%forcing_filename),''' for datetime ''',trim(datetime_str),''''; stop ":  ERROR EXIT"; endif

    !---------------------------------------------------------------------
    ! Read dimension lengths
    !---------------------------------------------------------------------
    ! x
    status = nf90_inq_dimid(ncid,this%name_dim_x,dimid_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_x),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid,dimid_x,len=dim_len_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_x),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    
    ! y
    status = nf90_inq_dimid(ncid,this%name_dim_y,dimid_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_y),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid,dimid_y,len=dim_len_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_y),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    
    ! time
    status = nf90_inq_dimid(ncid,this%name_dim_time,dimid_time)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_time),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid,dimid_time,len=dim_len_time)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_time),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    !---------------------------------------------------------------------
    ! Allocate read arrays
    !---------------------------------------------------------------------
    ! UU (wind speed in eastward direction)
    if(allocated(this%read_UU)) then
      if((size(this%read_UU,1).ne.dim_len_x).or.(size(this%read_UU,2).ne.dim_len_y).or.(size(this%read_UU,3).ne.dim_len_time)) then
        deallocate(this%read_UU)
        allocate(this%read_UU(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_UU = 0
      end if 
    else
      allocate(this%read_UU(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! VV (wind speed in northward direction)
    if(allocated(this%read_VV)) then
      if((size(this%read_VV,1).ne.dim_len_x).or.(size(this%read_VV,2).ne.dim_len_y).or.(size(this%read_VV,3).ne.dim_len_time)) then
        deallocate(this%read_VV)
        allocate(this%read_VV(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_VV = 0
      end if 
    else
      allocate(this%read_VV(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! sfctmp (surface temperature)
    if(allocated(this%read_sfctmp)) then
      if((size(this%read_sfctmp,1).ne.dim_len_x).or.(size(this%read_sfctmp,2).ne.dim_len_y).or.(size(this%read_sfctmp,3).ne.dim_len_time)) then
        deallocate(this%read_sfctmp)
        allocate(this%read_sfctmp(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_sfctmp = 0
      end if 
    else
      allocate(this%read_sfctmp(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! sfcprs (surface pressure)
    if(allocated(this%read_sfcprs)) then
      if((size(this%read_sfcprs,1).ne.dim_len_x).or.(size(this%read_sfcprs,2).ne.dim_len_y).or.(size(this%read_sfcprs,3).ne.dim_len_time)) then
        deallocate(this%read_sfcprs)
        allocate(this%read_sfcprs(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_sfcprs = 0
      end if 
    else
      allocate(this%read_sfcprs(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! swrad
    if(allocated(this%read_swrad)) then
      if((size(this%read_swrad,1).ne.dim_len_x).or.(size(this%read_swrad,2).ne.dim_len_y).or.(size(this%read_swrad,3).ne.dim_len_time)) then
        deallocate(this%read_swrad)
        allocate(this%read_swrad(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_swrad = 0
      end if 
    else
      allocate(this%read_swrad(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! lwrad
    if(allocated(this%read_lwrad)) then
      if((size(this%read_lwrad,1).ne.dim_len_x).or.(size(this%read_lwrad,2).ne.dim_len_y).or.(size(this%read_lwrad,3).ne.dim_len_time)) then
        deallocate(this%read_lwrad)
        allocate(this%read_lwrad(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_lwrad = 0
      end if 
    else
      allocate(this%read_lwrad(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! pcprate (precipitation rate)
    if(allocated(this%read_pcprate)) then
      if((size(this%read_pcprate,1).ne.dim_len_x).or.(size(this%read_pcprate,2).ne.dim_len_y).or.(size(this%read_pcprate,3).ne.dim_len_time)) then
        deallocate(this%read_pcprate)
        allocate(this%read_pcprate(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_pcprate = 0
      end if 
    else
      allocate(this%read_pcprate(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! rhf (relative humidity)
    if(allocated(this%read_rhf)) then
      if((size(this%read_rhf,1).ne.dim_len_x).or.(size(this%read_rhf,2).ne.dim_len_y).or.(size(this%read_rhf,3).ne.dim_len_time)) then
        deallocate(this%read_rhf)
        allocate(this%read_rhf(dim_len_x,dim_len_y,dim_len_time))
      else
        this%read_rhf = 0
      end if 
    else
      allocate(this%read_rhf(dim_len_x,dim_len_y,dim_len_time))
    end if

    ! time
    if(allocated(this%time)) then
      if(size(this%time,1).ne.dim_len_time) then
        deallocate(this%time)
        allocate(this%time(dim_len_time))
      else
        this%time = 0
      end if 
    else
      allocate(this%time(dim_len_time))
    end if

    !---------------------------------------------------------------------
    ! Read into read arrays
    !---------------------------------------------------------------------
    ! UU
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_UU,varid=varid_UU)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_UU),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_UU,values=this%read_UU)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_UU),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! VV
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_VV,varid=varid_VV)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_VV),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_VV,values=this%read_VV)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_VV),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! sfctmp
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_sfctmp,varid=varid_sfctmp)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_sfctmp),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_sfctmp,values=this%read_sfctmp)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_sfctmp),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! sfcprs
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_sfcprs,varid=varid_sfcprs)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_sfcprs),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_sfcprs,values=this%read_sfcprs)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_sfcprs),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! rfh
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_rfh,varid=varid_rfh)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_rfh),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_rfh,values=this%read_rhf)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_rfh),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! swrad
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_swrad,varid=varid_swrad)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_swrad),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_swrad,values=this%read_swrad)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_swrad),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! lwrad
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_lwrad,varid=varid_lwrad)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_lwrad),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_lwrad,values=this%read_lwrad)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_lwrad),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    
    ! pcprate
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_pcprate,varid=varid_pcprate)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_pcprate),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_pcprate,values=this%read_pcprate)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_pcprate),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    ! time
    status = nf90_inq_varid(ncid=ncid,name=this%name_var_time,varid=varid_time)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_var_time),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid=ncid,varid=varid_time,values=this%read_time)
    if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_var_time),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if

    !---------------------------------------------------------------------
    ! Close file
    !---------------------------------------------------------------------
    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then; write(*,*) 'ERROR Unable to close ''',trim(this%forcing_filename),''''; stop ":  ERROR EXIT"; end if

    !---------------------------------------------------------------------
    ! Check x and y dimension lengths against x and y dimension lengths of the domain
    !---------------------------------------------------------------------
    n_x = size(this%n_x,1); n_y = size(this%n_y,2)
    if((dim_len_x.ne.n_x).or.(dim_len_y.ne.n_y)) then; write(*,*) 'ERROR the x and y dimensions in the forcing file ',trim(this%forcings_file_name),' do not match the domain x and y dimensions';stop ":  ERROR EXIT"; end if

    !---------------------------------------------------------------------
    ! Set iread (i.e., the index value of nowdate in read_time)
    !---------------------------------------------------------------------
    if(allocated(time_dif)) then
      if(size(time_dif,1).ne.dim_len_time) then; deallocate(time_dim); allocate(time_dif(dim_len_time)); end if
    end if
    time_dif = abs(this%read_time-datetime_unix)
    if(any(time_dif.le.epsilon(datetime_unix))) then
      this%iread = minloc(time_dif,1)
    else
      write(*,*) 'ERROR Cound not find ''',trim(datetime_str),''' in forcing file ''',trim(this%forcing_filename),''' -- unix time =',datetime_unix; stop ":  ERROR EXIT"
    end if

    !---------------------------------------------------------------------
    ! Set iread_step (i.e., the index value of the next simulation time step in read_time minus iread)
    !---------------------------------------------------------------------
    iread_step = 1                     ! default value
    iread_next = iread + iread_step    ! default value
    next_datetime_unix = datetime_unix + this%dt 
    if(next_datetime_unix.le.this%max_file_datetime) then
      time_dif = abs(this%read_time-datetime)
      if(any(time_dif.le.epsilon(next_datetime_unix))) then
        iread_next = minloc(time_dif,1)
      else
        write(*,*) 'ERROR Cound not find second time step in forcing file ''',trim(this%forcing_filename),''' -- unix time =',datetime_unix; stop ":  ERROR EXIT"
      end if
      iread_step = iread_next - iread
      if(iread_step.lt.1) then; write(*,*) 'ERROR Unable to determine reading time step for ''',trim(this%forcing_filename),''''; stop ":  ERROR EXIT"; end if
    end if

  end subroutine ReadForcings

  subroutine SetForcings(this,datetime_unix,datetime_str)

    class(forcinggrid_type), intent(inout) :: this
    real*8,intent(in)                      :: datetime_unit       ! unix datetime (s since 1970-01-01 00:00:00) ?UTC? 
    character(len=12),intent(in)           :: datetime_str        ! character date ( YYYYMMDDHHmm )

    ! check if curr_datetime is within the unix time bounds of read arrays
    if(datetime_unix > this%max_file_datetime) then
      call this%ReadForcings(datetime_unix,datetime_str)
    end if

    ! check if iread is within read arrays
    if(iread.gt.size(this%read_time,1)) then
      write(*,*) 'ERROR Unable to find datetime ''',trim(datetime_str),''' in forcing file ''',trim(this%forcings_file_name),''' - unix time = ',datetime_unix; stop ":  ERROR EXIT"
    end if

    ! sanity check
    if(abs(this%read_time(iread)-datetime_unix).gt.epsilon(datetime_unix)) then
      write(*,*) 'ERROR Unable to find datetime ''',trim(datetime_str),''' in forcing file ''',trim(this%forcings_file_name),''' - unix time = ',datetime_unix; stop ":  ERROR EXIT"
    end if

    this%UU(:,:)     = this%read_UU(:,:,iread)  ! should iread be first in dimension order to improve performance/copy time lengths?
    this%VV(:,:)     = this%read_VV(:,:,iread)
    this%SFCTMP(:,:) = this%read_SFCTMP(:,:,iread)
    this%Q2(:,:)     = this%read_Q2(:,:,iread)
    this%SFCPRS(:,:) = this%read_SFCPRS(:,:,iread)
    this%SOLDN(:,:)  = this%read_SOLDN(:,:,iread)
    this%LWDN(:,:)   = this%read_LWDN(:,:,iread)
    this%PRCP(:,:)   = this%read_PRCP(:,:,iread)
    this%UU(:,:)     = this%read_UU(:,:,iread)

    ! advance iread
    iread = iread + iread_step

  end subroutine SetForcings

end module ForcingGridType

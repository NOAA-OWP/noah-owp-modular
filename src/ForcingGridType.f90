module ForcingGridType

  use netcdf
  use NamelistRead,   only: namelist_type
  use AttributesType, only: attributes_type
  use DateTimeUtilsModule
  
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
  
  ! variables for standalone forcings
  character(len=256)                                :: forcings_dir            ! name of the forcings directory
  character(len=256)                                :: forcings_file_prefix    ! prefix for forcings files
  character(len=256)                                :: forcings_file_name      ! name of the currently read-in forcings file
  character(len=7)                                  :: forcings_file_type      ! type of forcing file ( HOURLY, DAILY, MONTHLY, or YEARLY )
  integer                                           :: iread                   ! index position for argument date in read arrays
  integer                                           :: iread_step              ! step index size for each time step   
  integer                                           :: n_x                     ! domain n_x 
  integer                                           :: n_y                     ! domain n_y
  real,allocatable,dimension(:,:,:)                 :: read_UU                 ! read-in wind speed in eastward dir forcings values
  real,allocatable,dimension(:,:,:)                 :: read_VV                 ! read-in wind speed in northward dir forcings values
  real,allocatable,dimension(:,:,:)                 :: read_sfctmp             ! read-in surface temperature forcings values
  real,allocatable,dimension(:,:,:)                 :: read_sfcprs             ! read-in surface pressure forcings values
  real,allocatable,dimension(:,:,:)                 :: read_Q2                 ! read-in specific humidity forcings values
  real,allocatable,dimension(:,:,:)                 :: read_swrad              ! read-in short wave radiation forcings values
  real,allocatable,dimension(:,:,:)                 :: read_lwrad              ! read-in long wave radiation forcings values
  real,allocatable,dimension(:,:,:)                 :: read_pcprate            ! read-in precipitation rate forcings values
  real,allocatable,dimension(:)                     :: read_time               ! read-in time values
  real*8                                            :: datetime_file_min       ! minimum expected unix time (minutes) for current forcings file
  real*8                                            :: datetime_file_max       ! maximum expected unix time (minutes) for current forcings file
  character(len=256)                                :: name_forcings_pcprate   ! name of precipitation rate (mm/s) variable in forcings file(s) 
  character(len=256)                                :: name_forcings_sfctmp    ! name of surface temperature (K) variable in forcings file(s)
  character(len=256)                                :: name_forcings_sfcprs    ! name of surface pressure (pa) variable in forcings file(s)
  character(len=256)                                :: name_forcings_UU        ! name of wind speed in eastward dir (m/s) variable in forcings file(s)
  character(len=256)                                :: name_forcings_VV        ! name of wind speed in northward dir (m/s) variable in forcings file(s)
  character(len=256)                                :: name_forcings_swrad     ! name ofdownward shortwave radiation (w/m2) variable in forcings file(s) 
  character(len=256)                                :: name_forcings_lwrad     ! name ofdownward longwave radiation (w/m2) variable in forcings file(s)
  character(len=256)                                :: name_forcings_Q2        ! name of specific humidity (kg/kg) variable in forcings file(s)
  character(len=256)                                :: name_dim_x              ! name of NetCDF 'x' dimension (longitude dimension)
  character(len=256)                                :: name_dim_y              ! name of NetCDF 'y' dimension (latitude dimension)
  character(len=256)                                :: name_dim_time           ! name of NetCDF 'time' dimension (latitude dimension)
  real                                              :: dt                      ! model run timestep (s)

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
  
      this%forcings_dir          = namelist%forcings_dir
      this%forcings_file_prefix  = namelist%forcings_file_prefix
      this%forcings_file_type    = namelist%forcings_file_type
      this%n_x                   = attributes%metadata%n_x
      this%n_y                   = attributes%metadata%n_y
      this%name_forcings_pcprate = namelist%name_forcings_pcprate
      this%name_forcings_sfctmp  = namelist%name_forcings_sfctmp
      this%name_forcings_sfcprs  = namelist%name_forcings_sfcprs
      this%name_forcings_UU      = namelist%name_forcings_UU
      this%name_forcings_VV      = namelist%name_forcings_VV
      this%name_forcings_swrad   = namelist%name_forcings_swrad
      this%name_forcings_lwrad   = namelist%name_forcings_lwrad
      this%name_forcings_Q2      = namelist%name_forcings_Q2
      this%name_dim_x            = namelist%name_dim_x
      this%name_dim_y            = namelist%name_dim_y
      this%name_dim_time         = namelist%name_dim_time
      this%dt                    = namelist%dt
  
    end subroutine InitTransfer
  
    subroutine ReadForcings(this,datetime_unix_minutes,datetime_str)
  
      class(forcinggrid_type), intent(inout) :: this
      real*8,intent(in)                      :: datetime_unix_minutes                                      ! unix datetime (minutes since 1970-01-01 00:00:00) ?UTC? 
      character(len=12),intent(in)           :: datetime_str                                               ! character date ( YYYYMMDDHHmm )
      character(len=14)                      :: datetime_long_str                                          ! character date ( YYYYMMDDHHmmss )
      character(len=4)                       :: year_str                                                   ! date string
      character(len=2)                       :: month_str,day_str,minute_str,hour_str,second_str           ! more date strings
      integer                                :: year_int,month_int,day_int,hour_int,minute_int,second_int  ! date ints
      integer                                :: ncid                                                       ! netcdf file id
      integer                                :: ndays                                                      ! number of days in month
      real*8,allocatable,dimension(:)        :: time_dif                                                   ! difference between given time and time variable values      
      real*8                                 :: next_datetime_unix_minutes                                 ! unix datetime for next time step
      integer                                :: varid_pcprate                                              ! netcdf varid id for precip rate
      integer                                :: varid_sfctmp                                               ! netcdf varid id for surface temp rate
      integer                                :: varid_sfcprs                                               ! netcdf varid id for surface pressure rate
      integer                                :: varid_UU                                                   ! netcdf varid id for wind speed in eastward dir
      integer                                :: varid_VV                                                   ! netcdf varid id for wind speed in northward dir
      integer                                :: varid_swrad                                                ! netcdf varid id for short wave radiation
      integer                                :: varid_lwrad                                                ! netcdf varid id for long wave radiation
      integer                                :: varid_Q2                                                   ! netcdf varid id for specific humidity
      integer                                :: varid_time                                                 ! netcdf varid id for time
      integer                                :: dimid_y                                                    ! netcdf y dimension id
      integer                                :: dimid_x                                                    ! netcdf x dimension id
      integer                                :: dimid_time                                                 ! netcdf time dimension id
      integer                                :: dim_len_y                                                  ! length of y dimension
      integer                                :: dim_len_x                                                  ! length of x dimension
      integer                                :: dim_len_time                                               ! length of time dimension
      integer                                :: status                                                     ! status indicator
      integer                                :: iread_next                                                 ! index read position for next time step
      logical                                :: lexist                                                     ! logical indicator if file exists
  
      !---------------------------------------------------------------------
      ! Determine expected file name
      !---------------------------------------------------------------------
      year_str = datetime_str(1:4); month_str = datetime_str(5:6); day_str = datetime_str(7:8); hour_str = datetime_str(9:10)
      select case(this%forcings_file_type)
      case('YEARLY')
        this%forcings_file_name = trim(this%forcings_dir)//trim(this%forcings_file_prefix)//'.'//year_str//'.nc'
      case('MONTHLY')
        this%forcings_file_name = trim(this%forcings_dir)//trim(this%forcings_file_prefix)//'.'//year_str//month_str//'.nc'
      case('DAILY')
        this%forcings_file_name = trim(this%forcings_dir)//trim(this%forcings_file_prefix)//'.'//year_str//month_str//day_str//'.nc'
      case('HOURLY')
        this%forcings_file_name = trim(this%forcings_dir)//trim(this%forcings_file_prefix)//'.'//year_str//month_str//day_str//hour_str//'.nc'
      case default
        write(*,*) 'ERROR Unrecognized forcing file type ''',trim(this%forcings_file_type),''' -- but must be HOURLY, DAILY, MONTHLY, or YEARLY'; stop ":  ERROR EXIT"
      end select
  
      !---------------------------------------------------------------------
      ! Determine unix time bounds for file (i.e., file_min_time, file_max_time)
      !---------------------------------------------------------------------
      select case(this%forcings_file_type)
      case('YEARLY')
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = '01'; datetime_long_str(7:8) = '01'; datetime_long_str(9:10) = '00'; datetime_long_str(11:12) = '01'; datetime_long_str(13:14) = '01'
        this%datetime_file_min = date_to_unix (datetime_long_str)/60.
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = '12'; datetime_long_str(7:8) = '31'; datetime_long_str(9:10) = '23'; datetime_long_str(11:12) = '59'; datetime_long_str(13:14) = '59' 
        this%datetime_file_max = date_to_unix (datetime_long_str)/60.
      case('MONTHLY')
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = '01'; datetime_long_str(9:10) = '00'; datetime_long_str(11:12) = '01'; datetime_long_str(13:14) = '01' 
        this%datetime_file_min = date_to_unix (datetime_long_str) ! in seconds for call to unix_to_date
        call unix_to_date (this%datetime_file_min, year_int, month_int, day_int, hour_int, minute_int, second_int)
        call days_in_month(month_int,year_int,ndays)
        this%datetime_file_min = this%datetime_file_min/60.       ! change seconds to minutes
        write(day_str,'(i2)') ndays; if(ndays < 10) day_str(1:1) = '0'
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = day_str; datetime_long_str(9:10) = '23'; datetime_long_str(11:12) = '59'; datetime_long_str(13:14) = '59' 
        this%datetime_file_max = date_to_unix (datetime_long_str)/60.
      case('DAILY')
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = day_str; datetime_long_str(9:10) = '00'; datetime_long_str(11:12) = '01'; datetime_long_str(13:14) = '01'
        this%datetime_file_min = date_to_unix (datetime_long_str)/60.
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = day_str; datetime_long_str(9:10) = '23'; datetime_long_str(11:12) = '59'; datetime_long_str(13:14) = '59' 
        this%datetime_file_max = date_to_unix (datetime_long_str)/60.
      case('HOURLY')
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = day_str; datetime_long_str(9:10) = hour_str; datetime_long_str(11:12) = '01'; datetime_long_str(13:14) = '01'
        this%datetime_file_min = date_to_unix (datetime_long_str)/60.
        datetime_long_str(1:4) = year_str; datetime_long_str(5:6) = month_str; datetime_long_str(7:8) = day_str; datetime_long_str(9:10) = hour_str; datetime_long_str(11:12) = '59'; datetime_long_str(13:14) = '59' 
        this%datetime_file_max = date_to_unix (datetime_long_str)/60.
      case default
        write(*,*) 'ERROR Unrecognized forcing file type ''',trim(this%forcings_file_type),''' -- but must be HOURLY, DAILY, MONTHLY, or YEARLY'; stop ":  ERROR EXIT"
      end select
  
      !---------------------------------------------------------------------
      ! Check that file exists
      !---------------------------------------------------------------------
      inquire(file = trim(this%forcings_file_name), exist = lexist)
      if (.not. lexist) then; write(*,*) 'ERROR Could not find forcings file ''',trim(this%forcings_file_name),''' for datetime ''',trim(datetime_str),'''';stop "1:  ERROR EXIT";endif
  
      !---------------------------------------------------------------------
      ! Open file
      !---------------------------------------------------------------------
      status = nf90_open(path = trim(this%forcings_file_name), mode = nf90_nowrite, ncid = ncid)
      if (status /= nf90_noerr) then; write(*,*) 'ERROR Could not open ''',trim(this%forcings_file_name),''' for datetime ''',trim(datetime_str),''''; stop "2:  ERROR EXIT"; endif
  
      !---------------------------------------------------------------------
      ! Read dimension lengths
      !---------------------------------------------------------------------
      ! x
      status = nf90_inq_dimid(ncid,trim(this%name_dim_x),dimid_x)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_x),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_inquire_dimension(ncid,dimid_x,len=dim_len_x)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_x),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      
      ! y
      status = nf90_inq_dimid(ncid,trim(this%name_dim_y),dimid_y)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_y),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_inquire_dimension(ncid,dimid_y,len=dim_len_y)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_y),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      
      ! time
      status = nf90_inq_dimid(ncid,trim(this%name_dim_time),dimid_time)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find dimension ''',trim(this%name_dim_time),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_inquire_dimension(ncid,dimid_time,len=dim_len_time)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read length of dimension ''',trim(this%name_dim_time),''' in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      !---------------------------------------------------------------------
      ! Allocate read arrays
      !---------------------------------------------------------------------
      ! UU
      if(allocated(this%read_UU)) then
        if((size(this%read_UU,1).ne.dim_len_x).or.(size(this%read_UU,2).ne.dim_len_y).or.(size(this%read_UU,3).ne.dim_len_time)) then
          deallocate(this%read_UU)
          allocate(this%read_UU(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_UU = 0
        end if 
      else
        allocate(this%read_UU(dim_len_x,dim_len_y,dim_len_time))
        this%read_UU = 0
      end if
  
      ! VV 
      if(allocated(this%read_VV)) then
        if((size(this%read_VV,1).ne.dim_len_x).or.(size(this%read_VV,2).ne.dim_len_y).or.(size(this%read_VV,3).ne.dim_len_time)) then
          deallocate(this%read_VV)
          allocate(this%read_VV(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_VV = 0
        end if 
      else
        allocate(this%read_VV(dim_len_x,dim_len_y,dim_len_time))
        this%read_VV = 0
      end if
  
      ! sfctmp
      if(allocated(this%read_sfctmp)) then
        if((size(this%read_sfctmp,1).ne.dim_len_x).or.(size(this%read_sfctmp,2).ne.dim_len_y).or.(size(this%read_sfctmp,3).ne.dim_len_time)) then
          deallocate(this%read_sfctmp)
          allocate(this%read_sfctmp(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_sfctmp = 0
        end if 
      else
        allocate(this%read_sfctmp(dim_len_x,dim_len_y,dim_len_time))
        this%read_sfctmp = 0
      end if
  
      ! sfcprs
      if(allocated(this%read_sfcprs)) then
        if((size(this%read_sfcprs,1).ne.dim_len_x).or.(size(this%read_sfcprs,2).ne.dim_len_y).or.(size(this%read_sfcprs,3).ne.dim_len_time)) then
          deallocate(this%read_sfcprs)
          allocate(this%read_sfcprs(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_sfcprs = 0
        end if 
      else
        allocate(this%read_sfcprs(dim_len_x,dim_len_y,dim_len_time))
        this%read_sfcprs = 0
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
        this%read_swrad = 0
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
        this%read_lwrad = 0
      end if
  
      ! pcprate
      if(allocated(this%read_pcprate)) then
        if((size(this%read_pcprate,1).ne.dim_len_x).or.(size(this%read_pcprate,2).ne.dim_len_y).or.(size(this%read_pcprate,3).ne.dim_len_time)) then
          deallocate(this%read_pcprate)
          allocate(this%read_pcprate(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_pcprate = 0
        end if 
      else
        allocate(this%read_pcprate(dim_len_x,dim_len_y,dim_len_time))
        this%read_pcprate = 0
      end if
  
      ! Q2
      if(allocated(this%read_Q2)) then
        if((size(this%read_Q2,1).ne.dim_len_x).or.(size(this%read_Q2,2).ne.dim_len_y).or.(size(this%read_Q2,3).ne.dim_len_time)) then
          deallocate(this%read_Q2)
          allocate(this%read_Q2(dim_len_x,dim_len_y,dim_len_time))
        else
          this%read_Q2 = 0
        end if 
      else
        allocate(this%read_Q2(dim_len_x,dim_len_y,dim_len_time))
        this%read_Q2 = 0
      end if
  
      ! time
      if(allocated(this%read_time)) then
        if(size(this%read_time,1).ne.dim_len_time) then
          deallocate(this%read_time)
          allocate(this%read_time(dim_len_time))
        else
          this%read_time = 0
        end if 
      else
        allocate(this%read_time(dim_len_time))
        this%read_time = 0
      end if
  
      !---------------------------------------------------------------------
      ! Read into read arrays
      !---------------------------------------------------------------------
      ! UU
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_UU,varid=varid_UU)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_UU),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_UU,values=this%read_UU)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_UU),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! VV
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_VV,varid=varid_VV)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_VV),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_VV,values=this%read_VV)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_VV),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! sfctmp
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_sfctmp,varid=varid_sfctmp)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_sfctmp),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_sfctmp,values=this%read_sfctmp)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_sfctmp),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! sfcprs
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_sfcprs,varid=varid_sfcprs)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_sfcprs),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_sfcprs,values=this%read_sfcprs)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_sfcprs),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! Q2
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_Q2,varid=varid_Q2)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_Q2),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_Q2,values=this%read_Q2)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_Q2),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! swrad
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_swrad,varid=varid_swrad)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_swrad),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_swrad,values=this%read_swrad)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_swrad),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! lwrad
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_lwrad,varid=varid_lwrad)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_lwrad),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_lwrad,values=this%read_lwrad)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_lwrad),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      
      ! pcprate
      status = nf90_inq_varid(ncid=ncid,name=this%name_forcings_pcprate,varid=varid_pcprate)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_forcings_pcprate),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_pcprate,values=this%read_pcprate)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_forcings_pcprate),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      ! time
      status = nf90_inq_varid(ncid=ncid,name=this%name_dim_time,varid=varid_time)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to find ''',trim(this%name_dim_time),''' variable in forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
      status = nf90_get_var(ncid=ncid,varid=varid_time,values=this%read_time)
      if (status .ne. nf90_noerr) then; write(*,*) 'ERROR Unable to read ''',trim(this%name_dim_time),''' variable from forcing file ''',trim(this%forcings_file_name),'''';  stop ":  ERROR EXIT"; end if
  
      !---------------------------------------------------------------------
      ! Close file
      !---------------------------------------------------------------------
      status = nf90_close(ncid = ncid)
      if (status /= nf90_noerr) then; write(*,*) 'ERROR Unable to close ''',trim(this%forcings_file_name),''''; stop ":  ERROR EXIT"; end if
  
      !---------------------------------------------------------------------
      ! Check x and y dimension lengths against x and y dimension lengths of the domain
      !---------------------------------------------------------------------
      if((dim_len_x.ne.this%n_x).or.(dim_len_y.ne.this%n_y)) then; write(*,*) 'ERROR the x and y dimensions in the forcing file ',trim(this%forcings_file_name),' do not match the domain x and y dimensions';stop ":  ERROR EXIT"; end if
  
      !---------------------------------------------------------------------
      ! Set iread (i.e., the index value of nowdate in read_time)
      !---------------------------------------------------------------------
      if(allocated(time_dif)) then
        if(size(time_dif,1).ne.dim_len_time) then; deallocate(time_dif); allocate(time_dif(dim_len_time)); end if
      end if
      time_dif = abs(this%read_time-datetime_unix_minutes)
      if(any(time_dif.le.epsilon(datetime_unix_minutes))) then
        this%iread = minloc(time_dif,1)
      else
        write(*,*) 'ERROR Cound not find ''',trim(datetime_str),''' in forcing file ''',trim(this%forcings_file_name),''' -- unix time =',datetime_unix_minutes; stop ":55  ERROR EXIT"
      end if
  
      !---------------------------------------------------------------------
      ! Set iread_step (i.e., the index value of the next simulation time step in read_time minus iread -- cannot assume 1 because dt resolution of the file may be higher than domaingrid%dt)
      !---------------------------------------------------------------------
      this%iread_step = 1 ! default value
      next_datetime_unix_minutes = datetime_unix_minutes + (this%dt/60.)
      if(next_datetime_unix_minutes.le.this%datetime_file_max) then
        time_dif = abs(this%read_time-next_datetime_unix_minutes)
        iread_next = 0
        if(any(time_dif.le.epsilon(next_datetime_unix_minutes))) then
          iread_next = minloc(time_dif,1)
        else
          write(*,*) 'ERROR Cound not find second time step in forcing file ''',trim(this%forcings_file_name),''' -- unix time =',next_datetime_unix_minutes; stop ":  ERROR EXIT"
        end if
        this%iread_step = iread_next - this%iread
        if(this%iread_step.lt.1) then; write(*,*) 'ERROR Unable to determine reading time step for ''',trim(this%forcings_file_name),''''; stop ":  ERROR EXIT"; end if
      end if
  
    end subroutine ReadForcings
  
    subroutine SetForcings(this,datetime_unix_minutes,datetime_str)
  
      class(forcinggrid_type), intent(inout) :: this
      real*8,intent(in)                      :: datetime_unix_minutes  ! unix datetime (minutes since 1970-01-01 00:00:00) ?UTC? 
      character(len=12),intent(in)           :: datetime_str           ! character date ( YYYYMMDDHHmm )
  
      ! check if curr_datetime is within the unix time bounds of read arrays
      if(datetime_unix_minutes.gt.this%datetime_file_max) then
        call this%ReadForcings(datetime_unix_minutes,datetime_str)
      end if
  
      ! check if iread is within read arrays
      if(this%iread.gt.size(this%read_time,1)) then
        write(*,*) 'ERROR Unable to find datetime ''',trim(datetime_str),''' in forcing file ''',trim(this%forcings_file_name),''' - unix time = ',datetime_unix_minutes; stop ":  ERROR EXIT"
      end if
  
      ! sanity check
      if(abs(this%read_time(this%iread)-datetime_unix_minutes).gt.epsilon(datetime_unix_minutes)) then
        write(*,*) 'ERROR model time (date = ',trim(datetime_str),', unix [minutes] = ',datetime_unix_minutes,') does not match forcings file time (unix [minutes]',this%read_time(this%iread),')'; stop ":  ERROR EXIT"
      end if
  
      ! transfer forcings values
      this%UU(:,:)     = this%read_UU(:,:,this%iread)  ! should iread be first in dimension order to improve performance/copy time lengths?
      this%VV(:,:)     = this%read_VV(:,:,this%iread)
      this%SFCTMP(:,:) = this%read_sfctmp(:,:,this%iread)
      this%Q2(:,:)     = this%read_Q2(:,:,this%iread)
      this%SFCPRS(:,:) = this%read_sfcprs(:,:,this%iread)
      this%SOLDN(:,:)  = this%read_swrad(:,:,this%iread)
      this%LWDN(:,:)   = this%read_lwrad(:,:,this%iread)
      this%PRCP(:,:)   = this%read_pcprate(:,:,this%iread)
  
      ! advance iread
      this%iread = this%iread + this%iread_step
  
    end subroutine SetForcings
  
  end module ForcingGridType
  
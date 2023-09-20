module NetCDFVarsType

  use netcdf
  use NamelistRead, only: namelist_type

  implicit none
  save
  integer, parameter :: max_file_name_length = 512
  integer, parameter :: max_var_name_length  = 256

  type, public :: netcdf2dvar_type
  
    character(len=max_var_name_length) :: name

  end type

  type, extends(netcdf2dvar_type) :: netcdf2dvarINT_type

    integer,allocatable,dimension(:,:) :: data

  end type

  type, extends(netcdf2dvar_type) :: netcdf2dvarREAL_type

    real,allocatable,dimension(:,:) :: data
    
  end type

  type, public :: netcdf2dmetadata_type
    
    character(len=max_var_name_length)  :: name_att_dx    ! Name of 'dx' (x dimension spacing) attribute which must be assoicated with longitude NetCDF variable    
    character(len=max_var_name_length)  :: name_att_dy    ! Name of 'dy' (y dimension spacing) attribute which must be assoicated with latitude NetCDF variable
    character(len=max_var_name_length)  :: name_dim_x     ! Name of 'x' variable and dimension (i.e. name is assumed to be shared by the longitude NetCDF dimension and variable)
    character(len=max_var_name_length)  :: name_dim_y     ! Name of 'y' variable and dimension (i.e. name is assumed to be shared by the latitude NetCDF dimension and variable)
    integer                             :: n_x            ! Cell count in x dimension (longitude dimension)
    integer                             :: n_y            ! Cell count in y dimension (latitude dimension)
    real                                :: dx             ! Spacing of x dimension (longitude dimension)
    real                                :: dy             ! Spacing of y dimension (latitude dimension)
    integer                             :: dimid_x        ! NetCDF ID number for x dimension
    integer                             :: dimid_y        ! NetCDF ID number for y dimension
    integer                             :: integerMissing 
    real                                :: realMissing

  contains 

    procedure, public  :: InitTransfer

  end type

  type, public :: netcdfvars_type

    type(netcdf2dmetadata_type)           :: metadata    ! Populated via call to netcdfvars_type%ReadVar
    type(netcdf2dvarINT_type)             :: vegtyp      ! Populated via call to netcdfvars_type%ReadVar
    type(netcdf2dvarINT_type)             :: isltyp      ! Populated via call to netcdfvars_type%ReadVar
    type(netcdf2dvarINT_type)             :: soilcolor   ! Populated via call to netcdfvars_type%ReadVar
    type(netcdf2dvarREAL_type)            :: slope       ! Populated via call to netcdfvars_type%ReadVar
    type(netcdf2dvarREAL_type)            :: azimuth     ! Populated via call to netcdfvars_type%ReadVar
    real,allocatable,dimension(:)         :: lat         ! Populated via call to netcdfvars_type%ReadSpatial
    real,allocatable,dimension(:)         :: lon         ! Populated via call to netcdfvars_type%ReadSpatial
    character(len=max_file_name_length)   :: filename    ! NetCDF file name
    integer                               :: ncid        ! NetCDF file ID

  contains

    procedure, public     :: Init
    procedure, private    :: OpenNetcdf
    procedure, private    :: CloseNetcdf
    procedure, private    :: ReadSpatial
    procedure, private    :: ReadVar2D

  end type

  contains 

  subroutine Init(this,namelist)

    class(netcdfvars_type), intent(inout) :: this
    type(namelist_type),    intent(in)    :: namelist

    !----------------------------------------------------------------------------
    ! Transfer values
    !----------------------------------------------------------------------------
    this%filename = namelist%netcdfin_filename
    call this%metadata%InitTransfer(namelist)

    !----------------------------------------------------------------------------
    ! Open Netcdf file
    !----------------------------------------------------------------------------
    call this%OpenNetcdf()

    !----------------------------------------------------------------------------
    ! Read spatial information from file
    !----------------------------------------------------------------------------
    call this%ReadSpatial()

    !----------------------------------------------------------------------------
    ! Name the variables
    !----------------------------------------------------------------------------
    this%vegtyp%name    = "vegtyp"      ! namelist%name_var_vegtyp TODO: read-in from namelist.input
    this%isltyp%name    = "isltyp"      ! namelist%name_var_soilcolor TODO: read-in from namelist.input
    this%soilcolor%name = "soilcolor"   ! namelist%name_var_soilcolor TODO: read-in from namelist.input
    this%slope%name     = "slope"       ! namelist%name_var_slope TODO: read-in from namelist.input
    this%azimuth%name   = "azimuth"     ! namelist%name_var_azimuth TODO: read-in from namelist.input
          
    !----------------------------------------------------------------------------
    ! Read variables
    !----------------------------------------------------------------------------
    call this%ReadVar2D(this%vegtyp)
    call this%ReadVar2D(this%isltyp)
    call this%ReadVar2D(this%soilcolor)
    call this%ReadVar2D(this%slope)
    call this%ReadVar2D(this%azimuth)

    !----------------------------------------------------------------------------
    ! Close the Netcdf file
    !----------------------------------------------------------------------------
    call this%CloseNetcdf()

  end subroutine

  subroutine OpenNetcdf(this)

    class(netcdfvars_type), intent(inout) :: this
    logical                               :: lexist
    integer                               :: status

    associate(filename => this%filename, &
              ncid     => this%ncid)

    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

    end associate

  end subroutine

  subroutine CloseNetcdf(this)

    class(netcdfvars_type), intent(inout) :: this
    integer                               :: status

    associate(filename => this%filename, &
              ncid     => this%ncid)

    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine

  subroutine ReadSpatial(this)

    class(netcdfvars_type),intent(inout) :: this
    integer                              :: status
    integer                              :: ix, iy
    logical                              :: lexist
    integer                              :: read_nx     ! read-in n_x value
    integer                              :: read_ny     ! read-in n_y value
    real                                 :: read_dx     ! read-in dx value
    real                                 :: read_dy     ! read-in dy value
    real,allocatable,dimension(:)        :: read_lon    ! read-in longitude values along x dimensional grid edge
    real,allocatable,dimension(:)        :: read_lat    ! read-in latitude values along y dimensional grid edge
    integer                              :: varid_x     ! netcdf variable id for netcdf variable holding x dim coordinates (longitudes) for uniform_rectilinear grid
    integer                              :: varid_y     ! netcdf variable id for netcdf variable holding y dim coordinates (latitudes) for uniform_rectilinear grid
    
    associate(ncid           => this%ncid,                     &
              name_att_dx    => this%metadata%name_att_dx,     & 
              name_att_dy    => this%metadata%name_att_dy,     & 
              name_dim_x     => this%metadata%name_dim_x,      & 
              name_dim_y     => this%metadata%name_dim_y,      & 
              dimid_x        => this%metadata%dimid_x,         & 
              dimid_y        => this%metadata%dimid_y,         & 
              filename       => this%filename,                 &
              integerMissing => this%metadata%integerMissing,  &
              realMissing    => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Initialize to missing values
    !----------------------------------------------------------------------------
    read_nx = integerMissing
    read_ny = integerMissing
    read_dx = realMissing
    read_dy = realMissing
    varid_x = integerMissing
    varid_y = integerMissing
    dimid_x = integerMissing
    dimid_y = integerMissing

    !----------------------------------------------------------------------------
    ! Read n_x and n_y
    !----------------------------------------------------------------------------
    status = nf90_inq_dimid(ncid = ncid, name = trim(name_dim_x), dimid = dimid_x)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find dimension ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inq_dimid(ncid = ncid, name = trim(name_dim_y), dimid = dimid_y)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find dimension ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_x, len=read_nx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_x),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_y, len=read_ny)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_y),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Read dx and dy
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = trim(name_dim_x), varid = varid_x)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find variable ''',trim(name_dim_x),''' in ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_dim_y), varid = varid_y)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find variable ''',trim(name_dim_y),''' in ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_attribute(ncid = ncid, varid = varid_x, name = name_att_dx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find attribute ''',trim(name_att_dx),''' with variable ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inquire_attribute(ncid = ncid, varid = varid_y, name = name_att_dy)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find attribute ''',trim(name_att_dy),''' with variable ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid = ncid, varid = varid_x, name = name_att_dx, values = read_dx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read attribute ''',trim(name_att_dx),''' with variable ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid = ncid, varid = varid_y, name = name_att_dy, values = read_dy)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read attribute ''',trim(name_att_dy),''' with variable ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Allocate local arrays to hold lat and lon
    !----------------------------------------------------------------------------
    allocate(read_lon(read_nx))
    allocate(read_lat(read_ny))
    allocate(this%lon(read_nx))
    allocate(this%lat(read_ny))

    !----------------------------------------------------------------------------
    ! Read latitude and longitude values
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid, varid = varid_x, values = read_lon)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_x),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid = ncid, varid = varid_y, values = read_lat)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_y),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Transfer values
    !----------------------------------------------------------------------------
    if(read_nx /= integerMissing)  then; this%metadata%n_x = read_nx; else; write(*,*)  'ERROR : problem reading x-dimension resolution from ''',trim(filename),''''; stop; end if
    if(read_ny /= integerMissing)  then; this%metadata%n_y = read_ny; else; write(*,*)  'ERROR : problem reading y-dimension resolution from ''',trim(filename),''''; stop; end if
    if(read_dx /= realMissing)     then; this%metadata%dx  = read_dx; else; write(*,*)  'ERROR : problem reading x-dimension spacing from ''', trim(filename),''''; stop; end if
    if(read_dy /= realMissing)     then; this%metadata%dy  = read_dy; else; write(*,*)  'ERROR : problem reading y-dimension spacing from ''', trim(filename),''''; stop; end if
    if(read_lon(1) /= realMissing) then; this%lon(:)       = read_lon; else; write(*,*) 'ERROR : problem reading lon variable from ''',trim(filename),''''; stop; end if
    if(read_lat(1) /= realMissing) then; this%lat(:)       = read_lat; else; write(*,*) 'ERROR : problem reading lat variable from ''',trim(filename),''''; stop; end if

    end associate

  end subroutine ReadSpatial

  subroutine ReadVar2D(this,netcdf2dvar)

    class(netcdfvars_type),    intent(in)    :: this
    class(netcdf2dvar_type), intent(inout)   :: netcdf2dvar   
    integer                                  :: varid    
    integer                                  :: status
    integer                                  :: ndims
    integer,dimension(2)                     :: dimids
  
    associate(ncid           => this%ncid,                    & 
              n_x            => this%metadata%n_x,            &
              n_y            => this%metadata%n_y,            &
              varname        => netcdf2dvar%name,             &
              filename       => this%filename,                &
              integerMissing => this%metadata%integerMissing, &
              realMissing    => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Allocate array to be populated and set to missing value
    !----------------------------------------------------------------------------
    select type (netcdf2dvar)
    type is (netcdf2dvarINT_type)
      if(allocated(netcdf2dvar%data)) deallocate(netcdf2dvar%data)     ! Note that netcdf2dvar%data cannot be accessed outside the select type block. I tried and failed.
      allocate(netcdf2dvar%data(this%metadata%n_x,this%metadata%n_y))
      netcdf2dvar%data(:,:) = integerMissing
    type is (netcdf2dvarREAL_type)
      if(allocated(netcdf2dvar%data)) deallocate(netcdf2dvar%data)
      allocate(netcdf2dvar%data(this%metadata%n_x,this%metadata%n_y))
      netcdf2dvar%data(:,:) = realMissing
    end select

    !----------------------------------------------------------------------------
    ! Get NetCDF variable ID 
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = trim(varname), varid = varid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(varname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Check that variable dimensions are as expected
    !----------------------------------------------------------------------------
    status = nf90_inquire_variable(ncid = ncid, varid = varid, ndims = ndims)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get the number of dimensions for variable ''',trim(varname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
    if (ndims.ne.2) then
      write(*,*) 'The variable ''',trim(varname),''' in ''',trim(filename),''' has ',ndims,' dimensions but should have 2'
      stop ":  ERROR EXIT"
    end if
    status = nf90_inquire_variable(ncid = ncid, varid = varid, dimids = dimids)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get dimension IDs for variable ''',trim(varname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
    if(.NOT.(ANY(dimids == this%metadata%dimid_x)).or..NOT.(ANY(dimids == this%metadata%dimid_y))) then
      write(*,*) 'The dimensions of variable ''',trim(varname),''' in ''',trim(filename),''' do not match name_dim_x and name_dim_y as provided in namelist.input'
      stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Read from NetCDF file and check for missing values
    !----------------------------------------------------------------------------
    select type (netcdf2dvar)
    type is (netcdf2dvarINT_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = netcdf2dvar%data)
      if (status /= nf90_noerr) then
        write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"
      end if
      if(netcdf2dvar%data(1,1) == integerMissing) then
        write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop
      end if
    type is (netcdf2dvarREAL_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = netcdf2dvar%data)
      if (status /= nf90_noerr) then
        write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"
      end if
      if(netcdf2dvar%data(1,1) == realMissing) then
        write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop
      end if
    end select
  
    end associate
  
  end subroutine ReadVar2D

  subroutine InitTransfer(this,namelist)

    class(netcdf2dmetadata_type), intent(inout) :: this
    type(namelist_type),          intent(in)    :: namelist 

    this%name_dim_x         = "Longitude"                    ! namelist%name_dim_x TODO: read-in from namelist.input
    this%name_dim_y         = "Latitude"                     ! namelist%name_dim_y TODO: read-in from namelist.input
    this%name_att_dx        = "dx"                           ! namelist%name_att_dx TODO: read-in from namelist.input
    this%name_att_dy        = "dy"                           ! namelist%name_att_dy TODO: read-in from namelist.input
    this%n_x                = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%n_y                = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%dx                 = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%dy                 = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%dimid_x            = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%dimid_y            = -1                             ! set in netcdfvars_type%ReadSpatial 
    this%integerMissing     = namelist%integerMissing
    this%realMissing        = namelist%realMissing

  end subroutine

end module NetCDFVarsType
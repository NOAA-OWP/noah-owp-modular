module NetCDFVarsType

  use netcdf
  use NamelistRead, only: namelist_type

  implicit none
  save
  integer, parameter :: max_file_name_length = 512
  integer, parameter :: max_var_name_length = 256

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
    
    character(len=max_file_name_length) :: filename
    integer                             :: ncid
    character(len=max_var_name_length)  :: name_att_dx
    character(len=max_var_name_length)  :: name_att_dy
    character(len=max_var_name_length)  :: name_dim_x
    character(len=max_var_name_length)  :: name_dim_y
    character(len=max_var_name_length)  :: name_var_vegtyp
    character(len=max_var_name_length)  :: name_var_isltyp
    character(len=max_var_name_length)  :: name_var_soilcolor
    character(len=max_var_name_length)  :: name_var_slope
    character(len=max_var_name_length)  :: name_var_azimuth
    integer                             :: n_x
    integer                             :: n_y
    real                                :: dx
    real                                :: dy 
    integer                             :: dimid_x   
    integer                             :: dimid_y 
    integer                             :: integerMissing
    real                                :: realMissing

  end type

  type, public :: netcdfvars_type

    type(netcdf2dmetadata_type)   :: metadata
    type(netcdf2dvarINT_type)     :: vegtyp
    type(netcdf2dvarINT_type)     :: isltyp
    type(netcdf2dvarINT_type)     :: soilcolor
    type(netcdf2dvarREAL_type)    :: slope
    type(netcdf2dvarREAL_type)    :: azimuth
    real,allocatable,dimension(:) :: lat
    real,allocatable,dimension(:) :: lon

  contains

    procedure, public     :: Init
    procedure, public     :: OpenNetcdf
    procedure, public     :: CloseNetcdf
    procedure, private    :: ReadSpatial
    procedure, pass(this) :: ReadVarINT
    procedure, pass(this) :: ReadVarREAL
    generic,   public     :: ReadVar => ReadVarINT, ReadVarREAL

  end type

  contains 

  subroutine Init(this,namelist)

    class(netcdfvars_type), intent(inout) :: this
    type(namelist_type),    intent(in)    :: namelist
    integer                               :: ncid
    integer                               :: status
    logical                               :: lexist

    !----------------------------------------------------------------------------
    ! Transfer values
    !----------------------------------------------------------------------------
    this%metadata%filename           = namelist%netcdfinput_filename
    this%metadata%ncid               = -1
    this%metadata%name_dim_x         = "Longitude" ! namelist%name_dim_x TODO: read-in from namelist.input
    this%metadata%name_dim_y         = "Latitude"  ! namelist%name_dim_y
    this%metadata%name_att_dx        = "dx"        ! namelist%name_att_dx 
    this%metadata%name_att_dy        = "dy"        ! namelist%name_att_dy 
    this%metadata%name_var_vegtyp    = "vegtyp"    ! namelist%name_var_vegtyp
    this%metadata%name_var_isltyp    = "isltyp"    ! namelist%name_var_soilcolor
    this%metadata%name_var_soilcolor = "soilcolor" ! namelist%name_var_soilcolor
    this%metadata%name_var_slope     = "slope"     ! namelist%name_var_slope
    this%metadata%name_var_azimuth   = "azimuth"   ! namelist%name_var_azimuth
    this%metadata%n_x                = -1
    this%metadata%n_y                = -1
    this%metadata%dx                 = -1
    this%metadata%dy                 = -1
    this%metadata%dimid_x            = -1
    this%metadata%dimid_y            = -1
    this%metadata%integerMissing     = namelist%integerMissing
    this%metadata%realMissing        = namelist%realMissing

    !----------------------------------------------------------------------------
    ! Open Netcdf file
    !----------------------------------------------------------------------------
    call this%OpenNetcdf()

    !----------------------------------------------------------------------------
    ! Read spatial information
    !----------------------------------------------------------------------------
    call this%ReadSpatial()

    !----------------------------------------------------------------------------
    ! Name variables
    !----------------------------------------------------------------------------
    this%vegtyp%name    = "vegtyp"     ! namelist%name_var_vegtyp,    & TODO: read-in from namelist.input
    this%isltyp%name    = "isltyp"     ! namelist%name_var_isltyp,    &
    this%soilcolor%name = "soilcolor"  ! namelist%name_var_soilcolor, &
    this%slope%name     = "slope"      ! namelist%name_var_slope,     &
    this%azimuth%name   = "azimuth"    ! namelist%name_var_azimuth,   &
          
    !----------------------------------------------------------------------------
    ! Allocate variables
    !----------------------------------------------------------------------------
    allocate(this%vegtyp%data(this%metadata%n_x,this%metadata%n_y))
    allocate(this%isltyp%data(this%metadata%n_x,this%metadata%n_y))
    allocate(this%soilcolor%data(this%metadata%n_x,this%metadata%n_y))
    allocate(this%slope%data(this%metadata%n_x,this%metadata%n_y))
    allocate(this%azimuth%data(this%metadata%n_x,this%metadata%n_y))

    !----------------------------------------------------------------------------
    ! Set variables to missing values
    !----------------------------------------------------------------------------
    this%vegtyp%data(:,:)    = this%metadata%integerMissing
    this%isltyp%data(:,:)    = this%metadata%integerMissing
    this%soilcolor%data(:,:) = this%metadata%integerMissing
    this%slope%data(:,:)     = this%metadata%realMissing
    this%azimuth%data(:,:)   = this%metadata%realMissing

    !----------------------------------------------------------------------------
    ! Read variables
    !----------------------------------------------------------------------------
    call this%ReadVar(this%vegtyp)
    call this%ReadVar(this%isltyp)
    call this%ReadVar(this%soilcolor)
    call this%ReadVar(this%slope)
    call this%ReadVar(this%azimuth)

    !----------------------------------------------------------------------------
    ! Close Netcdf file
    !----------------------------------------------------------------------------
    call this%CloseNetcdf()

  end subroutine

  subroutine OpenNetcdf(this)

    class(netcdfvars_type), intent(inout) :: this
    logical                               :: lexist
    integer                               :: status

    associate(filename => this%metadata%filename, &
              ncid     => this%metadata%ncid)

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

    associate(filename => this%metadata%filename, &
              ncid     => this%metadata%ncid)

    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine

  subroutine ReadSpatial(this)

    class(netcdfvars_type)         :: this
    integer                        :: status
    integer                        :: ix, iy
    logical                        :: lexist
    integer                        :: read_nx     ! read-in n_x value
    integer                        :: read_ny     ! read-in n_y value
    real                           :: read_dx     ! read-in dx value
    real                           :: read_dy     ! read-in dy value
    real,allocatable,dimension(:)  :: read_lon    ! read-in longitude values along x dimensional grid edge
    real,allocatable,dimension(:)  :: read_lat    ! read-in latitude values along y dimensional grid edge
    integer                        :: varid_x     ! netcdf variable id for netcdf variable holding x dim coordinates (longitudes) for uniform_rectilinear grid
    integer                        :: varid_y     ! netcdf variable id for netcdf variable holding y dim coordinates (latitudes) for uniform_rectilinear grid
    integer                        :: dimid_x     ! netcdf dimension id for netcdf dimension for x dimension (longitude dimension)
    integer                        :: dimid_y     ! netcdf dimension id for netcdf dimension for y dimension (latitude dimension)
    
    associate(ncid           => this%metadata%ncid,            &
              name_att_dx    => this%metadata%name_att_dx,     & 
              name_att_dy    => this%metadata%name_att_dy,     & 
              name_dim_x     => this%metadata%name_dim_x,      & 
              name_dim_y     => this%metadata%name_dim_y,      & 
              filename       => this%metadata%filename,       &
              integerMissing => this%metadata%integerMissing, &
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

  subroutine ReadVarINT(this,netcdf2dvarINT)

    class(netcdfvars_type),    intent(in)    :: this
    type(netcdf2dvarINT_type), intent(inout) :: netcdf2dvarINT    
    integer                                  :: varid    
    integer                                  :: status
  
    associate(ncid           => this%metadata%ncid,           & 
              var            => netcdf2dvarINT%data,          &
              n_x            => this%metadata%n_x,            &
              n_y            => this%metadata%n_y,            &
              varname        => netcdf2dvarINT%name,          &
              filename       => this%metadata%filename,       &
              integerMissing => this%metadata%integerMissing)
  
    !----------------------------------------------------------------------------
    ! Get NetCDF variable ID 
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = trim(varname), varid = varid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(varname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Read from NetCDF file
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid,varid = varid, values = var)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Check values
    !----------------------------------------------------------------------------
    if(var(1,1) == integerMissing) then
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop
    end if
  
    end associate
  
  end subroutine ReadVarINT

  subroutine ReadVarREAL(this,netcdf2dvarREAL)

    class(netcdfvars_type),     intent(in)    :: this
    type(netcdf2dvarREAL_type), intent(inout) :: netcdf2dvarREAL   
    integer                                   :: varid    
    integer                                   :: status
  
    associate(ncid           => this%metadata%ncid,          & 
              var            => netcdf2dvarREAL%data,        &
              n_x            => this%metadata%n_x,           &
              n_y            => this%metadata%n_y,           &
              varname        => netcdf2dvarREAL%name,        &
              filename       => this%metadata%filename,      &
              realMissing    => this%metadata%realMissing)
  
    !----------------------------------------------------------------------------
    ! Get NetCDF variable ID 
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = trim(varname), varid = varid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(varname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Read from NetCDF file
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid,varid = varid, values = var)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"
    end if
  
    !----------------------------------------------------------------------------
    ! Check values
    !----------------------------------------------------------------------------
    if(var(1,1).eq.realMissing) then
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop
    end if
  
    end associate
  
  end subroutine ReadVarREAL

end module NetCDFVarsType
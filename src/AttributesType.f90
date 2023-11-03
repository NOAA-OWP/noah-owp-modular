module AttributesType

  use netcdf
  use NamelistRead, only: namelist_type

  implicit none
  integer, parameter :: max_file_name_length = 512
  integer, parameter :: max_var_name_length  = 256

  type, public :: attributes_2d_type 

    character(len=max_var_name_length) :: name
    integer                            :: varid

  end type

  type, extends(attributes_2d_type) :: attributes_2dint_type

    integer,allocatable,dimension(:,:) :: data

  end type

  type, extends(attributes_2d_type) :: attributes_2dreal_type

    real,allocatable,dimension(:,:)    :: data

  end type

  type, public :: attributes_metadata_type

    character(len=max_var_name_length)  :: name_var_vegtyp  
    character(len=max_var_name_length)  :: name_var_isltyp   
    character(len=max_var_name_length)  :: name_var_soilcolor 
    character(len=max_var_name_length)  :: name_var_slope    
    character(len=max_var_name_length)  :: name_var_azimuth
    character(len=max_var_name_length)  :: name_var_mask      
    character(len=max_var_name_length)  :: name_dim_x       
    character(len=max_var_name_length)  :: name_dim_y      
    character(len=max_var_name_length)  :: name_var_x       
    character(len=max_var_name_length)  :: name_var_y 
    integer                             :: varid_vegtyp
    integer                             :: varid_isltyp
    integer                             :: varid_soilcolor
    integer                             :: varid_slope
    integer                             :: varid_azimuth
    integer                             :: varid_mask
    integer                             :: varid_x
    integer                             :: varid_y
    integer                             :: n_x                             
    integer                             :: n_y                         
    real                                :: dx                        
    real                                :: dy                              
    integer                             :: dimid_x                      
    integer                             :: dimid_y                         
    integer                             :: integerMissing
    real                                :: realMissing

  end type

  type, public :: attributes_type

    type(attributes_metadata_type)      :: metadata
    type(attributes_2dint_type)         :: vegtyp    
    type(attributes_2dint_type)         :: isltyp     
    type(attributes_2dint_type)         :: soilcolor   
    type(attributes_2dreal_type)        :: slope      
    type(attributes_2dreal_type)        :: azimuth
    type(attributes_2dint_type)         :: mask     
    real,allocatable,dimension(:)       :: lat         
    real,allocatable,dimension(:)       :: lon         
    character(len=max_file_name_length) :: filename    
    integer                             :: ncid    

  contains

    procedure, public     :: Init
    procedure, private    :: InitTransfer
    procedure, private    :: ValidateNetcdf
    procedure, private    :: ReadSpatial
    procedure, private    :: ReadVars
    procedure, private    :: ReadVar2D
    procedure, private    :: OpenNetcdf
    procedure, private    :: CloseNetcdf

  end type

  contains 

  subroutine Init(this,namelist)

    class(attributes_type), intent(inout) :: this
    type(namelist_type),    intent(in)    :: namelist
    integer                               :: status

    !----------------------------------------------------------------------------
    ! Transfer namelist values
    !----------------------------------------------------------------------------
    call this%InitTransfer(namelist)

    !----------------------------------------------------------------------------
    ! Open netcdf file
    !----------------------------------------------------------------------------
    call this%OpenNetcdf()

    !----------------------------------------------------------------------------
    ! Validate netcdf file contents 
    !----------------------------------------------------------------------------
    call this%ValidateNetcdf()

    !----------------------------------------------------------------------------
    ! Read spatial information -- i.e., n_x, n_y, dx, dy, lat(:), lon(:)
    !----------------------------------------------------------------------------
    call this%ReadSpatial()

    !----------------------------------------------------------------------------
    ! Read all gridded attribute variables
    !----------------------------------------------------------------------------
    call this%ReadVars()

    !----------------------------------------------------------------------------
    ! Close netcdf file
    !----------------------------------------------------------------------------
    call this%CloseNetcdf()

  end subroutine

  subroutine ValidateNetcdf(this)

    class(attributes_type),intent(inout) :: this
    integer                              :: status
    logical                              :: lexist
    integer                              :: ndims
    integer,dimension(1)                 :: dimids_1d
    integer,dimension(2)                 :: dimids_2d

    associate(ncid               => this%ncid,                        &
              name_dim_x         => this%metadata%name_dim_x,         & 
              name_dim_y         => this%metadata%name_dim_y,         &
              name_var_x         => this%metadata%name_var_x,         & 
              name_var_y         => this%metadata%name_var_y,         & 
              name_var_vegtyp    => this%metadata%name_var_vegtyp,    & 
              name_var_isltyp    => this%metadata%name_var_isltyp,    & 
              name_var_soilcolor => this%metadata%name_var_soilcolor, &   
              name_var_slope     => this%metadata%name_var_slope,     &
              name_var_azimuth   => this%metadata%name_var_azimuth,   &
              name_var_mask      => this%metadata%name_var_mask,      &
              varid_x            => this%metadata%varid_x,            & 
              varid_y            => this%metadata%varid_y,            & 
              varid_vegtyp       => this%metadata%varid_vegtyp,       & 
              varid_isltyp       => this%metadata%varid_isltyp,       & 
              varid_soilcolor    => this%metadata%varid_soilcolor,    &   
              varid_slope        => this%metadata%varid_slope,        &
              varid_azimuth      => this%metadata%varid_azimuth,      &
              varid_mask         => this%metadata%varid_mask,         &
              dimid_x            => this%metadata%dimid_x,            &
              dimid_y            => this%metadata%dimid_y,            &
              filename           => this%filename,                    &
              integerMissing     => this%metadata%integerMissing)

    !----------------------------------------------------------------------------
    ! Set varids to missing values
    !----------------------------------------------------------------------------
    varid_x         = integerMissing
    varid_y         = integerMissing
    varid_vegtyp    = integerMissing
    varid_isltyp    = integerMissing
    varid_soilcolor = integerMissing 
    varid_slope     = integerMissing
    varid_azimuth   = integerMissing
    varid_mask      = integerMissing
    dimid_x         = integerMissing
    dimid_y         = integerMissing

    !----------------------------------------------------------------------------
    ! Check for required dimensions
    !----------------------------------------------------------------------------
    status = nf90_inq_dimid(ncid = ncid, name = trim(name_dim_x), dimid = dimid_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the dimension ''',trim(name_dim_x),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inq_dimid(ncid = ncid, name = trim(name_dim_y), dimid = dimid_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the dimension ''',trim(name_dim_y),''''; stop ":  ERROR EXIT"; end if
    
    !----------------------------------------------------------------------------
    ! Check for required variables
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_x), varid = varid_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_x),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_y), varid = varid_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_y),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_vegtyp), varid = varid_vegtyp)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_vegtyp),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_isltyp), varid = varid_isltyp)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_isltyp),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_soilcolor), varid = varid_soilcolor)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_soilcolor),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_slope), varid = varid_slope)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_slope),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_azimuth), varid = varid_azimuth)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_azimuth),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid = ncid, name = trim(name_var_mask), varid = varid_mask)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(name_var_mask),'''';  stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Check that required variables have required number of dimensions
    !----------------------------------------------------------------------------
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_x, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 1)          then; write(*,*) 'The variable ''',trim(name_var_x),''' in the file ''',trim(filename),''' must have 1 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_y, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 1)          then; write(*,*) 'The variable ''',trim(name_var_y),''' in the file ''',trim(filename),''' must have 1 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_vegtyp, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_vegtyp),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_vegtyp),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_isltyp, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_isltyp),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_isltyp),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_soilcolor, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_soilcolor),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_soilcolor),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_slope, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_slope),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_slope),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_azimuth, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_azimuth),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_azimuth),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_mask, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(name_var_mask),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(name_var_mask),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if


    !----------------------------------------------------------------------------
    ! Check that required variables have required dimensions
    !----------------------------------------------------------------------------
    dimids_1d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_x, dimids = dimids_1d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_1d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_x),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    dimids_1d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_y, dimids = dimids_1d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_1d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_y),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_vegtyp, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_vegtyp),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_vegtyp),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_vegtyp),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_isltyp, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_isltyp),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_isltyp),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_isltyp),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_soilcolor, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_soilcolor),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_soilcolor),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_soilcolor),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_slope, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_slope),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_slope),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_slope),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_azimuth, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_azimuth),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_azimuth),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_azimuth),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    dimids_2d = -1
    status = nf90_inquire_variable(ncid = ncid, varid = varid_mask, dimids = dimids_2d)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(name_var_mask),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_x))) then; write(*,*) 'The variable ''',trim(name_var_mask),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(.NOT.(ANY(dimids_2d == dimid_y))) then; write(*,*) 'The variable ''',trim(name_var_mask),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if

    end associate

  end subroutine

  subroutine ReadSpatial(this)

    class(attributes_type),intent(inout) :: this
    integer                              :: status

    associate(ncid           => this%ncid,                     &
              name_dim_x     => this%metadata%name_dim_x,      & 
              name_dim_y     => this%metadata%name_dim_y,      & 
              name_var_x     => this%metadata%name_var_x,      &
              name_var_y     => this%metadata%name_var_y,      &
              dimid_x        => this%metadata%dimid_x,         & 
              dimid_y        => this%metadata%dimid_y,         & 
              varid_x        => this%metadata%varid_x,         & 
              varid_y        => this%metadata%varid_y,         &
              dx             => this%metadata%dx,              &
              dy             => this%metadata%dy,              &
              n_x            => this%metadata%n_x,             &
              n_y            => this%metadata%n_y,             &
              filename       => this%filename,                 &
              integerMissing => this%metadata%integerMissing,  &
              realMissing    => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Initialize to missing values
    !----------------------------------------------------------------------------
    n_x = integerMissing
    n_y = integerMissing
    dx  = realMissing
    dy  = realMissing

    !----------------------------------------------------------------------------
    ! Read n_x and n_y
    !----------------------------------------------------------------------------
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_x, len=n_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_x),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (n_x .eq. integerMissing) then; write(*,*) 'Problem reading length of dimension ''',trim(name_dim_x),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_y, len=n_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_y),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (n_y .eq. integerMissing) then; write(*,*) 'Problem reading length of dimension ''',trim(name_dim_y),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Allocate local arrays and set to missing values
    !----------------------------------------------------------------------------
    allocate(this%lon(n_x))
    allocate(this%lat(n_y))
    this%lon(:) = realMissing
    this%lat(:) = realMissing

    !----------------------------------------------------------------------------
    ! Read latitude and longitude values
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid, varid = varid_x, values = this%lon)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_var_x),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if(this%lon(1) .eq. realMissing) then; write(*,*) 'Problem reading variable ''',trim(name_var_x),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid = ncid, varid = varid_y, values = this%lat)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_var_y),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if(this%lat(1) .eq. realMissing) then; write(*,*) 'Problem reading variable ''',trim(name_var_y),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Get dx and dy (i.e., the grid spacing from lat and lon arrays)
    ! Assume uniform rectilinear grid
    !----------------------------------------------------------------------------
    if(abs(this%lon(1)) >= abs(this%lon(2))) then
      dx = abs(this%lon(1))-abs(this%lon(2)) ! western hemisphere
    else if (abs(this%lon(1)) < abs(this%lon(2))) then
      dx = abs(this%lon(2))-abs(this%lon(1)) ! eastern hemisphere
    end if
    if(dx < 0.) then; write(*,*) 'Unable to infer dx from variable ''',trim(name_var_x),''' in ''',trim(filename),''' (dx should be postive but has value:',dx; stop ":  ERROR EXIT"; end if

    if(abs(this%lat(1)) >= abs(this%lat(2))) then
      dy = abs(this%lat(1))-abs(this%lat(2)) ! northern hemisphere
    else if (abs(this%lat(1)) < abs(this%lat(2))) then
      dy = abs(this%lat(2))-abs(this%lat(1)) ! southern hemisphere
    end if
    if(dy < 0.) then; write(*,*) 'Unable to infer dy from variable ''',trim(name_var_y),''' in ''',trim(filename),''' (dy should be postive but has value:',dy; stop ":  ERROR EXIT"; end if

    end associate

  end subroutine ReadSpatial

  subroutine ReadVars(this)

    class(attributes_type), intent(inout) :: this
    integer                               :: status

    !----------------------------------------------------------------------------
    ! Name the variables
    !----------------------------------------------------------------------------
    this%vegtyp%name     = this%metadata%name_var_vegtyp
    this%isltyp%name     = this%metadata%name_var_isltyp 
    this%soilcolor%name  = this%metadata%name_var_soilcolor
    this%slope%name      = this%metadata%name_var_slope 
    this%azimuth%name    = this%metadata%name_var_azimuth    
    this%mask%name       = this%metadata%name_var_mask 

    !----------------------------------------------------------------------------
    ! Set variable id numbers
    !----------------------------------------------------------------------------
    this%vegtyp%varid    = this%metadata%varid_vegtyp
    this%isltyp%varid    = this%metadata%varid_isltyp
    this%soilcolor%varid = this%metadata%varid_soilcolor
    this%slope%varid     = this%metadata%varid_slope   
    this%azimuth%varid   = this%metadata%varid_azimuth 
    this%mask%varid      = this%metadata%varid_mask      

    !----------------------------------------------------------------------------
    ! Read variables
    !----------------------------------------------------------------------------
    call this%ReadVar2D(this%vegtyp)
    call this%ReadVar2D(this%isltyp)
    call this%ReadVar2D(this%soilcolor)
    call this%ReadVar2D(this%slope)
    call this%ReadVar2D(this%azimuth)
    call this%ReadVar2D(this%mask)

  end subroutine

  subroutine ReadVar2D(this,attributes_2d)

    class(attributes_type),      intent(in)  :: this
    class(attributes_2d_type), intent(inout) :: attributes_2d    
    integer                                  :: status
  
    associate(ncid           => this%ncid,                      & 
              n_x            => this%metadata%n_x,              &
              n_y            => this%metadata%n_y,              &
              varname        => attributes_2d%name,              &
              varid          => attributes_2d%varid,             &
              filename       => this%filename,                  &
              integerMissing => this%metadata%integerMissing,   &
              realMissing    => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Allocate array to be populated and set to missing value
    !----------------------------------------------------------------------------
    select type (attributes_2d)
    type is (attributes_2dint_type)
      if(allocated(attributes_2d%data)) deallocate(attributes_2d%data)    
      allocate(attributes_2d%data(n_x,n_y))
      attributes_2d%data(:,:) = integerMissing
    type is (attributes_2dreal_type)
      if(allocated(attributes_2d%data)) deallocate(attributes_2d%data)
      allocate(attributes_2d%data(n_x,n_y))
      attributes_2d%data(:,:) = realMissing
    class default 
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''' -- attempted to allocate unrecognized data type'; stop
    end select
  
    !----------------------------------------------------------------------------
    ! Read and check for missing values
    !----------------------------------------------------------------------------
    select type (attributes_2d)
    type is (attributes_2dint_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = attributes_2d%data)
      if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
      if(attributes_2d%data(1,1) == integerMissing) then; write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop; end if
    type is (attributes_2dreal_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = attributes_2d%data)
      if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
      if(attributes_2d%data(1,1) == realMissing) then; write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop; end if
    class default 
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''' -- attempted to read unrecognized data type'; stop
    end select
  
    end associate
  
  end subroutine ReadVar2D

  subroutine OpenNetcdf(this)

    class(attributes_type), intent(inout) :: this
    logical                               :: lexist
    integer                               :: status

    associate(filename => this%filename, &
              ncid     => this%ncid)

    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then; write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"; endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then; write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"; endif

    end associate

  end subroutine

  subroutine CloseNetcdf(this)

    class(attributes_type), intent(inout) :: this
    integer                               :: status

    associate(filename => this%filename, &
              ncid     => this%ncid)

    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT"; end if

    end associate

  end subroutine

  subroutine InitTransfer(this,namelist)

    class(attributes_type), intent(inout) :: this
    type(namelist_type),    intent(in)    :: namelist

    !----------------------------------------------------------------------------
    ! Transfer values from namelist
    !----------------------------------------------------------------------------
    this%filename                    = namelist%attributes_filename
    this%metadata%name_var_vegtyp    = namelist%name_var_vegtyp                          
    this%metadata%name_var_isltyp    = namelist%name_var_isltyp
    this%metadata%name_var_soilcolor = namelist%name_var_soilcolor
    this%metadata%name_var_slope     = namelist%name_var_slope
    this%metadata%name_var_azimuth   = namelist%name_var_azimuth
    this%metadata%name_var_mask      = namelist%name_var_mask       
    this%metadata%name_dim_x         = namelist%name_dim_x
    this%metadata%name_dim_y         = namelist%name_dim_y
    this%metadata%name_var_x         = namelist%name_var_x
    this%metadata%name_var_y         = namelist%name_var_y
    this%metadata%integerMissing     = namelist%integerMissing
    this%metadata%realMissing        = namelist%realMissing

  end subroutine

end module AttributesType
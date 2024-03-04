module AttributesType

  use netcdf
  use NamelistRead, only: namelist_type

  implicit none
  integer, parameter :: max_file_name_length = 512
  integer, parameter :: max_var_name_length  = 256

  type, public :: attributes_var_type 
    character(len=max_var_name_length)  :: name
    integer                             :: varid
  end type

  type, extends(attributes_var_type) :: attributes_var_1d_type 
    integer,dimension(1)                :: dimlen
    integer,dimension(1)                :: dimids
  end type

  type, extends(attributes_var_type) :: attributes_var_2d_type 
    integer,dimension(2)                :: dimlen
    integer,dimension(2)                :: dimids
  end type

  type, extends(attributes_var_1d_type) :: attributes_var_1dreal_type
    real,allocatable,dimension(:)       :: data
  end type

  type, extends(attributes_var_2d_type) :: attributes_var_2dint_type
    integer,allocatable,dimension(:,:)  :: data
  end type

  type, extends(attributes_var_2d_type) :: attributes_var_2dreal_type
    real,allocatable,dimension(:,:)     :: data
  end type

  type, public :: attributes_metadata_type
    integer                             :: ncid    
    character(len=max_file_name_length) :: filename      
    character(len=max_var_name_length)  :: name_att_dx       
    character(len=max_var_name_length)  :: name_att_dy       
    character(len=max_var_name_length)  :: name_dim_x       
    character(len=max_var_name_length)  :: name_dim_y        
    integer                             :: n_x                             
    integer                             :: n_y                         
    real                                :: dx                        
    real                                :: dy                                                
    integer                             :: integerMissing
    real                                :: realMissing
  end type

  type, public :: attributes_type
    type(attributes_var_2dint_type)     :: vegtyp    
    type(attributes_var_2dint_type)     :: isltyp     
    type(attributes_var_2dint_type)     :: soilcolor   
    type(attributes_var_2dreal_type)    :: slope      
    type(attributes_var_2dreal_type)    :: azimuth
    type(attributes_var_2dint_type)     :: mask     
    type(attributes_var_1dreal_type)    :: lat
    type(attributes_var_1dreal_type)    :: lon       
    type(attributes_metadata_type)      :: metadata
  contains
    procedure, public                   :: Init
    procedure, private                  :: InitTransfer
    procedure, private                  :: ValidateFile
    procedure, private                  :: ReadVar
    procedure, private                  :: ReadVars
    procedure, private                  :: OpenNetcdf
    procedure, private                  :: CloseNetcdf
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
    ! Open file
    !----------------------------------------------------------------------------
    call this%OpenNetcdf()

    !----------------------------------------------------------------------------
    ! Validate file contents 
    !----------------------------------------------------------------------------
    call this%ValidateFile()

    !----------------------------------------------------------------------------
    ! Read variables
    !----------------------------------------------------------------------------
    call this%ReadVars()

    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    call this%CloseNetcdf()

  end subroutine

  subroutine ValidateFile(this)

    class(attributes_type),intent(inout) :: this
    integer                              :: status
    integer                              :: ndims
    integer,dimension(1)                 :: dimids_1d
    character(len=max_var_name_length)   :: iname
    integer                              :: ivarid
    integer                              :: varid_x, varid_y, dimid_x, dimid_y
    integer                              :: att_num_dx, att_num_dy, att_len_dx, att_len_dy
    real,dimension(1)                    :: x1, x2, y1, y2

    associate(ncid               => this%metadata%ncid,               &
              name_att_dx        => this%metadata%name_att_dx,        & 
              name_att_dy        => this%metadata%name_att_dy,        &
              name_dim_x         => this%metadata%name_dim_x,         & 
              name_dim_y         => this%metadata%name_dim_y,         &
              n_x                => this%metadata%n_x,                &
              n_y                => this%metadata%n_y,                &
              dx                 => this%metadata%dx,                 &
              dy                 => this%metadata%dy,                 &
              filename           => this%metadata%filename,           &
              integerMissing     => this%metadata%integerMissing,     &
              realMissing        => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Check for required dimensions + set dimension ids and n_x, n_y
    !----------------------------------------------------------------------------

    ! x
    iname = trim(name_dim_x)
    dimid_x = integerMissing
    status = nf90_inq_dimid(ncid = ncid, name = trim(iname), dimid = dimid_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the dimension ''',trim(iname),''''; stop ":  ERROR EXIT"; end if
    if (dimid_x .eq. integerMissing) then; write(*,*) 'Problem finding dimension ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_x, len=n_x)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (n_x .eq. integerMissing) then; write(*,*) 'Problem reading length of dimension ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    ! y
    iname = trim(name_dim_y)
    dimid_y = integerMissing
    status = nf90_inq_dimid(ncid = ncid, name = trim(iname), dimid = dimid_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the dimension ''',trim(iname),''''; stop ":  ERROR EXIT"; end if
    if (dimid_y .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid = ncid, dimid = dimid_y, len=n_y)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (n_y .eq. integerMissing) then; write(*,*) 'Problem reading length of dimension ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Check for required attributes
    !----------------------------------------------------------------------------
    
    ! dx
    iname = trim(name_att_dx)
    att_num_dx = integerMissing
    att_len_dx = integerMissing
    status = nf90_inquire_attribute(ncid = ncid, varid = NF90_GLOBAL, name = iname, len = att_len_dx, attnum = att_num_dx)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to find global attribute ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (att_len_dx .ne. 1) then; write(*,*) 'ERROR global attribute ''',trim(iname),''' in ''',trim(filename),''' must have length 1 but is length: ',att_len_dx;stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid = ncid, varid = NF90_GLOBAL, name = iname, values = dx)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read global attribute ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    ! dy
    iname = trim(name_att_dy)
    att_num_dy = integerMissing
    att_len_dy = integerMissing
    status = nf90_inquire_attribute(ncid = ncid, varid = NF90_GLOBAL, name = iname, len = att_len_dy, attnum = att_num_dy)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to find global attribute ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    if (att_len_dx .ne. 1) then; write(*,*) 'ERROR global attribute ''',trim(iname),''' in ''',trim(filename),''' must have length 1 but is length: ',att_len_dy;stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid = ncid, varid = NF90_GLOBAL, name = iname, values = dy)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to read global attribute ''',trim(iname),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Check for required variables + set variable ids
    !----------------------------------------------------------------------------
    
    ! vegtyp
    iname = this%vegtyp%name
    this%vegtyp%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%vegtyp%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%vegtyp%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! isltyp
    iname = this%isltyp%name
    this%isltyp%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%isltyp%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%isltyp%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! soil color
    iname = this%soilcolor%name
    this%soilcolor%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%soilcolor%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%soilcolor%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! slope
    iname = this%slope%name
    this%slope%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%slope%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%slope%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! azimuth
    iname = this%azimuth%name
    this%azimuth%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%azimuth%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%azimuth%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! mask
    iname = this%mask%name
    this%mask%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%mask%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%mask%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! lon
    iname = this%lon%name
    this%lon%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%lon%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%lon%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    ! lat
    iname = this%lat%name
    this%lat%varid = integerMissing
    status = nf90_inq_varid(ncid = ncid, name = trim(iname), varid = this%lat%varid)
    if (status .ne. nf90_noerr) then; write(*,*) 'The file ''',trim(filename),''' must have have the variable ''',trim(iname),'''';  stop ":  ERROR EXIT"; end if
    if (this%lat%varid .eq. integerMissing) then; write(*,*) 'Problem finding variable ''',trim(iname),''' in the file ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Check that required variables have required number of dimensions
    !----------------------------------------------------------------------------

    ! vegtyp
    iname = this%vegtyp%name
    ivarid = this%vegtyp%varid
    ndims = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! isltyp
    iname = this%isltyp%name
    ivarid = this%isltyp%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! soil color
    iname = this%soilcolor%name
    ivarid = this%soilcolor%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! slope
    iname = this%slope%name
    ivarid = this%slope%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! azimuth
    iname = this%azimuth%name
    ivarid = this%azimuth%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! mask
    iname = this%mask%name
    ivarid = this%mask%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 2)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 2 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! lon
    iname = this%lon%name
    ivarid = this%lon%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 1)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 1 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if
    
    ! lat
    iname = this%lat%name
    ivarid = this%lat%varid
    ndims = -1
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, ndims = ndims)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get the number of dimensions for variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (ndims  .ne. 1)          then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have 1 dimensions but has ',ndims;  stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Check that required variables have required dimensions
    !----------------------------------------------------------------------------

    ! vegtyp
    iname = this%vegtyp%name
    ivarid = this%vegtyp%varid
    this%vegtyp%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%vegtyp%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(this%vegtyp%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(this%vegtyp%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%vegtyp%dimlen(1) = n_x
    this%vegtyp%dimlen(2) = n_y

    ! isltyp
    iname = this%isltyp%name
    ivarid = this%isltyp%varid
    this%isltyp%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%isltyp%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if(this%isltyp%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if(this%isltyp%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%isltyp%dimlen(1) = n_x
    this%isltyp%dimlen(2) = n_y

    ! soil color
    iname = this%soilcolor%name
    ivarid = this%soilcolor%varid
    this%soilcolor%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%soilcolor%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%soilcolor%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if (this%soilcolor%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%soilcolor%dimlen(1) = n_x
    this%soilcolor%dimlen(2) = n_y

    ! slope
    iname = this%slope%name
    ivarid = this%slope%varid
    this%slope%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%slope%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%slope%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if (this%slope%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%slope%dimlen(1) = n_x
    this%slope%dimlen(2) = n_y

    ! azimuth
    iname = this%azimuth%name
    ivarid = this%azimuth%varid
    this%azimuth%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%azimuth%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%azimuth%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if (this%azimuth%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%azimuth%dimlen(1) = n_x
    this%azimuth%dimlen(2) = n_y

    ! mask
    iname = this%mask%name
    ivarid = this%mask%varid
    this%mask%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%mask%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%mask%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    if (this%mask%dimids(2).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%mask%dimlen(1) = n_x
    this%mask%dimlen(2) = n_y

    ! lon
    iname = this%lon%name
    ivarid = this%lon%varid
    this%lon%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%lon%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%lon%dimids(1).ne.dimid_x) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_x),'''';  stop ":  ERROR EXIT"; end if
    this%lon%dimlen(1) = n_x

    ! lat
    iname = this%lat%name
    ivarid = this%lat%varid
    this%lat%dimids = integerMissing
    status = nf90_inquire_variable(ncid = ncid, varid = ivarid, dimids = this%lat%dimids)
    if (status .ne. nf90_noerr) then; write(*,*) 'Unable to get dimension ID numbers for the variable ''',trim(iname),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    if (this%lat%dimids(1).ne.dimid_y) then; write(*,*) 'The variable ''',trim(iname),''' in the file ''',trim(filename),''' must have the dimension ''',trim(name_dim_y),'''';  stop ":  ERROR EXIT"; end if
    this%lat%dimlen(1) = n_y

    end associate

  end subroutine

  subroutine ReadVars(this)

    class(attributes_type), intent(inout) :: this
    integer                               :: status

    call this%ReadVar(this%lat)
    call this%ReadVar(this%lon)
    call this%ReadVar(this%vegtyp)
    call this%ReadVar(this%isltyp)
    call this%ReadVar(this%soilcolor)
    call this%ReadVar(this%slope)
    call this%ReadVar(this%azimuth)
    call this%ReadVar(this%mask)

  end subroutine

  subroutine ReadVar(this,attributes_var)

    class(attributes_type),      intent(in)   :: this
    class(attributes_var_type), intent(inout) :: attributes_var   
    integer                                   :: status
  
    associate(ncid           => this%metadata%ncid,             & 
              varname        => attributes_var%name,            &
              varid          => attributes_var%varid,           &
              filename       => this%metadata%filename,         &
              integerMissing => this%metadata%integerMissing,   &
              realMissing    => this%metadata%realMissing)

    !----------------------------------------------------------------------------
    ! Allocate array to be populated and set to missing value
    !----------------------------------------------------------------------------
    select type (attributes_var)
    type is (attributes_var_1dreal_type)
      if(allocated(attributes_var%data)) deallocate(attributes_var%data)    
      allocate(attributes_var%data(attributes_var%dimlen(1)))
      attributes_var%data(:) = realMissing
    type is (attributes_var_2dint_type)
      if(allocated(attributes_var%data)) deallocate(attributes_var%data)    
      allocate(attributes_var%data(attributes_var%dimlen(1),attributes_var%dimlen(2)))
      attributes_var%data(:,:) = integerMissing
    type is (attributes_var_2dreal_type)
      if(allocated(attributes_var%data)) deallocate(attributes_var%data)
      allocate(attributes_var%data(attributes_var%dimlen(1),attributes_var%dimlen(2)))
      attributes_var%data(:,:) = realMissing
    class default 
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''' -- attempted to allocate unrecognized data type'; stop
    end select
  
    !----------------------------------------------------------------------------
    ! Read and check for missing values
    !----------------------------------------------------------------------------
    select type (attributes_var)
    type is (attributes_var_1dreal_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = attributes_var%data)
      if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
      if(attributes_var%data(1) == realMissing) then; write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop; end if
    type is (attributes_var_2dint_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = attributes_var%data)
      if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
      if(attributes_var%data(1,1) == integerMissing) then; write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop; end if
    type is (attributes_var_2dreal_type)
      status = nf90_get_var(ncid = ncid,varid = varid, values = attributes_var%data)
      if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(varname),''' from ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
      if(attributes_var%data(1,1) == realMissing) then; write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''''; stop; end if
    class default 
      write(*,*) 'ERROR : problem reading ''',trim(varname),''' from ''',trim(filename),''' -- attempted to read unrecognized data type'; stop
    end select
  
    end associate
  
  end subroutine ReadVar

  subroutine OpenNetcdf(this)

    class(attributes_type), intent(inout) :: this
    logical                               :: lexist
    integer                               :: status

    associate(filename => this%metadata%filename, &
              ncid     => this%metadata%ncid)

    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then; write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"; endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then; write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"; endif

    end associate

  end subroutine

  subroutine CloseNetcdf(this)

    class(attributes_type), intent(inout) :: this
    integer                               :: status

    associate(filename => this%metadata%filename, &
              ncid     => this%metadata%ncid)

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
    this%metadata%filename       = namelist%attributes_filename
    this%lon%name                = namelist%name_var_lon
    this%lat%name                = namelist%name_var_lat
    this%vegtyp%name             = namelist%name_var_vegtyp                          
    this%isltyp%name             = namelist%name_var_isltyp
    this%soilcolor%name          = namelist%name_var_soilcolor
    this%slope%name              = namelist%name_var_slope
    this%azimuth%name            = namelist%name_var_azimuth
    this%mask%name               = namelist%name_var_mask       
    this%metadata%name_dim_x     = namelist%name_dim_x
    this%metadata%name_dim_y     = namelist%name_dim_y
    this%metadata%name_att_dx    = namelist%name_att_dx
    this%metadata%name_att_dy    = namelist%name_att_dy
    this%metadata%integerMissing = namelist%integerMissing
    this%metadata%realMissing    = namelist%realMissing

  end subroutine

end module AttributesType
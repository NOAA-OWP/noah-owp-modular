module GridlistRead
  
  use netcdf
  use NamelistRead, only: namelist_type

  implicit none

  type, public :: gridlist_type
  
  integer                            :: ncid  
  integer                            :: n_x
  integer                            :: n_y
  real                               :: dx
  real                               :: dy
  integer                            :: nveg
  character*256                      :: veg_class_name
  integer,allocatable,dimension(:,:) :: vegtyp
  real,allocatable,dimension(:,:)    :: lat 
  real,allocatable,dimension(:,:)    :: lon   

  contains

    procedure, public  :: ReadGridlist

  end type

contains

  subroutine ReadGridlist(this,namelist)

    class(gridlist_type)               :: this
    type(namelist_type),intent(in)     :: namelist 
    logical                            :: lexist
    integer                            :: status
    integer                            :: ix, iy

    ! x dimension and associated variables and attributes
    integer                            :: read_nx                  ! read-in n_x value
    real                               :: read_dx                  ! read-in dx value
    integer                            :: varid_x                  ! netcdf variable id for netcdf variable holding x dim coordinates (longitudes) for uniform_rectilinear grid
    integer                            :: dimid_x                  ! netcdf dimension id for netcdf dimension for x dimension (longitude dimension)
    character*256                      :: name_att_dx              ! name of dx attribute, which must be associated with x dimension variable 
    character*256                      :: name_dim_x               ! name of x dimension in netcdf file (e.g., 'Longitude'). Assume netcdf variable holding x dim coordinates (i.e., variable id = varid_x) has same name.
    real,allocatable,dimension(:)      :: read_lon                 ! read-in longitude values along x dimensional grid edge

    ! y dimension and associated variables and attributes
    integer                            :: read_ny                  ! read-in n_y value
    real                               :: read_dy                  ! read-in dy value
    integer                            :: varid_y                  ! netcdf variable id for netcdf variable holding y dim coordinates (latitudes) for uniform_rectilinear grid
    integer                            :: dimid_y                  ! netcdf dimension id for netcdf dimension for y dimension (latitude dimension)
    character*256                      :: name_att_dy              ! name of dy attribute, which must be associated with y dimension variable 
    character*256                      :: name_dim_y               ! name of y dimension in netcdf file (e.g., 'Latitude'). Assume netcdf variable holding y dim coordinates (i.e., variable id = varid_y) has same name.
    real,allocatable,dimension(:)      :: read_lat                 ! read-in latitude values along y dimensional grid edge

    ! vegtyp variables and attributes
    integer                            :: read_nveg                ! read-in nveg (number of vegtype / land use cover classes in classification scheme)
    integer                            :: varid_vegtyp             ! netcdf variable id for vegtyp variable
    character*256                      :: name_var_vegtyp          ! name of vegtyp variable in netcdf file
    integer,allocatable,dimension(:,:) :: read_vegtyp              ! to hold read-in vegtyp values before transferring to this%vegtyp
    character*256                      :: read_veg_class_name      ! read-in veg_class_name, which must be associated with vegtyp variable
    character*256                      :: name_att_nveg            ! name of nveg attribute, which must be associated with vegtyp variable
    character*256                      :: name_att_veg_class_name  ! name of veg_class_name attribute, which must be associated with vegtyp variable

    associate(filename       => namelist%grid_filename,  &
              integerMissing => namelist%integerMissing, &
              realMissing    => namelist%realMissing,    &
              stringMissing  => namelist%stringMissing,  &
              ncid           => this%ncid)

    !----------------------------------------------------------------------------
    ! Set required variable, dimension, and attribute names. *These must match what is found within the netcdf file.*
    !----------------------------------------------------------------------------
    name_dim_y              = 'Latitude'     
    name_dim_x              = 'Longitude'
    name_var_vegtyp         = 'vegtyp'
    name_att_dx             = 'dx'
    name_att_dy             = 'dy'
    name_att_nveg           = 'nveg'
    name_att_veg_class_name = 'veg_class_name'


    !----------------------------------------------------------------------------
    ! Initialize variables to be read-in as having missing values
    !----------------------------------------------------------------------------
    read_nx             = integerMissing
    read_ny             = integerMissing
    read_dx             = realMissing
    read_dy             = realMissing
    read_nveg           = integerMissing
    varid_x             = integerMissing
    varid_y             = integerMissing
    varid_vegtyp        = integerMissing
    dimid_x             = integerMissing
    dimid_y             = integerMissing
    read_veg_class_name = stringMissing

    !----------------------------------------------------------------------------
    ! Open the netcdf input file
    !----------------------------------------------------------------------------

    ! check that file exists
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    endif

    ! open it
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Check file for necessary variables, dimensions, and attributes. Set netcdf id values
    !----------------------------------------------------------------------------
    
    ! x dimension (netcdf dimension for netcdf variables)
    status = nf90_inq_dimid(ncid, trim(name_dim_x), dimid_x)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required dimension ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! x dimensional variable (netcdf variable holding grid longitudes)
    status = nf90_inq_varid(ncid, trim(name_dim_x), varid_x)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the required variable ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! dx attribute (associated with x dimensional variable)
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_x,name=name_att_dx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_dx),''' which should be associated with variable ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! y dimension (netcdf dimension for netcdf variables)
    status = nf90_inq_dimid(ncid, trim(name_dim_y), dimid_y)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required dimension ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! y dimensional variable (netcdf variable holding grid latitudes)
    status = nf90_inq_varid(ncid, trim(name_dim_y), varid_y)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required variable ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! dy attribute (associated with y dimensional variable)
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_y,name=name_att_dy)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_dy),''' which should be associated with variable ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! vegtyp variable
    status = nf90_inq_varid(ncid, name_var_vegtyp, varid_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required variable ''',trim(name_var_vegtyp),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! nveg attribute (associated with vegtyp variable)
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_vegtyp,name=name_att_nveg)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_nveg),''' which should be associated with variable ''',trim(name_var_vegtyp),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! veg_class_name attribute (associated with vegtyp variable)
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_vegtyp,name=name_att_veg_class_name)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_veg_class_name),''' which should be associated with variable ''',trim(name_var_vegtyp),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Get grid spatial info (i.e., n_x, n_y, dx, and dy)
    !----------------------------------------------------------------------------

    ! n_x
    status = nf90_inquire_dimension(ncid, dimid_x, len=read_nx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get length of dimension ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! n_y
    status = nf90_inquire_dimension(ncid, dimid_y, len=read_ny)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get length of dimension ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! dx
    status = nf90_get_att(ncid=ncid, varid=varid_x, name=name_att_dx, values=read_dx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_dx),''' associated with variable ''',trim(name_dim_x),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! dy
    status = nf90_get_att(ncid=ncid, varid=varid_y, name=name_att_dy, values=read_dy)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_dy),''' associated with variable ''',trim(name_dim_y),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate local arrays
    !----------------------------------------------------------------------------
    allocate(read_lon(read_nx))
    allocate(read_lat(read_ny))
    allocate(read_vegtyp(read_nx,read_ny))

    !----------------------------------------------------------------------------
    ! Allocate gridlist_type arrays
    !----------------------------------------------------------------------------
    allocate(this%lon(read_nx,read_ny))
    allocate(this%lat(read_nx,read_ny))
    allocate(this%vegtyp(read_nx,read_ny))

    !----------------------------------------------------------------------------
    ! Set local arrays to missing values
    !----------------------------------------------------------------------------
    read_lon(:)      = realMissing
    read_lat(:)      = realMissing
    read_vegtyp(:,:) = integerMissing

    !----------------------------------------------------------------------------
    ! Read variables and variable attributes
    !----------------------------------------------------------------------------

    ! lon
    status = nf90_get_var(ncid,varid_x,read_lon)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_dim_x),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! lat
    status = nf90_get_var(ncid,varid_y,read_lat)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_dim_y),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! vegtyp
    status = nf90_get_var(ncid,varid_vegtyp,read_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_var_vegtyp),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! nveg
    status = nf90_get_att(ncid=ncid, varid=varid_vegtyp, name=name_att_nveg, values=read_nveg)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_nveg),''' associated with variable ''',trim(name_var_vegtyp),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    ! veg_class_name
    status = nf90_get_att(ncid=ncid, varid=varid_vegtyp, name=name_att_veg_class_name, values=read_veg_class_name)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_veg_class_name),''' associated with variable ''',trim(name_var_vegtyp),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Transfer read-in values to gridlist_type 
    !----------------------------------------------------------------------------

    ! n_x
    if(read_nx /= integerMissing) then
      this%n_x = read_nx 
    else 
      write(*,*) 'ERROR : problem reading n_x from input file ''',trim(filename),''''; stop
    end if

    ! n_y
    if(read_ny /= integerMissing) then
      this%n_y = read_ny
    else 
      write(*,*) 'ERROR : problem reading n_y from input file ''',trim(filename),''''; stop
    end if

    ! dx
    if(read_dx /= realMissing) then
      this%dx = read_dx
    else 
      write(*,*) 'ERROR : problem reading dx from input file ''',trim(filename),''''; stop
    end if

    ! dy
    if(read_dy /= realMissing) then
      this%dy = read_dy
    else 
      write(*,*) 'ERROR : problem reading dy from input file ''',trim(filename),''''; stop
    end if

    ! vegtyp
    if(read_vegtyp(1,1) /= integerMissing) then
      this%vegtyp(:,:) = read_vegtyp(:,:)
    else 
      write(*,*) 'ERROR : problem reading vegtyp from input file ''',trim(filename),''''; stop
    end if

    ! nveg
    if(read_nveg /= integerMissing) then
      this%nveg = read_nveg
    else 
      write(*,*) 'ERROR : problem reading nveg from input file ''',trim(filename),''''; stop
    end if

    ! veg_class_name
    if(read_veg_class_name /= stringMissing) then
      this%veg_class_name = read_veg_class_name
    else 
      write(*,*) 'ERROR : problem reading veg_class_name from input file ''',trim(filename),''''; stop
    end if

    ! lon
    if(read_lon(1) /= realMissing) then
      do ix = 1, read_nx
        this%lon(ix,:) = read_lon(ix)
      end do
    else 
      write(*,*) 'ERROR : problem reading lon from input file ''',trim(filename),''''; stop
    end if

    ! lat
    if(read_lat(1) /= realMissing) then
      do iy = 1, read_ny
        this%lat(:,iy) = read_lat(iy)
      end do
    else 
      write(*,*) 'ERROR : problem reading lon from input file ''',trim(filename),''''; stop
    end if

    end associate

  end subroutine ReadGridlist 

end module GridlistRead

module GridInfoType
  
  use netcdf
  use NamelistRead, only: namelist_type

  implicit none
  save

  type, public :: gridinfo_type
  
  integer                            :: n_x                 ! number of grid cells in x dimension
  integer                            :: n_y                 ! number of grid cells in y dimension
  real                               :: dx                  ! distance between grid cell nodes in x dimension
  real                               :: dy                  ! distance between grid cell nodes in y dimension
  integer,allocatable,dimension(:,:) :: vegtyp              ! vegetation type
  integer,allocatable,dimension(:,:) :: isltyp              ! soil type
  real,allocatable,dimension(:,:)    :: lat                 ! latitude [degrees]  (-90 to 90)
  real,allocatable,dimension(:,:)    :: lon                 ! longitude [degrees] (-180 to 180)
  character*256                      :: name_att_dx         ! name of dx attribute, which must be associated with x dimension variable 
  character*256                      :: name_att_dy         ! name of dy attribute, which must be associated with y dimension variable 
  character*256                      :: name_dim_x          ! name of x dimension in netcdf file (e.g., 'Longitude'). Assume netcdf variable holding x dim coordinates (i.e., variable id = varid_x) has same name.
  character*256                      :: name_dim_y          ! name of y dimension in netcdf file (e.g., 'Latitude'). Assume netcdf variable holding y dim coordinates (i.e., variable id = varid_y) has same name.
  character*256                      :: name_var_vegtyp     ! name of vegtyp variable in netcdf file
  character*256                      :: name_var_isltyp     ! name of isltyp variable in netcdf file
  integer                            :: integerMissing 
  real                               :: realMissing   
  character(len=12)                  :: stringMissing 

  contains

    procedure, public  :: ReadGridInfo
    procedure, private :: ReadSpatial
    procedure, private :: ReadLandUse  
    procedure, private :: ReadSoils

  end type

contains

  subroutine ReadGridInfo(this,namelist)

    class(gridinfo_type)               :: this
    type(namelist_type),intent(in)     :: namelist 

    !----------------------------------------------------------------------------
    ! Set expected names for all NetCDF-defined variables, dimensions and attributes
    !----------------------------------------------------------------------------
    this%name_att_dx     = 'dx'    
    this%name_att_dy     = 'dy'       
    this%name_dim_x      = 'Longitude'
    this%name_dim_y      = 'Latitude'  
    this%name_var_vegtyp = 'vegtyp' 
    this%name_var_isltyp = 'isltyp'

    !----------------------------------------------------------------------------
    ! Give missing values to gridinfo_type
    !----------------------------------------------------------------------------
    this%integerMissing  = namelist%integerMissing
    this%realMissing     = namelist%realMissing
    this%stringMissing   = namelist%stringMissing

    !----------------------------------------------------------------------------
    ! Read spatial information from each NetCDF file
    !----------------------------------------------------------------------------
    call this%ReadSpatial(namelist%landuse_filename)
    call this%ReadSpatial(namelist%soils_filename)

    !----------------------------------------------------------------------------
    ! Allocate gridinfo_type arrays
    !----------------------------------------------------------------------------
    allocate(this%vegtyp(this%n_x,this%n_y)) ! land use
    allocate(this%isltyp(this%n_x,this%n_y)) ! soils

    !----------------------------------------------------------------------------
    ! Set gridinfo_type arrays to missing values
    !----------------------------------------------------------------------------
    this%vegtyp(:,:) = this%integerMissing ! land use
    this%isltyp(:,:) = this%integerMissing ! soils

    !----------------------------------------------------------------------------
    ! Read NetCDF files to populate gridinfo_type arrays
    !----------------------------------------------------------------------------
    call this%ReadLandUse(namelist%landuse_filename)
    call this%ReadSoils(namelist%soils_filename)

  end subroutine ReadGridInfo

  subroutine ReadSoils(this,filename)

    class(gridinfo_type)               :: this
    character*256,intent(in)           :: filename 
    logical                            :: lexist
    integer                            :: status       
    integer                            :: ix, iy
    integer                            :: ncid     ! NetCDF file ID number
    integer                            :: varid    ! NetCDF variable ID
    integer,allocatable,dimension(:,:) :: read_var ! Local array to hold read-in values

    associate(name_var       => this%name_var_isltyp, &
              var            => this%isltyp, &
              integerMissing => this%integerMissing)

    !----------------------------------------------------------------------------
    ! Check that file exists and open it
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Get NetCDF variable ID 
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = name_var, varid = varid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(name_var),''' in ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate and set local array to missing values
    !----------------------------------------------------------------------------
    allocate(read_var(this%n_x,this%n_y))
    read_var(:,:) = integerMissing

    !----------------------------------------------------------------------------
    ! Read from NetCDF file
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid,varid = varid, values = read_var)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_var),''' from ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Check and then transfer read-in values to gridinfo_type 
    !----------------------------------------------------------------------------
    if(read_var(1,1) /= integerMissing) then
      var(:,:) = read_var(:,:)
    else 
      write(*,*) 'ERROR : problem reading ''',trim(name_var),''' from ''',trim(filename),''''; stop
    end if

    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine ReadSoils 

  subroutine ReadLandUse(this,filename)

    class(gridinfo_type)               :: this
    character*256,intent(in)           :: filename 
    logical                            :: lexist
    integer                            :: status       
    integer                            :: ix, iy
    integer                            :: ncid     ! NetCDF file ID number
    integer                            :: varid    ! NetCDF variable ID
    integer,allocatable,dimension(:,:) :: read_var ! Local array to hold read-in values

    associate(name_var       => this%name_var_vegtyp, &
              var            => this%vegtyp, &
              integerMissing => this%integerMissing)

    !----------------------------------------------------------------------------
    ! Check that file exists and open it
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Get NetCDF variable ID 
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid = ncid, name = name_var, varid = varid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(name_var),''' in ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate and set local array to missing values
    !----------------------------------------------------------------------------
    allocate(read_var(this%n_x,this%n_y))
    read_var(:,:) = integerMissing

    !----------------------------------------------------------------------------
    ! Read from NetCDF file
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid,varid = varid, values = read_var)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_var),''' from ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Check and then transfer read-in values to gridinfo_type 
    !----------------------------------------------------------------------------
    if(read_var(1,1) /= integerMissing) then
      var(:,:) = read_var(:,:)
    else 
      write(*,*) 'ERROR : problem reading ''',trim(name_var),''' from ''',trim(filename),''''; stop
    end if

    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine ReadLandUse 

  subroutine ReadSpatial(this,filename)

    class(gridinfo_type)           :: this
    character*256,intent(in)       :: filename
    integer                        :: status
    integer                        :: ncid
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
    
    associate(integerMissing => this%integerMissing, &
              realMissing    => this%realMissing,    &
              stringMissing  => this%stringMissing,  &
              name_att_dx    => this%name_att_dx,    &
              name_att_dy    => this%name_att_dy,    &
              name_dim_x     => this%name_dim_x,     &
              name_dim_y     => this%name_dim_y)

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
    ! Check that file exists and open it
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

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
    ! Allocate local arrays
    !----------------------------------------------------------------------------
    allocate(read_lon(read_nx))
    allocate(read_lat(read_ny))

    !----------------------------------------------------------------------------
    ! Read latitude and longitude values
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid = ncid, varid = varid_x, values = read_lon)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_x),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid = ncid, varid = varid_y, values = read_lat)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_y),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Using allocated(this%lat) and allocated(this%lon) as indicators of a
    ! previous call to ReadSpatial. If no previous call to ReadSpatial, then 
    ! allocate this%lat and this%lon and then transfer values for all read-in 
    ! variables to 'this' instance of gridinfo_type. If this call is not the 
    ! first call to ReadSpatial, then check that read-in values match what was 
    ! previously transfered to 'this' instance of gridinfo_type.
    !----------------------------------------------------------------------------
    if((.NOT.(allocated(this%lat))).AND.(.NOT.(allocated(this%lon)))) then

      !----------------------------------------------------------------------------
      ! Allocate
      !----------------------------------------------------------------------------
      allocate(this%lon(read_nx,read_ny))
      allocate(this%lat(read_nx,read_ny))

      !----------------------------------------------------------------------------
      ! Transfer values
      !----------------------------------------------------------------------------
      if(read_nx /= integerMissing) then; this%n_x = read_nx; else; write(*,*) 'ERROR : problem reading x-dimension resolution from ''',trim(filename),''''; stop; end if
      if(read_ny /= integerMissing) then; this%n_y = read_ny; else; write(*,*) 'ERROR : problem reading y-dimension resolution from ''',trim(filename),''''; stop; end if
      if(read_dx /= realMissing)    then; this%dx  = read_dx; else; write(*,*) 'ERROR : problem reading x-dimension spacing from ''', trim(filename),''''; stop; end if
      if(read_dy /= realMissing)    then; this%dy  = read_dy; else; write(*,*) 'ERROR : problem reading y-dimension spacing from ''', trim(filename),''''; stop; end if
      if(read_lon(1) /= realMissing) then
        do ix = 1, read_nx
          this%lon(ix,:) = read_lon(ix)
        end do
      else; write(*,*) 'ERROR : problem reading lon from ''',trim(filename),''''; stop; end if
      if(read_lat(1) /= realMissing) then
        do iy = 1, read_ny
          this%lat(:,iy) = read_lat(iy)
        end do
      else; write(*,*) 'ERROR : problem reading lon from ''',trim(filename),''''; stop; end if
    
    else 

      !----------------------------------------------------------------------------
      ! Check that values match
      !----------------------------------------------------------------------------
      if(read_nx /= this%n_x) then; write(*,*) 'ERROR : x-dimension resolution from ''',trim(filename),''' does not match x-dimension resolution from other gridded inputs'; stop; end if
      if(read_ny /= this%n_y) then; write(*,*) 'ERROR : y-dimension resolution from ''',trim(filename),''' does not match y-dimension resolution from other gridded inputs'; stop; end if
      if(read_dx /= this%dx)  then; write(*,*) 'ERROR : x-dimension spacing from ''', trim(filename),''' does not match x-dimension spacing from other gridded inputs';  stop; end if
      if(read_dy /= this%dy)  then; write(*,*) 'ERROR : y-dimension spacing from ''', trim(filename),''' does not match y-dimension spacing from other gridded inputs';  stop; end if
      do ix = 1, read_nx
        if (all(read_lon(ix) /= this%lon(ix,:))) then; write(*,*) 'ERROR : longitude values from ''', trim(filename),''' do not match longitude values from other gridded inputs';  stop; end if
      end do
      do iy = 1, read_ny
        if (all(read_lat(iy) /= this%lat(:,iy))) then; write(*,*) 'ERROR : latitude values from ''', trim(filename),''' do not match latitude values from other gridded inputs';  stop; end if
      end do

    end if

    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    status = nf90_close(ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine ReadSpatial


end module GridInfoType

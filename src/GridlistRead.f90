module GridlistRead
  
  use netcdf
  use NamelistRead, only: namelist_type

  implicit none

  type, public :: gridlist_type
  
  integer                            :: ncid  
  integer                            :: n_x
  integer                            :: n_y
  integer                            :: dx
  integer                            :: dy
  integer,allocatable,dimension(:,:) :: vegtyp

  contains

    procedure, public  :: ReadGridlist

  end type

contains

  subroutine ReadGridlist(this,namelist)

    class(gridlist_type)               :: this
    type(namelist_type),intent(in)     :: namelist 
    logical                            :: lexist
    character*256                      :: name
    integer                            :: status
    integer                            :: read_nx
    integer                            :: read_ny
    integer                            :: read_dx
    integer                            :: read_dy
    integer                            :: varid_x
    integer                            :: varid_y
    integer                            :: varid_vegtyp
    integer                            :: dimid_x
    integer                            :: dimid_y
    character*256                      :: name_dim_y
    character*256                      :: name_dim_x 
    character*256                      :: name_att_dy 
    character*256                      :: name_att_dx 
    character*256                      :: name_var_x 
    character*256                      :: name_var_y 
    character*256                      :: name_var_vegtyp 
    integer,allocatable,dimension(:,:) :: read_vegtyp

    associate(filename       => namelist%grid_filename,  &
              integerMissing => namelist%integerMissing, &
              realMissing    => namelist%realMissing,    &
              ncid           => this%ncid)

    !----------------------------------------------------------------------------
    ! Set required variable, dimension, and attribute names
    !----------------------------------------------------------------------------
    name_dim_y      = 'Latitude'
    name_dim_x      = 'Longitude'
    name_var_y      = 'Latitude'
    name_var_x      = 'Longitude'
    name_att_dy     = 'dy'
    name_att_dx     = 'dx'
    name_var_vegtyp = 'vegtyp'

    !----------------------------------------------------------------------------
    ! Set scalars to missing values
    !----------------------------------------------------------------------------
    read_nx          = realMissing
    read_ny          = realMissing
    read_dx          = realMissing
    read_dy          = realMissing
    varid_x          = integerMissing
    varid_y          = integerMissing
    varid_vegtyp     = integerMissing
    dimid_x          = integerMissing
    dimid_y          = integerMissing

    !----------------------------------------------------------------------------
    ! Open the netcdf input file
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Get required variable, dimension, and attribute ids
    !----------------------------------------------------------------------------
    status = nf90_inq_dimid(ncid, trim(name_dim_x), dimid_x)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required dimension ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inq_dimid(ncid, trim(name_dim_y), dimid_y)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required dimension ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inq_varid(ncid, name_var_x, varid_x)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the required variable ''',trim(name_var_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_x,name=name_att_dx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_dx),''' which should be associated with variable ''',name_var_x,''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inq_varid(ncid, name_var_y, varid_y)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required variable ''',trim(name_var_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_y,name=name_att_dy)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required attribute ''',trim(name_att_dy),''' which should be associated with variable ''',name_var_y,''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inq_varid(ncid, name_var_vegtyp, varid_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find required variable ''',trim(name_var_vegtyp),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Get n_x, n_y, dx, and dy
    !----------------------------------------------------------------------------
    status = nf90_inquire_dimension(ncid, dimid_x, len=read_nx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get length of dimension ''',trim(name_dim_x),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_inquire_dimension(ncid, dimid_y, len=read_ny)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to get length of dimension ''',trim(name_dim_y),''' in input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_get_att(ncid=ncid, varid=varid_x, name=name_att_dx, values=read_dx)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_dx),''' associated with variable ''',name_var_x,''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if
    status = nf90_get_att(ncid=ncid, varid=varid_y, name=name_att_dy, values=read_dy)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read attribute ''',trim(name_att_dy),''' associated with variable ''',name_var_y,''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate local arrays
    !----------------------------------------------------------------------------
    allocate(read_vegtyp(read_nx,read_ny))

    !----------------------------------------------------------------------------
    ! Set arrays to missing
    !----------------------------------------------------------------------------
    read_vegtyp(:,:) = integerMissing

    !----------------------------------------------------------------------------
    ! Read variables
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid,varid_vegtyp,read_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_var_vegtyp),''' from input netcdf file ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Transfer values
    !----------------------------------------------------------------------------
    if(read_nx /= integerMissing) then
      this%n_x = read_nx 
    else 
      write(*,*) 'ERROR : problem reading n_x from input file ''',trim(filename),''''; stop
    end if
    if(read_ny /= integerMissing) then
      this%n_y = read_ny
    else 
      write(*,*) 'ERROR : problem reading n_y from input file ''',trim(filename),''''; stop
    end if
    if(read_dx /= realMissing) then
      this%dx = read_dx
    else 
      write(*,*) 'ERROR : problem reading dx from input file ''',trim(filename),''''; stop
    end if
    if(read_dy /= realMissing) then
      this%dy = read_dy
    else 
      write(*,*) 'ERROR : problem reading dy from input file ''',trim(filename),''''; stop
    end if
    if(read_vegtyp(1,1) /= integerMissing) then
      this%vegtyp(:,:) = read_vegtyp(:,:)
    else 
      write(*,*) 'ERROR : problem reading vegtyp from input file ''',trim(filename),''''; stop
    end if

    end associate

  end subroutine ReadGridlist 

end module GridlistRead

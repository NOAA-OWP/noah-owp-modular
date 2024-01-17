!
! This program tests the BMI functionality in Fortran
! The generic code can be used in any BMI-implemented Fortran model
!
program noahmp_driver_test

  !---------------------------------------------------------------------
  !  Modules
  !  Change from non-BMI: Only BMI modules need to be exposed
  !  The rest are used in ../src/NoahMPSurfaceModule.f90
  !---------------------------------------------------------------------
  use bminoahowp
  use bmif_2_0
  implicit none

  !external runsettest
  !external rungettest
  !---------------------------------------------------------------------
  !  Types
  !  Change from non-BMI: only the bmi_noahowp type needed
  !  Forcing, Energy, Water, etc. not needed
  !---------------------------------------------------------------------
    type (bmi_noahowp)  :: m
  
  !---------------------------------------------------------------------
  !  Local variable(s) for BMI testing
  !---------------------------------------------------------------------
    character (len = 80)                              :: arg              ! command line argument for config file
    integer                                           :: status           ! returning status values
    character (len = BMI_MAX_COMPONENT_NAME), pointer :: component_name   ! component name
    integer                                           :: count            ! var counts
    character (len = BMI_MAX_VAR_NAME), pointer       :: names_inputs(:)  ! var names
    character (len = BMI_MAX_VAR_NAME), pointer       :: names_outputs(:) ! var names
    character (len = BMI_MAX_VAR_NAME)                :: iname
    integer                                           :: n_inputs         ! n input vars
    integer                                           :: n_outputs        ! n output vars
    integer                                           :: n_params         ! n calibratable parameters
    integer                                           :: iBMI             ! loop counter
    character (len = 20)                              :: var_type         ! name of variable type
    character (len = 10)                              :: var_units        ! variable units
    integer                                           :: var_itemsize     ! memory size per var array element
    integer                                           :: var_nbytes       ! memory size over full var array
    double precision                                  :: timestep         ! timestep
    double precision                                  :: bmi_time         ! time output from BMI functions
    double precision                                  :: time_until       ! time to which update until should run
    double precision                                  :: end_time         ! time of last model time step
    double precision                                  :: current_time     ! current model time
    character (len = 1)                               :: ts_units         ! timestep units
    real, allocatable, target                         :: var_value_get_real(:) ! value of a variable
    real, allocatable                                 :: var_value_set_real(:) ! value of a variable
    integer, allocatable, target                      :: var_value_get_int(:) ! value of a variable
    integer, allocatable                              :: var_value_set_int(:) ! value of a variable
    integer                                           :: grid_int         ! grid value
    character (len = 20)                              :: grid_type        ! name of grid type
    integer                                           :: grid_rank        ! rank of grid
    integer, dimension(2)                             :: grid_shape       ! shape of grid (change dims if not X * Y grid)
    integer, dimension(3)                             :: grid_shape3d
    integer                                           :: i, j                ! generic index
    integer                                           :: grid_size        ! size of grid (ie. nX * nY)
    double precision, dimension(2)                    :: grid_spacing     ! resolution of grid in X & Y (change dims if not X * Y grid)
    double precision, dimension(2)                    :: grid_origin      ! X & Y origin of grid (change dims if not X * Y grid)
    double precision, allocatable, dimension(:)       :: grid_x           ! X coordinate of grid nodes (change dims if multiple nodes)
    double precision, allocatable, dimension(:)       :: grid_y           ! Y coordinate of grid nodes (change dims if multiple nodes)
    double precision, dimension(1)                    :: grid_z           ! Z coordinate of grid nodes (change dims if multiple nodes)
    integer                                           :: ix,iy,iz,n_x,n_y,n_z,iflat    ! 
    real, pointer                                     :: var_value_get_ptr(:) ! value of a variable for get_value_ptr
    real, allocatable, dimension(:,:)                 :: grid_temp_real   ! local grid to hold getter results for real type
    real, allocatable, dimension(:,:,:)               :: grid3d_temp_real
    integer, allocatable, dimension(:,:)              :: grid_temp_int    ! local grid to hold getter results for int type
    integer, dimension(3)                             :: grid_indices      ! grid indices (change dims as needed)
    character (len = BMI_MAX_VAR_NAME), dimension(17) :: names_params = [character(len=BMI_MAX_VAR_NAME) :: "CWP","VCMX25","MP","MFSNO","RSURF_SNOW","HVT","BEXP","SMCMAX","FRZX", &
                                                                                                            "DKSAT","KDT","RSURF_EXP","REFKDT","AXAJ","BXAJ","SLOPE","SCAMAX"]

    !---------------------------------------------------------------------
    !  Initialize the model
    !---------------------------------------------------------------------
    write(*,'(a)') "INITIALIZE THE MODEL ***********************************************************"
    write(*,'(a)') ''
    write(*,'(a)') "Initializing..."
    call get_command_argument(1, arg)
    status = m%initialize(arg)

    !---------------------------------------------------------------------
    ! Get model information
    ! component_name and input/output_var
    !---------------------------------------------------------------------
    write(*,'(a)') ''
    write(*,'(a)') "MODEL NAME AND INPUT/OUTPUT VARAIBLES *****************************************"
    write(*,'(a)') ''

    status = m%get_component_name(component_name)
    write(*,'(a,a)') "Component name = ", trim(component_name)


    !---------------------------------------------------------------------
    !  Print variable counts
    !---------------------------------------------------------------------
    status = m%get_input_item_count(count)
    write(*,'(a,I5)') "Total input vars = ", count
    n_inputs = count
    status = m%get_output_item_count(count)
    write(*,'(a,I5)') "Total output vars = ", count
    n_outputs = count
    count = size(names_params)
    write(*,'(a,I5)') "Total calibratable parameters = ", count
    n_params = count

    !---------------------------------------------------------------------
    !  Print variable names
    !---------------------------------------------------------------------
    status = m%get_input_var_names(names_inputs)
    do iBMI = 1, n_inputs
      write(*,'(a,a)') "Input var = ", trim(names_inputs(iBMI))
    end do
    status = m%get_output_var_names(names_outputs)
    do iBMI = 1, n_outputs
      write(*,'(a,a)') "Output var = ", trim(names_outputs(iBMI))
    end do
    do iBMI = 1, n_params
      write(*,'(a,a)') "Calibratable param = ", trim(names_params(iBMI))
    end do

    !---------------------------------------------------------------------
    !  Print basic info
    !---------------------------------------------------------------------
    write(*,'(a)')   ''
    write(*,'(a,a)') "VARIALBE INFORMTION*************************************************************"
    write(*,'(a)')   ''

    ! loop over variables
    count = n_inputs + n_outputs + n_params
    do j = 1, count

      ! get var name
      if(j <= n_inputs) then
        iname = trim(names_inputs(j))
      else if(j <= n_inputs + n_outputs) then
        iname = trim(names_outputs(j - n_inputs))
      else
        iname = trim(names_params(j - n_inputs - n_outputs))
      end if

      ! get basic var info
      status = m%get_var_grid(trim(iname),grid_int)
      status = m%get_grid_rank(grid_int,grid_rank)
      status = m%get_grid_size(grid_int, grid_size)
      status = m%get_var_type(trim(iname), var_type)
      status = m%get_var_units(trim(iname), var_units)
      status = m%get_var_itemsize(trim(iname), var_itemsize)
      status = m%get_var_nbytes(trim(iname), var_nbytes)

      ! print basic var info
      write(*,'(a,a)')  "The variable ", trim(iname)
      write(*,'(a,I5)') "has a grid id of ",grid_int
      write(*,'(a,I5)') "has a grid size of ",grid_size
      write(*,'(a,a)')  "has a type of ", var_type
      write(*,'(a,a)')  "units of ", var_units
      write(*,'(a,I5)') "a size (bytes per variable instance) of ", var_itemsize
      write(*,'(a,I5)') "and total n bytes (bytes across grid) of ", var_nbytes

      ! print grid info
      if(grid_rank == 2) then
        status = m%get_grid_shape(grid_int, grid_shape)
        n_x = grid_shape(2)
        n_y = grid_shape(1)
        write(*,'(a,I5)') "has a grid row count (n_y) of ",n_y
        write(*,'(a,I5)') "has a grid column count (n_x) of ",n_x
      else if(grid_rank == 3) then
        status = m%get_grid_shape(grid_int, grid_shape3d)
        n_z = grid_shape3d(1)
        n_y = grid_shape3d(2)
        n_x = grid_shape3d(3)
        write(*,'(a,I5)') "has a grid row count (n_y) of ",n_y
        write(*,'(a,I5)') "has a grid column count (n_x) of ",n_x
        write(*,'(a,I5)') "has a grid z count (n_z) of ",n_z
      end if
      write(*,'(a)')    ''

    end do

    !---------------------------------------------------------------------
    ! Print time information
    !---------------------------------------------------------------------
  
    write(*,'(a)') ''
    write(*,'(a)') "GET TIME INFORMATION ***********************************************************"
    write(*,'(a)') ''
    status = m%get_start_time(bmi_time)
    write(*,'(a,F20.5)') "The start time is ", bmi_time
    status = m%get_current_time(bmi_time)
    write(*,'(a,F20.5)') "The current time is ", bmi_time
    status = m%get_end_time(bmi_time)
    write(*,'(a,F20.5)') "The end time is ", bmi_time
    status = m%get_time_step(timestep)
    status = m%get_time_units(ts_units)
    write(*,'(a,F20.5)') "The time step is ", timestep
    write(*,'(a,a)') "with a unit of ", ts_units

    !---------------------------------------------------------------------
    ! Run the model
    !---------------------------------------------------------------------

    write(*,'(a)') ''
    write(*,'(a)') "RUN THE MODEL ******************************************************************"
    write(*,'(a)') ''

    !---------------------------------------------------------------------
    ! Run some time steps with the update_until function
    !---------------------------------------------------------------------
    time_until = 36000.0
    status = m%update_until(time_until)
  
    !---------------------------------------------------------------------
    ! Run the rest of the time with update in a loop
    !---------------------------------------------------------------------

    ! get the current and end time for running the execution loop
    status = m%get_current_time(current_time)
    status = m%get_end_time(end_time)

    ! loop through while current time <= end time
    write(*,'(a)') "Running..."
    do while (current_time < end_time)
      status = m%update()                       ! run the model one time step
      status = m%get_current_time(current_time) ! update current_time
    end do

    !---------------------------------------------------------------------
    ! Finalize with BMI
    !---------------------------------------------------------------------
    write(*,'(a)') "Finalizing..."
    status = m%finalize()
    write(*,'(a)') "Model is finalized!"
    write(*,'(a)') ''

    !---------------------------------------------------------------------
    ! Test the get/set_value functionality with BMI
    !---------------------------------------------------------------------

    write(*,'(a)') ''
    write(*,'(a)') "TEST get/set_value FUNCTIONALITY WITH BMI  ***************************************"
    do i = 1, count

      !---------------------------------------------------------------------
      ! Determine variable name 
      !---------------------------------------------------------------------
      if(i <= n_inputs) then
        iname = trim(names_inputs(i))
      else if(i <= n_inputs + n_outputs) then
        iname = trim(names_outputs(i - n_inputs))
      else
        iname = trim(names_params(i - n_inputs - n_outputs))
      end if
      write(*,'(a)') ''
      write(*,'(a)') trim(iname)

      !---------------------------------------------------------------------
      ! Test get_value functionality
      !---------------------------------------------------------------------
      write(*,'(a,a)') 'getting ',trim(iname)
      call rungettest(m,iname)
      if(trim(iname) .eq. 'SMCMAX') then
        write(*,'(a,a)') 'getting FRZX (FRZX is recalculated when setting SMCMAX)'
        call rungettest(m,'FRZX')
      else if(trim(iname) .eq. 'DKSAT') then
        write(*,'(a,a)') 'getting KDT (KDT is recalculated when setting DKSAT)'
        call rungettest(m,'KDT')
      else if(trim(iname) .eq. 'REFKDT') then
        write(*,'(a,a)') 'getting KDT (KDT is recalculated when setting REFKDT)'
        call rungettest(m,'KDT')
      end if

      !---------------------------------------------------------------------
      ! Test set_value functionality
      !---------------------------------------------------------------------
      write(*,'(a,a)') 'setting ',trim(iname)
      call runsettest(m,iname)

      !---------------------------------------------------------------------
      ! Run get_value test again to print updated values
      !---------------------------------------------------------------------
      write(*,'(a,a)') 'getting ',trim(iname)
      call rungettest(m,iname)
      if(trim(iname) .eq. 'SMCMAX') then
        write(*,'(a,a)') 'getting FRZX (FRZX recalculated when setting SMCMAX)'
        call rungettest(m,'FRZX')
      else if(trim(iname) .eq. 'DKSAT') then
        write(*,'(a,a)') 'getting KDT (KDT is recalculated when setting DKSAT)'
        call rungettest(m,'KDT')
      else if(trim(iname) .eq. 'REFKDT') then
        write(*,'(a,a)') 'getting KDT (KDT is recalculated when setting REFKDT)'
        call rungettest(m,'KDT')
      end if

    end do


  !---------------------------------------------------------------------
  ! Test the get_value_ptr functionality with BMI
  !---------------------------------------------------------------------
    print*,''
    print*,"TEST get_value_ptr FUNCTIONALITY WITH BMI***************************************"
    var_value_get_ptr => var_value_get_real
    ! test the get value pointer  functions
    ! Loop through the input vars
    do iBMI = 1, n_inputs
      status = m%get_value_ptr(trim(names_inputs(iBMI)), var_value_get_ptr)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_inputs(iBMI)), " from get_value_ptr returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_inputs(iBMI)), " from get_value_ptr returned ", status, " TEST FAILED!" 
      end if
    end do

    ! Loop through the output vars
    do iBMI = 1, n_outputs
      status = m%get_value_ptr(trim(names_outputs(iBMI)), var_value_get_ptr)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_outputs(iBMI)), " from get_value_ptr returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_outputs(iBMI)), " from get_value_ptr returned ", status, " TEST FAILED!" 
      end if
    end do

  !---------------------------------------------------------------------
  ! Test the get_value_at_indices functionality with BMI
  !---------------------------------------------------------------------
    ! Loop through the input vars
    do iBMI = 1, n_inputs
      status = m%get_value_at_indices(trim(names_inputs(iBMI)), var_value_get_real, grid_indices)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_inputs(iBMI)), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_inputs(iBMI)), " from get_value_at_indices returned ", status, " TEST FAILED!" 
      end if
      status = m%set_value_at_indices(trim(names_inputs(iBMI)), grid_indices, var_value_set_real)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_inputs(iBMI)), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_inputs(iBMI)), " from set_value_at_indices returned ", status, " TEST FAILED!" 
      end if
    end do
    
    ! Loop through the output vars
    do iBMI = 1, n_outputs
      status = m%get_value_at_indices(trim(names_outputs(iBMI)), var_value_get_real, grid_indices)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_outputs(iBMI)), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_outputs(iBMI)), " from get_value_at_indices returned ", status, " TEST FAILED!" 
      end if
      status = m%set_value_at_indices(trim(names_outputs(iBMI)), grid_indices, var_value_set_real)
      if ( status .eq. BMI_FAILURE ) then
        print*, trim(names_outputs(iBMI)), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
      else
        print*, trim(names_outputs(iBMI)), " from set_value_at_indices returned ", status, " TEST FAILED!" 
      end if
    end do

    nullify( var_value_get_ptr )
    if (allocated(var_value_get_real)) deallocate(var_value_get_real)
    if (allocated(var_value_set_real)) deallocate(var_value_set_real)

  !---------------------------------------------------------------------
  ! Test the grid info functionality with BMI
  !---------------------------------------------------------------------

    ! All vars currently have same spatial discretization
    ! Modify to test all discretizations if > 1
    
    print*,''
    print*,"TEST GRID INFO FUNCTIONALITY WITH BMI***************************************"

    ! get_var_grid
    iBMI = 1
    status = m%get_var_grid(trim(names_inputs(iBMI)), grid_int)
    print*, "The integer value for the ", trim(names_inputs(iBMI)), " grid is ", grid_int
    
    ! get_grid_type
    status = m%get_grid_type(grid_int, grid_type)
    print*, "The grid type for ", trim(names_inputs(iBMI)), " is ", trim(grid_type)
    
    ! get_grid_rank
    status = m%get_grid_rank(grid_int, grid_rank)
    print*, "The grid rank for ", trim(names_inputs(iBMI)), " is ", grid_rank
    
    ! get_grid_shape
    ! only scalars implemented thus far
    status = m%get_grid_shape(grid_int, grid_shape)
    if(grid_shape(1) == -1) then
      print*, "No grid shape for the grid type/rank"
    else
      print*, "The grid shape for the ",trim(names_inputs(iBMI)), " grid is ", grid_shape
    end if
    
    ! get_grid_size
    status = m%get_grid_size(grid_int, grid_size)
    print*, "The grid size for ", trim(names_inputs(iBMI)), " is ", grid_size
    
    ! get_grid_spacing
    ! only scalars implemented thus far
    status = m%get_grid_spacing(grid_int, grid_spacing)
    if(grid_spacing(1) == -1.d0) then
      print*, "No grid spacing for the grid type/rank"
    else
      print*, "The grid spacing for the ",trim(names_inputs(iBMI)), " grid is ", grid_spacing
    end if
    
    ! get_grid_origin
    ! only scalars implemented thus far
    status = m%get_grid_origin(grid_int, grid_origin)
    if(grid_origin(1) == -1.d0) then
      print*, "No grid origin for the grid type/rank"
    else
      print*, "The grid origin for the ",trim(names_inputs(iBMI)), " grid is ", grid_origin
    end if
    
    ! get_grid_x/y/z
    ! should return 0 for a 1 node "grid" because not currently spatially explicit
    if(allocated(grid_x)) deallocate(grid_x)
    if(allocated(grid_y)) deallocate(grid_y)
    allocate(grid_x(grid_shape(2)))
    allocate(grid_y(grid_shape(1)))
    status = m%get_grid_x(grid_int, grid_x)
    status = m%get_grid_y(grid_int, grid_y)
    status = m%get_grid_z(grid_int, grid_z)
    print*, "The X coord for grid ", grid_int, " is ", grid_x
    print*, "The Y coord for grid ", grid_int, " is ", grid_y
    print*, "The Z coord for grid ", grid_int, " is ", grid_z

  !---------------------------------------------------------------------
  ! The following functions are not implemented/only return BMI_FAILURE
  ! Change if your model implements them
  !---------------------------------------------------------------------
    print*,''
    print*,"NOTES **********************************************************************"
    print*, "The unstructured grid functions will return BMI_FAILURE"
    print*, "BMI functions that require ", trim(component_name), &
            " to use pointer vars are not implemented"
  
  !---------------------------------------------------------------------
  ! End test
  !---------------------------------------------------------------------
    print*,''
    print*, "All done testing!"
    print*, ''

end program
  
  subroutine rungettest(m,iname)
    
    use bminoahowp
    use bmif_2_0
    implicit none

    interface printvar
      subroutine print1dint(values)
        integer,dimension(:),intent(in) :: values
      end subroutine
      subroutine print1dreal(values)
        real,dimension(:),intent(in) :: values
      end subroutine
      subroutine print2dint(values)
        integer,dimension(:,:),intent(in) :: values
      end subroutine
      subroutine print2dreal(values)
        real,dimension(:,:),intent(in) :: values
      end subroutine
      subroutine print3dint(values)
        integer,dimension(:,:,:),intent(in) :: values
      end subroutine
      subroutine print3dreal(values)
        real,dimension(:,:,:),intent(in) :: values
      end subroutine
    end interface
  
    type (bmi_noahowp), intent(in)            :: m                  !
    character (len = *)                       :: iname              !
    character (len = 20)                      :: var_type           ! name of variable type
    integer, dimension(2)                     :: grid_shape         ! shape of grid (change dims if not X * Y grid)
    integer, dimension(3)                     :: grid_shape3d       !
    real, allocatable,dimension(:), target    :: var_value_get_real ! value of a variable
    integer, allocatable,dimension(:), target :: var_value_get_int  ! value of a variable
    real, allocatable, dimension(:,:)         :: grid_temp_real     ! local grid to hold getter results for real type
    real, allocatable, dimension(:,:,:)       :: grid3d_temp_real   !
    integer, allocatable, dimension(:,:)      :: grid_temp_int      ! local grid to hold getter results for int type
    integer                                   :: status             !
    integer                                   :: grid_int           !
    integer                                   :: grid_rank          !
    integer                                   :: grid_size          !
    integer                                   :: n_x,n_y,n_z        !
  
    status = m%get_var_grid(trim(iname),grid_int)
    status = m%get_grid_rank(grid_int, grid_rank)
    status = m%get_var_type(trim(iname), var_type)
    status = m%get_grid_size(grid_int, grid_size)
  
    if (grid_rank == 2) then
  
      ! get dims
      status = m%get_grid_shape(grid_int, grid_shape)
      n_x = grid_shape(2)
      n_y = grid_shape(1)
  
      if(var_type=='integer') then
  
        ! Allocate arrays
        if(allocated(var_value_get_int)) deallocate(var_value_get_int)
        if(allocated(grid_temp_int)) deallocate(grid_temp_int)
        allocate(var_value_get_int(grid_size))
        allocate(grid_temp_int(n_x,n_y))
  
        ! Get and print
        status = m%get_value(trim(iname), var_value_get_int)
        write(*,'(a)') "flattened ="
        call printvar(var_value_get_int)
        grid_temp_int = reshape(var_value_get_int,[n_x,n_y])
        write(*,'(a)') "in XY ="
        call printvar(grid_temp_int)
  
      else if (var_type =='real') then
  
        ! Allocate arrays
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        if(allocated(grid_temp_real)) deallocate(grid_temp_real)
        allocate(var_value_get_real(grid_size))
        allocate(grid_temp_real(n_x,n_y))
  
        ! Get and print
        status = m%get_value(trim(iname), var_value_get_real)
        write(*,'(a)') "flattened ="
        call printvar(var_value_get_real)
        grid_temp_real = reshape(var_value_get_real,[n_x,n_y])
        write(*,'(a)') "in XY ="
        call printvar(grid_temp_real)
  
      else
  
        ! Catch unrecognized data type
        write(*,'(a,a,a)') "**ERROR - unrecognized data type for variable ",trim(iname),"**"
  
      end if
  
    else if (grid_rank == 3) then

      status = m%get_grid_shape(grid_int, grid_shape3d)
      n_z = grid_shape3d(1)
      n_y = grid_shape3d(2)
      n_x = grid_shape3d(3)

      if(var_type=='real') then
  
        ! Allocate arrays
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        if(allocated(grid3d_temp_real)) deallocate(grid3d_temp_real)
        allocate(var_value_get_real(grid_size))
        allocate(grid3d_temp_real(n_x,n_y,n_z))
  
        ! Print existing values
        status = m%get_value(trim(trim(iname)), var_value_get_real)
        grid3d_temp_real = reshape(var_value_get_real,[n_x,n_y,n_z])
        write(*,'(a)') "in XYZ ="
        call printvar(grid3d_temp_real)
  
      else
                  
        ! If not real type
        ! Note: functionality not implemented for integer types because there are no 3D int vars
        write(*,'(a,a,a)') "**ERROR - unrecognized data type for variable ",trim(iname),"**"
  
      end if
    end if
  
  end subroutine
  
  subroutine runsettest(m,iname)
    
    use bminoahowp
    use bmif_2_0
    implicit none
  
    interface printvar
      subroutine print1dint(values)
        integer,dimension(:),intent(in) :: values
      end subroutine
      subroutine print1dreal(values)
        real,dimension(:),intent(in) :: values
      end subroutine
      subroutine print2dint(values)
        integer,dimension(:,:),intent(in) :: values
      end subroutine
      subroutine print2dreal(values)
        real,dimension(:,:),intent(in) :: values
      end subroutine
      subroutine print3dint(values)
        integer,dimension(:,:,:),intent(in) :: values
      end subroutine
      subroutine print3dreal(values)
        real,dimension(:,:,:),intent(in) :: values
      end subroutine
    end interface

    type (bmi_noahowp), intent(inout)         :: m                  !
    character (len = *)                       :: iname              !
    character (len = 20)                      :: var_type           ! name of variable type
    integer, dimension(2)                     :: grid_shape         ! shape of grid (change dims if not X * Y grid)
    integer, dimension(3)                     :: grid_shape3d       !
    real, allocatable,dimension(:), target    :: var_value_get_real ! value of a variable
    integer, allocatable,dimension(:), target :: var_value_get_int  ! value of a variable
    real, allocatable,dimension(:)            :: var_value_set_real ! value of a variable
    integer, allocatable,dimension(:)         :: var_value_set_int  ! value of a variable
    real, allocatable, dimension(:,:)         :: grid_temp_real     ! local grid to hold getter results for real type
    real, allocatable, dimension(:,:,:)       :: grid3d_temp_real   !
    integer, allocatable, dimension(:,:)      :: grid_temp_int      ! local grid to hold getter results for int type
    integer                                   :: status             !
    integer                                   :: grid_int           !
    integer                                   :: grid_rank          !
    integer                                   :: grid_size          !
    integer                                   :: j,n_x,n_y,n_z        !
   
    ! Get variable name and shape/size
    status = m%get_var_grid(trim(iname),grid_int)
    status = m%get_grid_rank(grid_int, grid_rank)
    status = m%get_var_type(trim(iname), var_type)
    status = m%get_grid_size(grid_int, grid_size)
  
    if (grid_rank == 2) then
  
      ! get dims
      status = m%get_grid_shape(grid_int, grid_shape)
      n_x = grid_shape(2)
      n_y = grid_shape(1)
  
      ! address int and real type differently
      if(var_type=='integer') then
  
        ! Allocate arrays
        if(allocated(var_value_get_int)) deallocate(var_value_get_int)
        if(allocated(var_value_set_int)) deallocate(var_value_set_int)
        if(allocated(grid_temp_int)) deallocate(grid_temp_int)
        allocate(var_value_get_int(grid_size))
        allocate(var_value_set_int(grid_size))
        allocate(grid_temp_int(n_x,n_y))
  
        ! Create and print replacement values
        var_value_set_int = (/ (j, j = 1, grid_size) /)
        write(*,'(a)') "our replacement value (flattened) ="
        call printvar(var_value_set_int)
        grid_temp_int = reshape(var_value_set_int,[n_x,n_y])
        write(*,'(a)') "our replacement value (in XY) ="
        call printvar(grid_temp_int)
  
        ! Set replacement values and print updated values
        status = m%set_value(trim(iname), var_value_set_int)
        if(status == BMI_FAILURE) then
          write(*,'(a,a,a)') "**FAILED to set variable ",trim(iname)," (variable may not have set_value listing)**"
        end if
  
      ! real type
      else if(var_type=='real') then
  
        ! Allocate arrays
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        if(allocated(var_value_set_real)) deallocate(var_value_set_real)
        if(allocated(grid_temp_real)) deallocate(grid_temp_real)
        allocate(var_value_get_real(grid_size))
        allocate(var_value_set_real(grid_size))
        allocate(grid_temp_real(n_x,n_y))
  
        ! Create and print replacement values
        var_value_set_real = (/ (real(j), j = 1, grid_size) /)
        write(*,'(a)') "our replacement value (flattened) = "
        call printvar(var_value_set_real)
        grid_temp_real = reshape(var_value_set_real,[n_x,n_y])
        write(*,'(a)') "our replacement value (in XY) ="
        call printvar(grid_temp_real)
  
        ! Set replacement values and print updated values
        status = m%set_value(trim(iname), var_value_set_real)
        if(status == BMI_FAILURE) then
          write(*,'(a,a,a)') "**FAILED to set variable ",trim(iname)," (variable may not have set_value listing)**"
        end if
  
      else
  
        !! If not real or integer type
        write(*,'(a,a,a)') "**ERROR - unrecognized data type for variable ",trim(iname),"**"
  
      end if
  
    else if (grid_rank == 3) then
  
      status = m%get_grid_shape(grid_int, grid_shape3d)
      n_z = grid_shape3d(1)
      n_y = grid_shape3d(2)
      n_x = grid_shape3d(3)
  
      if(var_type=='real') then
  
        ! Allocate arrays
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        if(allocated(var_value_set_real)) deallocate(var_value_set_real)
        if(allocated(grid3d_temp_real)) deallocate(grid3d_temp_real)
        allocate(var_value_get_real(grid_size))
        allocate(var_value_set_real(grid_size))
        allocate(grid3d_temp_real(n_x,n_y,n_z))
  
        ! Create and print replacement values
        var_value_set_real = (/ (j, j = 1, grid_size) /)
        grid3d_temp_real = reshape(var_value_set_real,[n_x,n_y,n_z])
        write(*,'(a)') "our replacement value (in XYZ) ="
        call printvar(grid3d_temp_real)
  
        ! Set replacement values and print updated values
        status = m%set_value(trim(iname), var_value_set_real)
        if(status == BMI_FAILURE) then
          write(*,'(a,a,a)') "**FAILED to set variable ",trim(iname)," (variable may not have set_value listing)**"
        end if
  
      else
  
        ! If not real type
        ! Note: functionality not implemented for integer types because there are no 3D int vars
        write(*,'(a,a,a)') "**ERROR - unrecognized data type for variable ",trim(iname),"**"
  
      end if
  
    end if
  
  end subroutine

  subroutine print1dint(vals)
    integer,dimension(:),intent(in) :: vals 
    integer                         :: i
    do i = 1, size(vals,1)
      write(*,'(G0.5,1x)',advance='no') vals(i) 
    end do
    write(*,*) ''
  end subroutine
  
  subroutine print1dreal(vals)
    real,dimension(:),intent(in) :: vals 
    integer                      :: i
    do i = 1, size(vals,1)
      write(*,'(G0.5,1x)',advance='no') vals(i) 
    end do
    write(*,*) ''
  end subroutine
  
  subroutine print2dint(vals)
    integer,dimension(:,:),intent(in) :: vals 
    integer                           :: iy,ix
    do iy = 1, size(vals,2)
      do ix = 1, size(vals,1)
        write(*,'(G0,1x)',advance='no') vals(ix,iy) 
      end do
      write(*,*) ''
    end do
  end subroutine
  
  subroutine print2dreal(vals)
    real,dimension(:,:),intent(in) :: vals 
    integer                        :: iy,ix
    do iy = 1, size(vals,2)
      do ix = 1, size(vals,1)
        write(*,'(G0.5,1x)',advance='no') vals(ix,iy) 
      end do
      write(*,*) ''
    end do
  end subroutine
  
  subroutine print3dint(vals)
    integer,dimension(:,:,:),intent(in) :: vals 
    integer                             :: iz,iy,ix
    do iz = 1, size(vals,3)
      write(*,'(a,I2)') 'z =',iz
      do iy = 1, size(vals,2)
        do ix = 1, size(vals,1)
          write(*,'(G0.5,1x)',advance='no') vals
        end do
        write(*,*) ''
      end do
    end do
  end subroutine
  
  subroutine print3dreal(vals)
    real,dimension(:,:,:),intent(in) :: vals 
    integer                          :: iz,iy,ix
    integer                          :: nz,ny,nx
    do iz = 1, size(vals,3)
      write(*,'(a,I2)') 'z =',iz
      do iy = 1, size(vals,2)
        do ix = 1, size(vals,1)
          write(*,'(G0.5,1x)',advance='no') vals(ix,iy,iz) 
        end do
        write(*,*) ''
      end do
    end do
  end subroutine

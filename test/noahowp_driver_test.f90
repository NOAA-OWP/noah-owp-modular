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
    integer, allocatable, dimension(:,:)              :: grid_temp_int    ! local grid to hold getter results for int type
    integer, dimension(3)                             :: grid_indices      ! grid indices (change dims as needed)
    real                                              :: time_1, time_2

  !---------------------------------------------------------------------
  !  Initialize
  !---------------------------------------------------------------------
    print*,"INITIALIZE THE MODEL ***********************************************************"
    print*, "Initializing..."
    call get_command_argument(1, arg)
    call cpu_time(time_1)
    status = m%initialize(arg)
    call cpu_time(time_2)
    print*,'Model initialization cpu_time (s) = ',time_2-time_1

  !---------------------------------------------------------------------
  ! Get model information
  ! component_name and input/output_var
  !---------------------------------------------------------------------
    print*,''
    print*,"MODEL NAME AND INPUT/OUTPUT VARAIBLES *****************************************"

    status = m%get_component_name(component_name)
    print*, "Component name = ", trim(component_name)

    status = m%get_input_item_count(count)
    print*, "Total input vars = ", count
    n_inputs = count

    status = m%get_output_item_count(count)
    print*, "Total output vars = ", count
    n_outputs = count

    status = m%get_input_var_names(names_inputs)
    do iBMI = 1, n_inputs
      print*, "Input var = ", trim(names_inputs(iBMI))
    end do

    status = m%get_output_var_names(names_outputs)
    do iBMI = 1, n_outputs
      print*, "Output var = ", trim(names_outputs(iBMI))
    end do
    
    ! Sum input and outputs to get total vars
    count = n_inputs + n_outputs

    ! Get other variable info
    print*,''
    print*,"VARIALBE INFORMTION*************************************************************"
    do j = 1, count
      if(j <= n_inputs) then
        iname = trim(names_inputs(j))
      else
        iname = trim(names_outputs(j - n_inputs))
      end if
      status = m%get_var_grid(trim(iname),grid_int)
      status = m%get_grid_shape(grid_int, grid_shape)
      status = m%get_grid_size(grid_int, grid_size)
      status = m%get_var_type(trim(iname), var_type)
      status = m%get_var_units(trim(iname), var_units)
      status = m%get_var_itemsize(trim(iname), var_itemsize)
      status = m%get_var_nbytes(trim(iname), var_nbytes)
      n_x = grid_shape(2)
      n_y = grid_shape(1)
      print*, "The variable ", trim(iname)
      print*, "    has a grid id of ",grid_int
      print*, "    has a grid row count (n_y) of ",n_y
      print*, "    has a grid column count (n_x) of ",n_x
      print*, "    has a total cell count (n_x * n_y) of ",grid_size
      print*, "    has a type of ", var_type
      print*, "    units of ", var_units
      print*, "    a size (bytes per grid cell or variable instance) of ", var_itemsize
      print*, "    and total n bytes (bytes across grid) of ", var_nbytes
    end do

  !---------------------------------------------------------------------
  ! Get time information
  !---------------------------------------------------------------------
  
    print*,''
    print*,"GET TIME INFORMATION ***********************************************************"

    status = m%get_start_time(bmi_time)
    print*, "The start time is ", bmi_time

    status = m%get_current_time(bmi_time)
    print*, "The current time is ", bmi_time

    status = m%get_end_time(bmi_time)
    print*, "The end time is ", bmi_time

    status = m%get_time_step(timestep)
    status = m%get_time_units(ts_units)
    print*, "The time step is ", timestep
    print*, "with a unit of ", ts_units

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

    print*,''
    print*,"RUN THE MODEL ******************************************************************"

    ! loop through while current time <= end time
    print*, "Running..."
    do while (current_time < end_time)
      status = m%update()                       ! run the model one time step
      status = m%get_current_time(current_time) ! update current_time
    end do

    !---------------------------------------------------------------------
    ! Finalize with BMI
    !---------------------------------------------------------------------
    print*, "Finalizing..."
    status = m%finalize()
    print*, "Model is finalized!"
    print*,''

  !---------------------------------------------------------------------
  ! Test the get/set_value functionality with BMI
  !---------------------------------------------------------------------

    print*,''
    print*,"TEST get/set_value FUNCTIONALITY WITH BMI  (2D VARIABLES ONLY)********************"
    do i = 1, count
      if(i <= n_inputs) then
        iname = trim(names_inputs(i))
      else
        iname = trim(names_outputs(i - n_inputs))
      end if

      ! Get variable name and shape/size
      status = m%get_var_type(trim(iname), var_type)
      status = m%get_var_grid(trim(iname),grid_int)
      status = m%get_grid_size(grid_int, grid_size)
      status = m%get_grid_shape(grid_int, grid_shape)
      n_x = grid_shape(2)
      n_y = grid_shape(1)
      print*,trim(trim(iname))
      print*,'    column count (n_x) = ',n_x
      print*,'    row count (n_y) = ',n_y

      ! Address real and integer variables differently. Do real variables now
      if(var_type=='real') then

        ! Allocate record keeping arrays
        if(.NOT.allocated(var_value_get_real)) allocate(var_value_get_real(grid_size))
        if(.NOT.allocated(var_value_set_real)) allocate(var_value_set_real(grid_size))
        if(.NOT.allocated(grid_temp_real)) allocate(grid_temp_real(n_x,n_y))
        if(size(var_value_get_real).ne.grid_size) then
          deallocate(var_value_get_real)
          allocate(var_value_get_real(grid_size))
        end if
        if(size(var_value_set_real).ne.grid_size) then
          deallocate(var_value_set_real)
          allocate(var_value_set_real(grid_size))
        end if
        if((size(grid_temp_real,1).ne.n_x).or.(size(grid_temp_real,2).ne.n_y)) then
          deallocate(grid_temp_real)
          allocate(grid_temp_real(n_x,n_y))
        end if

        ! Print existing value(s)
        call cpu_time(time_1)
        status = m%get_value(trim(iname), var_value_get_real)
        call cpu_time(time_2)
        print*, "    get_value function cpu time (s) =",time_2-time_1
        print*, "    from get_value (flattened) ="
        print*, var_value_get_real
        print*, "    from get_value (in XY) ="
        grid_temp_real = reshape(var_value_get_real,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(F10.1,1x)',advance='no') grid_temp_real(ix,iy) 
          end do
          write(*,*) ''
        end do

        ! Print replacement value(s) and set
        do j = 1, grid_size
          var_value_set_real(j) = j
        end do
        print*, "    our replacement value (flattened) = "
        print*, var_value_set_real
        print*, "    our replacement value (in XY) ="
        grid_temp_real = reshape(var_value_set_real,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(F10.1,1x)',advance='no') grid_temp_real(ix,iy) 
          end do
          write(*,*) ''
        end do
        call cpu_time(time_1)
        status = m%set_value(trim(iname), var_value_set_real)
        call cpu_time(time_2)
        print*, "    set_value function cpu time (s) =",time_2-time_1

        ! Print updated values
        status = m%get_value(trim(iname), var_value_get_real)
        print*, "    and the new value (flattened) = "
        print*, var_value_get_real
        print*, "    and the new value (in XY) = "
        grid_temp_real = reshape(var_value_get_real,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(F10.1,1x)',advance='no') grid_temp_real(ix,iy) 
          end do
          write(*,*) ''
        end do

      ! now do integer type variables
      else if(var_type=='integer') then

        ! Allocate record keeping arrays
        if(.NOT.allocated(var_value_get_int)) allocate(var_value_get_int(grid_size))
        if(.NOT.allocated(var_value_set_int)) allocate(var_value_set_int(grid_size))
        if(.NOT.allocated(grid_temp_int)) allocate(grid_temp_int(n_x,n_y))
        if(size(var_value_get_int).ne.grid_size) then
          deallocate(var_value_get_int)
          allocate(var_value_get_int(grid_size))
        end if
        if(size(var_value_set_int).ne.grid_size) then
          deallocate(var_value_set_int)
          allocate(var_value_set_int(grid_size))
        end if
        if((size(grid_temp_int,1).ne.n_x).or.(size(grid_temp_int,2).ne.n_y)) then
          deallocate(grid_temp_int)
          allocate(grid_temp_int(n_x,n_y))
        end if

        ! Print existing value(s)
        call cpu_time(time_1)
        status = m%get_value(trim(iname), var_value_get_int)
        call cpu_time(time_2)
        print*, "    get_value function cpu time (s) =",time_2-time_1
        print*, "    from get_value (flattened) ="
        print*, var_value_get_int
        print*, "    from get_value (in XY) ="
        grid_temp_int = reshape(var_value_get_int,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(i10,1x)',advance='no') grid_temp_int(ix,iy) 
          end do
          write(*,*) ''
        end do

        ! Print replacement value(s) and set
        do j = 1, grid_size
          var_value_set_int(j) = j
        end do
        print*, "    our replacement value (flattened) ="
        print*, var_value_set_int
        print*, "    our replacement value (in XY) ="
        grid_temp_int = reshape(var_value_set_int,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(i10,1x)',advance='no') grid_temp_int(ix,iy) 
          end do
          write(*,*) ''
        end do
        call cpu_time(time_1)
        status = m%set_value(trim(iname), var_value_set_int)
        call cpu_time(time_2)
        print*, "    set_value function cpu time (s) =",time_2-time_1

        ! Print updated values
        status = m%get_value(trim(iname), var_value_get_int)
        print*, "    and the new value (flattened) ="
        print*, var_value_get_int
        print*, "    and the new value (in XY) ="
        grid_temp_int = reshape(var_value_get_int,[n_x,n_y])
        do iy = 1, n_y
          do ix = 1, n_x
            write(*,'(i10,1x)',advance='no') grid_temp_int(ix,iy) 
          end do
          write(*,*) ''
        end do
      
      else

        !! If not real or integer type
        print*,'    unrecoganized data type --- TEST FAILED!'

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
    deallocate(var_value_get_real)
    deallocate(var_value_set_real)

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

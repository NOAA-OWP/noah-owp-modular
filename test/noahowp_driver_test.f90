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
    character (len = BMI_MAX_VAR_NAME)                :: name             ! var name
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
    real, allocatable, target                         :: var_value_get_real_temp(:) ! value of a variable
    real, allocatable                                 :: var_value_set_real(:) ! value of a variable
    integer, allocatable, target                      :: var_value_get_int(:) ! value of a variable
    integer, allocatable                              :: var_value_set_int(:) ! value of a variable
    integer                                           :: grid_int         ! grid value
    character (len = 20)                              :: grid_type        ! name of grid type
    integer                                           :: grid_rank        ! rank of grid
    integer, dimension(2)                             :: grid_shape       ! shape of grid (change dims if not X * Y grid)
    integer                                           :: j                ! generic index
    integer                                           :: grid_size        ! size of grid (ie. nX * nY)
    double precision, dimension(2)                    :: grid_spacing     ! resolution of grid in X & Y (change dims if not X * Y grid)
    double precision, dimension(2)                    :: grid_origin      ! X & Y origin of grid (change dims if not X * Y grid)
    double precision, dimension(1)                    :: grid_x           ! X coordinate of grid nodes (change dims if multiple nodes)
    double precision, dimension(1)                    :: grid_y           ! Y coordinate of grid nodes (change dims if multiple nodes)
    double precision, dimension(1)                    :: grid_z           ! Y coordinate of grid nodes (change dims if multiple nodes)
    real                                              :: temp_scalar
    real, pointer                                     :: var_value_get_real_ptr(:) ! value of a variable for get_value_ptr
    integer, pointer                                  :: var_value_get_int_ptr(:) ! value of a variable for get_value_ptr

    integer, allocatable, dimension(:)                :: grid_indices       ! grid indices (change dims as needed)
    character (len = BMI_MAX_VAR_NAME), dimension(17) :: names_params = [character(len=BMI_MAX_VAR_NAME) :: "CWP","VCMX25","MP","MFSNO","RSURF_SNOW","HVT","BEXP","SMCMAX","FRZX", &
                                                                                                            "DKSAT","KDT","RSURF_EXP","REFKDT","AXAJ","BXAJ","SLOPE","SCAMAX"]

  !---------------------------------------------------------------------
  !  Initialize
  !---------------------------------------------------------------------
    print*, "Initializing..."
    call get_command_argument(1, arg)
    status = m%initialize(arg)
    print*, "Component name = "

  !---------------------------------------------------------------------
  ! Get model information
  ! component_name and input/output_var
  !---------------------------------------------------------------------
    status = m%get_component_name(component_name)
    print*, "Component name = ", trim(component_name)

    status = m%get_input_item_count(count)
    print*, "Total input vars = ", count
    n_inputs = count

    status = m%get_output_item_count(count)
    print*, "Total output vars = ", count
    n_outputs = count

    count = size(names_params)
    print*, "Total calibratable parameters = ", count
    n_params = count

    status = m%get_input_var_names(names_inputs)
    do iBMI = 1, n_inputs
      print*, "Input var = ", trim(names_inputs(iBMI))
    end do

    status = m%get_output_var_names(names_outputs)
    do iBMI = 1, n_outputs
      print*, "Output var = ", trim(names_outputs(iBMI))
    end do

    do iBMI = 1, n_params
      print*, "Calibratable param = ", trim(names_params(iBMI))
    end do
    
    ! Sum input and outputs to get total vars
    count = n_inputs + n_outputs + n_params
    
    ! Get other variable info
    do j = 1, count
      name = ''
      if(j <= n_inputs) then
        name = trim(names_inputs(j))
      else if(j <= n_inputs + n_outputs) then
        name = trim(names_outputs(j - n_inputs))
      else
        name = trim(names_params(j - n_inputs - n_outputs))
      end if
      status = m%get_var_type(trim(name), var_type)
      status = m%get_var_units(trim(name), var_units)
      status = m%get_var_itemsize(trim(name), var_itemsize)
      status = m%get_var_nbytes(trim(name), var_nbytes)
      print*, "The variable ", trim(name)
      print*, "    has a type of ", var_type
      print*, "    units of ", var_units
      print*, "    a size of ", var_itemsize
      print*, "    and total n bytes of ", var_nbytes
    end do

  !---------------------------------------------------------------------
  ! Get time information
  !---------------------------------------------------------------------
    status = m%get_start_time(bmi_time)
    print*, "The start time is ", bmi_time

    status = m%get_current_time(bmi_time)
    print*, "The current time is ", bmi_time

    status = m%get_end_time(bmi_time)
    print*, "The end time is ", bmi_time

    status = m%get_time_step(timestep)
    status = m%get_time_units(ts_units)
    print*, " The time step is ", timestep
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

  !---------------------------------------------------------------------
  ! Test the get/set_value functionality with BMI
  !---------------------------------------------------------------------
    ! Loop through the input and output vars
    do iBMI = 1, count
      name = ''
      if(iBMI <= n_inputs) then
        name = trim(names_inputs(iBMI))
      else if(iBMI <= n_inputs + n_outputs) then
        name = trim(names_outputs(iBMI - n_inputs))
      else
        name = trim(names_params(iBMI - n_inputs - n_outputs))
      end if
      status = m%get_var_type(trim(name),var_type)
      status = m%get_var_grid(trim(name),grid_int)
      status = m%get_grid_size(grid_int,grid_size)
      if(var_type.eq.'integer') then
        if(allocated(var_value_get_int)) deallocate(var_value_get_int)
        if(allocated(var_value_set_int)) deallocate(var_value_set_int)
        allocate(var_value_get_int(grid_size))
        allocate(var_value_set_int(grid_size))
        var_value_set_int = 999
        status = m%get_value(trim(name), var_value_get_int)
        print*, trim(name), " from get_value = ", var_value_get_int
        print*, "    our replacement value = ", var_value_set_int
        status = m%set_value(trim(name), var_value_set_int)
        status = m%get_value(trim(name), var_value_get_int)
        print*, "    and the new value of ", trim(name), " = ", var_value_get_int
      else if (var_type.eq.'real') then
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        if(allocated(var_value_set_real)) deallocate(var_value_set_real)
        allocate(var_value_get_real(grid_size))
        allocate(var_value_set_real(grid_size))
        var_value_set_real = 999
        status = m%get_value(trim(name), var_value_get_real)
        print*, trim(name), " from get_value = ", var_value_get_real
        select case (trim(name))
        case('SMCMAX')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('FRZX', var_value_get_real_temp)
          print*,"    (FRZX is recalculated by setting SMCMAX)"
          print*,"    from get_value (for FRZX)= ", var_value_get_real_temp
        case('DKSAT')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('KDT', var_value_get_real_temp)
          print*,"    (KDT is recalculated by setting DKSAT)"
          print*,"    from get_value (for KDT)= ", var_value_get_real_temp
        case('REFKDT')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('KDT', var_value_get_real_temp)
          print*,"    (KDT is recalculated by setting REFKDT)"
          print*,"    from get_value (for KDT)= ", var_value_get_real_temp
        end select
        print*, "    our replacement value = ", var_value_set_real
        status = m%set_value(trim(name), var_value_set_real)
        status = m%get_value(trim(name), var_value_get_real)
        print*, "    and the new value of ", trim(name), " = ", var_value_get_real
        select case (trim(name))
        case('SMCMAX')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('FRZX', var_value_get_real_temp)
          print*,"    (FRZX is recalculated by setting SMCMAX)"
          print*,"    from get_value (for FRZX)= ", var_value_get_real_temp
        case('DKSAT')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('KDT', var_value_get_real_temp)
          print*,"    (KDT is recalculated by setting DKSAT)"
          print*,"    from get_value (for KDT)= ", var_value_get_real_temp
        case('REFKDT')
          if(allocated(var_value_get_real_temp)) deallocate(var_value_get_real_temp)
          allocate(var_value_get_real_temp(1))
          status = m%get_value('KDT', var_value_get_real_temp)
          print*,"    (KDT is recalculated by setting REFKDT)"
          print*,"    from get_value (for KDT)= ", var_value_get_real_temp
        end select
      end if
    end do
    
  !---------------------------------------------------------------------
  ! Test the get_value_ptr functionality with BMI
  !---------------------------------------------------------------------
    ! test the get value pointer  functions
    ! Loop through the input and output vars
    do iBMI = 1, count
      name = ''
      if(iBMI <= n_inputs) then
        name = trim(names_inputs(iBMI))
      else if(iBMI <= n_inputs + n_outputs) then
        name = trim(names_outputs(iBMI - n_inputs))
      else
        name = trim(names_params(iBMI - n_inputs - n_outputs))
      end if
      status = m%get_var_type(trim(name),var_type)
      status = m%get_var_grid(trim(name),grid_int)
      status = m%get_grid_size(grid_int,grid_size)
      if(var_type.eq.'integer') then
        if(allocated(var_value_get_int)) deallocate(var_value_get_int)
        allocate(var_value_get_int(grid_size))
        var_value_get_int_ptr => var_value_get_int
        status = m%get_value_ptr(trim(name), var_value_get_int_ptr)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from get_value_ptr returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from get_value_ptr returned ", status, " TEST FAILED!" 
        end if
      else if (var_type.eq.'real') then
        if(allocated(var_value_get_real)) deallocate(var_value_get_real)
        allocate(var_value_get_real(grid_size))
        var_value_get_real_ptr => var_value_get_real
        status = m%get_value_ptr(trim(name), var_value_get_real_ptr)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from get_value_ptr returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from get_value_ptr returned ", status, " TEST FAILED!" 
        end if
      end if
    end do

  !---------------------------------------------------------------------
  ! Test the get_value_at_indices functionality with BMI
  !---------------------------------------------------------------------
    ! Loop through the input vars
    do iBMI = 1, count
      name = ''
      if(iBMI <= n_inputs) then
        name = trim(names_inputs(iBMI))
      else if(iBMI <= n_inputs + n_outputs) then
        name = trim(names_outputs(iBMI - n_inputs))
      else
        name = trim(names_params(iBMI - n_inputs - n_outputs))
      end if
      status = m%get_var_type(trim(name),var_type)
      status = m%get_var_grid(trim(name),grid_int)
      status = m%get_grid_size(grid_int,grid_size)
      status = m%get_grid_rank(grid_int, grid_rank)
      if(allocated(grid_indices)) deallocate(grid_indices)
      allocate(grid_indices(grid_rank))
      grid_indices = 1
      if(var_type.eq.'integer') then
        status = m%get_value_at_indices(trim(name), var_value_get_int, grid_indices)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from get_value_at_indices returned ", status, " TEST FAILED!" 
        end if
        status = m%set_value_at_indices(trim(name), grid_indices, var_value_set_int)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from set_value_at_indices returned ", status, " TEST FAILED!" 
        end if
      else if (var_type.eq.'real') then 
        status = m%get_value_at_indices(trim(name), var_value_get_real, grid_indices)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from get_value_at_indices returned ", status, " TEST FAILED!" 
        end if
        status = m%set_value_at_indices(trim(name), grid_indices, var_value_set_real)
        if ( status .eq. BMI_FAILURE ) then
          print*, trim(name), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
        else
          print*, trim(name), " from set_value_at_indices returned ", status, " TEST FAILED!" 
        end if
      end if
    end do

    nullify( var_value_get_real_ptr )
    nullify( var_value_get_int_ptr )
    deallocate(var_value_get_real)
    deallocate(var_value_set_real)
    deallocate(var_value_get_int)
    deallocate(var_value_set_int)

  !---------------------------------------------------------------------
  ! Test the grid info functionality with BMI
  !---------------------------------------------------------------------
  do iBMI = 1, count

    name = ''
    if(iBMI <= n_inputs) then
      name = trim(names_inputs(iBMI))
    else if(iBMI <= n_inputs + n_outputs) then
      name = trim(names_outputs(iBMI - n_inputs))
    else
      name = trim(names_params(iBMI - n_inputs - n_outputs))
    end if
    
    ! get_var_grid
    status = m%get_var_grid(trim(name), grid_int)
    print*, "The integer value for the ", trim(name), " grid is ", grid_int
    
    ! get_grid_type
    status = m%get_grid_type(grid_int, grid_type)
    print*, "The grid type for ", trim(name), " is ", trim(grid_type)
    
    ! get_grid_rank
    status = m%get_grid_rank(grid_int, grid_rank)
    print*, "The grid rank for ", trim(name), " is ", grid_rank
    
    ! get_grid_shape
    ! only scalars implemented thus far
    status = m%get_grid_shape(grid_int, grid_shape)
    if(grid_shape(1) == -1) then
      print*, "No grid shape for the grid type/rank"
    end if
    
    ! get_grid_size
    status = m%get_grid_size(grid_int, grid_size)
    print*, "The grid size for ", trim(name), " is ", grid_size
    
    ! get_grid_spacing
    ! only scalars implemented thus far
    status = m%get_grid_spacing(grid_int, grid_spacing)
    if(grid_spacing(1) == -1.d0) then
      print*, "No grid spacing for the grid type/rank"
    end if
    
    ! get_grid_origin
    ! only scalars implemented thus far
    status = m%get_grid_origin(grid_int, grid_origin)
    if(grid_origin(1) == -1.d0) then
      print*, "No grid origin for the grid type/rank"
    end if
    
    ! get_grid_x/y/z
    ! should return 0 for a 1 node "grid" because not currently spatially explicit
    status = m%get_grid_x(grid_int, grid_x)
    status = m%get_grid_y(grid_int, grid_y)
    status = m%get_grid_z(grid_int, grid_z)
    print*, "The X coord for grid ", grid_int, " is ", grid_x
    print*, "The Y coord for grid ", grid_int, " is ", grid_y
    print*, "The Z coord for grid ", grid_int, " is ", grid_z
    
  end do

  !---------------------------------------------------------------------
  ! The following functions are not implemented/only return BMI_FAILURE
  ! Change if your model implements them
  !---------------------------------------------------------------------
  
    print*, "The unstructured grid functions will return BMI_FAILURE"
    print*, "BMI functions that require ", trim(component_name), &
            " to use pointer vars are not implemented"
  
  !---------------------------------------------------------------------
  ! End test
  !---------------------------------------------------------------------
    print*, "All done testing!"

end program
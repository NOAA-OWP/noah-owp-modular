!
! compile: 
!

program noahmp_driver

!---------------------------------------------------------------------
!  Modules
!  Change from non-BMI: Only BMI modules need to be exposed
!  The rest are used in ../src/NoahMPSurfaceModule.f90
!---------------------------------------------------------------------
  use bminoahmp
  use bmif_2_0

  implicit none

!---------------------------------------------------------------------
!  Types
!  Change from non-BMI: only the bmi_noahmp type needed
!  Forcing, Energy, Water, etc. not needed
!---------------------------------------------------------------------
  type (bmi_noahmp)  :: m
  
!---------------------------------------------------------------------
!  Local variable(s) 
!---------------------------------------------------------------------
  character (len = 80)  :: arg          ! command line argument for config file
  double precision      :: current_time ! current run time of model in s from beginning
  double precision      :: end_time     ! end of model simulation time in s

!---------------------------------------------------------------------
!  Local variable(s) for BMI testing
!---------------------------------------------------------------------
  character (len = BMI_MAX_COMPONENT_NAME), pointer :: component_name ! component name
  integer                                           :: status    ! returning status values
  integer                                           :: count     ! var counts
  character (len = BMI_MAX_VAR_NAME), pointer       :: names(:)  ! var names
  integer                                           :: n_inputs  ! n input vars
  integer                                           :: n_outputs ! n output vars
  integer                                           :: iBMI      ! loop counter
  double precision                                  :: timestep  ! timestep
  double precision                                  :: bmi_time  ! time output from BMI functions
  character (len = 1)                               :: ts_units  ! timestep units
  real, allocatable                                 :: var_value_get(:) ! value of a variable
  real, allocatable                                 :: var_value_set(:) ! value of a variable
  
!---------------------------------------------------------------------
!  Initialize
!  Change from non-BMI: All driver initialization code moved to
!  ../src/NoahMPSurfaceModule.f90
!---------------------------------------------------------------------
  print*, "Initializing..."
  call get_command_argument(1, arg)
  status = m%initialize(arg)

!---------------------------------------------------------------------
! Run the model with BMI
! Change from non-BMI: All model excution code moved to
! ../src/NoahMPSurfaceModule.f90
!---------------------------------------------------------------------
  ! get the current and end time for running the execution loop
  status = m%get_current_time(current_time)
  status = m%get_end_time(end_time)
  
  ! loop through while current time <= end time
  print*, "Running..."
  do while (current_time <= end_time)
    status = m%update()                       ! run the model one time step
    status = m%get_current_time(current_time) ! update current_time
  end do

!---------------------------------------------------------------------
! Do some BMI testing
! Uncomment this code and "local variable(s) for BMI testing" to run tests
!---------------------------------------------------------------------
!   status = m%get_end_time(bmi_time)
!   print*, "Updating until time = ", bmi_time
!   status = m%update_until(bmi_time)

  status = m%get_component_name(component_name)
  print*, "Component name = ", trim(component_name)
  
  status = m%get_input_item_count(count)
  print*, "Total input vars = ", count
  n_inputs = count
  
  status = m%get_output_item_count(count)
  print*, "Total output vars = ", count
  n_outputs = count
  
  status = m%get_input_var_names(names)
  do iBMI = 1, n_inputs 
    print*, "Input var = ", trim(names(iBMI))
  end do
  
  status = m%get_output_var_names(names)
  do iBMI = 1, n_outputs 
    print*, "Output var = ", trim(names(iBMI))
  end do

  allocate(var_value_get(1))
  allocate(var_value_set(1))
  var_value_set = 999.99
  status = m%get_value('SFCTMP', var_value_get)
  print*, "SFCTMP from get_value = ", var_value_get
  print*, "    our replacement value = ", var_value_set
  status = m%set_value('SFCTMP', var_value_set)
  status = m%get_value('SFCTMP', var_value_get)
  print*, "    and the new value of SFCTMP = ", var_value_get
  deallocate(var_value_get)
  deallocate(var_value_set)
  
  
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
! Finalize with BMI
! Change from non-BMI: All model finalization code moved to
! ../src/NoahMPSurfaceModule.f90
!---------------------------------------------------------------------
  print*, "Finalizing..."
  status = m%finalize()
  print*, "Finished!"

end program

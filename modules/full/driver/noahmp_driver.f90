program noahmp_driver
  
  !---------------------------------------------------------------------
  !  Modules
  !  Only the BMI modules need to be exposed
  !---------------------------------------------------------------------
    use bminoahmp
    use bmif_2_0

    implicit none

  !---------------------------------------------------------------------
  !  Types (only the bmi_noahmp type needed)
  !---------------------------------------------------------------------
    type (bmi_noahmp)  :: m
  
  !---------------------------------------------------------------------
  !  Local variable(s) 
  !---------------------------------------------------------------------
    character (len = 80)  :: arg          ! command line argument for namelist file
    double precision      :: current_time ! current run time of model in s from beginning
    double precision      :: end_time     ! end of model simulation time in s
    integer               :: status       ! returns status values

  !---------------------------------------------------------------------
  !  Initialize
  !  All driver initialization code in ../src/RunModule.f90
  !---------------------------------------------------------------------
    print*, "Initializing..."
    call get_command_argument(1, arg)
    status = m%initialize(arg)

  !---------------------------------------------------------------------
  ! Run the model with BMI
  ! All model excution code in ../src/RunModule.f90
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
  ! Finalize with BMI
  ! All model finalization code in ../src/RunModule.f90
  !---------------------------------------------------------------------
    print*, "Finalizing..."
    status = m%finalize()
    print*, "Finished!"

end program

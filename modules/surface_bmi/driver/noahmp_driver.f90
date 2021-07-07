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
  integer               :: status       ! returning status values

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
! Finalize with BMI
! Change from non-BMI: All model finalization code moved to
! ../src/NoahMPSurfaceModule.f90
!---------------------------------------------------------------------
  print*, "Finalizing..."
  status = m%finalize()
  print*, "Finished!"

end program

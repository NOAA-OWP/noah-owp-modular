module ErrorCheckModule

  ! General error checking routins

  implicit none

  private
  public:: sys_abort
  public:: is_within_bound

  interface is_within_bound
    module procedure is_within_bound_int
    module procedure is_within_bound_real
  end interface

contains

  subroutine sys_abort(err, message)

    ! terminate the program if error is detected (err is non-zero)

    implicit none

    integer, intent(in) :: err                  ! error code
    character(*), intent(in) :: message         ! error message

    if(err/=0)then
      write(*, '(A)') 'FATAL ERROR: '//trim(message)
      call flush(6)
      stop
    endif

  end subroutine sys_abort

  function is_within_bound_int(lower_bound, upper_bound, var) result(withinbound)

    ! check if a integer value is within specified bounds

    implicit none

    integer, intent(in) :: lower_bound
    integer, intent(in) :: upper_bound
    integer, intent(in) :: var
    logical :: withinbound

    withinbound = .true.
    if ( var < lower_bound .or. var > upper_bound ) then
       withinbound = .false.
    end if

  end function is_within_bound_int

  function is_within_bound_real(lower_bound, upper_bound, var) result(withinbound)

    ! check if a real value is within specified bounds

    implicit none

    real, intent(in) :: lower_bound
    real, intent(in) :: upper_bound
    real, intent(in) :: var
    logical :: withinbound

    withinbound = .true.
    if ( var < lower_bound .or. var > upper_bound ) then
       withinbound = .false.
    end if

  end function is_within_bound_real

end module ErrorCheckModule

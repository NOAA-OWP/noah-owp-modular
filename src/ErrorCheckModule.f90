module ErrorCheckModule

  ! General error checking routins

  implicit none

  private
  public:: sys_abort
  public:: is_within_bound

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


  function is_within_bound(lower_bound, upper_bound, var) result(withinbound)

    ! check if a value is within specified bounds

    implicit none

    integer, intent(in) :: lower_bound
    integer, intent(in) :: upper_bound
    integer, intent(in) :: var
    logical :: withinbound

    withinbound = .true.
    if ( var < lower_bound .or. var > upper_bound ) then
       withinbound = .false.
    end if

  end function

end module ErrorCheckModule

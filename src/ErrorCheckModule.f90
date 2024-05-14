module ErrorCheckModule

  ! General error checking routines
  implicit none
  integer, parameter, public :: NOM_SUCCESS = 0
  integer, parameter, public :: NOM_FAILURE = 1
  integer, parameter, public :: NOM_MESSAGE = 2

  private
  public:: is_within_bound
  public:: log_message

  interface is_within_bound
    module procedure is_within_bound_int
    module procedure is_within_bound_real
  end interface

contains

  subroutine log_message(err, message)

    ! log information, typically an error

    implicit none

    integer, intent(in) :: err                  ! error code
    character(*), intent(in) :: message         ! message

    ! If error, write the error. If message, write message unless NGEN_QUIET
    if(err==NOM_FAILURE)then
      write(*, '(A,I2,A)') ' Error Code: ', err, ',  Message: '//trim(message)
      call flush(6)
    endif
#ifndef NGEN_QUIET
    if(err==NOM_MESSAGE)then
      write(*, '(A,I2,A)') ' Error Code: ', err, ',  Message: '//trim(message)
      call flush(6)
    endif
#endif

  end subroutine log_message

  function is_within_bound_int(var, lower_bound, upper_bound) result(withinbound)

    ! check if a integer value is within specified bounds

    implicit none

    integer, intent(in) :: var
    integer, intent(in) :: lower_bound
    integer, intent(in) :: upper_bound
    logical :: withinbound

    withinbound = .true.
    if ( var < lower_bound .or. var > upper_bound ) then
       withinbound = .false.
    end if

  end function is_within_bound_int

  function is_within_bound_real(var, lower_bound, upper_bound) result(withinbound)

    ! check if a real value is within specified bounds

    implicit none

    real, intent(in) :: var
    real, intent(in) :: lower_bound
    real, intent(in) :: upper_bound
    logical :: withinbound

    withinbound = .true.
    if ( var < lower_bound .or. var > upper_bound ) then
       withinbound = .false.
    end if

  end function is_within_bound_real

end module ErrorCheckModule

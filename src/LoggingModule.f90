module LoggingModule

  ! Leveled diagnostic messages

  use iso_fortran_env, only: output_unit, error_unit

  implicit none

  private
  public:: write_log
  public:: LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, LOG_LEVEL_WARNING, LOG_LEVEL_SEVERE, LOG_LEVEL_FATAL

  ! Values match the NGWPC fork's logger so a fuller implementation can drop in
  integer, parameter :: LOG_LEVEL_DEBUG   = 10
  integer, parameter :: LOG_LEVEL_INFO    = 20
  integer, parameter :: LOG_LEVEL_WARNING = 30
  integer, parameter :: LOG_LEVEL_SEVERE  = 40
  integer, parameter :: LOG_LEVEL_FATAL   = 50

  integer, parameter :: LOG_LEVEL_THRESHOLD = LOG_LEVEL_INFO

contains

  subroutine write_log(message, level)

    ! Write message if level meets the threshold, warnings and worse to stderr

    implicit none

    character(*), intent(in) :: message
    integer, intent(in) :: level

    integer :: unit

    if(level < LOG_LEVEL_THRESHOLD) return

    if(level >= LOG_LEVEL_WARNING) then
      unit = error_unit
    else
      unit = output_unit
    endif

    write(unit, '(A)') 'NOAHOWP '//trim(level_name(level))//': '//trim(message)
    flush(unit)

  end subroutine write_log

  function level_name(level) result(name)

    implicit none

    integer, intent(in) :: level
    character(len=7) :: name

    select case(level)
      case(LOG_LEVEL_DEBUG)
        name = 'DEBUG'
      case(LOG_LEVEL_WARNING)
        name = 'WARNING'
      case(LOG_LEVEL_SEVERE)
        name = 'SEVERE'
      case(LOG_LEVEL_FATAL)
        name = 'FATAL'
      case default
        name = 'INFO'
    end select

  end function level_name

end module LoggingModule

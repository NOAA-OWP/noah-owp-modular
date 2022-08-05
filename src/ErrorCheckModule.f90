module ErrorCheckModule

  implicit none

  private
  public:: sys_abort

contains

  subroutine sys_abort(err, message)

    implicit none

    integer, intent(in) :: err                  ! error code
    character(*), intent(in) :: message         ! error message

    if(err/=0)then
      write(*, '(A)') 'FATAL ERROR: '//trim(message)
      call flush(6)
      stop
    endif

  end subroutine sys_abort

end module ErrorCheckModule

module NoahMPAsciiRead
  
  implicit none
  
contains
  
  subroutine open_forcing_file(filename)
    
    implicit none
    
    character*256, intent(in)  :: filename
    
    !---------------------------------------------------------------------
    !  local variables
    !---------------------------------------------------------------------
    logical :: lexist ! logical for whether the file specified by filename exists
    integer :: ierr   ! error code returned by open(iostat = ierr)
    !---------------------------------------------------------------------

    !  Check if the specified file exists
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
       write(*,'(/," ***** Problem *****")')
       write(*,'(" ***** File ''", A, "'' does not exist.")') trim(filename)
       write(*,'(" ***** Check the forcing file specified as a command-line argument",/)')
       stop ":  ERROR EXIT"
    endif
    
    ! Open the forcing file 
    open(10, file = trim(filename), form = 'formatted', action = 'read', iostat = ierr)
    if (ierr /= 0) then
       write(*,'("Problem opening file ''", A, "''")') trim(filename)
       stop ":  ERROR EXIT"
    endif
    
  end subroutine open_forcing_file
  
end module NoahMPAsciiRead
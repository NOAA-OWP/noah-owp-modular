module ErrorType

use ErrorCheckModule

implicit none
save
private

type, public :: error_type

  integer             :: error_flag        ! error flag
  character(len=256)  :: error_string      ! error string

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault     

end type error_type

contains   

  subroutine Init(this)

    class(error_type)   :: this

    call this%InitDefault()

  end subroutine Init


  subroutine InitDefault(this)

    class(error_type) :: this

    error_flag   = NOM_SUCCESS       ! ModuleErrorCheck variable
    error_string = 'EMPTY'

    this%error_flag   = error_flag   ! ModelType variable
    this%error_string = error_string

  end subroutine InitDefault

end module ErrorType

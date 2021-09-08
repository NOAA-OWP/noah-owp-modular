module LevelsType

use NamelistRead, only: namelist_type

implicit none
save
private

type, public :: levels_type

  integer :: nsoil
  integer :: nsnow
  integer :: nveg    

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer        

end type levels_type

contains   

  subroutine Init(this)

    class(levels_type) :: this

    call this%InitDefault()

  end subroutine Init

  subroutine InitDefault(this)

    class(levels_type) :: this

    this%nsoil  = huge(1)
    this%nsnow  = huge(1)
    this%nveg   = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(levels_type)   :: this
    type(namelist_type)  :: namelist

    this%nsoil  = namelist%nsoil
    this%nsnow  = namelist%nsnow
    this%nveg  = namelist%nveg    

  end subroutine InitTransfer

end module LevelsType

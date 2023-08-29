module LevelsType

use NamelistRead, only: namelist_type
use LevelsGridType, only: levelsgrid_type
implicit none
save
private

type, public :: levels_type

  integer :: nsoil  ! number of soil layers
  integer :: nsnow  ! number of snow layers
  integer :: nveg   ! number of vegetation types in chosen table

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault 
    procedure, public  :: InitTransfer        

end type levels_type

contains   

  subroutine Init(this,namelist)

    class(levels_type),  intent(inout) :: this
    type(namelist_type), intent(in)    :: namelist

    call this%InitDefault()

  end subroutine Init

  subroutine InitDefault(this)

    class(levels_type) :: this

    this%nsoil  = huge(1)
    this%nsnow  = huge(1)
    this%nveg   = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this,levelsgrid)

    class(levels_type)    :: this
    type(levelsgrid_type) :: levelsgrid

    this%nsoil  = levelsgrid%nsoil
    this%nsnow  = levelsgrid%nsnow
    this%nveg   = levelsgrid%nveg   

  end subroutine InitTransfer

end module LevelsType

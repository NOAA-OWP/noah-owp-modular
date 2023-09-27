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
    procedure, private :: InitTransfer
    procedure, public  :: TransferIn
    procedure, public  :: TransferOut

end type levels_type

contains   

  subroutine Init(this,namelist,levelsgrid)

    class(levels_type),    intent(inout) :: this
    type(namelist_type),   intent(in)    :: namelist
    type(levelsgrid_type), intent(in)    :: levelsgrid

    call this%InitDefault()
    call this%InitTransfer(levelsgrid)

  end subroutine Init

  subroutine InitDefault(this)

    class(levels_type), intent(inout) :: this

    this%nsoil  = huge(1)
    this%nsnow  = huge(1)
    this%nveg   = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this,levelsgrid)

    class(levels_type),    intent(inout) :: this
    type(levelsgrid_type), intent(in)    :: levelsgrid

    this%nsoil  = levelsgrid%nsoil
    this%nsnow  = levelsgrid%nsnow
    this%nveg   = levelsgrid%nveg   

  end subroutine InitTransfer

  subroutine TransferIn(this, levelsgrid, ix, iy)

    implicit none

    class(levels_type),    intent(inout) :: this
    type(levelsgrid_type), intent(in)    :: levelsgrid
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    ! Nothing to do

  end subroutine TransferIn

  subroutine TransferOut(this, levelsgrid, ix, iy)

    implicit none

    class(levels_type),    intent(in)    :: this
    type(levelsgrid_type), intent(inout) :: levelsgrid
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    ! Nothing to do      

  end subroutine TransferOut


end module LevelsType

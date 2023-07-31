module LevelsGridType

use NamelistRead, only: namelist_type
use GridlistRead, only: gridlist_type
implicit none
save
private

type, public :: levelsgrid_type

  integer :: nsoil  ! number of soil layers
  integer :: nsnow  ! number of snow layers
  integer :: nveg   ! number of vegetation types in chosen table

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault  
    procedure, private :: InitAllocate
    procedure, public  :: InitTransfer       

end type

contains   

  subroutine Init(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    ! Nothing to do

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(levelsgrid_type) :: this

    this%nsoil = huge(1)
    this%nsnow = huge(1)
    this%nveg  = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist, gridlist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist
    type(gridlist_type),intent(in)  :: gridlist

    this%nsoil = namelist%nsoil
    this%nsnow = namelist%nsnow
    this%nveg  = gridlist%nveg    

  end subroutine InitTransfer

end module LevelsGridType

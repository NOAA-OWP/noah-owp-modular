module LevelsType

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

end module LevelsType

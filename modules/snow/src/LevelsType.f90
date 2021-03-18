module LevelsType

implicit none
save
private

type, public :: levels_type

  integer :: soil
  integer :: snow

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

    this%soil  = huge(1)
    this%snow  = huge(1)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    use NamelistRead

    class(levels_type) :: this
    type(namelist_type) :: namelist
    
    this%soil  = namelist%nsoil
    this%snow  = namelist%nsnow

  end subroutine InitTransfer

end module LevelsType

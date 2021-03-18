module ForcingType

use NamelistRead

implicit none
save
private

type, public :: forcing_type

  real    :: uwind
  real    :: vwind

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type forcing_type

contains   

  subroutine Init(this, namelist)

    class(forcing_type) :: this
    type(namelist_type) :: namelist

    call this%InitDefault()

  end subroutine Init

  subroutine InitDefault(this)

    class(forcing_type) :: this

    this%uwind     = huge(1.0)
    this%vwind     = huge(1.0)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(forcing_type) :: this
    type(namelist_type) :: namelist

    this%uwind      = namelist%uwind
    this%vwind      = namelist%vwind

  end subroutine InitTransfer

end module ForcingType

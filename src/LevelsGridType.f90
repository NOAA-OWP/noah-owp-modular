module LevelsGridType

use NamelistRead, only: namelist_type
implicit none
save
private

type, public :: levelsgrid_type

  integer, allocatable, dimension(:,:) :: nsoil  ! number of soil layers
  integer, allocatable, dimension(:,:) :: nsnow  ! number of snow layers
  integer                              :: nveg   ! number of vegetation types in chosen table

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault  
    procedure, private :: InitAllocate       

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

    associate(n_x => namelist%n_x, &
             n_y => namelist%n_y)

    allocate(this%nsoil(n_x,n_y))
    allocate(this%nsnow(n_x,n_y))

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(levelsgrid_type) :: this

    this%nsoil(:,:)  = huge(1)
    this%nsnow(:,:)  = huge(1)
    this%nveg        = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    this%nsoil(:,:)  = namelist%nsoil
    this%nsnow(:,:)  = namelist%nsnow
    this%nveg        = namelist%nveg    

  end subroutine InitTransfer

end module LevelsGridType

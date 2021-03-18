module EnergyType

use NamelistRead

implicit none
save
private

type, public :: energy_type

  real    :: TV
  real    :: SFCTMP
  real    :: TG
  real    :: FCEV 
  real    :: FCTR
  logical :: FROZEN_CANOPY
  logical :: FROZEN_GROUND
  integer, allocatable, dimension(:) :: IMELT  ! melting state index [0-no melt;1-melt]
  real,    allocatable, dimension(:) :: STC    !snow/soil layer temperature [k]

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate        
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type energy_type

contains   

  subroutine Init(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

    allocate(this%IMELT (-namelist%nsnow+1:0)); this%IMELT(:) = huge(1)
    allocate(this%STC (-namelist%nsnow+1:namelist%nsoil)); this%STC(:) = huge(1.0)

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(energy_type) :: this

    this%TV        = huge(1.0)
    this%TG        = huge(1.0)
    this%SFCTMP    = huge(1.0)
    this%FCEV      = huge(1.0)
    this%FCTR      = huge(1.0)
    this%FROZEN_CANOPY = .false.
    this%FROZEN_GROUND = .false.

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module EnergyType

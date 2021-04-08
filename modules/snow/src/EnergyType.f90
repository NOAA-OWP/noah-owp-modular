module EnergyType

use NamelistRead

implicit none
save
private

type, public :: energy_type

  real    :: TV
  real    :: TG
  real    :: FCEV 
  real    :: FCTR
  real    :: IGS                               ! growing season index (0=off, 1=on)
  logical :: FROZEN_CANOPY
  logical :: FROZEN_GROUND
  integer, allocatable, dimension(:) :: IMELT  ! snow layer melting state index [0-no melt;1-melt]
  real,    allocatable, dimension(:) :: STC    ! snow/soil layer temperature [k]
  real,    allocatable, dimension(:) :: DF     ! snow and soil layer thermal conductivity [w/m/k]
  real,    allocatable, dimension(:) :: HCPCT  ! snow and soil layer heat capacity [j/m3/k]
  real,    allocatable, dimension(:) :: FACT   ! temporary variable used in phase change [s/j/m2/k]
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

    allocate(this%IMELT (-namelist%nsnow+1:0)); this%IMELT(:)              = huge(1)
    allocate(this%STC   (-namelist%nsnow+1:namelist%nsoil)); this%STC(:)   = huge(1.0)
    allocate(this%DF    (-namelist%nsnow+1:namelist%nsoil)); this%DF(:)    = huge(1.0)
    allocate(this%HCPCT (-namelist%nsnow+1:namelist%nsoil)); this%HCPCT(:) = huge(1.0)
    allocate(this%FACT  (-namelist%nsnow+1:namelist%nsoil)); this%FACT(:)  = huge(1.0)

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(energy_type) :: this

    this%TV        = huge(1.0)
    this%TG        = huge(1.0)
    this%FCEV      = huge(1.0)
    this%FCTR      = huge(1.0)
    this%IGS       = huge(1.0)
    this%FROZEN_CANOPY = .false.
    this%FROZEN_GROUND = .false.

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(energy_type) :: this
    type(namelist_type) :: namelist

  end subroutine InitTransfer

end module EnergyType

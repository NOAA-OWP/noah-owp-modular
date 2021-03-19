module OptionsType

use NamelistRead

implicit none
save
private

type, public :: options_type

  integer :: opt_snf    ! option for determining precipitation phase
  integer :: opt_run
  integer :: opt_drn
  integer :: opt_inf
  integer :: opt_infdv  ! options for infiltration in dynamic VIC runoff scheme **1 -> Philip scheme, 2 -> Green-Ampt scheme, 3 -> Smith-Parlange scheme

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type options_type

contains   

  subroutine Init(this)

    class(options_type) :: this

    call this%InitDefault()

  end subroutine Init

  subroutine InitDefault(this)

    class(options_type) :: this

    this%opt_snf   = huge(1)
    this%opt_run   = huge(1)
    this%opt_drn   = huge(1)
    this%opt_inf   = huge(1)
    this%opt_infdv = huge(1)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(options_type) :: this
    type(namelist_type) :: namelist

    this%opt_snf   = namelist%precip_phase_option
    this%opt_run   = namelist%runoff_option
    this%opt_drn   = namelist%drainage_option
    this%opt_inf   = namelist%frozen_soil_option
    this%opt_infdv = namelist%dynamic_vic_option

  end subroutine InitTransfer

end module OptionsType

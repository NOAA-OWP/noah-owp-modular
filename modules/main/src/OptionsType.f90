module OptionsType

use NamelistRead

implicit none
save
private

type, public :: options_type

  integer :: opt_snf    ! options for determining precipitation phase
  integer :: opt_run    ! options for runoff
  integer :: opt_drn    ! options for drainage 
  integer :: opt_inf    ! options for frozen soil permeability
  integer :: opt_infdv  ! options for infiltration in dynamic VIC runoff scheme **1 -> Philip scheme, 
                        !   2 -> Green-Ampt scheme, 3 -> Smith-Parlange scheme
  integer :: dveg       ! options for dynamic vegetation scheme
  integer :: opt_alb    ! options for snow albedo (1 = BATS, 2 = CLASS)
  integer :: opt_rad    ! options for radiative transfer
  integer :: opt_sfc    ! options for surface layer drag coeff (CH & CM): 1 = M-O; 2 = original Noah (Chen97)
                        !   3 -> MYJ consistent; 4->YSU consistent. (MB removed in v3.7 for further testing)
  integer :: opt_crs    ! options for canopy stomatal resistance (1 = Ball-Berry, 2 = Jarvis)
  integer :: opt_crop   ! options for crop model
                        !   0 -> No crop model, will run default dynamic vegetation
                        !   1 -> Liu, et al. 2016
                        !   2 -> Gecros, Yin and van Laar, 2005, not supported
  integer :: opt_stc    ! options for snow/soil temperature time scheme (only layer 1)
                        !   1 -> semi-implicit; flux top boundary condition
                        !   2 -> full implicit (original Noah); temperature top boundary condition
                        !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)
  integer :: opt_tbot   ! options for lower boundary condition of soil temperature 
                        !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                        ! **2 -> TBOT at ZBOT (8m) read from a file (original Noah)
  integer :: opt_frz    ! options for supercooled liquid water (or ice fraction)
                        ! **1 -> no iteration (Niu and Yang, 2006 JHM)
                        !   2 -> nonlinear effects, less permeable (old)
  
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
    this%dveg      = huge(1)
    this%opt_alb   = huge(1)
    this%opt_rad   = huge(1)
    this%opt_sfc   = huge(1)
    this%opt_crs   = huge(1)
    this%opt_crop  = huge(1)
    this%opt_stc   = huge(1)    
    this%opt_tbot   = huge(1)    
    this%opt_frz    = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(options_type) :: this
    type(namelist_type) :: namelist

    this%opt_snf   = namelist%precip_phase_option
    this%opt_run   = namelist%runoff_option
    this%opt_drn   = namelist%drainage_option
    this%opt_inf   = namelist%frozen_soil_option
    this%opt_infdv = namelist%dynamic_vic_option
    this%dveg      = namelist%dynamic_veg_option
    this%opt_alb   = namelist%snow_albedo_option
    this%opt_rad   = namelist%radiative_transfer_option
    this%opt_sfc   = namelist%sfc_drag_coeff_option    
    this%opt_crs   = namelist%canopy_stom_resist_option    
    this%opt_crop  = namelist%crop_model_option    
    this%opt_stc   = namelist%snowsoil_temp_time_option 
    this%opt_tbot  = namelist%soil_temp_boundary_option 
    this%opt_frz   = namelist%supercooled_water_option

  end subroutine InitTransfer

end module OptionsType

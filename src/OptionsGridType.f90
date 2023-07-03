module OptionsGridType

use NamelistRead, only: namelist_type
implicit none
save
private

type, public :: optionsgrid_type

  integer, allocatable, dimension(:,:) :: opt_snf   ! precip_phase_option: options for determining precipitation phase
                                                    !   1 -> Jordan (1991) SNTHERM equation
                                                    !   2 -> rain-snow air temperature threshold of 2.2°C
                                                    !   3 -> rain-snow air temperature threshold of 0°C
                                                    !   4 -> precipitation phase from weather model
                                                    !   5 -> user-defined air temperature threshold
                                                    !   6 -> user-defined wet bulb temperature threshold
                                                    !   7 -> binary logistic regression model from Jennings et al. (2018)
  integer, allocatable, dimension(:,:) :: opt_run   ! runoff_option: options for runoff
                                                    !   1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR)
                                                    !   2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR)
                                                    !   3 -> original surface and subsurface runoff (free drainage)
                                                    !   4 -> BATS surface and subsurface runoff (free drainage)
                                                    !   5 -> Miguez-Macho&Fan groundwater scheme (Miguez-Macho et al. 2007 JGR; Fan et al. 2007 JGR)
                                                    !   6 -> VIC runoff
                                                    !   7 -> Xinanjiang runoff
                                                    !   8 -> Dynamic VIC runoff
  integer, allocatable, dimension(:,:) :: opt_drn   ! drainage_option
                                                    ! options for drainage 
                                                    !   1 -> Subsurface runoff scheme from groundwater module (Niu et al. 2007 JGR)
                                                    !   2 -> Subsurface runoff scheme with an equilibrium water table (Niu et al. 2005 JGR)
                                                    !   3 -> original subsurface runoff (free drainage)
                                                    !   4 -> original subsurface runoff (same as opt_drn = 3 but uses values from opt_run = 4)
                                                    !   5 -> drainage with MMF shallow water table scheme
                                                    !   6 -> VIC with free drainage (same as opt_drn = 3 but uses values from opt_run = 6)
                                                    !   7 -> Xinanjiang runoff (same as opt_drn = 3 but uses values from opt_run = 7)
                                                    !   8 -> Dynamic VIC runoff (same as opt_drn = 3 but uses values from opt_run = 8)
  integer, allocatable, dimension(:,:) :: opt_inf   ! frozen_soil_option
                                                    ! options for frozen soil permeability
                                                    !   1 -> linear effects, more permeable (Niu and Yang, 2006, JHM)
                                                    !   2 -> nonlinear effects, less permeable (old)
  integer, allocatable, dimension(:,:) :: opt_infdv ! dynamic_vic_option
                                                    ! options for infiltration in dynamic VIC runoff scheme 
                                                    !   1 -> Philip scheme, 
                                                    !   2 -> Green-Ampt scheme
                                                    !   3 -> Smith-Parlange scheme
  integer, allocatable, dimension(:,:) :: dveg      ! dynamic_veg_option
                                                    ! options for dynamic vegetation scheme
                                                    !   1 -> off (use table LAI; use FVEG = SHDFAC from input)
                                                    !   2 -> on  (together with OPT_CRS = 1)
                                                    !   3 -> off (use table LAI; calculate FVEG)
                                                    !   4 -> off (use table LAI; use maximum vegetation fraction)
                                                    !   5 -> on  (use maximum vegetation fraction)
                                                    !   6 -> on  (use FVEG = SHDFAC from input)
                                                    !   7 -> off (use input LAI; use FVEG = SHDFAC from input)
                                                    !   8 -> off (use input LAI; calculate FVEG)
                                                    !   9 -> off (use input LAI; use maximum vegetation fraction)
  integer, allocatable, dimension(:,:) :: opt_alb   ! snow_albedo_option
                                                    ! options for snow albedo
                                                    !   1 -> BATS
                                                    !   2 -> CLASS
  integer, allocatable, dimension(:,:) :: opt_rad   ! radiative_transfer_option
                                                    ! options for radiative transfer
                                                    !   1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
                                                    !   2 -> two-stream applied to grid-cell (gap = 0)
                                                    !   3 -> two-stream applied to vegetated fraction (gap=1-FVEG)
  integer, allocatable, dimension(:,:) :: opt_sfc   ! sfc_drag_coeff_option
                                                    ! options for surface layer drag coeff (CH & CM)
                                                    !   1 -> M-O
                                                    !   2 -> original Noah (Chen97)
  integer, allocatable, dimension(:,:) :: opt_crs   ! canopy_stom_resist_option
                                                    ! options for canopy stomatal resistance
                                                    !   1 -> Ball-Berry
                                                    !   2 -> Jarvis
  integer, allocatable, dimension(:,:) :: opt_crop  ! crop_model_option
                                                    ! options for crop model
                                                    ! NO CROP MODEL CURRENTLY SUPPORTED 
                                                    !   0 -> No crop model, will run default dynamic vegetation
  integer, allocatable, dimension(:,:) :: opt_stc   ! snowsoil_temp_time_option
                                                    ! options for snow/soil temperature time scheme (only layer 1)
                                                    !   1 -> semi-implicit; flux top boundary condition
                                                    !   2 -> full implicit (original Noah); temperature top boundary condition
                                                    !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)
  integer, allocatable, dimension(:,:) :: opt_tbot  ! soil_temp_boundary_option
                                                    ! options for lower boundary condition of soil temperature 
                                                    !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                                                    !   2 -> TBOT at ZBOT (8m) read from a file (original Noah)
  integer, allocatable, dimension(:,:) :: opt_frz   ! supercooled_water_option
                                                    ! options for supercooled liquid water (or ice fraction)
                                                    !   1 -> no iteration (Niu and Yang, 2006 JHM)
                                                    !   2 -> nonlinear effects, less permeable (old)
  integer, allocatable, dimension(:,:) :: opt_btr   ! stomatal_resistance_option
                                                    ! options for soil moisture factor for stomatal resistance
                                                    !   1 -> Noah (soil moisture) 
                                                    !   2 -> CLM  (matric potential)
                                                    !   3 -> SSiB (matric potential)
  integer, allocatable, dimension(:,:) :: opt_rsf   ! evap_srfc_resistance_option
                                                    ! options for surface resistance to evaporation/sublimation
                                                    !   1 -> Sakaguchi and Zeng, 2009
                                                    !   2 -> Sellers (1992)
                                                    !   3 -> adjusted Sellers to decrease RSURF for wet soil
                                                    !   4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set in MPTABLE); AD v3.8
  integer, allocatable, dimension(:,:) :: opt_sub   ! subsurface_option
                                                    ! options for subsurface realization
                                                    !   1 -> full Noah-MP style subsurface
                                                    !   2 -> one-way coupled hydrostatic
                                                    !   3 -> two-way coupled (NOT IMPLEMENTED YET)

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault     
    procedure, private :: InitAllocate     
    procedure, private :: InitTransfer 

end type optionsgrid_type

contains   

  subroutine Init(this,namelist)

    class(optionsgrid_type)              :: this
    type(namelist_type)                  :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this,namelist)

    class(optionsgrid_type)             :: this
    type(namelist_type),     intent(in) :: namelist

    associate(n_x => namelist%n_x, &
              n_y => namelist%n_y)

    allocate(this%opt_snf(n_x,n_y))
    allocate(this%opt_run(n_x,n_y))
    allocate(this%opt_drn(n_x,n_y))
    allocate(this%opt_inf(n_x,n_y))
    allocate(this%opt_infdv(n_x,n_y))
    allocate(this%dveg(n_x,n_y))
    allocate(this%opt_alb(n_x,n_y))
    allocate(this%opt_rad(n_x,n_y))
    allocate(this%opt_sfc(n_x,n_y))
    allocate(this%opt_crs(n_x,n_y))
    allocate(this%opt_crop(n_x,n_y))
    allocate(this%opt_stc(n_x,n_y))
    allocate(this%opt_tbot(n_x,n_y)) 
    allocate(this%opt_frz(n_x,n_y))   
    allocate(this%opt_btr(n_x,n_y))
    allocate(this%opt_rsf(n_x,n_y))   
    allocate(this%opt_sub(n_x,n_y)) 

    end associate

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(optionsgrid_type), intent(out) :: this

    this%opt_snf(:,:)   = huge(1)
    this%opt_run(:,:)   = huge(1)
    this%opt_drn(:,:)   = huge(1)
    this%opt_inf(:,:)   = huge(1)
    this%opt_infdv(:,:) = huge(1)
    this%dveg(:,:)      = huge(1)
    this%opt_alb(:,:)   = huge(1)
    this%opt_rad(:,:)   = huge(1)
    this%opt_sfc(:,:)   = huge(1)
    this%opt_crs(:,:)   = huge(1)
    this%opt_crop(:,:)  = huge(1)
    this%opt_stc(:,:)   = huge(1)    
    this%opt_tbot(:,:)  = huge(1)    
    this%opt_frz(:,:)   = huge(1)    
    this%opt_btr(:,:)   = huge(1)    
    this%opt_rsf(:,:)   = huge(1)    
    this%opt_sub(:,:)   = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(optionsgrid_type)             :: this
    type(namelist_type), intent(in)     :: namelist

    this%opt_snf(:,:)   = namelist%precip_phase_option
    this%opt_run(:,:)   = namelist%runoff_option
    this%opt_drn(:,:)   = namelist%drainage_option
    this%opt_inf(:,:)   = namelist%frozen_soil_option
    this%opt_infdv(:,:) = namelist%dynamic_vic_option
    this%dveg(:,:)      = namelist%dynamic_veg_option
    this%opt_alb(:,:)   = namelist%snow_albedo_option
    this%opt_rad(:,:)   = namelist%radiative_transfer_option
    this%opt_sfc(:,:)   = namelist%sfc_drag_coeff_option    
    this%opt_crs(:,:)   = namelist%canopy_stom_resist_option    
    this%opt_crop(:,:)  = namelist%crop_model_option    
    this%opt_stc(:,:)   = namelist%snowsoil_temp_time_option 
    this%opt_tbot(:,:)  = namelist%soil_temp_boundary_option 
    this%opt_frz(:,:)   = namelist%supercooled_water_option
    this%opt_btr(:,:)   = namelist%stomatal_resistance_option
    this%opt_rsf(:,:)   = namelist%evap_srfc_resistance_option
    this%opt_sub(:,:)   = namelist%subsurface_option

  end subroutine InitTransfer

end module OptionsGridType

module NamelistRead

implicit none
save
private

type, public :: namelist_type

  real               :: dt                 ! model timestep (s)
  character(len=12)  :: startdate          ! UTC start datetime of the model run ( YYYYMMDDHHmm )
  character(len=12)  :: enddate            ! UTC end datetime of the model run ( YYYYMMDDHHmm )
  character(len=256) :: input_filename     ! directory/name of the input/forcing file
  character(len=256) :: output_filename    ! directory/name of the output file
  character(len=256) :: parameter_dir      ! name of the directory where parameter TBLs reside
  character(len=256) :: noahowp_table       ! name of noahowp parameter table
  character(len=256) :: soil_table         ! name of soil parameter table
  character(len=256) :: general_table      ! name of general parameter table
  character(len=256) :: soil_class_name    ! name of soil classification (STAS or STAS-RUC)
  character(len=256) :: veg_class_name     ! name of vegetation classification (MODIFIED_IGBP_MODIS_NOAH or USGS)
  real               :: lat                ! latitude (°)
  real               :: lon                ! longitude (°)
  real               :: ZREF               ! measurement height for wind speed (m)

  integer            :: isltyp             ! soil type
  integer            :: nsoil              ! number of soil layers
  integer            :: nsnow              ! number of snow layers
  integer            :: nveg               ! number of vegetation types
  integer            :: structure_option   ! use pre-set (1) or uniform (2) soil layer thicknesses
  real               :: soil_depth         ! soil layer thicknesses if structure_option = 2
  integer            :: vegtyp             ! land cover type
  integer            :: croptype           ! crop type (SET TO 0, no crops currently supported)
  integer            :: sfctyp             ! surface type (1 = land, 2 = lake)
  integer            :: soilcolor          ! soil color code

  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
  real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]
  real                            :: zwt     ! initial water table depth [m]

  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! initial uniform sh2o value
  real    :: initial_sice_value              ! initial uniform sice value
  real    :: initial_zwt                     ! initial water table depth [m]

  !--------------------!
  !   model options    !
  !--------------------!
  integer       :: precip_phase_option       ! options for determining precipitation phase (opt_snf)
  integer       :: runoff_option ! options for runoff (opt_run)
  integer       :: drainage_option ! options for drainage from bottom of soil column (opt_drn)
  integer       :: frozen_soil_option ! options for modifying frozen soil permeability (opt_inf)
  integer       :: dynamic_vic_option ! options for infiltration in dynamic VIC runoff (opt_infdv)
  integer       :: dynamic_veg_option ! options for dynamic vegetation scheme (dveg)
  integer       :: snow_albedo_option ! options for snow albedo (opt_alb)
  integer       :: radiative_transfer_option ! options for radiative transfer (opt_rad)
  integer       :: sfc_drag_coeff_option ! options for computing surface draf coefficient (opt_sfc)
  integer       :: canopy_stom_resist_option ! options for canopy stomatal resistance (opt_crs)
  integer       :: crop_model_option ! options for crop model (opt_crop, NOT SUPPORTED)
  integer       :: snowsoil_temp_time_option ! options for layer 1 snow/soil temperature time scheme (opt_stc)
  integer       :: soil_temp_boundary_option ! options for soil temp lower boundary condition (opt_tbot)
  integer       :: supercooled_water_option ! options for supercooled liquid water (opt_frz)
  integer       :: stomatal_resistance_option ! options for soil moisture factor for stomatal resistance (opt_btr)
  integer       :: evap_srfc_resistance_option ! options for surface resistance to evaporation/sublimation (opt_rsf)
  integer       :: subsurface_option ! options for subsurface realization (opt_sub)

  contains

    procedure, public  :: ReadNamelist

end type namelist_type

contains

  subroutine ReadNamelist(this, namelist_file)

    class(namelist_type) :: this
    ! Optional namelist_file path/filename to read
    character(len=*), intent (in), optional :: namelist_file
    ! Temporary var to hold the default, "namelist.input"
    ! or the value of namelist_file, if passed
    character(:), allocatable :: namelist_file_
    
    integer            :: iz
    real               :: dt
    character(len=12)  :: startdate
    character(len=12)  :: enddate
    character(len=256) :: input_filename
    character(len=256) :: output_filename
    character(len=256) :: parameter_dir
    character(len=256) :: soil_table
    character(len=256) :: veg_class_name
    character(len=256) :: general_table
    character(len=256) :: noahowp_table
    character(len=256) :: soil_class_name
    real               :: lat
    real               :: lon
    real               :: ZREF               ! measurement height for wind speed (m)

    integer       :: isltyp
    integer       :: nsoil
    integer       :: nsnow
    integer       :: nveg
    integer       :: structure_option
    real          :: soil_depth
    integer       :: vegtyp
    integer       :: croptype
    integer       :: sfctyp
    integer       :: soilcolor

    real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
    real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
    real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
    real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]
    real                            :: zwt     ! initial water table depth [m]

    logical :: initial_uniform                 ! initial all levels the same
    real    :: initial_sh2o_value              ! initial uniform sh2o value
    real    :: initial_sice_value              ! initial uniform sice value
    real    :: initial_zwt                     ! initial water table depth [m]

    !--------------------!
    !   model options    !
    !--------------------!
    integer       :: precip_phase_option
    integer       :: runoff_option
    integer       :: drainage_option
    integer       :: frozen_soil_option
    integer       :: dynamic_vic_option
    integer       :: dynamic_veg_option
    integer       :: snow_albedo_option
    integer       :: radiative_transfer_option
    integer       :: sfc_drag_coeff_option
    integer       :: canopy_stom_resist_option
    integer       :: crop_model_option
    integer       :: snowsoil_temp_time_option
    integer       :: soil_temp_boundary_option
    integer       :: supercooled_water_option
    integer       :: stomatal_resistance_option
    integer       :: evap_srfc_resistance_option
    integer       :: subsurface_option

    namelist / timing          / dt,startdate,enddate,input_filename,output_filename
    namelist / parameters      / parameter_dir, soil_table, general_table, noahowp_table, soil_class_name, veg_class_name
    namelist / location        / lat,lon
    namelist / forcing         / ZREF
    namelist / model_options   / precip_phase_option,runoff_option,drainage_option,frozen_soil_option,dynamic_vic_option,&
                                 dynamic_veg_option,snow_albedo_option,radiative_transfer_option,sfc_drag_coeff_option,&
                                 canopy_stom_resist_option,crop_model_option,snowsoil_temp_time_option,soil_temp_boundary_option,&
                                 supercooled_water_option,stomatal_resistance_option,evap_srfc_resistance_option,&
                                 subsurface_option
    namelist / structure       / isltyp,nsoil,nsnow,nveg,structure_option,soil_depth,&
                                 vegtyp,croptype,sfctyp,soilcolor
    namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o,zwt
    namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                                 initial_sice_value,initial_zwt

    !---------------------------------------------------------------------
    !  read input file, part 1
    !---------------------------------------------------------------------
    if( present(namelist_file) ) then
      namelist_file_ = namelist_file
      !print*, 'Reading namelist: ', trim(namelist_file_)
    else
      namelist_file_ = "namelist.input"
      !print*, 'No namelist filename supplied -- attempting to read namelist.input (default)'
    endif

    open(30, file=namelist_file_, form="formatted")
      read(30, timing)
      read(30, parameters)
      read(30, location)
      read(30, forcing)
      read(30, model_options)
      read(30, structure)
      read(30, uniform_initial)
    close(30)

    allocate (zsoil (       1:nsoil))   ! depth of layer-bottom from soil surface
    allocate (dzsnso(-nsnow+1:nsoil))   ! snow/soil layer thickness [m]
    allocate (sice  (       1:nsoil))   ! soil ice content [m3/m3]
    allocate (sh2o  (       1:nsoil))   ! soil liquid water content [m3/m3]

    !---------------------------------------------------------------------
    !  read input file, part 2: initialize
    !---------------------------------------------------------------------
    if(structure_option == 1) then       ! user-defined levels
      open(30, file=namelist_file_, form="formatted")
       read(30, fixed_initial)
      close(30)
    else if(structure_option == 2) then  ! fixed levels
      dzsnso = soil_depth / nsoil
      do iz = 1, nsoil
        zsoil(iz) = -1. * sum(dzsnso(1:iz))
      end do
      if(.not.initial_uniform) &
        stop "structure_option > 1 must have initial_uniform == .true."
    end if

    !---------------------------------------------------------------------
    !  transfer to structure
    !---------------------------------------------------------------------
    this%dt                 = dt
    this%startdate          = startdate
    this%enddate            = enddate
    this%input_filename     = input_filename
    this%output_filename    = output_filename
    this%parameter_dir      = parameter_dir
    this%soil_table         = soil_table
    this%general_table      = general_table
    this%noahowp_table       = noahowp_table
    this%soil_class_name    = soil_class_name
    this%veg_class_name     = veg_class_name
    this%lat                = lat
    this%lon                = lon
    this%ZREF               = ZREF

    this%isltyp           = isltyp
    this%nsoil            = nsoil
    this%nsnow            = nsnow
    this%nveg             = nveg
    this%structure_option = structure_option
    this%soil_depth       = soil_depth
    this%vegtyp           = vegtyp
    this%croptype         = croptype
    this%sfctyp           = sfctyp
    this%soilcolor        = soilcolor

    this%zsoil  = zsoil
    this%dzsnso = dzsnso
    this%sice   = sice
    this%sh2o   = sh2o
    this%zwt    = zwt

    this%initial_uniform    = initial_uniform
    this%initial_sh2o_value = initial_sh2o_value
    this%initial_sice_value = initial_sice_value
    this%initial_zwt        = initial_zwt

    this%precip_phase_option = precip_phase_option
    this%runoff_option       = runoff_option
    this%drainage_option     = drainage_option
    this%frozen_soil_option  = frozen_soil_option
    this%dynamic_vic_option  = dynamic_vic_option
    this%dynamic_veg_option  = dynamic_veg_option
    this%snow_albedo_option  = snow_albedo_option
    this%radiative_transfer_option  = radiative_transfer_option
    this%sfc_drag_coeff_option      = sfc_drag_coeff_option
    this%crop_model_option          = crop_model_option
    this%canopy_stom_resist_option  = canopy_stom_resist_option
    this%snowsoil_temp_time_option  = snowsoil_temp_time_option
    this%soil_temp_boundary_option  = soil_temp_boundary_option
    this%supercooled_water_option   = supercooled_water_option
    this%stomatal_resistance_option = stomatal_resistance_option
    this%evap_srfc_resistance_option= evap_srfc_resistance_option
    this%subsurface_option          = subsurface_option

  end subroutine ReadNamelist

end module NamelistRead

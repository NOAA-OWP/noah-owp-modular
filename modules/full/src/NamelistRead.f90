module modelConfigRead

implicit none
save
private

type, public :: modelConfig_type

  real               :: dt                 ! model timestep (s)
  integer            :: maxtime
  character(len=12)  :: startdate          ! Start date of the model run ( YYYYMMDDHHmm )
  character(len=12)  :: enddate            ! End date of the model run ( YYYYMMDDHHmm )
  character(len=256) :: input_filename     ! name of the input/forcing file
  character(len=256) :: output_filename    ! name of the output file
  real               :: lat                ! latitude (°)
  real               :: lon                ! longitude (°)
  real               :: preciprate         ! precipitation rate
  integer            :: precip_duration    ! duration of precipitation event (# of timesteps)
  integer            :: dry_duration       ! duration of dry event (# of timesteps)
  logical            :: precipitating      ! logical flag for when it is precipitating
  real               :: ZREF               ! measurement height for wind speed (m)

  integer       :: isltyp
  integer       :: nsoil
  integer       :: nsnow
  integer       :: nveg               ! number of vegetation types
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

  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value

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
  integer       :: crop_model_option
  integer       :: canopy_stom_resist_option
  integer       :: snowsoil_temp_time_option
  integer       :: soil_temp_boundary_option
  integer       :: supercooled_water_option
  integer       :: stomatal_resistance_option
  integer       :: evap_srfc_resistance_option

  contains

    procedure, public  :: ReadNamelist

end type modelConfig_type

contains

  subroutine ReadNamelist(this)

    class(modelConfig_type) :: this

    integer            :: iz

    real               :: dt
    integer            :: maxtime
    character(len=12)  :: startdate
    character(len=12)  :: enddate
    character(len=256) :: input_filename
    character(len=256) :: output_filename
    real               :: lat
    real               :: lon
    real               :: preciprate
    integer            :: precip_duration
    integer            :: dry_duration
    logical            :: precipitating
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

    logical :: initial_uniform                 ! initial all levels the same
    real    :: initial_sh2o_value              ! constant sh2o value
    real    :: initial_sice_value              ! constant sice value

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
    integer       :: crop_model_option
    integer       :: canopy_stom_resist_option
    integer       :: snowsoil_temp_time_option
    integer       :: soil_temp_boundary_option
    integer       :: supercooled_water_option
    integer       :: stomatal_resistance_option
    integer       :: evap_srfc_resistance_option

    namelist / timing          / dt,maxtime,startdate,enddate,input_filename,output_filename
    namelist / location        / lat,lon
    namelist / forcing         / preciprate,precip_duration,dry_duration,&
                                 precipitating,ZREF
    namelist / model_options   / precip_phase_option,runoff_option,drainage_option,frozen_soil_option,dynamic_vic_option,&
                                 dynamic_veg_option,snow_albedo_option,radiative_transfer_option,sfc_drag_coeff_option,&
                                 canopy_stom_resist_option,crop_model_option,snowsoil_temp_time_option,soil_temp_boundary_option,&
                                 supercooled_water_option,stomatal_resistance_option,evap_srfc_resistance_option
    namelist / structure       / isltyp,nsoil,nsnow,nveg,structure_option,soil_depth,&
                                 vegtyp,croptype,sfctyp,soilcolor
    namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o
    namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                                 initial_sice_value

!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file="namelist.input", form="formatted")
     read(30, timing)
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
      open(30, file="namelist.input", form="formatted")
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

    this%dt               = dt
    this%maxtime          = maxtime
    this%startdate        = startdate
    this%enddate          = enddate
    this%input_filename   = input_filename
    this%output_filename  = output_filename
    this%lat              = lat
    this%lon              = lon
    this%preciprate       = preciprate
    this%precip_duration  = precip_duration
    this%dry_duration     = dry_duration
    this%precipitating    = precipitating
    this%ZREF             = ZREF

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

    this%initial_uniform    = initial_uniform
    this%initial_sh2o_value = initial_sh2o_value
    this%initial_sice_value = initial_sice_value

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

  end subroutine ReadNamelist

end module modelConfigRead

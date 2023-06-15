module NoahowpmpReadNamelistModule

    use NoahowpmpIOType
    use NoahowpmpIOTypeInitModule
    use NoahowpmpReadTableModule
    use AsciiReadModule
    use OutputModule
    use DateTimeUtilsModule
    use ErrorCheckModule, only: sys_abort
    use ErrorCheckModule, only: is_within_bound
  
    implicit none
  
  contains
  
  subroutine NoahowpmpReadNamelist(NoahowpmpIO,namelist_file)

    implicit none
    type(NoahowpmpIO_type), intent(inout)   :: NoahowpmpIO
    character(len=*), intent(in)            :: namelist_file
    integer                                 :: ierr
    character(len=480)                      :: line

    integer            :: ix,iy,iz
    integer            :: n_x, n_y
    real               :: dt
    character(len=12)  :: startdate
    character(len=12)  :: enddate
    character(len=256) :: forcing_filename
    character(len=256) :: output_filename
    character(len=256) :: parameter_dir
    character(len=256) :: soil_table
    character(len=256) :: veg_class_name
    character(len=256) :: general_table
    character(len=256) :: noahowp_table
    character(len=256) :: soil_class_name
    real               :: lat
    real               :: lon
    real               :: terrain_slope
    real               :: azimuth
    real               :: ZREF               ! measurement height for wind speed (m)
    real               :: rain_snow_thresh

    integer       :: isltyp
    integer       :: nsoil
    integer       :: nsnow
    integer       :: nveg
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
    
    ! ----- END OF VARIABLE DECLARATIONS -------
    
    !--------------------------- !
    !   define namelist groups   !
    !--------------------------- !
    namelist / timing          / dt,startdate,enddate,forcing_filename,output_filename
    namelist / parameters      / parameter_dir, soil_table, general_table, noahowp_table,&
                                 soil_class_name, veg_class_name
    namelist / location        / lat,lon,terrain_slope,azimuth
    namelist / forcing         / ZREF,rain_snow_thresh
    namelist / model_options   / precip_phase_option,runoff_option,drainage_option,frozen_soil_option,&
                                 dynamic_vic_option,dynamic_veg_option,snow_albedo_option,&
                                 radiative_transfer_option,sfc_drag_coeff_option,canopy_stom_resist_option,&
                                 crop_model_option,snowsoil_temp_time_option,soil_temp_boundary_option,&
                                 supercooled_water_option,stomatal_resistance_option,&
                                 evap_srfc_resistance_option,subsurface_option
    namelist / structure       / isltyp,nsoil,nsnow,nveg,vegtyp,croptype,sfctyp,soilcolor
    namelist / initial_values  / zsoil,dzsnso,sice,sh2o,zwt    
    
    ! missing values against which namelist options can be checked
    integer            :: integerMissing
    real               :: realMissing
    character(len=12)  :: stringMissing
    
    ! ----------------------------------------------------------------------------------------------- !
    !   initialize all namelist variables to missing values to allow for checking after namelist read !
    ! ----------------------------------------------------------------------------------------------- !
    integerMissing   = -999999
    realMissing      = -999999.0
    stringMissing    = 'MISSING'
     
    iz               = integerMissing
    dt               = realMissing
    startdate        = stringMissing
    enddate          = stringMissing
    forcing_filename   = stringMissing
    output_filename  = stringMissing
    parameter_dir    = stringMissing
    soil_table       = stringMissing
    veg_class_name   = stringMissing
    general_table    = stringMissing
    noahowp_table    = stringMissing
    soil_class_name  = stringMissing
    lat              = realMissing
    lon              = realMissing
    terrain_slope    = realMissing
    azimuth          = realMissing
    ZREF             = realMissing            
    rain_snow_thresh = realMissing
   
    isltyp           = integerMissing
    nsoil            = integerMissing
    nsnow            = integerMissing
    nveg             = integerMissing
    vegtyp           = integerMissing
    croptype         = integerMissing
    sfctyp           = integerMissing
    soilcolor        = integerMissing
    zwt              = realMissing      

    precip_phase_option         = integerMissing
    runoff_option               = integerMissing
    drainage_option             = integerMissing
    frozen_soil_option          = integerMissing
    dynamic_vic_option          = integerMissing
    dynamic_veg_option          = integerMissing
    snow_albedo_option          = integerMissing
    radiative_transfer_option   = integerMissing
    sfc_drag_coeff_option       = integerMissing
    canopy_stom_resist_option   = integerMissing
    crop_model_option           = integerMissing
    snowsoil_temp_time_option   = integerMissing
    soil_temp_boundary_option   = integerMissing
    supercooled_water_option    = integerMissing
    stomatal_resistance_option  = integerMissing
    evap_srfc_resistance_option = integerMissing
    subsurface_option           = integerMissing

    !---------------------------------------------------------------------
    !  read namelist
    !---------------------------------------------------------------------
    ierr = 0
    if( trim(namelist_file) .ne. '' ) then
      open(30, file=namelist_file, form="formatted", status='old', iostat=ierr)
      if(ierr /= 0) then; write(*,'(A)') 'ERROR: user specified namelist file not found: '//trim(namelist_file); stop; end if
      !print*, 'Reading namelist: ', trim(namelist_file)
    else
      open(30, file='./namelist.input', form="formatted", status='old', iostat=ierr)
      if(ierr /= 0) then; write(*,'(A)') 'ERROR: default namelist file not found: ./namelist.input'; stop; end if
      !print*, 'No namelist filename supplied -- attempting to read namelist.input (default)'
    endif

    read(30, timing, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    read(30, parameters, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    read(30, location, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    read(30, forcing, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    read(30, model_options, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    read(30, structure, iostat=ierr)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      

    !---------------------------------------------------------------------
    !  Check model option validity, part 2
    !---------------------------------------------------------------------
    if (.not. is_within_bound(precip_phase_option, 1, 7)) then; call sys_abort(1,'model options: precip_phase_option should be 1-7'); end if
    if (.not. is_within_bound(runoff_option, 1, 8)) then; call sys_abort(1,'model options: runoff_option should be 1-8'); end if
    if (.not. is_within_bound(drainage_option, 1, 8)) then; call sys_abort(1,'model options: drainage_option should be 1-8'); end if
    if (.not. is_within_bound(frozen_soil_option ,1, 2)) then; call sys_abort(1,'model options: frozen_soil_option should be 1-2'); end if
    if (.not. is_within_bound(dynamic_vic_option ,1, 3)) then; call sys_abort(1,'model options: dynamic_vic_option should be 1-3'); end if
    if (.not. is_within_bound(dynamic_veg_option ,1, 9)) then; call sys_abort(1,'model options: dynamic_veg_option should be 1-9'); end if
    if (.not. is_within_bound(snow_albedo_option ,1, 2)) then; call sys_abort(1,'model options: snow_albedo_option should be 1-2'); end if
    if (.not. is_within_bound(radiative_transfer_option,1, 3)) then; call sys_abort(1,'model options: radiative_transfer_option should be 1-3'); end if
    if (.not. is_within_bound(sfc_drag_coeff_option, 1, 2)) then; call sys_abort(1,'model options: sfc_drag_coeff_option should be 1-3'); end if
    if (.not. is_within_bound(canopy_stom_resist_option, 1, 2)) then; call sys_abort(1,'model options: sfc_drag_coeff_option should be 1-2'); end if
    if (.not. is_within_bound(snowsoil_temp_time_option, 1, 3)) then; call sys_abort(1,'model options: snowsoil_temp_time_option should be 1-3'); end if
    if (.not. is_within_bound(soil_temp_boundary_option, 1, 2)) then; call sys_abort(1,'model options: soil_temp_boundary_option should be 1-2'); end if
    if (.not. is_within_bound(supercooled_water_option, 1, 2)) then; call sys_abort(1,'model options: supercooled_water_option should be 1-2'); end if
    if (.not. is_within_bound(stomatal_resistance_option, 1, 3)) then; call sys_abort(1,'model options: stomatal_resistance_option should be 1-3'); end if
    if (.not. is_within_bound(evap_srfc_resistance_option, 1, 4)) then; call sys_abort(1,'model options: evap_srfc_resistance_option should be 1-4'); end if
    if (.not. is_within_bound(subsurface_option, 1, 3)) then; call sys_abort(1,'model options: subsurface_option should be 1-3'); end if

    !  after reading # of soil layers, allocate local arrays and read soil structure info
    allocate (zsoil (       1:nsoil))   ! depth of layer-bottom from soil surface
    allocate (dzsnso(-nsnow+1:nsoil))   ! snow/soil layer thickness [m]
    allocate (sice  (       1:nsoil))   ! soil ice content [m3/m3]
    allocate (sh2o  (       1:nsoil))   ! soil liquid water content [m3/m3]
    
    ! pre-assign missing values
    sice(1)   = realMissing
    dzsnso(1) = realMissing
    sh2o(1)   = realMissing

    ! read remaining group from namelist
    read(30, initial_values)
    if (ierr/=0) then; backspace(30); read(30,fmt='(A)') line; write(*,'(A)') 'ERROR: invalid line in namelist: '//trim(line); stop; end if      
    close(30)
    
    ! calculate total soil depth and populate array for depth of layer-bottom from soil surface
    if(dzsnso(1) /= realMissing) then
      soil_depth = sum(dzsnso(1:nsoil))
      !soil_depth = 0
      do iz = 1, nsoil
        zsoil(iz) = -1. * sum(dzsnso(1:iz))      
      end do
    else 
      write(*,'(A)') 'ERROR: required entry dzsnso not found in namelist'; stop
    end if 

    !---------------------------------------------------------------------
    !  check xyz dimensions and move to NoahowpmpIO
    !---------------------------------------------------------------------
    ! n_x and n_y tobe read in via namelist.input but currently hardcoding here
    n_x = 2
    n_y = 3
    if(n_x              /= integerMissing) then; NoahowpmpIO%n_x = n_x; else; write(*,'(A)') 'ERROR: required entry n_x not found in namelist'; stop; end if
    if(n_y              /= integerMissing) then; NoahowpmpIO%n_y = n_y; else; write(*,'(A)') 'ERROR: required entry n_y not found in namelist'; stop; end if
    if(nsoil            /= integerMissing) then; NoahowpmpIO%nsoil = nsoil; else; write(*,'(A)') 'ERROR: required entry nsoil not found in namelist'; stop; end if
    if(nsnow            /= integerMissing) then; NoahowpmpIO%nsnow = nsnow; else; write(*,'(A)') 'ERROR: required entry nsnow not found in namelist'; stop; end if
    if(forcing_filename /= stringMissing) then; NoahowpmpIO%forcing_filename = forcing_filename; else; write(*,'(A)') 'ERROR: required entry forcing_filename not found in namelist'; stop; end if
    if(output_filename  /= stringMissing) then; NoahowpmpIO%output_filename = output_filename; else; write(*,'(A)') 'ERROR: required entry output_filename not found in namelist'; stop; end if
    if(parameter_dir    /= stringMissing) then; NoahowpmpIO%parameter_dir = parameter_dir; else; write(*,'(A)') 'ERROR: required entry parameter_dir not found in namelist'; stop; end if
    if(soil_table       /= stringMissing) then; NoahowpmpIO%soil_table = soil_table; else; write(*,'(A)') 'ERROR: required entry soil_table  not found in namelist'; stop; end if
    if(general_table    /= stringMissing) then; NoahowpmpIO%general_table = general_table; else; write(*,'(A)') 'ERROR: required entry general_table not found in namelist'; stop; end if
    if(noahowp_table    /= stringMissing) then; NoahowpmpIO%noahowp_table = noahowp_table; else; write(*,'(A)') 'ERROR: required entry noahowp_table not found in namelist'; stop; end if
    if(soil_class_name  /= stringMissing) then; NoahowpmpIO%soil_class_name = soil_class_name; else; write(*,'(A)') 'ERROR: required entry soil_class_name not found in namelist'; stop; end if
    if(veg_class_name   /= stringMissing) then; NoahowpmpIO%veg_class_name = veg_class_name; else; write(*,'(A)') 'ERROR: required entry veg_class_name not found in namelist'; stop; end if

    !---------------------------------------------------------------------
    !  allocate NoahowpmpIO arrays
    !---------------------------------------------------------------------
    call NoahowpmpIOTypeInit(NoahowpmpIO)

    !---------------------------------------------------------------------
    !  read in table variables
    !---------------------------------------------------------------------
    call TableVarsRead(NoahowpmpIO)

    !---------------------------------------------------------------------
    !  transfer remaining values to NoahowpmpIO
    !---------------------------------------------------------------------
    if(dt               /= realMissing)   then; NoahowpmpIO%dt = dt; else; write(*,'(A)') 'ERROR: required entry dt not found in namelist'; stop; end if 
    if(startdate        /= stringMissing) then; NoahowpmpIO%startdate = startdate; else; write(*,'(A)') 'ERROR: required entry startdate not found in namelist'; stop; end if
    if(enddate          /= stringMissing) then; NoahowpmpIO%enddate = enddate; else; write(*,'(A)') 'ERROR: required entry enddate not found in namelist'; stop; end if

    if(lat              /= realMissing) then; NoahowpmpIO%lat(:,:) = lat; else; write(*,'(A)') 'ERROR: required entry lat not found in namelist'; stop; end if
    if(lon              /= realMissing) then; NoahowpmpIO%lon(:,:) = lon; else; write(*,'(A)') 'ERROR: required entry lon not found in namelist'; stop; end if
    if(terrain_slope    /= realMissing) then; NoahowpmpIO%terrain_slope(:,:) = terrain_slope; else; write(*,'(A)') 'ERROR: required entry terrain_slope not found in namelist'; stop; end if
    if(azimuth          /= realMissing) then; NoahowpmpIO%azimuth(:,:) = azimuth; else; write(*,'(A)') 'ERROR: required entry azimuth not found in namelist'; stop; end if
    if(zref             /= realMissing) then; NoahowpmpIO%ZREF(:,:) = ZREF; else; write(*,'(A)') 'ERROR: required entry ZREF not found in namelist'; stop; end if
    if(rain_snow_thresh /= realMissing) then; NoahowpmpIO%rain_snow_thresh = rain_snow_thresh; else; write(*,'(A)') 'ERROR: required entry rain_snow_thresh not found in namelist'; stop; end if

    if(isltyp     /= integerMissing) then; NoahowpmpIO%isltyp(:,:) = isltyp; else; write(*,'(A)') 'ERROR: required entry isltyp not found in namelist'; stop; end if
    if(nveg       /= integerMissing) then; NoahowpmpIO%nveg = nveg; else; write(*,'(A)') 'ERROR: required entry nveg not found in namelist'; stop; end if
    if(vegtyp     /= integerMissing) then; NoahowpmpIO%vegtyp(:,:) = vegtyp; else; write(*,'(A)') 'ERROR: required entry vegtyp not found in namelist'; stop; end if
    if(croptype   /= integerMissing) then; NoahowpmpIO%croptype(:,:) = croptype; else; write(*,'(A)') 'ERROR: required entry croptype not found in namelist'; stop; end if
    if(sfctyp     /= integerMissing) then; NoahowpmpIO%IST(:,:) = sfctyp; else; write(*,'(A)') 'ERROR: required entry sfctyp not found in namelist'; stop; end if
    if(soilcolor  /= integerMissing) then; NoahowpmpIO%soilcolor(:,:) = soilcolor; else; write(*,'(A)') 'ERROR: required entry soilcolor not found in namelist'; stop; end if
    if(zwt        /= realMissing) then; NoahowpmpIO%zwt(:,:) = zwt; else; write(*,'(A)') 'ERROR: required entry zwt not found in namelist'; stop; end if

    if(precip_phase_option         /= integerMissing) then; NoahowpmpIO%opt_snf(:,:) = precip_phase_option; else; write(*,'(A)') 'ERROR: required entry precip_phase_option not found in namelist'; stop; end if
    if(runoff_option               /= integerMissing) then; NoahowpmpIO%opt_run(:,:) = runoff_option; else; write(*,'(A)') 'ERROR: required entry runoff_option not found in namelist'; stop; end if
    if(drainage_option             /= integerMissing) then; NoahowpmpIO%opt_drn(:,:) = drainage_option; else; write(*,'(A)') 'ERROR: required entry drainage_option not found in namelist'; stop; end if
    if(frozen_soil_option          /= integerMissing) then; NoahowpmpIO%opt_inf(:,:) = frozen_soil_option; else; write(*,'(A)') 'ERROR: required entry frozen_soil_option not found in namelist'; stop; end if
    if(dynamic_vic_option          /= integerMissing) then; NoahowpmpIO%opt_infdv(:,:) = dynamic_vic_option; else; write(*,'(A)') 'ERROR: required entry dynamic_vic_option not found in namelist'; stop; end if
    if(dynamic_veg_option          /= integerMissing) then; NoahowpmpIO%dveg(:,:) = dynamic_veg_option; else; write(*,'(A)') 'ERROR: required entry dynamic_veg_option not found in namelist'; stop; end if
    if(snow_albedo_option          /= integerMissing) then; NoahowpmpIO%opt_alb(:,:) = snow_albedo_option; else; write(*,'(A)') 'ERROR: required entry snow_albedo_option not found in namelist'; stop; end if
    if(radiative_transfer_option   /= integerMissing) then; NoahowpmpIO%opt_rad(:,:) = radiative_transfer_option; else; write(*,'(A)') 'ERROR: required entry radiative_transfer_option not found in namelist'; stop; end if
    if(sfc_drag_coeff_option       /= integerMissing) then; NoahowpmpIO%opt_sfc(:,:) = sfc_drag_coeff_option; else; write(*,'(A)') 'ERROR: required entry sfc_drag_coeff_option not found in namelist'; stop; end if
    if(crop_model_option           /= integerMissing) then; NoahowpmpIO%opt_crop(:,:) = crop_model_option; else; write(*,'(A)') 'ERROR: required entry crop_model_option not found in namelist'; stop; end if
    if(canopy_stom_resist_option   /= integerMissing) then; NoahowpmpIO%opt_crs(:,:) = canopy_stom_resist_option; else; write(*,'(A)') 'ERROR: required entry canopy_stom_resist_option not found in namelist'; stop; end if
    if(snowsoil_temp_time_option   /= integerMissing) then; NoahowpmpIO%opt_stc(:,:) = snowsoil_temp_time_option; else; write(*,'(A)') 'ERROR: required entry snowsoil_temp_time_option not found in namelist'; stop; end if
    if(soil_temp_boundary_option   /= integerMissing) then; NoahowpmpIO%opt_tbot(:,:) = soil_temp_boundary_option; else; write(*,'(A)') 'ERROR: required entry soil_temp_boundary_option not found in namelist'; stop; end if
    if(supercooled_water_option    /= integerMissing) then; NoahowpmpIO%opt_frz(:,:) = supercooled_water_option; else; write(*,'(A)') 'ERROR: required entry supercooled_water_option not found in namelist'; stop; end if
    if(stomatal_resistance_option  /= integerMissing) then; NoahowpmpIO%opt_btr(:,:) = stomatal_resistance_option; else; write(*,'(A)') 'ERROR: required entry stomatal_resistance_option not found in namelist'; stop; end if
    if(evap_srfc_resistance_option /= integerMissing) then; NoahowpmpIO%opt_rsf(:,:) = evap_srfc_resistance_option; else; write(*,'(A)') 'ERROR: required entry evap_srfc_resistance_option not found in namelist'; stop; end if
    if(subsurface_option           /= integerMissing) then; NoahowpmpIO%opt_sub(:,:) = subsurface_option; else; write(*,'(A)') 'ERROR: required entry subsurface_option not found in namelist'; stop; end if

    if(zsoil(1).eq.realMissing) then
      write(*,'(A)') 'ERROR: required entry zsoil not found in namelist'
      stop
    else
      do iz = 1, nsoil
        NoahowpmpIO%zsoil(:,:,iz) = zsoil(iz)
      end do
    end if
    if(dzsnso(1).eq.realMissing) then
      write(*,'(A)') 'ERROR: required entry dzsnso not found in namelist'
      stop
    else
      do iz = -nsnow+1,nsoil
        NoahowpmpIO%dzsnso(:,:,iz) = dzsnso(iz)
      end do
    end if
    if(sice(1).eq.realMissing) then
      write(*,'(A)') 'ERROR: required entry sice not found in namelist'
      stop
    else
      do iz = 1, nsoil
        NoahowpmpIO%sice(:,:,iz) = sice(iz)
      end do
    end if
    if(sh2o(1).eq.realMissing) then
      write(*,'(A)') 'ERROR: required entry sh2o not found in namelist'
      stop
    else
      do iz = 1, nsoil
        NoahowpmpIO%sh2o(:,:,iz) = sh2o(iz)
      end do
    end if

      ! Initializations
      ! for soil water
      !water%zwt       = -100.0       ! should only be needed for run=1
    NoahowpmpIO%smcwtd(:,:)    = 0.0          ! should only be needed for run=5
    NoahowpmpIO%deeprech(:,:)  = 0.0          ! should only be needed for run=5
    NoahowpmpIO%qinsur(:,:)    = 0.0          !
    NoahowpmpIO%runsrf(:,:)    = 0.0          !
    NoahowpmpIO%runsub(:,:)    = 0.0          !
    NoahowpmpIO%qdrain(:,:)    = 0.0          !
    NoahowpmpIO%wcnd(:,:,:)      = 0.0          !
    NoahowpmpIO%fcrmax(:,:)    = 0.0          !
    NoahowpmpIO%snoflow(:,:)   = 0.0          ! glacier outflow for all RUNSUB options, [mm/s]
    NoahowpmpIO%qseva(:,:)     = 0.0          ! soil evaporation [mm/s]
    NoahowpmpIO%etrani(:,:,:)    = 0.0          ! transpiration from each level[mm/s]
    NoahowpmpIO%btrani(:,:,:)    = 0.0          ! soil water transpiration factor (0 to 1) by soil layer
    NoahowpmpIO%btran(:,:)     = 0.0          ! soil water transpiration factor (0 to 1)

    ! for canopy water
    NoahowpmpIO%RAIN(:,:)      = 0.0          ! rainfall mm/s
    NoahowpmpIO%SNOW(:,:)      = 0.0          ! snowfall mm/s
    NoahowpmpIO%BDFALL(:,:)    = 0.0        ! bulk density of snowfall (kg/m3)
    NoahowpmpIO%FB_snow(:,:)   = 0.0          ! canopy fraction buried by snow (computed from phenology)
    NoahowpmpIO%FP(:,:)        = 1.0          ! fraction of the gridcell that receives precipitation
    NoahowpmpIO%CANLIQ(:,:)    = 0.0          ! canopy liquid water [mm]
    NoahowpmpIO%CANICE(:,:)    = 0.0          ! canopy frozen water [mm]
    NoahowpmpIO%FWET(:,:)      = 0.0          ! canopy fraction wet or snow
    NoahowpmpIO%CMC(:,:)       = 0.0          ! intercepted water per ground area (mm)
    NoahowpmpIO%QINTR(:,:)    = 0.0           ! interception rate for rain (mm/s)
    NoahowpmpIO%QDRIPR(:,:)   = 0.0           ! drip rate for rain (mm/s)
    NoahowpmpIO%QTHROR(:,:)   = 0.0           ! throughfall for rain (mm/s)
    NoahowpmpIO%QINTS(:,:)    = 0.0           ! interception (loading) rate for snowfall (mm/s)
    NoahowpmpIO%QDRIPS(:,:)   = 0.0           ! drip (unloading) rate for intercepted snow (mm/s)
    NoahowpmpIO%QTHROS(:,:)   = 0.0           ! throughfall of snowfall (mm/s)
    NoahowpmpIO%QRAIN(:,:)    = 0.0           ! rain at ground srf (mm/s) [+]
    NoahowpmpIO%QSNOW(:,:)    = 0.0           ! snow at ground srf (mm/s) [+]
    NoahowpmpIO%SNOWHIN(:,:)  = 0.0           ! snow depth increasing rate (m/s)
    NoahowpmpIO%ECAN(:,:)     = 0.0           ! evap of intercepted water (mm/s) [+]
    NoahowpmpIO%ETRAN(:,:)    = 0.0           ! transpiration rate (mm/s) [+]

    ! for snow water
    NoahowpmpIO%QVAP(:,:)     = 0.0           ! evaporation/sublimation rate mm/s 
    NoahowpmpIO%ISNOW(:,:)    = 0
    NoahowpmpIO%SNOWH(:,:)    = 0.0
    NoahowpmpIO%SNEQV(:,:)    = 0.0
    NoahowpmpIO%SNEQVO(:,:)   = 0.0
    NoahowpmpIO%BDSNO(:,:)    = 0.0
    NoahowpmpIO%PONDING(:,:)  = 0.0
    NoahowpmpIO%PONDING1(:,:) = 0.0
    NoahowpmpIO%PONDING2(:,:) = 0.0
    NoahowpmpIO%QSNBOT(:,:)   = 0.0
    NoahowpmpIO%QSNFRO(:,:)   = 0.0
    NoahowpmpIO%QSNSUB(:,:)   = 0.0
    NoahowpmpIO%QDEW(:,:)     = 0.0
    NoahowpmpIO%QSDEW(:,:)    = 0.0
    NoahowpmpIO%SNICE(:,:,:)    = 0.0
    NoahowpmpIO%SNLIQ(:,:,:)    = 0.0
    NoahowpmpIO%FICEOLD(:,:,:)  = 0.0
    NoahowpmpIO%FSNO(:,:)     = 0.0

    ! for energy-related variable
    NoahowpmpIO%TV(:,:)      = 298.0        ! leaf temperature [K]
    NoahowpmpIO%TG(:,:)      = 298.0        ! ground temperature [K]
    NoahowpmpIO%CM(:,:)      = 0.0          ! momentum drag coefficient
    NoahowpmpIO%CH(:,:)      = 0.0          ! heat drag coefficient
    NoahowpmpIO%FCEV(:,:)    = 5.0          ! constant canopy evaporation (w/m2) [+ to atm ]
    NoahowpmpIO%FCTR(:,:)    = 5.0          ! constant transpiration (w/m2) [+ to atm]
    NoahowpmpIO%IMELT(:,:,:)   = 1 ! freeze
    NoahowpmpIO%STC(:,:,:)     = 298.0
    NoahowpmpIO%COSZ(:,:)    = 0.7        ! cosine of solar zenith angle
    NoahowpmpIO%ICE(:,:)     = 0          ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
    NoahowpmpIO%ALB(:,:)     = 0.6        ! initialize snow albedo in CLASS routine
    NoahowpmpIO%ALBOLD(:,:)  = 0.6        ! initialize snow albedo in CLASS routine
    NoahowpmpIO%FROZEN_CANOPY(:,:) = .false. ! used to define latent heat pathway
    NoahowpmpIO%FROZEN_GROUND(:,:) = .false. 

    ! -- forcings 
    ! these are initially set to huge(1) -- to trap errors may want to set to a recognizable flag if they are
    !   supposed to be assigned below (eg -9999)
    !forcing%UU       = 0.0        ! wind speed in u direction (m s-1)
    !forcing%VV       = 0.0        ! wind speed in v direction (m s-1)
    !forcing%SFCPRS   = 0.0        ! pressure (pa)
    !forcing%SFCTMP   = 0.0        ! surface air temperature [k]
    !forcing%Q2       = 0.0        ! mixing ratio (kg/kg)
    !forcing%PRCP     = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
    !forcing%SOLDN    = 0.0        ! downward shortwave radiation (w/m2)
    !forcing%LWDN     = 0.0        ! downward longwave radiation (w/m2)
    
    ! forcing-related variables
    NoahowpmpIO%PRCPCONV(:,:) = 0.0        ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
    NoahowpmpIO%PRCPNONC(:,:) = 0.0        ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
    NoahowpmpIO%PRCPSHCV(:,:) = 0.0        ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
    NoahowpmpIO%PRCPSNOW(:,:) = 0.0        ! snow entering land model [mm/s]              ! MB/AN : v3.7
    NoahowpmpIO%PRCPGRPL(:,:) = 0.0        ! graupel entering land model [mm/s]           ! MB/AN : v3.7
    NoahowpmpIO%PRCPHAIL(:,:) = 0.0        ! hail entering land model [mm/s]              ! MB/AN : v3.7
    NoahowpmpIO%THAIR(:,:)    = 0.0        ! potential temperature (k)
    NoahowpmpIO%QAIR(:,:)     = 0.0        ! specific humidity (kg/kg) (q2/(1+q2))
    NoahowpmpIO%EAIR(:,:)     = 0.0        ! vapor pressure air (pa)
    NoahowpmpIO%RHOAIR(:,:)   = 0.0        ! density air (kg/m3)
    NoahowpmpIO%SWDOWN(:,:)   = 0.0        ! downward solar filtered by sun angle [w/m2]
    NoahowpmpIO%FPICE(:,:)    = 0.0        ! fraction of ice                AJN
    NoahowpmpIO%JULIAN        = 0.0        ! Setting arbitrary julian day
    NoahowpmpIO%YEARLEN       = 365        ! Setting year to be normal (i.e. not a leap year)  
    NoahowpmpIO%FOLN(:,:)     = 1.0        ! foliage nitrogen concentration (%); for now, set to nitrogen saturation
    NoahowpmpIO%TBOT(:,:)     = 285.0      ! bottom condition for soil temperature [K]

    ! domain variables
    do iz = -NoahowpmpIO%nsnow+1,0
      NoahowpmpIO%zsnso(:,:,iz) = 0.0
    end do
    do iz = 1,NoahowpmpIO%nsoil
      NoahowpmpIO%zsnso(:,:,iz)    = zsoil(iz)
    end do

    NoahowpmpIO%smc = NoahowpmpIO%sh2o + NoahowpmpIO%sice  ! volumetric soil water
    NoahowpmpIO%smc_init = NoahowpmpIO%smc               ! initial SMC

    ! time variables
    NoahowpmpIO%nowdate   = NoahowpmpIO%startdate ! start the model with nowdate = startdate
    NoahowpmpIO%start_datetime = date_to_unix(NoahowpmpIO%startdate)
    NoahowpmpIO%end_datetime   = date_to_unix(NoahowpmpIO%enddate)
    NoahowpmpIO%itime     = 1                ! initialize the time loop counter at 1
    NoahowpmpIO%time_dbl  = 0.d0             ! start model run at t = 0
    call get_utime_list (NoahowpmpIO%start_datetime, NoahowpmpIO%end_datetime, NoahowpmpIO%dt, NoahowpmpIO%sim_datetimes)  ! makes unix-time list for desired records (end-of-timestep)
    NoahowpmpIO%ntime = size (NoahowpmpIO%sim_datetimes) 

!---------------------------------------------------------------------
! Open the forcing file
! Code adapted from the ASCII_IO from NOAH-MP V1.1
! Compiler directive NGEN_FORCING_ACTIVE to be defined if 
! Nextgen forcing is being used (https://github.com/NOAA-OWP/ngen)
!---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
    call open_forcing_file(NoahowpmpIO%forcing_filename)
#endif
    
!---------------------------------------------------------------------
! create output file and add initial values
! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
!---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    call initialize_output(NoahowpmpIO%output_filename, NoahowpmpIO%ntime, NoahowpmpIO%nsoil, NoahowpmpIO%nsnow, NoahowpmpIO%n_x, NoahowpmpIO%n_y)
#endif

  end subroutine

end module NoahowpmpReadNamelistModule
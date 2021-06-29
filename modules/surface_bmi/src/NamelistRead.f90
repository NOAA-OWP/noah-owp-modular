module NamelistRead

implicit none
save
private

type, public :: namelist_type

  real              :: dt                 ! model timestep (s)
  integer           :: maxtime
  character(len=12) :: startdate          ! Start date of the model run ( YYYYMMDDHHmm ) 
  character(len=12) :: enddate            ! End date of the model run ( YYYYMMDDHHmm ) 
  character*256     :: input_filename     ! name of the input/forcing file
  character*256     :: output_filename    ! name of the output file
  real              :: lat                ! latitude (°)
  real              :: lon                ! longitude (°)
  real              :: preciprate         ! precipitation rate 
  integer           :: precip_duration    ! duration of precipitation event (# of timesteps)
  integer           :: dry_duration       ! duration of dry event (# of timesteps)
  logical           :: precipitating      ! logical flag for when it is precipitating
  real              :: ZREF               ! measurement height for wind speed (m)
  
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
  real                            :: zwt     ! initial water table depth [m]

  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value
  real    :: initial_zwt                     ! constant water table depth [m]

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

  !--------------------!
  !  soil parameters   !
  !--------------------!

  real, dimension(12) ::      bb  ! b parameter
  real, dimension(12) ::   satdk  ! conductivity at saturation
  real, dimension(12) ::   satdw  ! diffusivity at saturation
  real, dimension(12) ::  maxsmc  ! porosity
  real, dimension(12) ::  satpsi  ! matric potential at saturation
  real, dimension(12) ::  wltsmc  ! wilting point
  real, dimension(12) ::  refsmc  ! field capacity
  real, dimension(12) :: pctsand  ! percent sand
  real, dimension(12) :: pctclay  ! percent clay
  real, dimension(12) ::   bvic   ! VIC or DVIC model infiltration parameter
  real, dimension(12) ::   AXAJ   ! Xinanjiang: Tension water distribution inflection parameter [-]
  real, dimension(12) ::   BXAJ   ! Xinanjiang: Tension water distribution shape parameter [-]
  real, dimension(12) ::   XXAJ   ! Xinanjiang: Free water distribution shape parameter [-]
  real, dimension(12) ::   G      ! Mean Capillary Drive (m) for infiltration models
  real, dimension(12) ::   BBVIC  ! DVIC heterogeniety parameter for infiltration 
  real, dimension(12) ::   QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
  real                ::   slope  ! free drainage parameter
  real                ::  refkdt  ! infiltration parameter for Schaake scheme
  real                ::   refdk  ! reference diffusivity for Schaake scheme
  real                ::   csoil  ! volumetric soil heat capacity [j/m3/K]
  real                ::   Z0     ! bare soil roughness length (m)
  real                ::   CZIL   ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
  real                ::   ZBOT   ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL

  !--------------------!
  !  Vegetation parameters   !
  !--------------------!
  real, dimension(20)     ::   LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC ! lai by month & vegtyp
  real, dimension(20)     ::   SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC ! sai by month & vegtyp
  real, dimension(20)     ::   CH2OP
  real, dimension(20)     ::   NROOT
  real, dimension(20)     ::   HVT            ! canopy top height (m)
  real, dimension(20)     ::   HVB            ! canopy bottom height (m)
  real, dimension(20)     ::   TMIN           ! minimum temperature for photosynthesis (k)
  real, dimension(20)     ::   SHDFAC         ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real, dimension(20)     ::   SHDMAX         ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real, dimension(20)     ::   Z0MVT          ! momentum roughness length (m)
  real, dimension(20)     ::   RC             ! tree crown radius (m)
  real, dimension(20)     ::   XL             ! leaf/stem orientation index
  real, dimension(20)     ::   BP             ! minimum leaf conductance (umol/m**2/s)
  real, dimension(20)     ::   FOLNMX         ! foliage nitrogen concentration when f(n)=1 (%)
  real, dimension(20)     ::   QE25           ! quantum efficiency at 25c (umol co2 / umol photon)
  real, dimension(20)     ::   VCMX25         ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
  real, dimension(20)     ::   MP             ! slope of conductance-to-photosynthesis relationship
  real, dimension(20)     ::   RGL            ! Parameter used in radiation stress function
  real, dimension(20)     ::   RSMIN          ! Minimum stomatal resistance [s m-1]
  real, dimension(20)     ::   HS             ! Parameter used in vapor pressure deficit function
  real, dimension(20)     ::   AKC            ! q10 for kc25
  real, dimension(20)     ::   AKO            ! q10 for ko25
  real, dimension(20)     ::   AVCMX          ! q10 for vcmx25
  real, dimension(20)     ::   RSMAX          ! Maximal stomatal resistance [s m-1]
  real                    ::   CWP            ! canopy wind absorption coefficient (formerly CWPVT)
  real                    ::   C3PSN          ! photosynth. pathway: 0. = c4, 1. = c3 
  real                    ::   DLEAF          ! characteristic leaf dimension (m)
  real                    ::   KC25           ! co2 michaelis-menten constant at 25c (pa)
  real                    ::   KO25           ! o2 michaelis-menten constant at 25c (pa)
  real, dimension(20)     ::   RHOL_VIS       ! leaf reflectance in visible
  real, dimension(20)     ::   RHOL_NIR       ! leaf reflectance in near infrared
  real, dimension(20)     ::   RHOS_VIS       ! stem reflectance in visible
  real, dimension(20)     ::   RHOS_NIR       ! stem reflectance in near infrared
  real, dimension(20)     ::   TAUL_VIS       ! leaf transmittance in visible
  real, dimension(20)     ::   TAUL_NIR       ! leaf transmittance in near infrared
  real, dimension(20)     ::   TAUS_VIS       ! stem transmittance in visible
  real, dimension(20)     ::   TAUS_NIR       ! stem transmittance in near infrared
  real, dimension(20,2)   ::   RHOL_TABLE     ! leaf reflectance table (1 = vis, 2 = NIR)
  real, dimension(20,2)   ::   RHOS_TABLE     ! stem reflectance table (1 = vis, 2 = NIR)
  real, dimension(20,2)   ::   TAUL_TABLE     ! leaf transmittance table (1 = vis, 2 = NIR)
  real, dimension(20,2)   ::   TAUS_TABLE     ! stem transmittance table (1 = vis, 2 = NIR)
  real, dimension(20,12)  ::   LAIM_TABLE     ! monthly leaf area index, one-sided
  real, dimension(20,12)  ::   SAIM_TABLE     ! monthly stem area index, one-sided
  

  !--------------------!
  !  snow parameters   !
  !--------------------!
  real                ::   SSI     ! liquid water holding capacity of snowpack (m3/m3)
  real                ::   MFSNO   ! fractional snow covered area (FSNO) curve parameter
  real                ::   Z0SNO   ! snow surface roughness length (m)
  real                ::   SWEMX        ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
  real                ::   TAU0         ! tau0 from Yang97 eqn. 10a
  real                ::   GRAIN_GROWTH ! growth from vapor diffusion Yang97 eqn. 10b
  real                ::   EXTRA_GROWTH ! extra growth near freezing Yang97 eqn. 10c
  real                ::   DIRT_SOOT    ! dirt and soot term Yang97 eqn. 10d
  real                ::   BATS_COSZ    ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
  real                ::   BATS_VIS_NEW ! new snow visible albedo
  real                ::   BATS_NIR_NEW ! new snow NIR albedo
  real                ::   BATS_VIS_AGE ! age factor for diffuse visible snow albedo Yang97 eqn. 17
  real                ::   BATS_NIR_AGE ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
  real                ::   BATS_VIS_DIR ! cosz factor for direct visible snow albedo Yang97 eqn. 15
  real                ::   BATS_NIR_DIR ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
  real                ::   RSURF_SNOW   ! surface resistence for snow [s/m]
  real                ::   RSURF_EXP    ! exponent in the shape parameter for soil resistance option 1
  
  !--------------------!
  ! radiation parameters !
  !--------------------!
  real, dimension(8)    ::   ALBSAT_VIS    ! saturated soil VIS albedo per SOILCOLOR
  real, dimension(8)    ::   ALBSAT_NIR    ! saturated soil NIR albedo per SOILCOLOR
  real, dimension(8)    ::   ALBDRY_VIS    ! dry soil VIS albedo per SOILCOLOR
  real, dimension(8)    ::   ALBDRY_NIR    ! dry soil NIR albedo per SOILCOLOR
  real, dimension(8,2)  ::   ALBSAT_TABLE  ! saturated soil albedo table per SOILCOLOR (1=vis, 2=nir)
  real, dimension(8,2)  ::   ALBDRY_TABLE  ! dry soil albedo table per SOILCOLOR (1=vis, 2=nir)
  real, dimension(2)    ::   ALBICE        ! Land ice albedo (1=vis, 2=nir)
  real, dimension(2)    ::   ALBLAK        ! Lake ice albedo (1=vis, 2=nir)
  real, dimension(2)    ::   OMEGAS        ! two-stream parameter omega for snow (1=vis, 2=nir)
  real                  ::   BETADS        ! two-stream parameter betad for snow
  real                  ::   BETAIS        ! two-stream parameter betaI for snow
  real, dimension(2)    ::   EG            ! emissivity of land surface (1=soil,2=lake)
  
  !--------------------!
  !  land parameters   !
  !--------------------!
  integer       :: ISURBAN                   ! vegtype code for urban land cover
  integer       :: ISWATER                   ! vegtype code for water
  integer       :: ISBARREN                  ! vegtype code for barren land cover
  integer       :: ISICE                     ! vegtype code for ice/snow land cover
  integer       :: ISCROP                    ! vegtype code for crop land cover
  integer       :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
  integer       :: NATURAL                   ! vegtype code for cropland/grassland mosaic
  integer       :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
  integer       :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
  integer       :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial

  contains

    procedure, public  :: ReadNamelist         

end type namelist_type
     
contains   

  subroutine ReadNamelist(this, config_file)
  
    class(namelist_type) :: this
    
    character(len=*), intent (in) :: config_file
    
    integer       :: iz

    real              :: dt
    integer           :: maxtime
    character(len=12) :: startdate
    character(len=12) :: enddate
    character*256     :: input_filename
    character*256     :: output_filename
    real              :: lat
    real              :: lon
    real              :: preciprate
    integer           :: precip_duration
    integer           :: dry_duration
    logical           :: precipitating
    real              :: ZREF               ! measurement height for wind speed (m)
    
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
    real    :: initial_sh2o_value              ! constant sh2o value
    real    :: initial_sice_value              ! constant sice value
    real    :: initial_zwt                     ! constant water table depth [m]

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

    !--------------------!
    !  soil parameters   !
    !--------------------!

    real, dimension(12) ::      bb  ! b parameter
    real, dimension(12) ::   satdk  ! conductivity at saturation
    real, dimension(12) ::   satdw  ! diffusivity at saturation
    real, dimension(12) ::  maxsmc  ! porosity
    real, dimension(12) ::  satpsi  ! matric potential at saturation
    real, dimension(12) ::  wltsmc  ! wilting point
    real, dimension(12) ::  refsmc  ! field capacity
    real, dimension(12) :: pctsand  ! percent sand
    real, dimension(12) :: pctclay  ! percent clay
    real, dimension(12) ::   bvic   !VIC or DVIC model infiltration parameter
    real, dimension(12) ::   AXAJ   !Xinanjiang: Tension water distribution inflection parameter [-]
    real, dimension(12) ::   BXAJ   !Xinanjiang: Tension water distribution shape parameter [-]
    real, dimension(12) ::   XXAJ   !Xinanjiang: Free water distribution shape parameter [-]
    real, dimension(12) ::   G      !Mean Capillary Drive (m) for infiltration models
    real, dimension(12) ::   BBVIC  !DVIC heterogeniety parameter for infiltration 
    real, dimension(12) ::   QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
    real                ::   slope  ! free drainage parameter
    real                ::  refkdt  ! infiltration parameter for Schaake scheme
    real                ::   refdk  ! reference diffusivity for Schaake scheme
    real                ::   csoil  ! volumetric soil heat capacity [j/m3/K]
    real                ::   Z0     ! bare soil roughness length (m)
    real                ::   CZIL   ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
    real                ::   ZBOT   ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL

    !--------------------!
    !  Vegetation parameters   !
    !--------------------!
    real, dimension(20)     ::   LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(20)     ::   SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(20)     ::   CH2OP
    real, dimension(20)     ::   NROOT
    real, dimension(20)     ::   HVT            ! canopy top height (m)
    real, dimension(20)     ::   HVB            ! canopy bottom height (m)
    real, dimension(20)     ::   TMIN           ! minimum temperature for photosynthesis (k)
    real, dimension(20)     ::   SHDFAC         ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   SHDMAX         ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   Z0MVT          ! momentum roughness length (m)
    real, dimension(20)     ::   RC             ! tree crown radius (m)
    real, dimension(20)     ::   XL             ! leaf/stem orientation index
    real, dimension(20)     ::   BP             ! minimum leaf conductance (umol/m**2/s)
    real, dimension(20)     ::   FOLNMX         ! foliage nitrogen concentration when f(n)=1 (%)
    real, dimension(20)     ::   QE25           ! quantum efficiency at 25c (umol co2 / umol photon)
    real, dimension(20)     ::   VCMX25         ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real, dimension(20)     ::   MP             ! slope of conductance-to-photosynthesis relationship
    real, dimension(20)     ::   RGL            ! Parameter used in radiation stress function
    real, dimension(20)     ::   RSMIN          ! Minimum stomatal resistance [s m-1]
    real, dimension(20)     ::   HS             ! Parameter used in vapor pressure deficit function
    real, dimension(20)     ::   AKC            ! q10 for kc25
    real, dimension(20)     ::   AKO            ! q10 for ko25
    real, dimension(20)     ::   AVCMX          ! q10 for vcmx25
    real, dimension(20)     ::   RSMAX          ! Maximal stomatal resistance [s m-1]
    real                    ::   CWP            ! canopy wind absorption coefficient (formerly CWPVT)
    real                    ::   C3PSN          ! photosynth. pathway: 0. = c4, 1. = c3 
    real                    ::   DLEAF          ! characteristic leaf dimension (m)
    real                    ::   KC25           ! co2 michaelis-menten constant at 25c (pa)
    real                    ::   KO25           ! o2 michaelis-menten constant at 25c (pa)
    real, dimension(20)     ::   RHOL_VIS       ! leaf reflectance in visible
    real, dimension(20)     ::   RHOL_NIR       ! leaf reflectance in near infrared
    real, dimension(20)     ::   RHOS_VIS       ! stem reflectance in visible
    real, dimension(20)     ::   RHOS_NIR       ! stem reflectance in near infrared
    real, dimension(20)     ::   TAUL_VIS       ! leaf transmittance in visible
    real, dimension(20)     ::   TAUL_NIR       ! leaf transmittance in near infrared
    real, dimension(20)     ::   TAUS_VIS       ! stem transmittance in visible
    real, dimension(20)     ::   TAUS_NIR       ! stem transmittance in near infrared
    real, dimension(20,2)   ::   RHOL_TABLE     ! leaf reflectance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   RHOS_TABLE     ! stem reflectance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   TAUL_TABLE     ! leaf transmittance table (1 = vis, 2 = NIR)
    real, dimension(20,2)   ::   TAUS_TABLE     ! stem transmittance table (1 = vis, 2 = NIR)
    real, dimension(20,12)  ::   LAIM_TABLE     !monthly leaf area index, one-sided
    real, dimension(20,12)  ::   SAIM_TABLE     !monthly stem area index, one-sided
    
    !--------------------!
    !  snow parameters   !
    !--------------------!
    real                ::   SSI     ! liquid water holding capacity of snowpack (m3/m3)
    real                ::   MFSNO   ! fractional snow covered area (FSNO) curve parameter
    real                ::   Z0SNO   ! snow surface roughness length (m)
    real                ::   SWEMX        ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
    real                ::   TAU0         ! tau0 from Yang97 eqn. 10a
    real                ::   GRAIN_GROWTH ! growth from vapor diffusion Yang97 eqn. 10b
    real                ::   EXTRA_GROWTH ! extra growth near freezing Yang97 eqn. 10c
    real                ::   DIRT_SOOT    ! dirt and soot term Yang97 eqn. 10d
    real                ::   BATS_COSZ    ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real                ::   BATS_VIS_NEW ! new snow visible albedo
    real                ::   BATS_NIR_NEW ! new snow NIR albedo
    real                ::   BATS_VIS_AGE ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real                ::   BATS_NIR_AGE ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real                ::   BATS_VIS_DIR ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real                ::   BATS_NIR_DIR ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real                ::   RSURF_SNOW   ! surface resistence for snow [s/m]
    real                ::   RSURF_EXP    ! exponent in the shape parameter for soil resistance option 1
    
    !--------------------!
    ! radiation parameters !
    !--------------------!
    real, dimension(8)    ::   ALBSAT_VIS    ! saturated soil VIS albedo per SOILCOLOR
    real, dimension(8)    ::   ALBSAT_NIR    ! saturated soil NIR albedo per SOILCOLOR
    real, dimension(8)    ::   ALBDRY_VIS    ! dry soil VIS albedo per SOILCOLOR
    real, dimension(8)    ::   ALBDRY_NIR    ! dry soil NIR albedo per SOILCOLOR
    real, dimension(8,2)  ::   ALBSAT_TABLE  ! saturated soil albedo table per SOILCOLOR (1=vis, 2=nir)
    real, dimension(8,2)  ::   ALBDRY_TABLE  ! dry soil albedo table per SOILCOLOR (1=vis, 2=nir)
    real, dimension(2)    ::   ALBICE        ! Land ice albedo (1=vis, 2=nir)
    real, dimension(2)    ::   ALBLAK        ! Lake ice albedo (1=vis, 2=nir)
    real, dimension(2)    ::   OMEGAS        ! two-stream parameter omega for snow (1=vis, 2=nir)
    real                  ::   BETADS        ! two-stream parameter betad for snow
    real                  ::   BETAIS        ! two-stream parameter betaI for snow
    real, dimension(2)    ::   EG            ! emissivity of land surface (1=soil,2=lake)
    
    !--------------------!
    !  land parameters   !
    !--------------------!
    integer       :: ISURBAN                   ! vegtype code for urban land cover
    integer       :: ISWATER                   ! vegtype code for water
    integer       :: ISBARREN                  ! vegtype code for barren land cover
    integer       :: ISICE                     ! vegtype code for ice/snow land cover
    integer       :: ISCROP                    ! vegtype code for crop land cover
    integer       :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
    integer       :: NATURAL                   ! vegtype code for cropland/grassland mosaic
    integer       :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
    integer       :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
    integer       :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial

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
    namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o,zwt
    namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                                 initial_sice_value,initial_zwt
    namelist / soil_parameters / bb,satdk,satdw,maxsmc,satpsi,wltsmc, &
                                 refsmc,pctsand,pctclay,bvic,AXAJ,BXAJ,XXAJ,&
                                 BBVIC,G,QUARTZ,slope,refkdt,refdk,CSOIL,Z0,CZIL,ZBOT
    namelist / snow_parameters / SSI,MFSNO,Z0SNO,SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
                                 BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
                                 RSURF_SNOW,RSURF_EXP
    namelist / veg_parameters  / CH2OP,NROOT,HVT,HVB,TMIN,SHDFAC,SHDMAX,Z0MVT,RC,XL,&
                                 BP,FOLNMX,QE25,VCMX25,MP,RGL,RSMIN,HS,AKC,AKO,AVCMX,RSMAX,&
                                 CWP,C3PSN,DLEAF,KC25,KO25,&
                                 RHOL_VIS,RHOL_NIR,RHOS_VIS,RHOS_NIR,TAUL_VIS,TAUL_NIR,TAUS_VIS,TAUS_NIR,&
                                 LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN,&
                                 LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC,&
                                 SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN,&
                                 SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    namelist / radiation_parameters / ALBSAT_VIS,ALBSAT_NIR,ALBDRY_VIS,ALBDRY_NIR,ALBICE,ALBLAK,OMEGAS,BETADS,BETAIS,EG
    namelist / land_parameters / ISURBAN,ISWATER,ISBARREN,ISICE,ISCROP,EBLFOREST,NATURAL,LOW_DENSITY_RESIDENTIAL,&
                                 HIGH_DENSITY_RESIDENTIAL,HIGH_INTENSITY_INDUSTRIAL
 
!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file= config_file, form="formatted")
     read(30, timing)
     read(30, location)
     read(30, forcing)
     read(30, model_options)
     read(30, structure)
     read(30, uniform_initial)
     read(30, soil_parameters)
     read(30, snow_parameters)
     read(30, veg_parameters)
     read(30, radiation_parameters)
     read(30, land_parameters)
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

    this%bb      = bb
    this%satdk   = satdk
    this%satdw   = satdw
    this%maxsmc  = maxsmc
    this%satpsi  = satpsi
    this%wltsmc  = wltsmc
    this%refsmc  = refsmc
    this%pctsand = pctsand
    this%pctclay = pctclay
    this%bvic    = bvic
    this%AXAJ    = AXAJ
    this%BXAJ    = BXAJ
    this%XXAJ    = XXAJ
    this%BBVIC   = BBVIC
    this%G       = G
    this%QUARTZ  = QUARTZ
    this%slope   = slope
    this%refkdt  = refkdt
    this%refdk   = refdk
    this%csoil   = csoil
    this%Z0      = Z0
    this%CZIL    = CZIL
    this%ZBOT    = ZBOT
    
    this%RHOL_TABLE(1:20, 1) = RHOL_VIS(1:20)
    this%RHOL_TABLE(1:20, 2) = RHOL_NIR(1:20)
    this%RHOS_TABLE(1:20, 1) = RHOS_VIS(1:20)
    this%RHOS_TABLE(1:20, 2) = RHOS_NIR(1:20)
    this%TAUL_TABLE(1:20, 1) = TAUL_VIS(1:20)
    this%TAUL_TABLE(1:20, 2) = TAUL_NIR(1:20)
    this%TAUS_TABLE(1:20, 1) = TAUS_VIS(1:20)
    this%TAUS_TABLE(1:20, 2) = TAUS_NIR(1:20)
        
    this%LAIM_TABLE(1:20, 1) = LAI_JAN(1:20)
    this%LAIM_TABLE(1:20, 2) = LAI_FEB(1:20)
    this%LAIM_TABLE(1:20, 3) = LAI_MAR(1:20)
    this%LAIM_TABLE(1:20, 4) = LAI_APR(1:20)
    this%LAIM_TABLE(1:20, 5) = LAI_MAY(1:20)
    this%LAIM_TABLE(1:20, 6) = LAI_JUN(1:20)
    this%LAIM_TABLE(1:20, 7) = LAI_JUL(1:20)
    this%LAIM_TABLE(1:20, 8) = LAI_AUG(1:20)
    this%LAIM_TABLE(1:20, 9) = LAI_SEP(1:20)
    this%LAIM_TABLE(1:20,10) = LAI_OCT(1:20)
    this%LAIM_TABLE(1:20,11) = LAI_NOV(1:20)
    this%LAIM_TABLE(1:20,12) = LAI_DEC(1:20)

    this%SAIM_TABLE(1:20, 1) = SAI_JAN(1:20)
    this%SAIM_TABLE(1:20, 2) = SAI_FEB(1:20)
    this%SAIM_TABLE(1:20, 3) = SAI_MAR(1:20)
    this%SAIM_TABLE(1:20, 4) = SAI_APR(1:20)
    this%SAIM_TABLE(1:20, 5) = SAI_MAY(1:20)
    this%SAIM_TABLE(1:20, 6) = SAI_JUN(1:20)
    this%SAIM_TABLE(1:20, 7) = SAI_JUL(1:20)
    this%SAIM_TABLE(1:20, 8) = SAI_AUG(1:20)
    this%SAIM_TABLE(1:20, 9) = SAI_SEP(1:20)
    this%SAIM_TABLE(1:20,10) = SAI_OCT(1:20)
    this%SAIM_TABLE(1:20,11) = SAI_NOV(1:20)
    this%SAIM_TABLE(1:20,12) = SAI_DEC(1:20)

    this%CH2OP   = CH2OP
    this%NROOT   = NROOT
    this%HVT     = HVT
    this%HVB     = HVB
    this%TMIN    = TMIN
    this%SHDFAC  = SHDFAC
    this%SHDMAX  = SHDMAX
    this%Z0MVT   = Z0MVT
    this%RC      = RC
    this%XL      = XL
    this%BP      = BP
    this%FOLNMX  = FOLNMX
    this%QE25    = QE25
    this%VCMX25  = VCMX25
    this%MP      = MP
    this%RGL     = RGL
    this%RSMIN   = RSMIN
    this%HS      = HS
    this%AKC     = AKC
    this%AKO     = AKO
    this%AVCMX   = AVCMX
    this%RSMAX   = RSMAX
    this%CWP     = CWP
    this%C3PSN   = C3PSN
    this%DLEAF   = DLEAF
    this%KC25    = KC25
    this%KO25    = KO25

    this%SSI          = SSI
    this%MFSNO        = MFSNO
    this%Z0SNO        = Z0SNO
    this%SWEMX        = SWEMX
    this%TAU0         = TAU0
    this%GRAIN_GROWTH = GRAIN_GROWTH
    this%EXTRA_GROWTH = EXTRA_GROWTH
    this%DIRT_SOOT    = DIRT_SOOT
    this%BATS_COSZ    = BATS_COSZ
    this%BATS_VIS_NEW = BATS_VIS_NEW
    this%BATS_NIR_NEW = BATS_NIR_NEW
    this%BATS_VIS_AGE = BATS_VIS_AGE
    this%BATS_NIR_AGE = BATS_NIR_AGE
    this%BATS_VIS_DIR = BATS_VIS_DIR
    this%BATS_NIR_DIR = BATS_NIR_DIR
    this%RSURF_SNOW   = RSURF_SNOW
    this%RSURF_EXP    = RSURF_EXP
    
    this%ALBSAT_TABLE(1:8, 1) = ALBSAT_VIS(1:8)
    this%ALBSAT_TABLE(1:8, 2) = ALBSAT_NIR(1:8)
    this%ALBDRY_TABLE(1:8, 1) = ALBDRY_VIS(1:8)
    this%ALBDRY_TABLE(1:8, 2) = ALBDRY_NIR(1:8)
    this%ALBICE               = ALBICE
    this%ALBLAK               = ALBLAK
    this%OMEGAS               = OMEGAS
    this%BETADS               = BETADS
    this%BETAIS               = BETAIS
    this%EG                   = EG
    
    this%ISURBAN                   = ISURBAN
    this%ISWATER                   = ISWATER
    this%ISBARREN                  = ISBARREN
    this%ISICE                     = ISICE
    this%ISCROP                    = ISCROP
    this%EBLFOREST                 = EBLFOREST
    this%NATURAL                   = NATURAL
    this%LOW_DENSITY_RESIDENTIAL   = LOW_DENSITY_RESIDENTIAL
    this%HIGH_DENSITY_RESIDENTIAL  = HIGH_DENSITY_RESIDENTIAL
    this%HIGH_INTENSITY_INDUSTRIAL = HIGH_INTENSITY_INDUSTRIAL
    

  end subroutine ReadNamelist

end module NamelistRead

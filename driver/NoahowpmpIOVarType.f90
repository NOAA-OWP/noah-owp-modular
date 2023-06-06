module NoahowpmpIOVarType

  implicit none
  type, public :: NoahowpmpIO_type

  !--------------------------------------------------------------------------------------------------------------
  ! Time
  !--------------------------------------------------------------------------------------------------------------
  real                                              :: DT                ! run timestep (s)
  character(len=12)                                 :: startdate         ! Start date of the model run ( YYYYMMDDHHmm ) 
  character(len=12)                                 :: enddate           ! End date of the model run ( YYYYMMDDHHmm ) 
  character(len=12)                                 :: nowdate           ! Current date of the model run ( YYYYMMDDHHmm ) 
  real*8                                            :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8                                            :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8                                            :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8, allocatable                               :: sim_datetimes (:) ! vector of unix sim times given start/end dates and dt (try 'ki8' type)
  integer                                           :: itime             ! current integer time step of model run
  integer                                           :: ntime             ! total number of integer time steps in model run
  double precision                                  :: time_dbl          ! current time of model run in seconds from beginning

  !--------------------------------------------------------------------------------------------------------------
  ! DomainType
  !--------------------------------------------------------------------------------------------------------------

  integer                                           :: ix                !
  integer                                           :: iy                !
  integer                                           :: nx                !
  integer                                           :: ny                !
  real, allocatable, dimension(:,:)                 :: lat               ! latitude (°)
  real, allocatable, dimension(:,:)                 :: lon               ! longitude (°)
  real, allocatable, dimension(:,:)                 :: ZREF              ! measurement height of wind speed (m)
  real, allocatable, dimension(:,:)                 :: terrain_slope     ! terrain slope (°)
  real, allocatable, dimension(:,:)                 :: azimuth           ! terrain azimuth or aspect (° clockwise from north)
  integer, allocatable, dimension(:,:)              :: vegtyp            ! land cover type
  integer, allocatable, dimension(:,:)              :: croptype          ! crop type
  integer, allocatable, dimension(:,:)              :: isltyp            ! soil type
  integer, allocatable, dimension(:,:)              :: IST               ! surface type 1-soil; 2-lake
  real, allocatable, dimension(:,:,:)               :: zsoil             ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:,:,:)               :: dzsnso            ! snow/soil layer thickness [m]
  real, allocatable, dimension(:,:,:)               :: zsnso             ! depth of snow/soil layer-bottom

  !--------------------------------------------------------------------------------------------------------------
  ! EnergyType
  !--------------------------------------------------------------------------------------------------------------

  real,allocatable,dimension(:,:)                   :: TV                ! vegetation temperature (k)
  real,allocatable,dimension(:,:)                   :: TG                ! ground temperature (k)
  real,allocatable,dimension(:,:)                   :: FCEV              ! canopy evaporation energy flux (w/m2)
  real,allocatable,dimension(:,:)                   :: FCTR              ! canopy transpiration energy flux (w/m2)
  real,allocatable,dimension(:,:)                   :: IGS               ! growing season index (0=off, 1=on)
  logical,allocatable,dimension(:,:)                :: FROZEN_CANOPY     ! binary frozen canopy status (true when TV <= parameters%TFRZ)
  logical,allocatable,dimension(:,:)                :: FROZEN_GROUND     ! binary frozen ground status (true when TG <= parameters%TFRZ)
  integer, allocatable, dimension(:,:,:)            :: IMELT             ! snow layer melting state index [0-no melt;1-melt]
  real,allocatable, dimension(:,:,:)                :: STC               ! snow/soil layer temperature [k]
  real,allocatable, dimension(:,:,:)                :: DF                ! snow and soil layer thermal conductivity [w/m/k]
  real,allocatable, dimension(:,:,:)                :: HCPCT             ! snow and soil layer heat capacity [j/m3/k]
  real,allocatable, dimension(:,:,:)                :: FACT              ! temporary variable used in phase change [s/j/m2/k]
  
  ! Heat advected by precipitation
  real,allocatable,dimension(:,:)                   :: PAHV              ! precipitation advected heat - vegetation net (W/m2)
  real,allocatable,dimension(:,:)                   :: PAHG              ! precipitation advected heat - under canopy net (W/m2)
  real,allocatable,dimension(:,:)                   :: PAHB              ! precipitation advected heat - bare ground net (W/m2)
  REAL,allocatable,dimension(:,:)                   :: PAH               ! precipitation advected heat - total (W/m2)
  
  ! Albedo
  real,allocatable,dimension(:,:)                   :: TAUSS             ! non-dimensional snow age
  real,allocatable,dimension(:,:)                   :: FAGE              ! snow age (0 = new snow)
  real,allocatable,dimension(:,:)                   :: ALB               ! broadband albedo in CLASS scheme
  real,allocatable,dimension(:,:)                   :: ALBOLD            ! broadband albedo at previous timestep 
  
  real, allocatable, dimension(:,:,:)               :: ALBD              ! surface albedo (direct)
  real, allocatable, dimension(:,:,:)               :: ALBI              ! surface albedo (diffuse)
  real, allocatable, dimension(:,:,:)               :: ALBGRD            ! ground albedo (direct)
  real, allocatable, dimension(:,:,:)               :: ALBGRI            ! ground albedo (diffuse)
  real, allocatable, dimension(:,:,:)               :: ALBSND            ! snow albedo for direct(1=vis, 2=nir)
  real, allocatable, dimension(:,:,:)               :: ALBSNI            ! snow albedo for diffuse
  
  real, allocatable, dimension(:,:,:)               :: FABD   ! flux abs by veg (per unit direct flux)
  real, allocatable, dimension(:,:,:)               :: FABI   ! flux abs by veg (per unit diffuse flux)
  real, allocatable, dimension(:,:,:)               :: FTDD   ! down direct flux below veg (per unit dir flux)
  real, allocatable, dimension(:,:,:)               :: FTDI   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:,:,:)               :: FTID   ! down diffuse flux below veg (per unit dir flux)
  real, allocatable, dimension(:,:,:)               :: FTII   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:,:,:)               :: FREVD  ! direct flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREVI  ! diffuse flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREGD  ! direct flux reflected by ground (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREGI  ! direct flux reflected by ground (per unit incoming flux)
  real, allocatable, dimension(:,:,:)               :: RHO    ! leaf/stem reflectance weighted by fraction LAI and SAI
  real, allocatable, dimension(:,:,:)               :: TAU    ! leaf/stem transmittance weighted by fraction LAI and SAI

  
  ! Shortwave radiation
  real,allocatable,dimension(:,:)                   :: COSZ    ! cosine solar zenith angle [0-1]
  real,allocatable,dimension(:,:)                   :: COSZ_HORIZ ! cosine solar zenith angle for flat ground [0-1]
  real,allocatable,dimension(:,:)                   :: BGAP    ! between canopy gap fraction for beam (-)
  real,allocatable,dimension(:,:)                   :: WGAP    ! within canopy gap fraction for beam (-)
  REAL,allocatable,dimension(:,:)                   :: FSUN    ! sunlit fraction of canopy (-)
  REAL,allocatable,dimension(:,:)                   :: FSHA    ! shaded fraction of canopy (-)
  REAL,allocatable,dimension(:,:)                   :: LAISUN  ! sunlit leaf area (-)
  REAL,allocatable,dimension(:,:)                   :: LAISHA  ! shaded leaf area (-)
  REAL,allocatable,dimension(:,:)                   :: PARSUN  ! average absorbed par for sunlit leaves (w/m2)
  REAL,allocatable,dimension(:,:)                   :: PARSHA  ! average absorbed par for shaded leaves (w/m2)
  REAL,allocatable,dimension(:,:)                   :: SAV     ! solar radiation absorbed by vegetation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: SAG     ! solar radiation absorbed by ground (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSA     ! total absorbed solar radiation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSR     ! total reflected solar radiation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSRV    ! reflected solar radiation by vegetation
  REAL,allocatable,dimension(:,:)                   :: FSRG    ! reflected solar radiation by ground

  ! Other, uncategorized
  REAL,allocatable,dimension(:,:)                   :: TAH     ! canopy air tmeperature (K)
  REAL,allocatable,dimension(:,:)                   :: EAH     ! canopy water vapor pressure (Pa)  
  REAL,allocatable,dimension(:,:)                   :: ZPD     ! zero plane displacement, ground (m)
  REAL,allocatable,dimension(:,:)                   :: Z0MG    ! z0 momentum, ground (m)
  REAL,allocatable,dimension(:,:)                   :: Z0M     ! roughness length, momentum (m)
  REAL,allocatable,dimension(:,:)                   :: ZLVL    ! reference height (m)  
  REAL,allocatable,dimension(:,:)                   :: CMV     ! momentum drag coefficient (vegetated surface)  
  REAL,allocatable,dimension(:,:)                   :: CMB     ! momentum drag coefficient (bare ground)  
  REAL,allocatable,dimension(:,:)                   :: CM      ! momentum drag coefficient (weighted version of CMV + CMB by FVEG)  
  REAL,allocatable,dimension(:,:)                   :: CH      ! drag coefficient for heat
  REAL,allocatable,dimension(:,:)                   :: TGB     ! ground temperature (K)
  REAL,allocatable,dimension(:,:)                   :: QSFC    ! mixing ratio at lowest model layer (g/g)
  REAL,allocatable,dimension(:,:)                   :: EMV     ! vegetation emissivity (-)
  REAL,allocatable,dimension(:,:)                   :: EMG     ! ground emissivity (-)
  REAL,allocatable,dimension(:,:)                   :: GAMMAV  ! psychrometric constant (Pa/K)
  REAL,allocatable,dimension(:,:)                   :: GAMMAG  ! psychrometric constant (Pa/K)  
!  REAL                                             :: GAMMA   ! psychrometric constant (Pa/K)  NOT USED IN CURRENT VERSION
  REAL,allocatable,dimension(:,:)                   :: EVC     ! evaporation heat flux (w/m2)  [+= to atm]  
  REAL,allocatable,dimension(:,:)                   :: IRC     ! net longwave radiation (w/m2) [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: IRG     ! net longwave radiation (w/m2) [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHC     ! sensible heat flux (w/m2)     [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHG     ! sensible heat flux (w/m2)     [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHB     ! sensible heat flux (w/m2) [+ to atm] 
  REAL,allocatable,dimension(:,:)                   :: EVG     ! evaporation heat flux (w/m2)  [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: EVB     ! latent heat flux (w/m2)   [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: TR      ! transpiration heat flux (w/m2)[+= to atm]
  REAL,allocatable,dimension(:,:)                   :: GH      ! ground heat (w/m2) [+ = to soil]
  REAL,allocatable,dimension(:,:)                   :: GHB     ! ground heat flux (w/m2)  [+ to soil] 
  REAL,allocatable,dimension(:,:)                   :: GHV     ! ground heat flux [w/m2]  [+ to soil]
  REAL,allocatable,dimension(:,:)                   :: T2MV    ! 2 m height air temperature (k)
  REAL,allocatable,dimension(:,:)                   :: CHLEAF  ! leaf exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: CHUC    ! under canopy exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: CHV2    ! sensible heat exch. coeff. over vegetated fraction (m/s)  
  REAL,allocatable,dimension(:,:)                   :: CHB2    ! sensible heat exch. coeff. bare-ground (m/s)
  REAL,allocatable,dimension(:,:)                   :: Q2V     ! check
  REAL,allocatable,dimension(:,:)                   :: LATHEAV ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL,allocatable,dimension(:,:)                   :: LATHEAG ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL,allocatable,dimension(:,:)                   :: LATHEA  ! latent heat of vaporization/subli (j/kg) (bare ground version)
  REAL,allocatable,dimension(:,:)                   :: RSURF   ! ground surface resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RHSUR   ! relative humidity in surface soil/snow air space (-)  
  REAL,allocatable,dimension(:,:)                   :: TAUXV   ! wind stress: e-w (n/m2) vegetation
  REAL,allocatable,dimension(:,:)                   :: TAUYV   ! wind stress: n-s (n/m2) vegetation
  REAL,allocatable,dimension(:,:)                   :: TAUXB   ! wind stress: e-w (n/m2) bare ground
  REAL,allocatable,dimension(:,:)                   :: TAUYB   ! wind stress: n-s (n/m2) bare ground
  REAL,allocatable,dimension(:,:)                   :: TAUX    ! wind stress: e-w (n/m2)
  REAL,allocatable,dimension(:,:)                   :: TAUY    ! wind stress: n-s (n/m2)  
  REAL,allocatable,dimension(:,:)                   :: CAH2    ! sensible heat conductance for diagnostics
  REAL,allocatable,dimension(:,:)                   :: EHB2    ! sensible heat conductance for diagnostics (bare ground)
  REAL,allocatable,dimension(:,:)                   :: T2MB    ! 2 m height air temperature (K)  
  REAL,allocatable,dimension(:,:)                   :: Q2B     ! bare ground heat conductance
  REAL,allocatable,dimension(:,:)                   :: TGV     ! ground surface temp. [k]
  REAL,allocatable,dimension(:,:)                   :: CHV     ! sensible heat exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: RSSUN   ! sunlit leaf stomatal resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RSSHA   ! shaded leaf stomatal resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RB      ! leaf boundary layer resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: FIRA    ! total net LW. rad (w/m2)   [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: FSH     ! total sensible heat (w/m2) [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: FGEV    ! ground evaporation (w/m2)  [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: TRAD    ! radiative temperature (k)
  REAL,allocatable,dimension(:,:)                   :: IRB     ! net longwave rad. [w/m2] [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: SSOIL   ! ground heat flux (w/m2)   [+ to soil]
  REAL,allocatable,dimension(:,:)                   :: T2M     ! 2-meter air temperature (k)
  REAL,allocatable,dimension(:,:)                   :: TS      ! surface temperature (k)
  REAL,allocatable,dimension(:,:)                   :: CHB     ! sensible heat exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: Q1
  REAL,allocatable,dimension(:,:)                   :: Q2E
  REAL,allocatable,dimension(:,:)                   :: Z0WRF   ! combined z0 sent to coupled model
  REAL,allocatable,dimension(:,:)                   :: EMISSI  ! net surface emissivity  
  REAL,allocatable,dimension(:,:)                   :: PSN     ! total photosyn. (umolco2/m2/s) [+]
  REAL ,allocatable,dimension(:,:)                  :: PSNSUN  ! sunlit photosynthesis (umolco2/m2/s)  
  REAL,allocatable,dimension(:,:)                   :: PSNSHA  ! shaded photosynthesis (umolco2/m2/s)    
  REAL,allocatable,dimension(:,:)                   :: APAR    ! total photosyn. active energy (w/m2)
  REAL,allocatable,dimension(:,:)                   :: QMELT   ! snowmelt [mm/s]
  
  REAL,allocatable,dimension(:,:)                   :: LH      ! latent heat (total) flux [W m-2]
  REAL,allocatable,dimension(:,:)                   :: TGS     ! ground surface temperature (K, takes value of TG when SNOWH <= 0.05 and STC[0] when SNOWH > 0.05) 
                                  ! this is a temporary fix for when coupled to subsurface modules
                                  ! TODO: further investigation
  integer,allocatable,dimension(:,:)                :: ICE     ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
   
  !--------------------------------------------------------------------------------------------------------------
  ! ForcingType
  !--------------------------------------------------------------------------------------------------------------
  
  ! atmospheric inputs (meteorology, chemistry)
  real,allocatable,dimension(:,:)                   :: SFCPRS       ! surface pressure (pa)
  real,allocatable,dimension(:,:)                   :: SFCTMP       ! surface air temperature [K]
  real,allocatable,dimension(:,:)                   :: Q2           ! specific humidity (note: in some Noah-MP versions Q2 is mixing ratio)
  real,allocatable,dimension(:,:)                   :: PRCP         ! total input precipitation[mm/s]
  real,allocatable,dimension(:,:)                   :: PRCPCONV     ! convective precipitation entering  [mm/s]
  real,allocatable,dimension(:,:)                   :: PRCPNONC     ! non-convective precipitation entering [mm/s]
  real,allocatable,dimension(:,:)                   :: PRCPSHCV     ! shallow convective precip entering  [mm/s]
  real,allocatable,dimension(:,:)                   :: PRCPSNOW     ! snow entering land model [mm/s] 
  real,allocatable,dimension(:,:)                   :: PRCPGRPL     ! graupel entering land model [mm/s]
  real,allocatable,dimension(:,:)                   :: PRCPHAIL     ! hail entering land model [mm/s]             
  real,allocatable,dimension(:,:)                   :: SOLDN        ! downward shortwave radiation (w/m2)
  real,allocatable,dimension(:,:)                   :: LWDN         ! atmospheric longwave radiation (w/m2)
  real,allocatable,dimension(:,:)                   :: FOLN         ! foliage nitrogen concentration (%)
  real,allocatable,dimension(:,:)                   :: O2PP         ! atmospheric co2 concentration partial pressure (pa)
  real,allocatable,dimension(:,:)                   :: CO2PP        ! atmospheric o2 concentration partial pressure (pa) 
  real,allocatable,dimension(:,:)                   :: UU           ! wind speed in eastward dir (m/s)  
  real,allocatable,dimension(:,:)                   :: VV           ! wind speed in northward dir (m/s)  
  
  ! surface inputs
  real,allocatable,dimension(:,:)                   :: TBOT         ! bottom condition for soil temperature [K]

  ! outputs
  real,allocatable,dimension(:,:)                   :: UR           ! wind speed at reference height (m/s)
  real,allocatable,dimension(:,:)                   :: THAIR        ! potential temperature (k)
  real,allocatable,dimension(:,:)                   :: QAIR         ! specific humidity (kg/kg) (q2/(1+q2))
  real,allocatable,dimension(:,:)                   :: EAIR         ! vapor pressure air (pa)
  real,allocatable,dimension(:,:)                   :: RHOAIR       ! density air (kg/m3)
  real,allocatable,dimension(:,:)                   :: FPICE        ! fraction of ice in precipitation (-)
  real,allocatable,dimension(:,:)                   :: SWDOWN       ! downward solar filtered by sun angle [w/m2]
  real                                              :: JULIAN       ! julian day of year
  integer                                           :: YEARLEN      ! year length (days)
               
  real, allocatable, dimension(:,:,:)               :: SOLAD  !incoming direct solar radiation (w/m2)
  real, allocatable, dimension(:,:,:)               :: SOLAI  !incoming diffuse solar radiation (w/m2)

  !--------------------------------------------------------------------------------------------------------------
  ! LevelsType
  !--------------------------------------------------------------------------------------------------------------

  integer,allocatable,dimension(:,:)                :: nsoil  ! number of soil layers
  integer,allocatable,dimension(:,:)                :: nsnow  ! number of snow layers
  integer,allocatable,dimension(:,:)                :: nveg   ! number of vegetation types in chosen table

  !--------------------------------------------------------------------------------------------------------------
  ! OptionsType
  !--------------------------------------------------------------------------------------------------------------


  integer,allocatable,dimension(:,:)                :: opt_snf    ! precip_phase_option: options for determining precipitation phase
  integer,allocatable,dimension(:,:)                :: opt_run    ! runoff_option: options for runoff
  integer,allocatable,dimension(:,:)                :: opt_drn    ! drainage_option
  integer,allocatable,dimension(:,:)                :: opt_inf    ! frozen_soil_option
  integer,allocatable,dimension(:,:)                :: opt_infdv  ! dynamic_vic_option
  integer,allocatable,dimension(:,:)                :: dveg       ! dynamic_veg_option
  integer,allocatable,dimension(:,:)                :: opt_alb    ! snow_albedo_option
  integer,allocatable,dimension(:,:)                :: opt_rad    ! radiative_transfer_option
  integer,allocatable,dimension(:,:)                :: opt_sfc    ! sfc_drag_coeff_option
  integer,allocatable,dimension(:,:)                :: opt_crs    ! canopy_stom_resist_option
  integer,allocatable,dimension(:,:)                :: opt_crop   ! crop_model_option
  integer,allocatable,dimension(:,:)                :: opt_stc    ! snowsoil_temp_time_option
  integer,allocatable,dimension(:,:)                :: opt_tbot   ! soil_temp_boundary_option
  integer,allocatable,dimension(:,:)                :: opt_frz    ! supercooled_water_option
  integer,allocatable,dimension(:,:)                :: opt_btr    ! stomatal_resistance_option
  integer,allocatable,dimension(:,:)                :: opt_rsf    ! evap_srfc_resistance_option
  integer,allocatable,dimension(:,:)                :: opt_sub    ! subsurface_option

  !--------------------------------------------------------------------------------------------------------------
  ! ParametersType
  !--------------------------------------------------------------------------------------------------------------

  real, allocatable, dimension(:,:,:)               :: bexp                      ! b parameter
  real, allocatable, dimension(:,:,:)               :: smcmax                    ! porosity (volumetric)
  real, allocatable, dimension(:,:,:)               :: smcwlt                    ! wilting point
  real, allocatable, dimension(:,:,:)               :: smcref                    ! field capacity
  real, allocatable, dimension(:,:,:)               :: dksat                     ! saturated conductivity
  real, allocatable, dimension(:,:,:)               :: dwsat                     ! saturated diffusivity
  real, allocatable, dimension(:,:,:)               :: psisat                    ! saturated matric potential
  real ,allocatable,dimension(:,:)                  :: bvic                      ! VIC or DVIC model infiltration parameter
  real,allocatable,dimension(:,:)                   :: AXAJ                      ! Xinanjiang: Tension water distribution inflection parameter [-]
  real,allocatable,dimension(:,:)                   :: BXAJ                      ! Xinanjiang: Tension water distribution shape parameter [-]
  real,allocatable,dimension(:,:)                   :: XXAJ                      ! Xinanjiang: Free water distribution shape parameter [-]
  real,allocatable,dimension(:,:)                   :: BBVIC                     ! DVIC heterogeniety parameter for infiltration
  real,allocatable,dimension(:,:)                   :: G                         ! Mean Capillary Drive (m) for infiltration models
  real,allocatable,dimension(:,:)                   :: QUARTZ                    ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
  real,allocatable,dimension(:,:)                   :: kdt                       !
  real                                              :: refkdt                    !
  real                                              :: refdk                     !
  real                                              :: csoil                     ! volumetric soil heat capacity [j/m3/K]
  real                                              :: Z0                        ! bare soil roughness length (m)
  real                                              :: CZIL                      ! Parameter used in the calculation of the roughness length for heat, originally in GENPARM.TBL
  real                                              :: ZBOT                      ! Depth (m) of lower boundary soil temperature, originally in GENPARM.TBL
  real,allocatable,dimension(:,:)                   :: frzx                      !
  real                                              :: slope                     ! drainage parameter
  real                                              :: timean
  real                                              :: fsatmx
  real,allocatable,dimension(:,:)                   :: ZWT_INIT                  ! initial water table depth below surface [m]
  logical,allocatable,dimension(:,:)                :: urban_flag
  real,allocatable,dimension(:,:,:)                 :: LAIM                      ! monthly LAI
  real,allocatable,dimension(:,:,:)                 :: SAIM                      ! monthly SAI
  real,allocatable,dimension(:,:)                   :: LAI
  real,allocatable,dimension(:,:)                   :: SAI
  real,allocatable,dimension(:,:)                   :: CH2OP                     ! maximum intercepted h2o per unit lai+sai (mm)
  integer,allocatable,dimension(:,:)                :: NROOT                     ! vegetation root level
  real,allocatable,dimension(:,:)                   :: HVT                       ! canopy top height (m)
  real,allocatable,dimension(:,:)                   :: HVB                       ! canopy bottom height (m)
  real,allocatable,dimension(:,:)                   :: TMIN                      ! minimum temperature for photosynthesis (k)
  real,allocatable,dimension(:,:)                   :: SHDFAC                    ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real,allocatable,dimension(:,:)                   :: SHDMAX                    ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real,allocatable,dimension(:,:)                   :: Z0MVT                     ! momentum roughness length (m)
  real,allocatable,dimension(:,:)                   :: RC                        ! tree crown radius (m)
  real,allocatable,dimension(:,:)                   :: XL                        ! leaf/stem orientation index
  real,allocatable,dimension(:,:)                   :: BP                        ! minimum leaf conductance (umol/m**2/s)
  real,allocatable,dimension(:,:)                   :: FOLNMX                    ! foliage nitrogen concentration when f(n)=1 (%)
  real,allocatable,dimension(:,:)                   :: QE25                      ! quantum efficiency at 25c (umol co2 / umol photon)
  real,allocatable,dimension(:,:)                   :: VCMX25                    ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
  real,allocatable,dimension(:,:)                   :: MP                        ! slope of conductance-to-photosynthesis relationship
  real,allocatable,dimension(:,:)                   :: RGL                       ! Parameter used in radiation stress function
  real,allocatable,dimension(:,:)                   :: RSMIN                     ! Minimum stomatal resistance [s m-1]
  real,allocatable,dimension(:,:)                   :: HS                        ! Parameter used in vapor pressure deficit function
  real,allocatable,dimension(:,:)                   :: AKC                       ! q10 for kc25
  real,allocatable,dimension(:,:)                   :: AKO                       ! q10 for ko25
  real,allocatable,dimension(:,:)                   :: AVCMX                     ! q10 for vcmx25
  real,allocatable,dimension(:,:)                   :: RSMAX                     ! Maximal stomatal resistance [s m-1]
  real,allocatable,dimension(:,:)                   :: CWP                       ! canopy wind absorption coefficient (formerly CWPVT)
  real,allocatable,dimension(:,:)                   :: C3PSN                     ! photosynth. pathway: 0. = c4, 1. = c3 [by vegtype]
  real,allocatable,dimension(:,:)                   :: DLEAF                     ! characteristic leaf dimension (m)
  real,allocatable,dimension(:,:)                   :: KC25                      ! co2 michaelis-menten constant at 25c (pa)
  real,allocatable,dimension(:,:)                   :: KO25                      ! o2 michaelis-menten constant at 25c (pa)
  real,allocatable,dimension(:,:)                   :: ELAI
  real,allocatable,dimension(:,:)                   :: ESAI
  real,allocatable,dimension(:,:)                   :: VAI                       ! sum of ELAI + ESAI
  logical,allocatable,dimension(:,:)                :: VEG                       ! grid cell is vegetated (true) or not (false)
  real,allocatable,dimension(:,:)                   :: FVEG                      ! vegetation fraction
  real,allocatable,dimension(:,:,:)                 :: RHOL                      ! leaf reflectance (1 = vis, 2 = NIR)
  real,allocatable,dimension(:,:,:)                 :: RHOS                      ! stem reflectance (1 = vis, 2 = NIR)
  real,allocatable,dimension(:,:,:)                 :: TAUL                      ! leaf transmittance (1 = vis, 2 = NIR)
  real,allocatable,dimension(:,:,:)                 :: TAUS                      ! stem transmittance (1 = vis, 2 = NIR)
  integer                                           :: ISURBAN                   ! vegtype code for urban land cover
  integer                                           :: ISWATER                   ! vegtype code for water
  integer                                           :: ISBARREN                  ! vegtype code for barren land cover
  integer                                           :: ISICE                     ! vegtype code for ice/snow land cover
  integer                                           :: ISCROP                    ! vegtype code for crop land cover
  integer                                           :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
  integer                                           :: NATURAL                   ! vegtype code for cropland/grassland mosaic
  integer                                           :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
  integer                                           :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
  integer                                           :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial
  real                                              :: SB                        ! Stefan-Boltzmann constant (w/m2/k4)
  real                                              :: VKC                       ! von Karman constant
  real                                              :: TFRZ                      ! freezing/melting point (k)
  real                                              :: HSUB                      ! latent heat of sublimation (j/kg)
  real                                              :: HVAP                      ! latent heat of vaporization (j/kg)
  real                                              :: HFUS                      ! latent heat of fusion (j/kg)
  real                                              :: CWAT                      ! specific heat capacity of water (j/m3/k)
  real                                              :: CICE                      ! specific heat capacity of ice (j/m3/k)
  real                                              :: CPAIR                     ! heat capacity dry air at const pres (j/kg/k)
  real                                              :: TKWAT                     ! thermal conductivity of water (w/m/k)
  real                                              :: TKICE                     ! thermal conductivity of ice (w/m/k)
  real                                              :: TKAIR                     ! thermal conductivity of air (w/m/k) (not used MB: 20140718)
  real                                              :: RAIR                      ! gas constant for dry air (j/kg/k)
  real                                              :: RW                        ! gas constant for  water vapor (j/kg/k)
  real                                              :: DENH2O                    ! density of water (kg/m3)
  real                                              :: DENICE                    ! density of ice (kg/m3)
  real                                              :: THKW                      ! thermal conductivity of water in soil module (W/m/K)
  real                                              :: THKO                      ! thermal conductivity of for other soil components in soil module (W/m/K)
  real                                              :: THKQTZ                    ! thermal conductivity of quartz in soil module (W/m/K)
  real                                              :: SSI                       ! liquid water holding capacity for snowpack (m3/m3)
  real,allocatable,dimension(:,:)                   :: MFSNO                     ! fractional snow covered area (FSNO) curve parameter
  real                                              :: Z0SNO                     ! snow surface roughness length (m)
  real                                              :: SWEMX                     ! new SWE required (QSNOW * dt) to fully cover old snow (mm)
  real                                              :: TAU0                      ! tau0 from Yang97 eqn. 10a
  real                                              :: GRAIN_GROWTH              ! growth from vapor diffusion Yang97 eqn. 10b
  real                                              :: EXTRA_GROWTH              ! extra growth near freezing Yang97 eqn. 10c
  real                                              :: DIRT_SOOT                 ! dirt and soot term Yang97 eqn. 10d
  real                                              :: BATS_COSZ                 ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
  real                                              :: BATS_VIS_NEW              ! new snow visible albedo
  real                                              :: BATS_NIR_NEW              ! new snow NIR albedo
  real                                              :: BATS_VIS_AGE              ! age factor for diffuse visible snow albedo Yang97 eqn. 17
  real                                              :: BATS_NIR_AGE              ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
  real                                              :: BATS_VIS_DIR              ! cosz factor for direct visible snow albedo Yang97 eqn. 15
  real                                              :: BATS_NIR_DIR              ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
  real                                              :: RSURF_SNOW                ! surface resistence for snow [s/m]
  real                                              :: RSURF_EXP                 ! exponent in the shape parameter for soil resistance option 1
  real,allocatable,dimension(:,:,:)                 :: ALBSAT                    ! saturated soil albedo (1=vis, 2=nir)
  real,allocatable,dimension(:,:,:)                 :: ALBDRY                    ! dry soil albedo (1=vis, 2=nir)
  real,allocatable,dimension(:,:,:)                 :: ALBICE                    ! Land ice albedo (1=vis, 2=nir)
  real,allocatable,dimension(:,:,:)                 :: ALBLAK                    ! Lake ice albedo (1=vis, 2=nir)
  real,allocatable,dimension(:,:,:)                 :: OMEGAS                    ! two-stream parameter omega for snow (1=vis, 2=nir)
  real                                              :: BETADS                    ! two-stream parameter betad for snow
  real                                              :: BETAIS                    ! two-stream parameter betaI for snow
  real,allocatable,dimension(:,:,:)                 :: EG                        ! emissivity of land surface (1=soil,2=lake)
  real                                              :: WSLMAX                    ! maximum lake water storage (mm)
  real                                              :: max_liq_mass_fraction     ! For snow water retention
  real                                              :: SNOW_RET_FAC              ! snowpack water release timescale factor (1/s)
  integer                                           :: NBAND                     ! Number of shortwave bands (2, visible and NIR)
  real                                              :: MPE                       ! MPE is nominally small to prevent dividing by zero error
  real,allocatable,dimension(:,:)                   :: TOPT                      ! Optimum transpiration air temperature [K]
  real                                              :: O2                        ! o2 partial pressure, from MPTABLE.TBL
  real                                              :: CO2                       ! co2 partial pressure, from MPTABLE.TBL
  real                                              :: PSIWLT                    ! matric potential for wilting point (m)  (orig a fixed param.)
  real                                              :: TBOT                      ! bottom condition for soil temp. (k)
  real                                              :: GRAV                      ! acceleration due to gravity (m/s2)
  real                                              :: rain_snow_thresh          ! user-defined rain-snow temperature threshold (°C)
  integer                                           :: ix,iy,iz

  !--------------------------------------------------------------------------------------------------------------
  ! WaterType
  !--------------------------------------------------------------------------------------------------------------

  real,allocatable,dimension(:,:)                             :: qinsur      ! water input on soil surface [m/s]
  real,allocatable,dimension(:,:)                             :: qseva       ! soil surface evap rate [m/s]
  real,allocatable,dimension(:,:)                             :: EVAPOTRANS  ! evapotranspiration, sum of QSEVA + ETRAN [m/s]
  real,allocatable,dimension(:,:)                             :: runsrf      ! surface runoff [mm/s] 
  real,allocatable,dimension(:,:)                             :: runsub      ! baseflow (sturation excess) [mm/s]
  real,allocatable,dimension(:,:)                             :: qdrain      ! soil-bottom free drainage [mm/s] 
  real,allocatable,dimension(:,:)                             :: zwt         ! the depth to water table [m]
  real,allocatable,dimension(:,:)                             :: smcwtd      ! soil water content between bottom of the soil and water table [m3/m3]
  real,allocatable,dimension(:,:)                             :: deeprech    ! recharge to or from the water table when deep [m]
  real,allocatable,dimension(:,:)                             :: fcrmax      ! maximum of fcr (-)
  real,allocatable,dimension(:,:)                             :: snoflow     ! glacier outflow, added to RUNSUB
  real,allocatable,dimension(:,:)                             :: pddum       ! infiltration rate at surface (m/s)
  real,allocatable,dimension(:,:)                             :: FACC        ! accumulated infiltration rate (m/s) in dynamic vic option
  real,allocatable,dimension(:,:)                             :: sicemax     ! maximum soil ice content (m3/m3)
  real,allocatable,dimension(:,:)                             :: FB_snow     ! canopy fraction buried by snow
  real,allocatable,dimension(:,:)                             :: rain        ! rainfall (mm/s)
  real,allocatable,dimension(:,:)                             :: snow        ! snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: bdfall      ! bulk density of new snowfall (kg/m3)
  real,allocatable,dimension(:,:)                             :: FP          ! fraction of the gridcell that receives precipitation
  real,allocatable,dimension(:,:)                             :: canliq      ! intercepted liquid water (mm)
  real,allocatable,dimension(:,:)                             :: canice      ! intercepted ice mass (mm)
  real,allocatable,dimension(:,:)                             :: FWET        ! wetted or snowed fraction of the canopy (-)
  real,allocatable,dimension(:,:)                             :: CMC         ! total canopy moisture content (CANLIQ + CANICE) (mm)
  real,allocatable,dimension(:,:)                             :: QINTR       ! interception rate for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QDRIPR      ! drip rate for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QTHROR      ! throughfall for rain (mm/s)
  real,allocatable,dimension(:,:)                             :: QINTS       ! interception (loading) rate for snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: QDRIPS      ! drip (unloading) rate for intercepted snow (mm/s)
  real,allocatable,dimension(:,:)                             :: QTHROS      ! throughfall of snowfall (mm/s)
  real,allocatable,dimension(:,:)                             :: QRAIN       ! rain at ground surface (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: QSNOW       ! snow at ground surface (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: SNOWHIN     ! snow depth increasing rate (m/s)
  real,allocatable,dimension(:,:)                             :: ECAN        ! evaporation of intercepted water (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: ETRAN       ! transpiration rate (mm/s) [+]
  real,allocatable,dimension(:,:)                             :: QSNFRO      ! snow surface frost rate[mm/s]
  real,allocatable,dimension(:,:)                             :: QSNSUB      ! snow surface sublimation rate[mm/s]
  real,allocatable,dimension(:,:)                             :: SNOWH       ! snow height [m]
  real,allocatable,dimension(:,:)                             :: SNEQV       ! snow water eqv. [mm]
  real,allocatable,dimension(:,:)                             :: SNEQVO      ! snow water eqv. of previous time step [mm]
  real,allocatable,dimension(:,:)                             :: BDSNO       ! bulk density of snowpack (kg/m3)
  real,allocatable,dimension(:,:)                             :: QSNBOT      ! melting water out of snow bottom [mm/s]
  real,allocatable,dimension(:,:)                             :: PONDING
  real,allocatable,dimension(:,:)                             :: PONDING1
  real,allocatable,dimension(:,:)                             :: PONDING2
  real,allocatable,dimension(:,:)                             :: QVAP        ! ground surface evaporation/sublimation rate mm/s
  real,allocatable,dimension(:,:)                             :: QDEW        ! ground surface dew rate [mm/s]
  real,allocatable,dimension(:,:)                             :: QSDEW       ! soil surface dew rate [mm/s]
  real,allocatable,dimension(:,:)                             :: WSLAKE      ! water storage in lake (can be -) (mm)
  real,allocatable,dimension(:,:)                             :: runsrf_dt   ! temporal time step for surface runoff calculations
  real,allocatable,dimension(:,:)                             :: ASAT        ! accumulated saturation in VIC runoff scheme
  
  integer,allocatable,dimension(:,:)                          :: ISNOW       ! actual no. of snow layers 
  real, allocatable, dimension(:,:,:)                         :: smc         ! total soil water content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: smc_init    ! initial total soil water content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: sice        ! total soil ice content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: sh2o        ! total soil liquid content [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: etrani      ! transpiration rate (mm/s) [+]
  real, allocatable, dimension(:,:,:)                         :: BTRANI      ! Soil water transpiration factor (0 - 1)
  real, allocatable, dimension(:,:,:)                         :: wcnd        ! hydraulic conductivity (m/s)
  real, allocatable, dimension(:,:,:)                         :: fcr         ! impermeable fraction due to frozen soil 
  real, allocatable, dimension(:,:,:)                         :: FICEOLD     ! ice fraction at last timestep
  real, allocatable, dimension(:,:,:)                         :: SNICE       ! snow layer ice [mm]
  real, allocatable, dimension(:,:,:)                         :: SNLIQ       ! snow layer liquid water [mm] 
  real, allocatable, dimension(:,:,:)                         :: SNICEV      ! snow layer partial volume of ice [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: SNLIQV      ! snow layer partial volume of liquid water [m3/m3]
  real, allocatable, dimension(:,:,:)                         :: FICE        ! fraction of ice at current time step
  real, allocatable, dimension(:,:,:)                         :: EPORE       ! snow layer effective porosity [m3/m3]
  
  real,allocatable,dimension(:,:)                             :: FSNO        ! fraction of grid cell with snow cover
  real,allocatable,dimension(:,:)                             :: BTRAN       ! soil water transpiration factor (0 to 1)  

  end type

end module NoahowpmpIOVarType
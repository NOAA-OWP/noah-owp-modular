! NOTES
!   needs options:  OPT_STC, OPT_SFC
!   opt_crop==2 is not supported [gecros]

! contains
!  VegeFluxMain, BareFluxMain

! relies on routines in FluxUtilityModule:
! RAGRB, STOMATA, CANRES, CALHUM, ESAT, SFCDIF1, SFCDIF2

module EtFluxModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains
  
  !== begin VegeFluxMain ==================================================================================

  SUBROUTINE VegeFluxMain (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in)    :: levels
    type (parameters_type), intent(in)    :: parameters
    type (    domain_type)                :: domain
    type (    energy_type)                :: energy
    type (     water_type)                :: water
    type (   forcing_type)                :: forcing
    type (   options_type), intent(in)    :: options
    

  !SUBROUTINE VEGE_FLUX(parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
  !                     DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
  !                     UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
  !                     EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & !in
  !                     FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
  !                     ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
  !                     Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO,          & !in
  !                     CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
  !                     RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & !in
  !                     FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
  !                     RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV    ,PAHG     , & !in
  !                     EAH     ,TAH     ,TV      ,TG      ,CM      , & !inout
  !                     CH      ,DX      ,DZ8W    ,                   & !
  !                     TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
  !                     SHC     ,EVG     ,EVC     ,TR      ,GH      , & !out
  !                     T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
  !                     QC      ,QSFC    ,PSFC    ,                   & !in
  !                     Q2V     ,CAH2    ,CHLEAF  ,CHUC,              & !inout 
  !                     SH2O,JULIAN, SWDOWN, PRCP, FB, FSR, GECROS1D)      ! Gecros 

    ! --------------------------------------------------------------------------------------------------
    ! use newton-raphson iteration to solve for vegetation (tv) and
    ! ground (tg) temperatures that balance the surface energy budgets

    ! vegetated:
    ! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
    ! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
    ! --------------------------------------------------------------------------------------------------
  ! input
!  type (noahmp_parameters), intent(in) :: parameters

!  INTEGER,                         INTENT(IN) :: ILOC   !grid index
!  INTEGER,                         INTENT(IN) :: JLOC   !grid index
!  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
!  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
!  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
!  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
!  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
!  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
!  REAL,                            INTENT(INOUT) :: SAV    !solar rad absorbed by veg (w/m2)
!  REAL,                            INTENT(INOUT) :: SAG    !solar rad absorbed by ground (w/m2)
!  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
!  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
!  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
!  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
!  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
!  REAL,                            INTENT(IN) :: THAIR  !potential temp at reference height (k)
!  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at zlvl (pa)
!  REAL,                            INTENT(IN) :: QAIR   !specific humidity at zlvl (kg/kg)
!  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m**3)
!  REAL,                            INTENT(IN) :: DT     !time step (s)
!  REAL,                            INTENT(IN) :: FSNO     !snow fraction
!  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
!  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
!  REAL,                            INTENT(IN) :: CWP    !canopy wind parameter

!  REAL,                            INTENT(IN) :: VAI    !total leaf area index + stem area index
!  REAL,                            INTENT(IN) :: LAISUN !sunlit leaf area index, one-sided (m2/m2)
!  REAL,                            INTENT(IN) :: LAISHA !shaded leaf area index, one-sided (m2/m2)
!  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
!  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
!  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum (m)
!  REAL,                            INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
!  REAL,                            INTENT(IN) :: EMV    !vegetation emissivity
!  REAL,                            INTENT(IN) :: EMG    !ground emissivity

!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thinkness of snow/soil layers (m)
!  REAL,                            INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
!  REAL,                            INTENT(IN) :: CANICE !intercepted ice mass (mm)
!  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
!  REAL,                            INTENT(IN) :: GAMMAV  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEAV !latent heat of vaporization/subli (j/kg)
!  REAL,                            INTENT(IN) :: GAMMAG  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEAG !latent heat of vaporization/subli (j/kg)
!  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
!  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
!  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
!  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
!  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
!  REAL,                            INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
!  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
!  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
!  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)

!  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
!  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
!  REAL                           , INTENT(IN) :: DX     !grid spacing
!  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
!  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
!  REAL, INTENT(IN)   :: PAHV  !precipitation advected heat - canopy net IN (W/m2)
!  REAL, INTENT(IN)   :: PAHG  !precipitation advected heat - ground net IN (W/m2)

! input/output
!  REAL,                         INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
!  REAL,                         INTENT(INOUT) :: TAH    !canopy air temperature (k)
!  REAL,                         INTENT(INOUT) :: TV     !vegetation temperature (k)
!  REAL,                         INTENT(INOUT) :: TG     !ground temperature (k)
!  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
!  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
! -FSA + FIRA + FSH + (FCEV + FCTR + FGEV) + FCST + SSOIL = 0
!  REAL,                           INTENT(OUT) :: TAUXV  !wind stress: e-w (n/m2)
!  REAL,                           INTENT(OUT) :: TAUYV  !wind stress: n-s (n/m2)
!  REAL,                           INTENT(OUT) :: IRC    !net longwave radiation (w/m2) [+= to atm]
!  REAL,                           INTENT(OUT) :: SHC    !sensible heat flux (w/m2)     [+= to atm]
!  REAL,                           INTENT(OUT) :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
!  REAL,                           INTENT(OUT) :: IRG    !net longwave radiation (w/m2) [+= to atm]
!  REAL,                           INTENT(OUT) :: SHG    !sensible heat flux (w/m2)     [+= to atm]
!  REAL,                           INTENT(OUT) :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
!  REAL,                           INTENT(OUT) :: TR     !transpiration heat flux (w/m2)[+= to atm]
!  REAL,                           INTENT(OUT) :: GH     !ground heat (w/m2) [+ = to soil]
!  REAL,                           INTENT(OUT) :: T2MV   !2 m height air temperature (k)
!  REAL,                           INTENT(OUT) :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
!  REAL,                           INTENT(OUT) :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
!  REAL,                           INTENT(OUT) :: CHLEAF !leaf exchange coefficient
!  REAL,                           INTENT(OUT) :: CHUC   !under canopy exchange coefficient
!  REAL,                           INTENT(OUT) :: Q2V
!  REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
!  REAL :: U10V    !10 m wind speed in eastward dir (m/s) 
!  REAL :: V10V    !10 m wind speed in eastward dir (m/s) 
!  REAL :: WSPD
!  REAL, INTENT(OUT) :: RSSUN        ! sunlit leaf stomatal resistance (s/m)
!  REAL, INTENT(OUT) :: RSSHA        ! shaded leaf stomatal resistance (s/m)
!  REAL, INTENT(OUT) :: CAH2         ! sensible heat conductance for diagnostics


    ! ------------------------ local variables ----------------------------------------------------
    REAL :: CW           ! water vapor exchange coefficient
    REAL :: FV           ! friction velocity (m/s)
    REAL :: WSTAR        ! friction velocity n vertical direction (m/s) (only for SFCDIF2)
    REAL :: Z0H          ! roughness length, sensible heat (m)
    REAL :: Z0HG         ! roughness length, sensible heat (m)
    REAL :: RB           ! bulk leaf boundary layer resistance (s/m)
    REAL :: RAMC         ! aerodynamic resistance for momentum (s/m)
    REAL :: RAHC         ! aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWC         ! aerodynamic resistance for water vapor (s/m)
    REAL :: RAMG         ! aerodynamic resistance for momentum (s/m)
    REAL :: RAHG         ! aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWG         ! aerodynamic resistance for water vapor (s/m)

    REAL :: MOL          ! Monin-Obukhov length (m)
    REAL :: DTV          ! change in tv, last iteration (k)
    REAL :: DTG          ! change in tg, last iteration (k)

    REAL :: AIR,CIR      !coefficients for ir as function of ts**4
    REAL :: CSH          !coefficients for sh as function of ts
    REAL :: CEV          !coefficients for ev as function of esat[ts]
    REAL :: CGH          !coefficients for st as function of ts
    REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
    REAL :: ATA,BTA      !coefficients for tah as function of ts
    REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]

    REAL :: ESTV         !saturation vapor pressure at tv (pa)
    REAL :: ESTG         !saturation vapor pressure at tg (pa)
    REAL :: DESTV        !d(es)/dt at ts (pa/k)
    REAL :: DESTG        !d(es)/dt at tg (pa/k)
    REAL :: ESATW        !es for water
    REAL :: ESATI        !es for ice
    REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
    REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice

    REAL :: FM           !momentum stability correction, weighted by prior iters
    REAL :: FH           !sen heat stability correction, weighted by prior iters
    REAL :: FHG          !sen heat stability correction, ground
    REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]

    REAL :: A            !temporary calculation
    REAL :: B            !temporary calculation
    REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
    REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
    REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
    REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
    REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
    REAL :: COND         !sum of conductances (s/m)
    REAL :: UC           !wind speed at top of canopy (m/s)
    REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
    REAL :: H            !temporary sensible heat flux (w/m2)
    REAL :: HG           !temporary sensible heat flux (w/m2)
    REAL :: MOZ          !Monin-Obukhov stability parameter
    REAL :: MOZG         !Monin-Obukhov stability parameter
    REAL :: MOZOLD       !Monin-Obukhov stability parameter from prior iteration
    REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
    REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
    REAL :: CH2          !Surface exchange at 2m
    REAL :: THSTAR       !Surface exchange at 2m

    REAL :: THVAIR
    REAL :: THAH 
    REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
    REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
    REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
    REAL :: EAH2         !2m vapor pressure over canopy
    REAL :: QFX        !moisture flux
    REAL :: E1           

    REAL :: VAIE         !total leaf area index + stem area index,effective
    REAL :: LAISUNE      !sunlit leaf area index, one-sided (m2/m2),effective
    REAL :: LAISHAE      !shaded leaf area index, one-sided (m2/m2),effective

    INTEGER :: K         !index
    INTEGER :: ITER      !iteration index

    !jref - NITERC test from 5 to 20  
    INTEGER, PARAMETER :: NITERC = 20   !number of iterations for surface temperature
    !jref - NITERG test from 3-5
    INTEGER, PARAMETER :: NITERG = 5   !number of iterations for ground temperature
    INTEGER            :: MOZSGN    !number of times MOZ changes sign
    !REAL              :: MPE       ! prevents overflow error if division by zero  AW: now included in parameters

    INTEGER :: LITER     !Last iteration

    REAL, INTENT(IN)                    :: JULIAN, SWDOWN, PRCP, FB
    REAL, INTENT(INOUT)                 :: FSR
    REAL,DIMENSION(1:60), INTENT(INOUT) :: GECROS1D 
    REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O    !soil liquid water
  
    REAL :: ROOTD, WUL, WLL, Thickness, TLAIE, GLAIE, TLAI, GLAI, FRSU
    INTEGER :: NROOT, J

    REAL :: T, TDC       !Kelvin to degree Celsius with limit -50 to +50

    character(len=80) ::  message
    ! ---------------------------------------------------------------------------------------------
  
    ! associate variables to keep variable names intact in the code below  
    associate(&
      ! used in resistance calculations
      SFCTMP   => forcing%SFCTMP     ,&   ! intent(in)    : real  air temperature at reference height (K)  
      TAH      => forcing%SFCTMP     ,&   ! intent(in)    : real  canopy temperature (K)  
      TV       => energy%TV          ,&   ! intent(in)    : real  vegetation temperature (K)  
      RHOAIR   => forcing%RHOAIR      &   ! intent(in)    : real  density air (kg/m3)
    )  
    ! ---- end associate block --------------------------------------------------------------------

    TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

    !MPE = 1E-6    set in parameters
    LITER = 0
    FV = 0.1

    ! ---------------------------------------------------------------------------------------------
    ! initialization variables that do not depend on stability iteration
    ! ---------------------------------------------------------------------------------------------
    DTV = 0.; DTG    = 0.
    MOZ = 0.; MOZSGN = 0; MOZOLD = 0.
    FH2 = 0.; HG     = 0.; H     = 0.
    QFX = 0.

    ! limit LAI
    VAIE    = MIN(6.,VAI   )
    LAISUNE = MIN(6.,LAISUN)
    LAISHAE = MIN(6.,LAISHA)

    ! saturation vapor pressure at ground temperature
    T = TDC(TG)
    CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
    IF (T .GT. 0.) THEN
      ESTG = ESATW
    ELSE
      ESTG = ESATI
    END IF

    !jref - consistent surface specific humidity for sfcdif3 and sfcdif4
    QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  

    ! canopy height
    HCAN = parameters%HVT
    UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
    UC = UR*LOG((HCAN-ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7
    IF((HCAN-ZPD) <= 0.) THEN
      WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
      call wrf_message ( message )
      WRITE(message,*) 'i,j point=',ILOC, JLOC
      call wrf_message ( message )
      WRITE(message,*) 'HCAN  =',HCAN
      call wrf_message ( message )
      WRITE(message,*) 'ZPD   =',ZPD
      call wrf_message ( message )
      write (message, *) 'SNOWH =',SNOWH
      call wrf_message ( message )
      call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
    END IF

    ! prepare for longwave rad.
    AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*SB*TG**4  
    CIR = (2.-EMV*(1.-EMG))*EMV*SB

    ! ---------------------------------------------------------------------------------------------
    loop1: DO ITER = 1, NITERC    !  begin stability iteration

      IF(ITER == 1) THEN
        Z0H  = Z0M  
        Z0HG = Z0MG
      ELSE
        Z0H  = Z0M    !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        Z0HG = Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
      END IF

      ! aerodyn resistances between heights zlvl and d+z0v
      IF(OPT_SFC == 1) THEN
        CALL SFCDIF1(parameters, ITER, SFCTMP, RHOAIR, H, forcing%QAIR,     &  ! in
                     energy%ZLVL, energy%ZPD, energy%Z0M, Z0H, forcing%UR,  &  ! in
                     MOZ, MOZSGN, FM, FH, FM2, FH2,                         &  ! inout
                     energy%CM, energy%CH, FV, CH2)                            ! out
        ! note, local vars:  ITER H ZOH MOZ MOZSGN FM FH FM2 FH2 FV CH2
      ENDIF
     
      IF(OPT_SFC == 2) THEN
        CALL SFCDIF2(parameters, ITER, energy%Z0M, TAH, forcing%THAIR, forcing%UR,  & ! in
                     energy%ZLVL, energy%CM, energy%CH, MOZ, WSTAR,                 & ! in
                     FV )                                                     ! out
        ! Undo the multiplication by windspeed that SFCDIF2 
        ! applies to exchange coefficients CH and CM:
        CH = CH / UR
        CM = CM / UR
      ENDIF

      RAMC = MAX(1.,1./(CM*UR))
      RAHC = MAX(1.,1./(CH*UR))
      RAWC = RAHC

      ! calculate aerodynamic resistance between heights z0g and d+z0v, RAG, and leaf
      ! boundary layer resistance, RB
       
      ! orig
      !CALL RAGRB(parameters,ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !in
      !           ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
      !           Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
      !           TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
      !           RAMG   ,RAHG   ,RAWG   ,RB     )           !out
      CALL RAGRB(parameters, ITER, VAIE, forcing%RHOAIR, HG, TAH, energy%ZPD, &  ! in
                 energy%Z0MG, Z0HG, HCAN, UC, Z0H, FV, domain%VEGTYP, &          ! in
                 energy%TV, MOZG, FHG, &                                         ! inout
                 RAMG, RAHG, RAWG, RB)                                           ! out                  
      ! note, local vars:  ITER VAIE HG ZOHG HCAN UC Z0H FV MOZH FHG RAMG, RAHG, RAWG, RB
       
      ! es and d(es)/dt evaluated at tv
      T = TDC(TV)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTV  = ESATW
        DESTV = DSATW
      ELSE
        ESTV  = ESATI
        DESTV = DSATI
      END IF

      ! calculate stomatal resistance and photosynthesis (two options available)
      IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  ! Ball-Berry
           ! orig
           !CALL STOMATA (parameters,VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !in       
           !              TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
           !              O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
           !              RSSUN ,PSNSUN)                         !out
           !CALL STOMATA (parameters,VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !in
           !              TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
           !              O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
           !              RSSHA ,PSNSHA)                         !out 
           ! sun
          CALL STOMATA (parameters, domain%VEGTYP, energy%PARSUN, forcing%FOLN, energy%TV, &     ! in
                        ESTV, energy%EAH, forcing%SFCTMP, forcing%SFCPRS, forcing%O2PP, & ! in
                        forcing%CO2PP, energy%IGS, water%BTRAN, RB, &                     ! in
                        energy%RSSUN , energy%PSNSUN)                                     ! out
          ! shade
          CALL STOMATA (parameters, domain%VEGTYP, energy%PARSHA, forcing%FOLN, energy%TV, &     ! in
                        ESTV, energy%EAH, forcing%SFCTMP, forcing%SFCPRS, forcing%O2PP, & ! in
                        forcing%CO2PP, energy%IGS, water%BTRAN, RB, &                     ! in
                        energy%RSSHA , energy%PSNSHA)            
        END IF

        ! calculate sunlit and shaded resistances and leaf photosynthesis
        IF (OPT_CRS == 2) then  ! Jarvis
          ! sun
          !CALL  CANRES (parameters,PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
          !             RSSUN ,PSNSUN,ILOC  ,JLOC   )          !out
          ! shade
          !CALL  CANRES (parameters,PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
          !             RSSHA ,PSNSHA,ILOC  ,JLOC   )          !out
          ! sun
          CALL  CANRES (parameters, energy%PARSUN, energy%TV, water%BTRAN ,energy%EAH, forcing%SFCPRS, &  ! in
                        energy%RSSUN, energy%PSNSUN )                                                     ! out
          ! shade
          CALL  CANRES (parameters, energy%PARSHA, energy%TV, water%BTRAN, energy%EAH, forcing%SFCPRS, &  ! in
                        energy%RSSHA, energy%PSNSHA )                                                     ! out                   
        END IF
        
        ! Call GECROS
        ! Note:  GECROS option (opt_crop == 2) is not currently supported. 
       
      END IF

      ! prepare for sensible heat flux above veg.
      CAH  = 1./RAHC
      CVH  = 2.*VAIE/RB
      CGH  = 1./RAHG
      COND = CAH + CVH + CGH
      ATA  = (SFCTMP*CAH + TG*CGH) / COND
      BTA  = CVH/COND
      CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH

      ! prepare for latent heat flux above veg.
      CAW  = 1./RAWC
      CEW  = FWET*VAIE/RB

      IF (OPT_CROP /= 2) THEN
        CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
      ELSE
        !RSSUN and RSSHA are in resistance per unit LAI in the Jarvis and Ball-Berry!. RSSUN and RSSHA of Gecros are in s/m
        CTW  = (1.-FWET)*(1./(RB/(FRSU*GLAIE)+RSSUN) + 1./(RB/((1.-FRSU)*GLAIE)+RSSHA)) !transpiration conductance leaf to canopy air
      ENDIF
      CGW  = 1./(RAWG+RSURF)
      COND = CAW + CEW + CTW + CGW
      AEA  = (EAIR*CAW + ESTG*CGW) / COND
      BEA  = (CEW+CTW)/COND
      CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   ! Barlage: change to vegetation v3.6
      CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMAV

      ! evaluate surface fluxes with current temperature and solve for dts
      TAH = ATA + BTA*TV               ! canopy air T.
      EAH = AEA + BEA*ESTV             ! canopy air e

      IRC = FVEG*(AIR + CIR*TV**4)
      SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
      EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV ! Barlage: change to v in v3.6
      TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV
      IF (TV > TFRZ) THEN
        EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    ! Barlage: add if block for canice in v3.6
      ELSE
        EVC = MIN(CANICE*LATHEAV/DT,EVC)
      END IF

      B   = SAV-IRC-SHC-EVC-TR+PAHV                          !additional w/m2
      A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) !volumetric heat capacity
      DTV = B/A
      IRC = IRC + FVEG*4.*CIR*TV**3*DTV
      SHC = SHC + FVEG*CSH*DTV
      EVC = EVC + FVEG*CEV*DESTV*DTV
      TR  = TR  + FVEG*CTR*DESTV*DTV                               

      ! update vegetation surface temperature
      TV  = TV + DTV
      !TAH = ATA + BTA*TV               ! canopy air T; update here for consistency

      ! for computing M-O length in the next iteration
      H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
      HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG

      ! consistent specific humidity from canopy air vapor pressure
      QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)

      IF (LITER == 1) THEN
        exit loop1 
      ENDIF
      IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
        LITER = 1
      ENDIF

    END DO loop1 ! end stability iteration

    ! under-canopy fluxes and tg
    AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
    CIR = EMG*SB
    CSH = RHOAIR*CPAIR/RAHG
    CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  ! Barlage: change to ground v3.6
    CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

    ! ========= LOOP 2 ========================
    loop2: DO ITER = 1, NITERG

      T = TDC(TG)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTG  = ESATW
        DESTG = DSATW
      ELSE
        ESTG  = ESATI
        DESTG = DSATI
      END IF

      IRG = CIR*TG**4 + AIR
      SHG = CSH * (TG         - TAH         )
      EVG = CEV * (ESTG*RHSUR - EAH         )
      GH  = CGH * (TG         - STC(ISNOW+1))

      B = SAG-IRG-SHG-EVG-GH+PAHG
      A = 4.*CIR*TG**3+CSH+CEV*DESTG+CGH
      DTG = B/A

      IRG = IRG + 4.*CIR*TG**3*DTG
      SHG = SHG + CSH*DTG
      EVG = EVG + CEV*DESTG*DTG
      GH  = GH  + CGH*DTG
      TG  = TG  + DTG

    END DO loop2
     
    !TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)

    ! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
    IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
    IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
      IF(OPT_STC == 1) TG  = TFRZ
      IF(OPT_STC == 3) TG  = (1.-FSNO)*TG + FSNO*TFRZ   ! MB: allow TG>0C during melt v3.7
      IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
      SHG = CSH * (TG         - TAH)
      EVG = CEV * (ESTG*RHSUR - EAH)
      GH  = SAG+PAHG - (IRG+SHG+EVG)
    END IF
    END IF

    ! wind stresses
    TAUXV = -RHOAIR*CM*UR*UU
    TAUYV = -RHOAIR*CM*UR*VV

    ! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
    ! calculation.
    !     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
    !     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
    !     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
    !     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG

    ! 2m temperature over vegetation ( corrected for low CQ2V values )
    IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN
      !      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.E-5 ) THEN
        T2MV = TAH
        !         Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
        Q2V  = QSFC
      ELSE
        T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1./CAH2
        !         Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
    ENDIF

    ! update CH for output
    CH = CAH
    CHLEAF = CVH
    CHUC = 1./RAHG
  
  END SUBROUTINE VegeFluxMain


! AW need work on following subroutine

  ! == begin BareFluxMain ==================================================================================

  SUBROUTINE BareFluxMain (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in)    :: levels
    type (parameters_type)                :: parameters
    type (    domain_type)                :: domain
    type (    energy_type)                :: energy
    type (     water_type)                :: water
    type (   forcing_type)                :: forcing
    type (   options_type)                :: options
    
!  SUBROUTINE BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
!                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
!                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
!                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,FSNO    , & !in
!                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
!                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
!                        TGB     ,CM      ,CH      ,          & !inout
!                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
!                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !out
!                        QC      ,QSFC    ,PSFC    ,                   & !in
!                        SFCPRS  ,Q2B     ,EHB2    )                     !in    

    ! --------------------------------------------------------------------------------------------------
    ! use newton-raphson iteration to solve ground (tg) temperature
    ! that balances the surface energy budgets for bare soil fraction.
    !    bare soil:
    !      -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
    ! ----------------------------------------------------------------------
    
  ! input
  !  type (noahmp_parameters), intent(in) :: parameters
!  integer                        , INTENT(IN) :: ILOC   !grid index
!  integer                        , INTENT(IN) :: JLOC   !grid index
!  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
!  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
!  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
!  REAL,                            INTENT(IN) :: DT     !time step (s)
!  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
!  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
!  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
!  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
!  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
!  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
!  REAL,                            INTENT(IN) :: THAIR  !potential temperature at height zlvl (k)
!  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
!  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
!  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
!  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
!  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
!  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
!  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
!  REAL,                            INTENT(IN) :: EMG    !ground emissivity
!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
!  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
!  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
!  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
!  REAL,                            INTENT(IN) :: FSNO     !snow fraction

!  INTEGER                        , INTENT(IN) :: IVGTYP
!  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
!  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
!  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
!  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
!  REAL                           , INTENT(IN) :: DX     !horisontal grid spacing
!  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
!  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!  REAL, INTENT(IN)   :: PAHB  !precipitation advected heat - ground net IN (W/m2)

! input/output
!  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
!  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
!  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
!     -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
!  REAL,                           INTENT(OUT) :: TAUXB  !wind stress: e-w (n/m2)
!  REAL,                           INTENT(OUT) :: TAUYB  !wind stress: n-s (n/m2)
!  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
!  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
!  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
!  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
!  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
!  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
!  REAL :: EHB    !bare ground heat conductance
!  REAL :: U10B    !10 m wind speed in eastward dir (m/s)
!  REAL :: V10B    !10 m wind speed in eastward dir (m/s)
!  REAL :: WSPD
!    REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics


    ! ------------------------ local variables ---------------------------
    REAL :: EHB     !bare ground heat conductance
    REAL :: U10B    !10 m wind speed in eastward dir (m/s)
    REAL :: V10B    !10 m wind speed in eastward dir (m/s)
    REAL :: WSPD

    REAL :: TAUX       !wind stress: e-w (n/m2)
    REAL :: TAUY       !wind stress: n-s (n/m2)
    REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
    REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
    REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
    REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
    REAL :: FIRE       !emitted ir (w/m2)
    REAL :: TRAD       !radiative temperature (k)
    REAL :: TAH        !"surface" temperature at height z0h+zpd (k)

    REAL :: CW         !water vapor exchange coefficient
    REAL :: FV         !friction velocity (m/s)
    REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
    REAL :: Z0H        !roughness length, sensible heat, ground (m)
    REAL :: RB         !bulk leaf boundary layer resistance (s/m)
    REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
    REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
    REAL :: MOL        !Monin-Obukhov length (m)
    REAL :: DTG        !change in tg, last iteration (k)

    REAL :: CIR        !coefficients for ir as function of ts**4
    REAL :: CSH        !coefficients for sh as function of ts
    REAL :: CEV        !coefficients for ev as function of esat[ts]
    REAL :: CGH        !coefficients for st as function of ts

    REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
    REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
    REAL :: CH2B       !exchange coefficient for 2m temp.
    REAL :: CQ2B       !exchange coefficient for 2m temp.
    REAL :: THVAIR     !virtual potential air temp
    REAL :: THGH       !potential ground temp
    REAL :: EMB        !momentum conductance
    REAL :: QFX        !moisture flux
    REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
    INTEGER :: VEGTYP     !vegetation type set to isbarren
    REAL :: E1

    REAL :: ESTG       !saturation vapor pressure at tg (pa)
    REAL :: DESTG      !d(es)/dt at tg (pa/K)
    REAL :: ESATW      !es for water
    REAL :: ESATI      !es for ice
    REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
    REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice

    REAL :: A, B       !temporary calculation
    REAL :: H          !temporary sensible heat flux (w/m2)
    REAL :: MOZ        !Monin-Obukhov stability parameter
    REAL :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
    REAL :: FM         !momentum stability correction, weighted by prior iters
    REAL :: FH         !sen heat stability correction, weighted by prior iters
    INTEGER :: MOZSGN  !number of times MOZ changes sign
    REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
    REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
    REAL :: CH2          !Surface exchange at 2m

    INTEGER :: ITER    !iteration index
    INTEGER :: NITERB  !number of iterations for surface temperature
    REAL    :: MPE     !prevents overflow error if division by zero
    DATA NITERB /5/
    SAVE NITERB
    REAL :: T, TDC     !Kelvin to degree Celsius with limit -50 to +50
    ! -----------------------------------------------------------------
    
    TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

    ! -----------------------------------------------------------------
    ! initialization variables that do not depend on stability iteration
    ! -----------------------------------------------------------------
    MPE = 1E-6
    DTG = 0.
    MOZ    = 0.
    MOZSGN = 0
    MOZOLD = 0.
    FH2    = 0.
    H      = 0.
    QFX    = 0.
    FV     = 0.1

    CIR = EMG*SB
    CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

    ! -----------------------------------------------------------------
    loop3: DO ITER = 1, NITERB  ! begin stability iteration

      IF(ITER == 1) THEN
        Z0H = Z0M 
      ELSE
        Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
      END IF

      IF(OPT_SFC == 1) THEN
        CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                     ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                     MPE    ,ILOC   ,JLOC   ,                 & !in
                     MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                     CM     ,CH     ,FV     ,CH2     )          !out
      ENDIF

      IF(OPT_SFC == 2) THEN
        CALL SFCDIF2(parameters,ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !in
                     ZLVL   ,ILOC   ,JLOC   ,         & !in
                     CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                     FV     )                                   !out
        ! Undo the multiplication by windspeed that SFCDIF2 
        ! applies to exchange coefficients CH and CM:
        CH = CH / UR
        CM = CM / UR
        IF(SNOWH > 0.) THEN
          CM = MIN(0.01,CM)   ! CM & CH are too large, causing
          CH = MIN(0.01,CH)   ! computational instability
        END IF

      ENDIF

      RAMB = MAX(1.,1./(CM*UR))
      RAHB = MAX(1.,1./(CH*UR))
      RAWB = RAHB

      ! variables for diagnostics         
      EMB = 1./RAMB
      EHB = 1./RAHB

      ! es and d(es)/dt evaluated at tg
      T = TDC(TGB)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
         ESTG  = ESATW
         DESTG = DSATW
      ELSE
         ESTG  = ESATI
         DESTG = DSATI
      END IF

      CSH = RHOAIR*CPAIR/RAHB
      CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)

      ! surface fluxes and dtg
      IRB = CIR * TGB**4 - EMG*LWDN
      SHB = CSH * (TGB        - SFCTMP      )
      EVB = CEV * (ESTG*RHSUR - EAIR        )
      GHB = CGH * (TGB        - STC(ISNOW+1))

      B   = SAG-IRB-SHB-EVB-GHB+PAHB
      A   = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
      DTG = B/A

      IRB = IRB + 4.*CIR*TGB**3*DTG
      SHB = SHB + CSH*DTG
      EVB = EVB + CEV*DESTG*DTG
      GHB = GHB + CGH*DTG

      ! update ground surface temperature
      TGB = TGB + DTG

      ! for M-O length
      H = CSH * (TGB - SFCTMP)

      T = TDC(TGB)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTG  = ESATW
      ELSE
        ESTG  = ESATI
      END IF
      QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))
      QFX  = (QSFC-QAIR)*CEV*GAMMA/CPAIR

    END DO loop3 ! end stability iteration
    ! -----------------------------------------------------------------

    ! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
    IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
      IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
        IF(OPT_STC == 1) TGB = TFRZ
        IF(OPT_STC == 3) TGB = (1.-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C during melt v3.7
        IRB = CIR * TGB**4 - EMG*LWDN
        SHB = CSH * (TGB        - SFCTMP)
        EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
        GHB = SAG+PAHB - (IRB+SHB+EVB)
      END IF
    END IF

    ! wind stresses
    TAUXB = -RHOAIR*CM*UR*UU
    TAUYB = -RHOAIR*CM*UR*VV

    ! 2m air temperature
    IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
      EHB2  = FV*VKC/LOG((2.+Z0H)/Z0H)
      EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2B  = EHB2
      IF (EHB2.lt.1.E-5 ) THEN
        T2MB  = TGB
        Q2B   = QSFC
      ELSE
        T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
        Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
      ENDIF
      IF (parameters%urban_flag) Q2B = QSFC
    END IF

    ! update CH 
    CH = EHB

  END SUBROUTINE BareFluxMain

  
end module EtFluxModule
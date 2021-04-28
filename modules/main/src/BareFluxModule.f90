! NOTES
!   needs options:  OPT_STC, OPT_SFC

! contains
!   BareFluxMain
!   SFCDIF1, SFCDIF2, ESAT


module BareFluxModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains
  
  !== BareFluxMain ==================================================================================

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
    REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics
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

      IRB   = CIR * TGB**4 - EMG*LWDN
      SHB   = CSH * (TGB        - SFCTMP      )
      EVB   = CEV * (ESTG*RHSUR - EAIR        )
      GHB   = CGH * (TGB        - STC(ISNOW+1))

      B     = SAG-IRB-SHB-EVB-GHB+PAHB
      A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
      DTG   = B/A

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

      QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR

    END DO loop3 ! end stability iteration
    ! -----------------------------------------------------------------

    ! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

    IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
      IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          IF(OPT_STC == 1) TGB = TFRZ
          IF(OPT_STC == 3) TGB  = (1.-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C during melt v3.7
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
  
  
  
  !== begin sfcdif1 ==================================================================================

  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MPE    ,ILOC   ,JLOC   ,                 & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
! -------------------------------------------------------------------------------------------------
! computing surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! inputs
    
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,              INTENT(IN) :: ILOC   !grid index
    INTEGER,              INTENT(IN) :: JLOC   !grid index
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! in & out

    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters

! outputs

    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

! locals
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
! -------------------------------------------------------------------------------------------------
! Monin-Obukhov stability parameter moz for next iteration

    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal("STOP in Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF

! accumulate number of times moz changes sign.

    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF

! evaluate stability-dependent variables using moz from prior iteration
    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2

! 2-meter
       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF

! except for first iteration, weight stability factors for previous
! iteration to help avoid flip-flops from one iteration to the next

    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF

! exchange coefficients

    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
! friction velocity

    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1

!== begin sfcdif2 ==================================================================================

  SUBROUTINE SFCDIF2(parameters,ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !in
                     ZLM    ,ILOC   ,JLOC   ,         & !in
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !in
                     USTAR  )                                   !out

! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR

    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4

    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA

    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
! PAULSON'S SURFACE FUNCTIONS
    PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
         &        +2.* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.* YY
    PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)
    PSPHS (YY)= 5.* YY

! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
    ILECH = 0

! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0

! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)

! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
    ELSE
       IF (RLMO .lt. 0.)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
       END IF

! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ZILITINKEVITCH FIX FOR ZT
       ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       IF(SIMM < 1.e-6) SIMM = 1.e-6        ! Limit stability function
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
       IF(SIMH < 1.e-6) SIMH = 1.e-6        ! Limit stability function
       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
       ELSE
          WSTAR2 = 0.0
       END IF
!-----------------------------------------------------------------------
       RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
       RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
       RLMO = RLMA

!       write(*,'(a20,10f15.6)')'SFCDIF: RLMO=',RLMO,RLMN,ELFC , AKHS , DTHV , USTAR
!    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF2

  
  
  
  !== begin esat =====================================================================================

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
  !---------------------------------------------------------------------------------------------------
  ! use polynomials to calculate saturation vapor pressure and derivative with
  ! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
  IMPLICIT NONE
  !---------------------------------------------------------------------------------------------------
  ! in / out
  REAL, intent(in)  :: T              !temperature
  REAL, intent(out) :: ESW            !saturation vapor pressure over water (pa)
  REAL, intent(out) :: ESI            !saturation vapor pressure over ice (pa)
  REAL, intent(out) :: DESW           !d(esat)/dt over water (pa/K)
  REAL, intent(out) :: DESI           !d(esat)/dt over ice (pa/K)

  ! local
  REAL :: A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
  REAL :: B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
  REAL :: C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
  REAL :: D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT  
  
  
  
end module InterceptionModule
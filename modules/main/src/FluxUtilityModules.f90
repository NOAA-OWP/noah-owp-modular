! Utility subroutines supporting flux routines (eg VegeFlux and BareFlux)
!   contains: RAGRB, STOMATA, CANRES, CALHUM (called by CANRES), [NOT gecros]
!             ESAT, SFCDIF1, SFCDIF2  


module FluxUtilityModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType
  implicit none
  
  !REAL, PARAMETER      :: CP = 1004.5, RD = 287.04, SIGMA = 5.67E-8,    &
  !                        CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
  !                        LSUBF = 3.335E+5

contains
  
  ! == begin RAGRB ====================================================================================
  SUBROUTINE RAGRB(parameters, ITER, VAI, RHOAIR, HG, TAH, ZPD, Z0MG, & ! in
                   Z0HG, HCAN, UC, Z0H, FV, VEGTYP, &         ! in
                   TV, MOZG, FHG, &                                     ! inout
                   RAMG, RAHG, RAWG, RB)                                ! out
    ! --------------------------------------------------------------------------------------------------
    ! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
    ! resistance RB
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type(parameters_type), intent(in)   :: parameters ! parameters data structure
    !INTEGER,              INTENT(IN)    :: ILOC       ! grid index (not used)
    !INTEGER,              INTENT(IN)    :: JLOC       ! grid index (not used)
    INTEGER,              INTENT(IN)    :: ITER       ! iteration index
    INTEGER,              INTENT(IN)    :: VEGTYP     ! vegetation physiology type
    REAL,                 INTENT(IN)    :: VAI        ! total LAI + stem area index, one sided
    REAL,                 INTENT(IN)    :: RHOAIR     ! density air (kg/m3)
    REAL,                 INTENT(IN)    :: HG         ! ground sensible heat flux (w/m2)
    REAL,                 INTENT(IN)    :: TV         ! vegetation temperature (k)
    REAL,                 INTENT(IN)    :: TAH        ! air temperature at height z0h+zpd (k)
    REAL,                 INTENT(IN)    :: ZPD        ! zero plane displacement (m)
    REAL,                 INTENT(IN)    :: Z0MG       ! roughness length, momentum, ground (m)
    REAL,                 INTENT(IN)    :: HCAN       ! canopy height (m) [note: hcan >= z0mg]
    REAL,                 INTENT(IN)    :: UC         ! wind speed at top of canopy (m/s)
    REAL,                 INTENT(IN)    :: Z0H        ! roughness length, sensible heat (m)
    REAL,                 INTENT(IN)    :: Z0HG       ! roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN)    :: FV         ! friction velocity (m/s)
    ! in & out
    REAL,              INTENT(INOUT)    :: MOZG       ! Monin-Obukhov stability parameter
    REAL,              INTENT(INOUT)    :: FHG        ! stability correction
    ! outputs
    REAL,                INTENT(OUT)    :: RAMG       ! aerodynamic resistance for momentum (s/m)
    REAL                 INTENT(OUT)    :: RAHG       ! aerodynamic resistance for sensible heat (s/m)
    REAL                 INTENT(OUT)    :: RAWG       ! aerodynamic resistance for water vapor (s/m)
    REAL                 INTENT(OUT)    :: RB         ! bulk leaf boundary layer resistance (s/m)

    ! local
    REAL :: KH                   ! turbulent transfer coefficient, sensible heat, (m2/s)
    REAL :: TMP1                 ! temporary calculation
    REAL :: TMP2                 ! temporary calculation
    REAL :: TMPRAH2              ! temporary calculation for aerodynamic resistances
    REAL :: TMPRB                ! temporary calculation for rb
    real :: MOLG, FHGNEW, CWPC
    ! --------------------------------------------------------------------------------------------------

    ! stability correction to below canopy resistance
    MOZG = 0.; MOLG = 0.

    IF(ITER > 1) THEN
      TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
      IF (ABS(TMP1) .LE. parameters%MPE) TMP1 = parameters%MPE
      MOLG = -1. * FV**3 / TMP1
      MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
    END IF

    IF (MOZG < 0.) THEN
      FHGNEW  = (1. - 15.*MOZG)**(-0.25)
    ELSE
      FHGNEW  = 1.+ 4.7*MOZG
    ENDIF

    IF (ITER == 1) THEN
      FHG = FHGNEW
    ELSE
      FHG = 0.5 * (FHG+FHGNEW)
    ENDIF

    CWPC = (parameters%CWP * VAI * HCAN * FHG)**0.5
    !CWPC = (CWP*FHG)**0.5

    TMP1 = EXP( -CWPC*Z0HG/HCAN )
    TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
    TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)

    ! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.
    KH = MAX ( VKC*FV*(HCAN-ZPD), parameters%MPE )
    RAMG = 0.
    RAHG = TMPRAH2 / KH
    RAWG = RAHG

    ! leaf boundary layer resistance
    TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
    RB     = TMPRB * SQRT(parameters%DLEAF/UC)
    RB     = MAX(RB,100.0)
    !RB = 200
    
  END SUBROUTINE RAGRB


  ! == begin stomata ==================================================================================

  SUBROUTINE STOMATA (parameters, VEGTYP, APAR, FOLN, TV, EI, EA, & ! in
                      SFCTMP, SFCPRS, O2, CO2, IGS, BTRAN, RB,    & ! in
                      RS, PSN)                                      ! out
    IMPLICIT NONE

    ! --------------------------------------------------------------------------------------------------
    ! input
    type (parameters_type), intent(in) :: parameters
    INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type
    REAL, INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)
    REAL, INTENT(IN)    :: MPE    !prevents division by zero errors
    REAL, INTENT(IN)    :: TV     !foliage temperature (k)
    REAL, INTENT(IN)    :: EI     !vapor pressure inside leaf (sat vapor press at tv) (pa)
    REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
    REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
    REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa) -- partial pressures, from parameters type
    REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
    REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
    REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
    REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
    REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
    REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)
    ! output
    REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
    REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]
    ! inout
    !REAL                :: RLB    !boundary layer resistance (s m2 / umol)

    ! ------------------------ local variables ----------------------------------------------------
    INTEGER :: ITER     !iteration index
    INTEGER :: NITER    !number of iterations
    DATA NITER /3/
    SAVE NITER

    REAL :: AB          !used in statement functions
    REAL :: BC          !used in statement functions
    REAL :: F1          !generic temperature response (statement function)
    REAL :: F2          !generic temperature inhibition (statement function)
    REAL :: TC          !foliage temperature (degree Celsius)
    REAL :: CS          !co2 concentration at leaf surface (pa)
    REAL :: KC          !co2 Michaelis-Menten constant (pa)
    REAL :: KO          !o2 Michaelis-Menten constant (pa)
    REAL :: A,B,C,Q     !intermediate calculations for RS
    REAL :: R1,R2       !roots for RS
    REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
    REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
    REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
    REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
    REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
    REAL :: CP          !co2 compensation point (pa)
    REAL :: CI          !internal co2 (pa)
    REAL :: AWC         !intermediate calculation for wc
    REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
    REAL :: J           !electron transport (umol co2/m2/s)
    REAL :: CEA         !constrain ea or else model blows up
    REAL :: CF          !s m2/umol -> s/m

    F1(AB,BC) = AB**((BC-25.)/10.)
    F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
    REAL :: T
    ! ---------------------------------------------------------------------------------------------

    ! initialize RS=RSMAX and PSN=0 because will only do calculations
    ! for APAR > 0, in which case RS <= RSMAX and PSN >= 0
    CF = SFCPRS/(8.314*SFCTMP)*1.e06
    RS = 1./parameters%BP * CF
    PSN = 0.

    IF (APAR .LE. 0.) RETURN

    FNF = MIN( FOLN/MAX(parameters%MPE,parameters%FOLNMX), 1.0 )
    TC  = TV-TFRZ
    PPF = 4.6*APAR
    J   = PPF*parameters%QE25
    KC  = parameters%KC25 * F1(parameters%AKC,TC)
    KO  = parameters%KO25 * F1(parameters%AKO,TC)
    AWC = KC * (1.+O2/KO)
    CP  = 0.5*KC/KO*O2*0.21
    VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX,TC)

    ! first guess ci
    CI = 0.7*CO2*parameters%C3PSN + 0.4*CO2*(1.-parameters%C3PSN)

    ! rb: s/m -> s m**2 / umol
    RLB = RB/CF

    ! constrain ea
    CEA = MAX(0.25*EI*parameters%C3PSN+0.40*EI*(1.-parameters%C3PSN), MIN(EA,EI) )

    ! ci iteration
    !jref: C3PSN is equal to 1 for all veg types.
    DO ITER = 1, NITER
      WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*parameters%C3PSN  + J*(1.-parameters%C3PSN)
      WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*parameters%C3PSN + VCMX*(1.-parameters%C3PSN)
      WE = 0.5*VCMX*parameters%C3PSN + 4000.*VCMX*CI/SFCPRS*(1.-parameters%C3PSN)
      PSN = MIN(WJ,WC,WE) * IGS

      CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, parameters%MPE )
      A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
      B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.
      C = -RLB
      IF (B .GE. 0.) THEN
        Q = -0.5*( B + SQRT(B*B-4.*A*C) )
      ELSE
        Q = -0.5*( B - SQRT(B*B-4.*A*C) )
      END IF
      R1 = Q/A
      R2 = C/Q
      RS = MAX(R1,R2)
      CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
    END DO 

    ! rs, rb:  s m**2 / umol -> s/m
    RS = RS*CF

  END SUBROUTINE STOMATA

  ! == begin canres ===================================================================================
  SUBROUTINE CANRES (parameters, PAR, TV, BTRAN ,EAH, SFCPRS, & ! in
                        RS , PSN )                              ! out
    ! --------------------------------------------------------------------------------------------------
    ! calculate canopy resistance which depends on incoming solar radiation,
    ! air temperature, atmospheric water vapor pressure deficit at the
    ! lowest model level, and soil moisture (preferably unfrozen soil
    ! moisture rather than total)
    ! --------------------------------------------------------------------------------------------------
    ! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), Jacquemin and
    ! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
    ! eqns 12-14 and table 2 of sec. 3.1.2
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type (parameters_type), intent(in)    :: parameters
    REAL,                     INTENT(IN)  :: PAR    ! par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: TV     ! canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS ! surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    ! water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: RCSOIL ! soil moisture stress factor

    ! outputs
    REAL,                     INTENT(OUT) :: RS     ! canopy resistance per unit LAI
    REAL,                     INTENT(OUT) :: PSN    ! foliage photosynthesis (umolco2/m2/s)

    ! local params
    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     ! water vapor mixing ratio (kg/kg)
    REAL                                  :: Q2SAT  ! saturation Q2
    REAL                                  :: DQSDT2 ! d(Q2SAT)/d(T)  (calculated but not used)

    ! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set in REDPRM
    ! ----------------------------------------------------------------------
    ! initialize canopy resistance multiplier terms.
    ! ----------------------------------------------------------------------
    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

    !  compute Q2 and Q2SAT
    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixing ratio [kg/kg]

    !CALL CALHUM(parameters, SFCTMP, SFCPRS, Q2SAT, DQSDT2) orig
    CALL CALHUM(TV, SFCPRS, Q2SAT, DQSDT2)

    ! contribution due to incoming solar radiation
    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

    ! contribution due to air temperature
    RCT = 1.0- 0.0016* ( (parameters%TOPT - TV)**2.0)
    RCT = MAX (RCT,0.0001)

    ! contribution due to vapor pressure deficit
    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0., Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

    ! determine canopy resistance due to all factors
    RC  = parameters%RSMIN / (RCS * RCT * RCQ * RS)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES


  ! == begin calhum ===================================================================================
  SUBROUTINE CALHUM (TV, SFCPRS, Q2SAT, DQSDT2)
    ! --------------------------------------------------------------------------------------------------
    ! calculate saturated mixing ratio and its derivative with temperature
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !  type (noahmp_parameters), intent(in) :: parameters   (not used)
    REAL, INTENT(IN)       :: TV        ! canopy air temperature
    REAL, INTENT(IN)       :: SFCPRS    ! surface temperature and pressure (from forcing%SFCTMP, forcing%SFCPRS)
    REAL, INTENT(OUT)      :: Q2SAT     ! saturated mixing ratio (g/g)
    REAL, INTENT(OUT)      :: DQSDT2    ! derivative of Q2SAT with Temperature (is not used anywhere in NOAHMP)
    
    ! local
    REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                              A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                              EPSILON=0.622
    REAL                   :: ES, SFCPRSX

    ! Q2SAT: saturated mixing ratio
    ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./TV) )
    ! convert SFCPRS from Pa to KPa
    SFCPRSX = SFCPRS*1.E-3
    Q2SAT = EPSILON * ES / (SFCPRSX-ES)
    
    ! AW: replace the following (mult/div inline instead)
    ! convert from  g/g to g/kg
    !Q2SAT = Q2SAT * 1.E3         ! note: Q2SAT is currently a 'mixing ratio'
    ! DQSDT2 is calculated assuming Q2SAT is a specific humidity
    !DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(TV-A4)**2
    ! DG Q2SAT needs to be in g/g when returned for SFLX
    !Q2SAT = Q2SAT / 1.E3
    
    ! DQSDT2 is calculated using Q2SAT converted to specific humidity (g/kg) from mix. ratio (g/g)
    DQSDT2 = (Q2SAT/1.E3)/(1+(Q2SAT/1.E3)) * A23M4/(TV-A4)**2

  END SUBROUTINE CALHUM
  
  
  ! == begin sfcdif1 ==================================================================================
  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
    ! -------------------------------------------------------------------------------------------------
    ! computing surface drag coefficient CM for momentum and CH for heat
    ! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type (parameters_type), intent(in) :: parameters
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
       IF (ABS(TMP1) .LE. parameters%MPE) TMP1 = parameters%MPE
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
    IF(ABS(CMFM) <= parameters%MPE) CMFM = parameters%MPE
    IF(ABS(CHFH) <= parameters%MPE) CHFH = parameters%MPE
    IF(ABS(CM2FM2) <= parameters%MPE) CM2FM2 = parameters%MPE
    IF(ABS(CH2FH2) <= parameters%MPE) CH2FH2 = parameters%MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
    ! friction velocity
    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1


  ! == begin sfcdif2 ==================================================================================
  SUBROUTINE SFCDIF2(parameters, ITER, Z0, THZ0, THLM, SFCSPD, ZLM, & ! in
                     AKMS, AKHS, RLMO, WSTAR2,                      & ! inout
                     USTAR  )                                         ! out
    ! -------------------------------------------------------------------------------------------------
    ! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
    ! -------------------------------------------------------------------------------------------------
    ! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
    ! SEE CHEN ET AL (1997, BLM)
    ! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    ! in, inout, out
    type (parameters_type), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR

    ! local
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

  
  ! == begin ESAT =====================================================================================
  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
    !---------------------------------------------------------------------------------------------------
    ! use polynomials to calculate saturation vapor pressure and derivative with
    ! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
    !---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! in, out
    REAL, intent(in)  :: T              ! temperature
    REAL, intent(out) :: ESW            ! saturation vapor pressure over water (pa)
    REAL, intent(out) :: ESI            ! saturation vapor pressure over ice (pa)
    REAL, intent(out) :: DESW           ! d(esat)/dt over water (pa/K)
    REAL, intent(out) :: DESI           ! d(esat)/dt over ice (pa/K)

    ! local
    REAL :: A0,A1,A2,A3,A4,A5,A6        ! coefficients for esat over water
    REAL :: B0,B1,B2,B3,B4,B5,B6        ! coefficients for esat over ice
    REAL :: C0,C1,C2,C3,C4,C5,C6        ! coefficients for dsat over water
    REAL :: D0,D1,D2,D3,D4,D5,D6        ! coefficients for dsat over ice

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

  
  
end module FluxUtilityModule
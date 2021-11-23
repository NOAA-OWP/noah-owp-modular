module AlbedoModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains

  !== begin ALBEDO ==================================================================================

  SUBROUTINE ALBEDO (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type), intent(in) :: options

    ! ------------------------ local variables ---------------------------
    INTEGER              :: IB       ! do-loop index
    INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

    REAL                 :: WL       !fraction of LAI+SAI that is LAI
    REAL                 :: WS       !fraction of LAI+SAI that is SAI

    REAL                 :: GDIR     !average projected leaf/stem area in solar direction
    REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area
    ! ----------------------------------------------------------------------

    ! initialize output because solar radiation only done if COSZ > 0
    
    energy%BGAP = 0.0
    energy%WGAP = 0.0
    
    DO IB = 1, parameters%NBAND
      energy%ALBD(IB)   = 0.0
      energy%ALBI(IB)   = 0.0
      energy%ALBGRD(IB) = 0.0
      energy%ALBGRI(IB) = 0.0
      energy%ALBSND(IB) = 0.0
      energy%ALBSNI(IB) = 0.0
      energy%FABD(IB)   = 0.0
      energy%FABI(IB)   = 0.0
      energy%FTDD(IB)   = 0.0
      energy%FTID(IB)   = 0.0
      energy%FTII(IB)   = 0.0
      IF (IB == 1) energy%FSUN = 0.0
    END DO

    ! When COSZ is less then zero (ie sun is down), skip the albedo calculations
    IF(energy%COSZ <= 0) GOTO 100

    ! weight reflectance/transmittance by LAI and SAI
    DO IB = 1, parameters%NBAND
      WL  = parameters%ELAI / MAX(parameters%VAI, parameters%MPE)
      WS  = parameters%ESAI / MAX(parameters%VAI, parameters%MPE)
      energy%RHO(IB) = MAX(parameters%RHOL(IB) * WL + parameters%RHOS(IB) * WS, parameters%MPE)
      energy%TAU(IB) = MAX(parameters%TAUL(IB) * WL + parameters%TAUS(IB) * WS, parameters%MPE)
    END DO
    
    ! Age the snow surface to calculate snow albedo
    CALL SNOW_AGE(domain, parameters, energy, water)
    
    ! snow albedos: only if COSZ > 0 and FSNO > 0
    IF(options%OPT_ALB == 1) &
       CALL SNOWALB_BATS(parameters, energy)
    IF(options%OPT_ALB == 2) THEN
       CALL SNOWALB_CLASS(domain, parameters, energy, water)
       energy%ALBOLD = energy%ALB
    END IF
    
    ! ground surface albedo
    CALL GROUNDALB(domain, parameters, energy, water)
    
    ! loop over parameters%NBAND wavebands to calculate surface albedos and solar
    ! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)
    DO IB = 1, parameters%NBAND
      ! direct shortwave
      IC = 0
      CALL TWOSTREAM (parameters, energy, water, options, GDIR, EXT, IB, IC)
      ! diffuse shortwave
      IC = 1
      CALL TWOSTREAM (parameters, energy, water, options, GDIR, EXT, IB, IC)
    END DO

    ! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.
    EXT = GDIR/energy%COSZ * SQRT(1.-energy%RHO(1)-energy%TAU(1))
    energy%FSUN = (1.-EXP(-EXT*parameters%VAI)) / MAX(EXT*parameters%VAI,parameters%MPE)
    EXT = energy%FSUN

    IF (EXT < 0.01) THEN
      WL = 0.
    ELSE
      WL = EXT
    END IF
    energy%FSUN = WL

    100 CONTINUE
    
  END SUBROUTINE ALBEDO
  
  !== begin SNOW_AGE ==================================================================================

  SUBROUTINE SNOW_AGE (domain, parameters, energy, water)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    domain_type), intent(in) :: domain
    type (    energy_type)             :: energy
    type (     water_type), intent(in) :: water

    ! ------------------------ local variables ---------------------------
    REAL            :: TAGE       !total aging effects
    REAL            :: AGE1       !effects of grain growth due to vapor diffusion
    REAL            :: AGE2       !effects of grain growth at freezing of melt water
    REAL            :: AGE3       !effects of soot
    REAL            :: DELA       !temporary variable
    REAL            :: SGE        !temporary variable
    REAL            :: DELS       !temporary variable
    REAL            :: DELA0      !temporary variable
    REAL            :: ARG        !temporary variable
    ! See Yang et al. (1997) J.of Climate for detail.
    ! ----------------------------------------------------------------------

    ! Age the snow surface when snow present
    IF(water%SNEQV <= 0.0) THEN
      energy%TAUSS = 0.0
    ELSE
      DELA0 = domain%DT / parameters%TAU0
      ARG   = parameters%GRAIN_GROWTH * (1./parameters%TFRZ - 1./energy%TG)
      AGE1  = EXP(ARG)
      AGE2  = EXP(MIN(0., parameters%EXTRA_GROWTH * ARG))
      AGE3  = parameters%DIRT_SOOT
      TAGE  = AGE1 + AGE2 + AGE3
      DELA  = DELA0 * TAGE
      DELS  = MAX(0.0, water%SNEQV - water%SNEQVO) / parameters%SWEMX
      SGE   = (energy%TAUSS + DELA) * (1.0 - DELS)
      energy%TAUSS = MAX(0.,SGE)
    ENDIF

    ! Compute snow age 
    energy%FAGE = energy%TAUSS/(energy%TAUSS + 1.0)
  
  END SUBROUTINE SNOW_AGE
  
  
  !== begin SNOWALB_BATS ==================================================================================

  SUBROUTINE SNOWALB_BATS (parameters, energy)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    energy_type)             :: energy

    ! ------------------------ local variables ---------------------------
    REAL :: FZEN                 ! zenith angle correction
    REAL :: SL1                  ! temporary variable in zenith angle correction
    REAL :: SL2                  ! temporary variable in zenith angle correction
    REAL :: CF1                  ! temporary variable in zenith angle correction
    ! --------------------------------------------------------------------

    ! zero albedos for all points
    energy%ALBSND(1: parameters%NBAND) = 0.
    energy%ALBSNI(1: parameters%NBAND) = 0.

    ! Compute zenith angle correction 
    SL1 = 1.0 / parameters%BATS_COSZ
    SL2 = 2.0 * parameters%BATS_COSZ
    CF1 = ((1.0 + SL1)/(1.0 + SL2 * energy%COSZ) - SL1)
    FZEN = MAX(CF1, 0.0)

    ! Compute snow albedo for diffuse solar radiation (1 = vis, 2 = NIR)
    energy%ALBSNI(1) = parameters%BATS_VIS_NEW * (1. - parameters%BATS_VIS_AGE * energy%FAGE)         
    energy%ALBSNI(2) = parameters%BATS_NIR_NEW * (1. - parameters%BATS_NIR_AGE * energy%FAGE)        
    
    ! Compute snow albedo for direct solar radiation (1 = vis, 2 = NIR)
    energy%ALBSND(1) = energy%ALBSNI(1) + parameters%BATS_VIS_DIR * FZEN * (1. - energy%ALBSNI(1))    !  vis direct
    energy%ALBSND(2) = energy%ALBSNI(2) + parameters%BATS_NIR_DIR * FZEN * (1. - energy%ALBSNI(2))    !  nir direct
    
  END SUBROUTINE SNOWALB_BATS
    
    
  !== begin SNOWALB_CLASS ==================================================================================

  SUBROUTINE SNOWALB_CLASS (domain, parameters, energy, water)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    domain_type), intent(in) :: domain
    type (    energy_type)             :: energy
    type (     water_type), intent(in) :: water

    ! zero albedos for all points
    energy%ALBSND(1: parameters%NBAND) = 0.
    energy%ALBSNI(1: parameters%NBAND) = 0.
    
    ! Compute albedo as function of time and albedo at previous time step
    energy%ALB = 0.55 + (energy%ALBOLD - 0.55) * EXP(-0.01 * domain%DT / 3600.)

    ! Refresh the snow surface albedo when sufficient snow
    IF (water%QSNOW > 0.) then
      energy%ALB = energy%ALB + MIN(water%QSNOW, parameters%SWEMX/domain%DT) *&
                   (0.84 - energy%ALB) / (parameters%SWEMX/domain%DT)
    ENDIF

    ! Set all direct & diffuse (vis & nir) albedos to ALB
    energy%ALBSNI(1) = energy%ALB         ! vis diffuse
    energy%ALBSNI(2) = energy%ALB         ! nir diffuse
    energy%ALBSND(1) = energy%ALB         ! vis direct
    energy%ALBSND(2) = energy%ALB         ! nir direct
    
  END SUBROUTINE SNOWALB_CLASS
    
    
  !== begin GROUNDALB ==================================================================================

  SUBROUTINE GROUNDALB (domain, parameters, energy, water)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    domain_type), intent(in) :: domain
    type (    energy_type)             :: energy
    type (     water_type), intent(in) :: water

    ! ------------------------ local variables ---------------------------
    INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
    REAL                                  :: INC    !soil water correction factor for soil albedo
    REAL                                  :: ALBSOD !soil albedo (direct)
    REAL                                  :: ALBSOI !soil albedo (diffuse)
    ! ----------------------------------------------------------------------
    
    DO IB = 1, parameters%NBAND
      INC = MAX(0.11 - 0.40 * water%SMC(1), 0.)
      IF (domain%IST == 1)  THEN                     !soil
        ALBSOD = MIN(parameters%ALBSAT(IB) + INC, parameters%ALBDRY(IB))
        ALBSOI = ALBSOD
      ELSE IF (energy%TG > parameters%TFRZ) THEN               !unfrozen lake, wetland
        ALBSOD = 0.06 / (MAX(0.01, energy%COSZ)**1.7 + 0.15)
        ALBSOI = 0.06
      ELSE                                      !frozen lake, wetland
        ALBSOD = parameters%ALBLAK(IB)
        ALBSOI = ALBSOD
      END IF
      
      ! Compute surface as function of bare ground and snow albedo, weighted by FSNO
      energy%ALBGRD(IB) = ALBSOD * (1. - water%FSNO) + energy%ALBSND(IB) * water%FSNO
      energy%ALBGRI(IB) = ALBSOI * (1. - water%FSNO) + energy%ALBSNI(IB) * water%FSNO

    END DO

  END SUBROUTINE GROUNDALB
  
  !== begin TWOSTREAM ==================================================================================

  SUBROUTINE TWOSTREAM (parameters, energy, water, options, GDIR, EXT, IB, IC)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    energy_type)             :: energy
    type (     water_type), intent(in) :: water
    type (   options_type), intent(in) :: options
    
    INTEGER,                intent(in) :: IB     ! Index of NBAND (1 = vis, 2 = NIR)
    INTEGER,                intent(in) :: IC     ! Direct = 0; diffuse = 1
    
    REAL,                  intent(out) :: GDIR   ! average projected leaf/stem area in solar direction
    REAL,                  intent(out) :: EXT    ! optical depth direct beam per unit leaf + stem area

    
    ! ------------------------ local variables ---------------------------
    REAL                              :: OMEGA   !fraction of intercepted radiation that is scattered
    REAL                              :: OMEGAL  !omega for leaves
    REAL                              :: BETAI   !upscatter parameter for diffuse radiation
    REAL                              :: BETAIL  !betai for leaves
    REAL                              :: BETAD   !upscatter parameter for direct beam radiation
    REAL                              :: BETADL  !betad for leaves
    REAL                              :: AVMU    !average diffuse optical depth

    REAL                              :: COSZI   !0.001 <= cosz <= 1.000
    REAL                              :: ASU     !single scattering albedo
    REAL                              :: CHIL    ! -0.4 <= xl <= 0.6

    REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
    REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
    REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
    REAL                              :: PHI1,PHI2,SIGMA
    REAL                              :: FTDS,FTIS,FRES
    REAL                              :: DENFVEG
    REAL                              :: VAI_SPREAD
    REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
    REAL                              :: THETAZ
    
    !  variables for the modified two-stream scheme
    !  Niu and Yang (2004), JGR

    REAL, PARAMETER :: PAI = 3.14159265 
    REAL :: HD       !crown depth (m)
    REAL :: BB       !vertical crown radius (m)
    REAL :: THETAP   !angle conversion from SZA 
    REAL :: FA       !foliage volume density (m-1)
    REAL :: NEWVAI   !effective LSAI (-)
    REAL :: KOPEN    !gap fraction for diffue light (-)
    REAL :: GAP      !total gap fraction for beam ( <=1-shafac )
    ! ----------------------------------------------------------------------
    ! compute within and between gaps
    VAI_SPREAD = parameters%VAI
    IF(parameters%VAI == 0.0) THEN
      GAP     = 1.0
      KOPEN   = 1.0
    ELSE
      IF(options%OPT_RAD == 1) THEN
        DENFVEG = -LOG(MAX(1.0 - parameters%FVEG, 0.01)) / (PAI * parameters%RC**2)
        HD      = parameters%HVT - parameters%HVB
        BB      = 0.5 * HD
        THETAP  = ATAN(BB / parameters%RC * TAN(ACOS(MAX(0.01, energy%COSZ))) )
        energy%BGAP = EXP(-DENFVEG * PAI * parameters%RC**2/COS(THETAP) )
        FA      = parameters%VAI/(1.33 * PAI * parameters%RC**3.0 *(BB/parameters%RC)*DENFVEG)
        NEWVAI  = HD * FA
        energy%WGAP    = (1.0 - energy%BGAP) * EXP(-0.5*NEWVAI/energy%COSZ)
        GAP     = MIN(1.0 - parameters%FVEG, energy%BGAP + energy%WGAP)
        KOPEN   = 0.05
      END IF
      IF(options%OPT_RAD == 2) THEN
        GAP     = 0.0
        KOPEN   = 0.0
      END IF
      IF(options%OPT_RAD == 3) THEN
        GAP     = 1.0 - parameters%FVEG
        KOPEN   = 1.0 - parameters%FVEG
      END IF
    END IF

    ! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
    ! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
    ! and OMEGA*BETAI are calculated and then divided by the new OMEGA
    ! because the product OMEGA*BETAI, OMEGA*BETAD is used in solution.
    ! also, the transmittances and reflectances (TAU, RHO) are linear
    ! weights of leaf and stem values.

    COSZI  = MAX(0.001, energy%COSZ)
    CHIL   = MIN( MAX(parameters%XL, -0.4), 0.6)
    IF (ABS(CHIL) <= 0.01) CHIL = 0.01
    PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
    PHI2   = 0.877 * (1.-2.*PHI1)
    GDIR   = PHI1 + PHI2*COSZI
    EXT    = GDIR/COSZI
    AVMU   = ( 1. - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
    OMEGAL = energy%RHO(IB) + energy%TAU(IB)
    TMP0   = GDIR + PHI2*COSZI
    TMP1   = PHI1*COSZI
    ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
    BETADL = (1.+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
    BETAIL = 0.5 * ( energy%RHO(IB)+energy%TAU(IB) + (energy%RHO(IB)-energy%TAU(IB)) &
           * ((1.+CHIL)/2.)**2 ) / OMEGAL

    ! adjust omega, betad, and betai for intercepted snow
    ! TO DO: update this logic as TV > TFRZ doesn't necessarily mean the canopy is snow-free
    ! KSJ 2021-04-19
    IF (energy%TV > parameters%TFRZ) THEN                                !no snow
       TMP0 = OMEGAL
       TMP1 = BETADL
       TMP2 = BETAIL
    ELSE
       TMP0 =   (1.-water%FWET)*OMEGAL            +&
                water%FWET * parameters%OMEGAS(IB)
       TMP1 = ( (1.-water%FWET) * OMEGAL * BETADL +&
                water%FWET*parameters%OMEGAS(IB)*parameters%BETADS ) / TMP0
       TMP2 = ( (1.-water%FWET) * OMEGAL * BETAIL +&
                water%FWET*parameters%OMEGAS(IB)*parameters%BETAIS ) / TMP0
    END IF

    OMEGA = TMP0
    BETAD = TMP1
    BETAI = TMP2

    ! absorbed, reflected, transmitted fluxes per unit incoming radiation
    B = 1. - OMEGA + OMEGA*BETAI
    C = OMEGA*BETAI
    TMP0 = AVMU*EXT
    D = TMP0 * OMEGA*BETAD
    F = TMP0 * OMEGA*(1.-BETAD)
    TMP1 = B*B - C*C
    H = SQRT(TMP1) / AVMU
    SIGMA = TMP0*TMP0 - TMP1
    if ( ABS (SIGMA) < 1.e-6 ) SIGMA = SIGN(1.e-6,SIGMA)
    P1 = B + AVMU*H
    P2 = B - AVMU*H
    P3 = B + TMP0
    P4 = B - TMP0
    S1 = EXP(-H * parameters%VAI)
    S2 = EXP(-EXT * parameters%VAI)
    IF (IC == 0) THEN        ! direct
       U1 = B - C/energy%ALBGRD(IB)
       U2 = B - C*energy%ALBGRD(IB)
       U3 = F + C*energy%ALBGRD(IB)
    ELSE                     ! diffuse
       U1 = B - C/energy%ALBGRI(IB)
       U2 = B - C*energy%ALBGRI(IB)
       U3 = F + C*energy%ALBGRI(IB)
    END IF
    TMP2 = U1 - AVMU*H
    TMP3 = U1 + AVMU*H
    D1 = P1*TMP2/S1 - P2*TMP3*S1
    TMP4 = U2 + AVMU*H
    TMP5 = U2 - AVMU*H
    D2 = TMP4/S1 - TMP5*S1
    H1 = -D*P4 - C*F
    TMP6 = D - H1*P3/SIGMA
    TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
    H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
    H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
    H4 = -F*P3 - C*D
    TMP8 = H4/SIGMA
    TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
    H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
    H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
    H7 = (C*TMP2) / (D1*S1)
    H8 = (-C*TMP3*S1) / D1
    H9 = TMP4 / (D2*S1)
    H10 = (-TMP5*S1) / D2

    !------------ Change below --------------
    ! All of the suffixed variables are now defined
    ! as part of their type instead of being passed
    ! KSJ 2021-04-18
    
    ! downward direct and diffuse fluxes below vegetation
    ! Niu and Yang (2004), JGR.
    IF (IC == 0) THEN              ! direct
       FTDS = S2                           *(1.0-GAP) + GAP
       FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
       energy%FTDD(IB) = FTDS
       energy%FTID(IB) = FTIS
    ELSE
       FTDS = 0.
       FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
       energy%FTDI(IB) = FTDS
       energy%FTII(IB) = FTIS
    END IF

    ! flux reflected by the surface (veg. and ground)
    IF (IC == 0) THEN
      FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + energy%ALBGRD(IB)*GAP
      FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  )
      FREBAR = energy%ALBGRD(IB)*GAP          !jref - separate veg. and ground reflection
      energy%ALBD(IB) = FRES
      energy%FREVD(IB) = FREVEG
      energy%FREGD(IB) = FREBAR
      ! Flux absorbed by radiation
      energy%FABD(IB) = 1. - energy%ALBD(IB) - (1.-energy%ALBGRD(IB))*energy%FTDD(IB) &
                              - (1.-energy%ALBGRI(IB))*energy%FTID(IB)
    ELSE
      FRES   = (H7 + H8) *(1.0-KOPEN) + energy%ALBGRI(IB)*KOPEN
      FREVEG = (H7 + H8) *(1.0-KOPEN) + energy%ALBGRI(IB)*KOPEN
      FREBAR = 0                                !jref - separate veg. and ground reflection
      energy%ALBI(IB) = FRES
      energy%FREVI(IB) = FREVEG
      energy%FREGI(IB) = FREBAR
      ! Flux absorbed by radiation
      energy%FABI(IB) = 1. - energy%ALBI(IB) - (1.-energy%ALBGRD(IB))*energy%FTDI(IB) &
                              - (1.-energy%ALBGRI(IB))*energy%FTII(IB)
    END IF

  END SUBROUTINE TWOSTREAM
    
end module AlbedoModule
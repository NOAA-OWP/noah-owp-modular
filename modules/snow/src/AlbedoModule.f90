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
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    INTEGER              :: IB       ! do-loop index
    INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

    REAL                 :: ALB


    REAL                 :: WL       !fraction of LAI+SAI that is LAI
    REAL                 :: WS       !fraction of LAI+SAI that is SAI
    REAL                 :: MPE      !prevents overflow for division by zero

    REAL, DIMENSION(1:2) :: FTDI     !down direct flux below veg per unit dif flux = 0
    REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSND   !snow albedo (direct)
    REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSNI   !snow albedo (diffuse)

    REAL                 :: VAI      !ELAI+ESAI
    REAL                 :: GDIR     !average projected leaf/stem area in solar direction
    REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area
    ! ----------------------------------------------------------------------
    
    ! Set MPE to prevent overlfow when otherwise you would be dividing by zero
    MPE = 1.E-06

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
      WL  = parameters%ELAI / MAX(parameters%VAI, MPE)
      WS  = parameters%ESAI / MAX(parameters%VAI, MPE)
      energy%RHO(IB) = MAX(parameters%RHOL(IB) * WL + parameters%RHOS(IB) * WS, MPE)
      energy%TAU(IB) = MAX(parameters%TAUL(IB) * WL + parameters%TAUS(IB) * WS, MPE)
    END DO
    
    ! Age the snow surface to calculate snow albedo
    CALL SNOW_AGE(domain, parameters, energy, water)
    
    ! snow albedos: only if COSZ > 0 and FSNO > 0
    IF(options%OPT_ALB == 1) &
       CALL SNOWALB_BATS(domain, levels, options, parameters, forcing, energy, water)
    IF(options%OPT_ALB == 2) THEN
       CALL SNOWALB_CLASS(domain, levels, options, parameters, forcing, energy, water)
       ALBOLD = ALB
    END IF
    
    ! ground surface albedo
    CALL GROUNDALB
    
    ! loop over parameters%NBAND wavebands to calculate surface albedos and solar
    ! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)
    DO IB = 1, parameters%NBAND
      ! direct shortwave
      IC = 0
      CALL TWOSTREAM 
      ! diffuse shortwave
      IC = 1
      CALL TWOSTREAM
    END DO
      

    ! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.
    EXT = GDIR/COSZ * SQRT(1.-RHO(1)-TAU(1))
    FSUN = (1.-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
    EXT = FSUN

    IF (EXT < 0.01) THEN
      WL = 0.
    ELSE
      WL = EXT 
    END IF
    FSUN = WL

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
      DELS  = (water%QSNOW * domain%dt) / parameters%SWEMX
      SGE   = (energy%TAUSS + DELA) * (1.0 - DELS)
      energy%TAUSS = MAX(0.,SGE)
    ENDIF

    ! Compute snow age 
    energy%FAGE = energy%TAUSS/(energy%TAUSS + 1.0)
  
  END SUBROUTINE SNOW_AGE
  
  
  !== begin SNOWALB_BATS ==================================================================================

  SUBROUTINE SNOWALB_BATS (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    ! ----------------------------------------------------------------------
    
  END SUBROUTINE SNOWALB_BATS
    
    
  !== begin SNOWALB_CLASS ==================================================================================

  SUBROUTINE SNOWALB_CLASS (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    ! ----------------------------------------------------------------------
    
  END SUBROUTINE SNOWALB_CLASS
    
end module AlbedoModule
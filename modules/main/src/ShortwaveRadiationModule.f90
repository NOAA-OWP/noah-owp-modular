module ShortwaveRadiationModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType
  use AlbedoModule

  implicit none

contains

  !== begin ShortwaveRadiationMain, formerly RADIATION ===========================================

  SUBROUTINE ShortwaveRadiationMain (domain, levels, options, parameters, forcing, energy, water)
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
    
    ! Compute albedo for all surfaces (e.g., snow, bare ground, veg)
    CALL ALBEDO(domain, levels, options, parameters, forcing, energy, water)
    
    ! Compute FSHA, LAISUN, and LAISHA terms
    energy%FSHA = 1.0 - energy%FSUN
    energy%LAISUN = parameters%ELAI * energy%FSUN
    energy%LAISHA = parameters%ELAI * energy%FSHA

    ! Compute net solar radiation
    CALL NetSolarRadiation(domain, levels, options, parameters, forcing, energy, water)
    print*, "Combined SW flux = ", energy%SAV + energy%SAG + energy%FSRV + energy%FSRG

  END SUBROUTINE ShortwaveRadiationMain
  
  !== begin NetSolarRadiation, formerly SURRAD ===========================================

  SUBROUTINE NetSolarRadiation (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    INTEGER                          :: IB      ! do-loop index for waveband (1=vis, 2=nir)

    REAL                             :: ABS     !absorbed solar radiation (w/m2)
    REAL                             :: RNIR    !reflected solar radiation [nir] (w/m2)
    REAL                             :: RVIS    !reflected solar radiation [vis] (w/m2)
    REAL                             :: LAIFRA  !leaf area fraction of canopy
    REAL                             :: TRD     !transmitted solar radiation: direct (w/m2)
    REAL                             :: TRI     !transmitted solar radiation: diffuse (w/m2)
    REAL, DIMENSION(1:2)             :: CAD     !direct beam absorbed by canopy (w/m2)
    REAL, DIMENSION(1:2)             :: CAI     !diffuse radiation absorbed by canopy (w/m2)
    ! ----------------------------------------------------------------------

    ! Zero out the solar fluxes
    energy%SAG = 0.
    energy%SAV = 0.
    energy%FSA = 0.

    ! loop over the vis and nir bands
    DO IB = 1, parameters%NBAND

      ! absorbed by canopy
      CAD(IB) = forcing%SOLAD(IB) * energy%FABD(IB)    
      CAI(IB) = forcing%SOLAI(IB) * energy%FABI(IB)
      energy%SAV = energy%SAV + CAD(IB) + CAI(IB)
      energy%FSA = energy%FSA + CAD(IB) + CAI(IB)
 
      ! transmitted solar fluxes incident on ground
      TRD = forcing%SOLAD(IB) * energy%FTDD(IB)
      TRI = forcing%SOLAD(IB) * energy%FTID(IB) + forcing%SOLAI(IB) * energy%FTII(IB)

      ! solar radiation absorbed by ground surface
      ABS = TRD * (1.-energy%ALBGRD(IB)) + TRI * (1.-energy%ALBGRI(IB))
      energy%SAG = energy%SAG + ABS
      energy%FSA = energy%FSA + ABS
    END DO

    ! partition visible canopy absorption to sunlit and shaded fractions
    ! to get average absorbed par for sunlit and shaded leaves
    LAIFRA = parameters%ELAI / MAX(parameters%VAI, parameters%MPE)
    IF (energy%FSUN > 0.0) THEN
      energy%PARSUN = (CAD(1) + energy%FSUN * CAI(1)) * LAIFRA / MAX(energy%LAISUN, parameters%MPE)
      energy%PARSHA = (energy%FSHA * CAI(1)) * LAIFRA / MAX(energy%LAISHA, parameters%MPE)
    ELSE
      energy%PARSUN = 0.
      energy%PARSHA = (CAD(1) + CAI(1)) * LAIFRA / MAX(energy%LAISHA, parameters%MPE)
    ENDIF
    
         
    ! reflected solar radiation
    RVIS = energy%ALBD(1) * forcing%SOLAD(1) + energy%ALBI(1) * forcing%SOLAI(1)
    RNIR = energy%ALBD(2) * forcing%SOLAD(2) + energy%ALBI(2) * forcing%SOLAI(2)
    energy%FSR = RVIS + RNIR

    ! reflected solar radiation of veg. and ground (combined ground)
    energy%FSRV = energy%FREVD(1) * forcing%SOLAD(1) + energy%FREVI(1) * forcing%SOLAI(1) + &
                  energy%FREVD(2) * forcing%SOLAD(2) + energy%FREVI(2) * forcing%SOLAI(2)
    energy%FSRG = energy%FREGD(1) * forcing%SOLAD(1) + energy%FREGI(1) * forcing%SOLAI(1) + &
                  energy%FREGD(2) * forcing%SOLAD(2) + energy%FREGI(2) * forcing%SOLAI(2)

  END SUBROUTINE NetSolarRadiation
  
end module ShortwaveRadiationModule
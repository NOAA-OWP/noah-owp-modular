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

  !== begin RADIATION ==================================================================================

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
    ! surface abeldo

    CALL ALBEDO(domain, levels, options, parameters, forcing, energy, water)
    
    ! surface radiation

    FSHA = 1.-FSUN
    LAISUN = ELAI*FSUN
    LAISHA = ELAI*FSHA
    VAI = ELAI+ ESAI
    IF (VAI > 0.) THEN
      VEG = .TRUE.
    ELSE
      VEG = .FALSE.
    END IF

       CALL SURRAD (parameters,MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !in
                    LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !in
                    FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !in
                    ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !in
                    PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !out
                    FSR    ,                                 & !out
                    FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !inout
                    FSRG)


  END SUBROUTINE ShortwaveRadiationMain
  
end module ShortwaveRadiationModule
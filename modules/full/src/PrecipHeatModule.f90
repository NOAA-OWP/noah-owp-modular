module PrecipHeatModule

  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType

  implicit none

contains

  !== begin PRECIP_HEAT ==================================================================================

  SUBROUTINE PRECIP_HEAT (parameters, forcing, energy, water)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    energy_type)             :: energy
    type (     water_type), intent(in) :: water
    type (   forcing_type), intent(in) :: forcing

    ! ------------------------ local variables ---------------------------
    real    :: PAH_AC  ! precipitation advected heat - air to canopy (W/m2)
    real    :: PAH_CG  ! precipitation advected heat - canopy to ground (W/m2)
    real    :: PAH_AG  ! precipitation advected heat - air to ground (W/m2)
    ! ----------------------------------------------------------------------

    PAH_AC  = 0.
    PAH_CG  = 0.
    PAH_AG  = 0.
    energy%PAHV    = 0.
    energy%PAHG    = 0.
    energy%PAHB    = 0.
    energy%PAH     = 0.


    ! --------------------------- liquid water ------------------------------
    ! heat transported by liquid water

    PAH_AC = parameters%FVEG * water%RAIN * (parameters%CWAT/1000.0) * (forcing%SFCTMP - energy%TV)
    PAH_CG = water%QDRIPR * (parameters%CWAT/1000.0) * (energy%TV - energy%TG)
    PAH_AG = water%QTHROR * (parameters%CWAT/1000.0) * (forcing%SFCTMP - energy%TG)

    ! --------------------------- canopy ice ------------------------------
    ! heat transported by snow/ice

    PAH_AC = PAH_AC + parameters%FVEG * water%SNOW * (parameters%CICE/1000.0) * (forcing%SFCTMP - energy%TV)
    PAH_CG = PAH_CG + water%QDRIPS * (parameters%CICE/1000.0) * (energy%TV - energy%TG)
    PAH_AG = PAH_AG + water%QTHROS * (parameters%CICE/1000.0) * (forcing%SFCTMP - energy%TG)

    energy%PAHV = PAH_AC - PAH_CG
    energy%PAHG = PAH_CG
    energy%PAHB = PAH_AG

    IF (parameters%FVEG > 0.0 .AND. parameters%FVEG < 1.0) THEN
      energy%PAHG = energy%PAHG / parameters%FVEG         ! these will be multiplied by fraction later
    	energy%PAHB = energy%PAHB / (1.0 - parameters%FVEG)
    ELSEIF (parameters%FVEG <= 0.0) THEN
      energy%PAHB = energy%PAHG + energy%PAHB         ! for case of canopy getting buried
      energy%PAHG = 0.0
      energy%PAHV = 0.0
    ELSEIF (parameters%FVEG >= 1.0) THEN
      energy%PAHB = 0.0
    END IF

    energy%PAHV = MAX(energy%PAHV,-20.0)       ! Put some artificial limits here for stability
    energy%PAHV = MIN(energy%PAHV,20.0)
    energy%PAHG = MAX(energy%PAHG,-20.0)
    energy%PAHG = MIN(energy%PAHG,20.0)
    energy%PAHB = MAX(energy%PAHB,-20.0)
    energy%PAHB = MIN(energy%PAHB,20.0)

  END SUBROUTINE PRECIP_HEAT

end module PrecipHeatModule
module SoilWaterModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  
  implicit none
  
contains

!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (levels, options, parameters, water)

! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (    levels_type), intent(in) :: levels
  type (   options_type), intent(in) :: options
  type (parameters_type), intent(in) :: parameters
  type (     water_type)             :: water

! local
  INTEGER   :: K              ! do-loop index
  REAL      :: SICE_SMC_RATIO ! Ratio of frozen soil to total water content
! ----------------------------------------------------------------------

! Loop through each soil level and check SMC is constant
! Preserve the ratios of SICE:SMC and SH2O:SMC
    DO K = 1,levels%nsoil
      IF (water%SMC(K) /= water%SMC_INIT(K)) THEN
        SICE_SMC_RATIO = water%SICE(K) / water%SMC(K)
        water%SMC(K)   = water%SMC_INIT(K)
        water%SICE(K)  = water%SMC(K) * SICE_SMC_RATIO
        water%SH2O(K)  = water%SMC(K) - water%SICE(K)
      ENDIF
    END DO
    
    ! Check to ensure ZWT is equal to initial water table height
    IF (water%ZWT /= parameters%ZWT_INIT) water%ZWT = parameters%ZWT_INIT

  END SUBROUTINE SOILWATER

end module SoilWaterModule

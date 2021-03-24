module ForcingModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use SoilWaterModule
  use CanopyWaterModule
  use SnowWaterModule
  use AtmProcessing

  implicit none

contains

!== begin energy subroutine ================================================================================

  SUBROUTINE ForcingMain (domain, levels, options, parameters, forcing, energy, water)
!---------------------------------------------------------------------
! Main module for all water components
!---------------------------------------------------------------------

  type (levels_type),     intent(in)   :: levels
  type (domain_type)                   :: domain
  type (parameters_type)               :: parameters
  type (options_type),    intent(in)   :: options
  type (water_type)                    :: water
  type (forcing_type),    intent(in)   :: forcing 
  type (energy_type)                   :: energy


	  ! Add local variables as needed
	  ! KJ 2021-03-24
! ------------------------ local variables ---------------------------

!---------------------------------------------------------------------

    ! Call the ATM subroutine to process other variables such as
    ! air density and precipitation phase
    call ATM(domain, levels, options, parameters, forcing, energy, water)


  END SUBROUTINE ForcingMain   

end module ForcingModule

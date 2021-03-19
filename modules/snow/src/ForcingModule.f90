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



	  ! NEED TO CHANGE FROM HERE DOWN
	  ! KJ 2021-03-18



! ------------------------ local variables ---------------------------
  real    :: dtheta_max = 0.0   ! maximum value of theta change in all levels
  real    :: totalwat   = 0.0   ! total soil water [mm]
  real    :: tw0        = 0.0   ! initial total soil water [mm]
  real    :: acsrf      = 0.0   ! accumulated surface runoff [mm]
  real    :: acsub      = 0.0   ! accumulated drainage [mm]
  real    :: acpcp      = 0.0   ! accumulated precipitation [mm]
  real    :: errwat     = 0.0   ! accumulated error [mm]
  logical :: done               ! logical check
  integer :: IZ 
  real, dimension(1:levels%soil) :: smcold        !previous timestep smc
!---------------------------------------------------------------------

! C

  END SUBROUTINE ForcingMain   

end module ForcingModule

module NoahowpType
  
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use NoahowpGridTypeModule
  
  implicit none

  type :: noahowp_type

    type(levels_type)     :: levels
    type(domain_type)     :: domain
    type(options_type)    :: options
    type(parameters_type) :: parameters
    type(water_type)      :: water
    type(forcing_type)    :: forcing
    type(energy_type)     :: energy

    contains

    procedure, public  :: Init
    
  end type noahowp_type

  contains   

  subroutine Init(this,noahowpgrid)

    class(noahowp_type)                   :: this
    type(noahowpgrid_type), intent(in)    :: noahowpgrid

    call this%domain%Init(noahowpgrid)
    call this%options%Init()
    call this%parameters%Init(noahowpgrid)
    call this%levels%Init()
    call this%water%Init(noahowpgrid)
    call this%forcing%Init()
    call this%energy%Init(noahowpgrid)

  end subroutine Init

end module NoahowpType
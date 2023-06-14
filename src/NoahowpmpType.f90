module NoahowpmpType
  
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use NoahowpmpIOType
  
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

  subroutine Init(this,NoahowpmpIO)

    class(noahowp_type)                   :: this
    type(NoahowpmpIO_type), intent(in)    :: NoahowpmpIO

    call this%domain%Init(NoahowpmpIO)
    call this%options%Init()
    call this%parameters%Init(NoahowpmpIO)
    call this%levels%Init()
    call this%water%Init(NoahowpmpIO)
    call this%forcing%Init()
    call this%energy%Init(NoahowpmpIO)

  end subroutine Init

end module NoahowpmpType
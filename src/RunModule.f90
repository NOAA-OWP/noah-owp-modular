! module for executing Noah-OWP-Modular model in a streamlined way

module RunModule
  
  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use AsciiReadModule
  use OutputModule
  use UtilitiesModule
  use ForcingModule
  use InterceptionModule
  use EnergyModule
  use WaterModule
  use DateTimeUtilsModule
  use NoahowpType
  
  implicit none

contains
  
  SUBROUTINE solve_noahowp(noahowp)
    type (noahowp_type), intent (inout) :: noahowp
    integer, parameter                  :: iunit        = 10 ! Fortran unit number to attach to the opened file
    integer                             :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    integer                             :: ierr              ! error code for reading forcing data
    integer                             :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  ! current UNIX timestep details

    associate(levels     => noahowp%levels, &
              domain     => noahowp%domain, &
              options    => noahowp%options, &
              parameters => noahowp%parameters, &
              water      => noahowp%water, &
              forcing    => noahowp%forcing, &
              energy     => noahowp%energy)
    
    !---------------------------------------------------------------------
    ! call the main utility routines
    !---------------------------------------------------------------------
    call UtilitiesMain (domain%itime, domain, forcing, energy)

    !---------------------------------------------------------------------
    ! call the main forcing routines
    !---------------------------------------------------------------------
    call ForcingMain (options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main interception routines
    !---------------------------------------------------------------------
    call InterceptionMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main energy balance routines
    !---------------------------------------------------------------------
    call EnergyMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! call the main water routines (canopy + snow + soil water components)
    !---------------------------------------------------------------------
    call WaterMain (domain, levels, options, parameters, forcing, energy, water)

    !---------------------------------------------------------------------
    ! add to output file
    ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
    ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
    !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    call add_to_output(domain, water, energy, forcing, domain%itime, levels%nsoil,levels%nsnow)
#endif
    
    end associate ! terminate associate block
  END SUBROUTINE solve_noahowp

end module RunModule

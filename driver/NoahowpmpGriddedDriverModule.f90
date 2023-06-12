module NoahowpmpGriddedDriverModule
  
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
  use NoahowpmpType
  use NoahowpmpIOType

  use DomainVarInTransferModule
  
  implicit none

contains

  SUBROUTINE cleanup(model)
    implicit none
    type(noahowp_type), intent(inout) :: model
      
      !---------------------------------------------------------------------
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call finalize_output()
#endif
  
  END SUBROUTINE cleanup

  SUBROUTINE GriddedDriverMain(NoahowpmpIO,Noahowpmp)

    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp   
    integer                               :: ix, iy

    forcing_timestep = NoahowpmpIO%dt
    #ifndef NGEN_FORCING_ACTIVE
        call read_forcing_text(iunit, NoahowpmpIO%nowdate, forcing_timestep, &
             read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS, read_SOLDN, read_LWDN, read_PRCP, ierr)
        NoahowpmpIO%UU(:,:) = read_UU
        NoahowpmpIO%VV(:,:) = read_VV
        NoahowpmpIO%SFCTMP(:,:) = read_SFCTMP
        NoahowpmpIO%Q2(:,:) = read_Q2
        NoahowpmpIO%SFCPRS(:,:) = read_SFCPRS
        NoahowpmpIO%SOLDN(:,:) = read_SOLDN
        NoahowpmpIO%LWDN(:,:) = read_LWDN
        NoahowpmpIO%PRCP(:,:) = read_PRCP
        NoahowpmpIO%UU(:,:) = read_UU
    #endif
       
    model%domain%itime    = model%domain%itime + 1 ! increment the integer time by 1
    model%domain%time_dbl = dble(model%domain%time_dbl + model%domain%dt) ! increment model time in seconds by DT

    do ix = 1, NoahowpmpIO%n_x
      do iy = 1, NoahowpmpIO%n_y

        NoahowpmpIO%ix = ix
        NoahowpmpIO%iy = iy
        call DomainVarInTransfer(Noahowpmp, NoahowpmpIO)
        call EnergyVarInTransfer(Noahowpmp, NoahowpmpIO)
        call ForcingVarInTransfer(Noahowpmp, NoahowpmpIO)
        call OptionsVarInTransfer(Noahowpmp, NoahowpmpIO)
        call ParametersVarInTransfer(Noahowpmp, NoahowpmpIO)
        call WaterVarInTransfer(Noahowpmp, NoahowpmpIO)
        
        call solve_noahowp(Noahowpmp)

      end do
    end do

  END SUBROUTINE GriddedDriverMain

  

end module NoahowpmpGriddedDriverModule
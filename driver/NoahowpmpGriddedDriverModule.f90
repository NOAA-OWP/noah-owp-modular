module NoahowpmpGriddedDriverModule
  
  use AsciiReadModule
  use RunModule
  use NoahowpmpType
  use NoahowpmpIOType

  use DomainVarTransferModule
  use EnergyVarTransferModule
  use ForcingVarTransferModule
  use OptionsVarTransferModule
  use ParametersVarTransferModule
  use WaterVarTransferModule
  use LevelsVarTransferModule
  
  implicit none

contains 

  SUBROUTINE GriddedDriverMain(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type)                    :: Noahowpmp   
    integer                               :: ix, iy, ierr
    integer                               :: iunit = 10
    real                                  :: read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS
    real                                  :: read_SOLDN, read_LWDN, read_PRCP
    integer                               :: idt


#ifndef NGEN_FORCING_ACTIVE
    idt = NoahowpmpIO%itime * (NoahowpmpIO%dt / 60)
    call geth_newdate(NoahowpmpIO%startdate, idt, NoahowpmpIO%nowdate)
    call read_forcing_text(iunit, NoahowpmpIO%nowdate, int(NoahowpmpIO%dt), &
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
       
    call Noahowpmp%Init(NoahowpmpIO)
    do ix = 1, NoahowpmpIO%n_x
      do iy = 1, NoahowpmpIO%n_y

        NoahowpmpIO%ix = ix
        NoahowpmpIO%iy = iy
        call DomainVarInTransfer(Noahowpmp, NoahowpmpIO)
        call LevelsVarInTransfer(Noahowpmp, NoahowpmpIO)
        call EnergyVarInTransfer(Noahowpmp, NoahowpmpIO)
        call ForcingVarInTransfer(Noahowpmp, NoahowpmpIO)
        call OptionsVarInTransfer(Noahowpmp, NoahowpmpIO)
        call ParametersVarInTransfer(Noahowpmp, NoahowpmpIO)
        call WaterVarInTransfer(Noahowpmp, NoahowpmpIO)
        
        call solve_noahowp(Noahowpmp)

        call DomainVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call LevelsVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call EnergyVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call ForcingVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call OptionsVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call ParametersVarOutTransfer(Noahowpmp, NoahowpmpIO)
        call WaterVarOutTransfer(Noahowpmp, NoahowpmpIO)

      end do
    end do

    !Increment time
    NoahowpmpIO%itime    = NoahowpmpIO%itime + 1 ! increment the integer time by 1
    NoahowpmpIO%time_dbl = dble(NoahowpmpIO%time_dbl + NoahowpmpIO%dt) ! increment model time in seconds by DT

  END SUBROUTINE GriddedDriverMain

end module NoahowpmpGriddedDriverModule
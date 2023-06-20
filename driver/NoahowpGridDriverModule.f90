module NoahowpGridDriverModule
  
  use AsciiReadModule
  use RunModule
  use NoahowpType
  use NoahowpGridTypeModule
  use DomainVarTransferModule
  use EnergyVarTransferModule
  use ForcingVarTransferModule
  use OptionsVarTransferModule
  use ParametersVarTransferModule
  use WaterVarTransferModule
  use LevelsVarTransferModule
  
  implicit none

contains 

  SUBROUTINE NoahowpGridDriverMain(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type)                    :: noahowp                                             !local instance 
    integer                               :: ix, iy, ierr
    integer                               :: iunit = 10
    real                                  :: read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS !to read in forcing
    real                                  :: read_SOLDN, read_LWDN, read_PRCP                    !to read in forcing
    integer                               :: idt                                                 !to iterate nowdate

#ifndef NGEN_FORCING_ACTIVE

    !ifndef NGEN_FORCING_ACTIVE, read forcings for time step, else then 
    !forcings must be updated/set via bmi function calls prior to call 
    !to bmi_update/NoahowpGridDriverMain

    !Update noahowpgrid%nowdate 
    !I think this needs to be done here rather than in UtilitiesMain if NGEN isn't doing forcing
    !idt = noahowpgrid%itime * (noahowpgrid%dt / 60)
    !call geth_newdate(noahowpgrid%startdate, idt, noahowpgrid%nowdate)

    !Read for forcings for nowdate
    call read_forcing_text(iunit, noahowpgrid%nowdate, int(noahowpgrid%dt), &
          read_UU, read_VV, read_SFCTMP, read_Q2, read_SFCPRS, read_SOLDN, read_LWDN, read_PRCP, ierr)

    !Give read-in forcings to all grid cells
    noahowpgrid%UU(:,:)     = read_UU
    noahowpgrid%VV(:,:)     = read_VV
    noahowpgrid%SFCTMP(:,:) = read_SFCTMP
    noahowpgrid%Q2(:,:)     = read_Q2
    noahowpgrid%SFCPRS(:,:) = read_SFCPRS
    noahowpgrid%SOLDN(:,:)  = read_SOLDN
    noahowpgrid%LWDN(:,:)   = read_LWDN
    noahowpgrid%PRCP(:,:)   = read_PRCP
    noahowpgrid%UU(:,:)     = read_UU

#endif
       
    !Initialize local instance of noahowp_type (noahowpgrid is needed to set z dim length. Or we could just pass nsoil and nsnow)
    call noahowp%Init(noahowpgrid)

    !Iterate over x and y dimensions
    do ix = 1, noahowpgrid%n_x
      do iy = 1, noahowpgrid%n_y

        !Give ix and iy to noahowp_type
        noahowpgrid%ix = ix
        noahowpgrid%iy = iy

        !Transfer variable values from noahowpgrid_type to noahowp_type
        call DomainVarInTransfer       (noahowp, noahowpgrid)
        call LevelsVarInTransfer       (noahowp, noahowpgrid)
        call EnergyVarInTransfer       (noahowp, noahowpgrid)
        call ForcingVarInTransfer      (noahowp, noahowpgrid)
        call OptionsVarInTransfer      (noahowp, noahowpgrid)
        call ParametersVarInTransfer   (noahowp, noahowpgrid)
        call WaterVarInTransfer        (noahowp, noahowpgrid)
        
        !Execute column model
        call solve_noahowp             (noahowp)

        !Transfer variable values from noahowp_type back to noahowpgrid_type
        call DomainVarOutTransfer      (noahowp, noahowpgrid)
        call LevelsVarOutTransfer      (noahowp, noahowpgrid)
        call EnergyVarOutTransfer      (noahowp, noahowpgrid)
        call ForcingVarOutTransfer     (noahowp, noahowpgrid)
        call OptionsVarOutTransfer     (noahowp, noahowpgrid)
        call ParametersVarOutTransfer  (noahowp, noahowpgrid)
        call WaterVarOutTransfer       (noahowp, noahowpgrid)

      end do
    end do

    !Advance time
    noahowpgrid%itime    = noahowpgrid%itime + 1 ! increment the integer time by 1
    noahowpgrid%time_dbl = dble(noahowpgrid%time_dbl + noahowpgrid%dt) ! increment model time in seconds by DT

  END SUBROUTINE NoahowpGridDriverMain

end module NoahowpGridDriverModule
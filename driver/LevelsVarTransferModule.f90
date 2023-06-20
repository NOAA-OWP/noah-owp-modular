module LevelsVarTransferModule

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine LevelsVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(in)    :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowp%levels%nsoil = noahowpgrid%nsoil
    noahowp%levels%nsnow = noahowpgrid%nsnow
    noahowp%levels%nveg = noahowpgrid%nveg
            
    end associate

  end subroutine LevelsVarInTransfer

  subroutine LevelsVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(in)    :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    ! Nothing to do      
    
    end associate

  end subroutine LevelsVarOutTransfer

end module LevelsVarTransferModule
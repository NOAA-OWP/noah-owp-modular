module LevelsVarTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine LevelsVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(in)    :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%levels%nsoil = NoahowpmpIO%nsoil
    noahowpmp%levels%nsnow = NoahowpmpIO%nsnow
    noahowpmp%levels%nveg = NoahowpmpIO%nveg
            
    end associate

  end subroutine LevelsVarInTransfer

  subroutine LevelsVarOutTransfer(Noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    end associate

  end subroutine LevelsVarOutTransfer

end module LevelsVarTransferModule
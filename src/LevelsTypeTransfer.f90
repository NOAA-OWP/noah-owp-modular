module LevelsTypeTransfer

    use LevelsType
    use LevelsGridType

    implicit none
  
  contains
  
  subroutine LevelsVarInTransfer(levels, levelsgrid, ix, iy)

    implicit none

    type(levels_type),     intent(inout) :: levels
    type(levelsgrid_type), intent(in)    :: levelsgrid
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    levels%nsoil = levelsgrid%nsoil
    levels%nsnow = levelsgrid%nsnow
    levels%nveg = levelsgrid%nveg

  end subroutine LevelsVarInTransfer

  subroutine LevelsVarOutTransfer(levels, levelsgrid, ix, iy)

    implicit none

    type(levels_type),     intent(in)    :: levels
    type(levelsgrid_type), intent(inout) :: levelsgrid
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    ! Nothing to do      

  end subroutine LevelsVarOutTransfer

end module LevelsTypeTransfer
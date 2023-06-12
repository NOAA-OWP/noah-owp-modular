module OptionsVarTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine OptionsVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)
  
    noahowpmp%options%opt_snf   = NoahowpmpIO%opt_snf(ix,iy)
    noahowpmp%options%opt_run   = NoahowpmpIO%opt_run(ix,iy)
    noahowpmp%options%opt_drn   = NoahowpmpIO%opt_drn(ix,iy)
    noahowpmp%options%opt_inf   = NoahowpmpIO%opt_inf(ix,iy)
    noahowpmp%options%opt_infdv = NoahowpmpIO%opt_infdv(ix,iy)
    noahowpmp%options%dveg      = NoahowpmpIO%dveg(ix,iy)
    noahowpmp%options%opt_alb   = NoahowpmpIO%opt_alb(ix,iy)
    noahowpmp%options%opt_rad   = NoahowpmpIO%opt_rad(ix,iy)
    noahowpmp%options%opt_sfc   = NoahowpmpIO%opt_sfc(ix,iy)
    noahowpmp%options%opt_crs   = NoahowpmpIO%opt_crs(ix,iy)
    noahowpmp%options%opt_crop  = NoahowpmpIO%opt_crop(ix,iy)
    noahowpmp%options%opt_stc   = NoahowpmpIO%opt_stc(ix,iy)
    noahowpmp%options%opt_tbot  = NoahowpmpIO%opt_tbot(ix,iy)
    noahowpmp%options%opt_frz   = NoahowpmpIO%opt_frz(ix,iy)
    noahowpmp%options%opt_btr   = NoahowpmpIO%opt_btr(ix,iy)
    noahowpmp%options%opt_rsf   = NoahowpmpIO%opt_rsf(ix,iy)
    noahowpmp%options%opt_sub   = NoahowpmpIO%opt_sub(ix,iy)
          
    end associate

  end subroutine OptionsVarInTransfer

  subroutine OptionsVarOutTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)
  
    NoahowpmpIO%opt_snf(ix,iy) = noahowpmp%options%opt_snf
    NoahowpmpIO%opt_run(ix,iy) = noahowpmp%options%opt_run
    NoahowpmpIO%opt_drn(ix,iy) = noahowpmp%options%opt_drn
    NoahowpmpIO%opt_inf(ix,iy) = noahowpmp%options%opt_inf
    NoahowpmpIO%opt_infdv(ix,iy) = noahowpmp%options%opt_infdv
    NoahowpmpIO%dveg(ix,iy) = noahowpmp%options%dveg
    NoahowpmpIO%opt_alb(ix,iy) = noahowpmp%options%opt_alb
    NoahowpmpIO%opt_rad(ix,iy) = noahowpmp%options%opt_rad
    NoahowpmpIO%opt_sfc(ix,iy) = noahowpmp%options%opt_sfc
    NoahowpmpIO%opt_crs(ix,iy) = noahowpmp%options%opt_crs
    NoahowpmpIO%opt_crop(ix,iy) = noahowpmp%options%opt_crop
    NoahowpmpIO%opt_stc(ix,iy) = noahowpmp%options%opt_stc
    NoahowpmpIO%opt_tbot(ix,iy) = noahowpmp%options%opt_tbot
    NoahowpmpIO%opt_frz(ix,iy) = noahowpmp%options%opt_frz
    NoahowpmpIO%opt_btr(ix,iy) = noahowpmp%options%opt_btr
    NoahowpmpIO%opt_rsf(ix,iy) = noahowpmp%options%opt_rsf
    NoahowpmpIO%opt_sub(ix,iy) = noahowpmp%options%opt_sub
          
    end associate

  end subroutine OptionsVarOutTransfer

end module OptionsVarTransferModule
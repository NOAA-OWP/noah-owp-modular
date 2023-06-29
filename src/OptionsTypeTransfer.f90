module OptionsTypeTransfer

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine OptionsVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(in)    :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)
  
    noahowp%options%opt_snf   = noahowpgrid%opt_snf(ix,iy)
    noahowp%options%opt_run   = noahowpgrid%opt_run(ix,iy)
    noahowp%options%opt_drn   = noahowpgrid%opt_drn(ix,iy)
    noahowp%options%opt_inf   = noahowpgrid%opt_inf(ix,iy)
    noahowp%options%opt_infdv = noahowpgrid%opt_infdv(ix,iy)
    noahowp%options%dveg      = noahowpgrid%dveg(ix,iy)
    noahowp%options%opt_alb   = noahowpgrid%opt_alb(ix,iy)
    noahowp%options%opt_rad   = noahowpgrid%opt_rad(ix,iy)
    noahowp%options%opt_sfc   = noahowpgrid%opt_sfc(ix,iy)
    noahowp%options%opt_crs   = noahowpgrid%opt_crs(ix,iy)
    noahowp%options%opt_crop  = noahowpgrid%opt_crop(ix,iy)
    noahowp%options%opt_stc   = noahowpgrid%opt_stc(ix,iy)
    noahowp%options%opt_tbot  = noahowpgrid%opt_tbot(ix,iy)
    noahowp%options%opt_frz   = noahowpgrid%opt_frz(ix,iy)
    noahowp%options%opt_btr   = noahowpgrid%opt_btr(ix,iy)
    noahowp%options%opt_rsf   = noahowpgrid%opt_rsf(ix,iy)
    noahowp%options%opt_sub   = noahowpgrid%opt_sub(ix,iy)
          
    end associate

  end subroutine OptionsVarInTransfer

  subroutine OptionsVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(in)    :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)
  
    noahowpgrid%opt_snf(ix,iy) = noahowp%options%opt_snf
    noahowpgrid%opt_run(ix,iy) = noahowp%options%opt_run
    noahowpgrid%opt_drn(ix,iy) = noahowp%options%opt_drn
    noahowpgrid%opt_inf(ix,iy) = noahowp%options%opt_inf
    noahowpgrid%opt_infdv(ix,iy) = noahowp%options%opt_infdv
    noahowpgrid%dveg(ix,iy) = noahowp%options%dveg
    noahowpgrid%opt_alb(ix,iy) = noahowp%options%opt_alb
    noahowpgrid%opt_rad(ix,iy) = noahowp%options%opt_rad
    noahowpgrid%opt_sfc(ix,iy) = noahowp%options%opt_sfc
    noahowpgrid%opt_crs(ix,iy) = noahowp%options%opt_crs
    noahowpgrid%opt_crop(ix,iy) = noahowp%options%opt_crop
    noahowpgrid%opt_stc(ix,iy) = noahowp%options%opt_stc
    noahowpgrid%opt_tbot(ix,iy) = noahowp%options%opt_tbot
    noahowpgrid%opt_frz(ix,iy) = noahowp%options%opt_frz
    noahowpgrid%opt_btr(ix,iy) = noahowp%options%opt_btr
    noahowpgrid%opt_rsf(ix,iy) = noahowp%options%opt_rsf
    noahowpgrid%opt_sub(ix,iy) = noahowp%options%opt_sub
          
    end associate

  end subroutine OptionsVarOutTransfer

end module OptionsTypeTransfer
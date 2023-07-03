module OptionsTypeTransfer

    use OptionsType
    use OptionsGridType

    implicit none
  
  contains
  
  subroutine OptionsVarInTransfer(options, optionsgrid, ix, iy)

    implicit none

    type(options_type),         intent(inout)    :: options
    type(optionsgrid_type),     intent(in)       :: optionsgrid
    integer,                    intent(in)       :: ix
    integer,                    intent(in)       :: iy
  
    options%opt_snf   = optionsgrid%opt_snf(ix,iy)
    options%opt_run   = optionsgrid%opt_run(ix,iy)
    options%opt_drn   = optionsgrid%opt_drn(ix,iy)
    options%opt_inf   = optionsgrid%opt_inf(ix,iy)
    options%opt_infdv = optionsgrid%opt_infdv(ix,iy)
    options%dveg      = optionsgrid%dveg(ix,iy)
    options%opt_alb   = optionsgrid%opt_alb(ix,iy)
    options%opt_rad   = optionsgrid%opt_rad(ix,iy)
    options%opt_sfc   = optionsgrid%opt_sfc(ix,iy)
    options%opt_crs   = optionsgrid%opt_crs(ix,iy)
    options%opt_crop  = optionsgrid%opt_crop(ix,iy)
    options%opt_stc   = optionsgrid%opt_stc(ix,iy)
    options%opt_tbot  = optionsgrid%opt_tbot(ix,iy)
    options%opt_frz   = optionsgrid%opt_frz(ix,iy)
    options%opt_btr   = optionsgrid%opt_btr(ix,iy)
    options%opt_rsf   = optionsgrid%opt_rsf(ix,iy)
    options%opt_sub   = optionsgrid%opt_sub(ix,iy)

  end subroutine OptionsVarInTransfer

  subroutine OptionsVarOutTransfer(noahowp, optionsgrid)

    implicit none

    type(options_type),         intent(in)       :: options
    type(optionsgrid_type),     intent(inout)    :: optionsgrid
    integer,                    intent(in)       :: ix
    integer,                    intent(in)       :: iy
  
    ! Nothing to do

  end subroutine OptionsVarOutTransfer

end module OptionsTypeTransfer
module OptionsTypeTransfer

    use OptionsType
    use OptionsGridType

    implicit none
  
  contains
  
  subroutine OptionsVarInTransfer(options, optionsgrid)

    implicit none

    type(options_type),         intent(inout)    :: options
    type(optionsgrid_type),     intent(in)       :: optionsgrid
  
    options%opt_snf   = optionsgrid%opt_snf
    options%opt_run   = optionsgrid%opt_run
    options%opt_drn   = optionsgrid%opt_drn
    options%opt_inf   = optionsgrid%opt_inf
    options%opt_infdv = optionsgrid%opt_infdv
    options%dveg      = optionsgrid%dveg
    options%opt_alb   = optionsgrid%opt_alb
    options%opt_rad   = optionsgrid%opt_rad
    options%opt_sfc   = optionsgrid%opt_sfc
    options%opt_crs   = optionsgrid%opt_crs
    options%opt_crop  = optionsgrid%opt_crop
    options%opt_stc   = optionsgrid%opt_stc
    options%opt_tbot  = optionsgrid%opt_tbot
    options%opt_frz   = optionsgrid%opt_frz
    options%opt_btr   = optionsgrid%opt_btr
    options%opt_rsf   = optionsgrid%opt_rsf
    options%opt_sub   = optionsgrid%opt_sub

  end subroutine OptionsVarInTransfer

  subroutine OptionsVarOutTransfer(options, optionsgrid)

    implicit none

    type(options_type),         intent(in)       :: options
    type(optionsgrid_type),     intent(inout)    :: optionsgrid
  
    ! Nothing to do

  end subroutine OptionsVarOutTransfer

end module OptionsTypeTransfer
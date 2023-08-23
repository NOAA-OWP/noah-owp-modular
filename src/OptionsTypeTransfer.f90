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
  
    ! Nothing to do

  end subroutine OptionsVarInTransfer

  subroutine OptionsVarOutTransfer(options, optionsgrid, ix, iy)

    implicit none

    type(options_type),         intent(in)       :: options
    type(optionsgrid_type),     intent(inout)    :: optionsgrid
    integer,                    intent(in)       :: ix
    integer,                    intent(in)       :: iy
  
    ! Nothing to do

  end subroutine OptionsVarOutTransfer

end module OptionsTypeTransfer
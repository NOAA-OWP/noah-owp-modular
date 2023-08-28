module ForcingTypeTransfer

    use ForcingType
    use ForcingGridType

    implicit none
  
  contains
  
  subroutine ForcingVarInTransfer(forcing, forcinggrid, ix, iy)

    implicit none

    type(forcinggrid_type), intent(in)    :: forcinggrid
    type(forcing_type),     intent(inout) :: forcing
    integer,                intent(in)    :: ix
    integer,                intent(in)    :: iy

    forcing%SFCPRS = forcinggrid%SFCPRS(ix,iy)
    forcing%SFCTMP = forcinggrid%SFCTMP(ix,iy)
    forcing%Q2 = forcinggrid%Q2(ix,iy)
    forcing%PRCP = forcinggrid%PRCP(ix,iy)
    forcing%PRCPCONV = forcinggrid%PRCPCONV(ix,iy)
    forcing%PRCPNONC = forcinggrid%PRCPNONC(ix,iy)
    forcing%PRCPSHCV = forcinggrid%PRCPSHCV(ix,iy)
    forcing%PRCPSNOW = forcinggrid%PRCPSNOW(ix,iy)
    forcing%PRCPGRPL = forcinggrid%PRCPGRPL(ix,iy)
    forcing%PRCPHAIL = forcinggrid%PRCPHAIL(ix,iy)            
    forcing%SOLDN = forcinggrid%SOLDN(ix,iy)
    forcing%LWDN = forcinggrid%LWDN(ix,iy)
    forcing%FOLN = forcinggrid%FOLN(ix,iy)
    forcing%O2PP = forcinggrid%O2PP(ix,iy)
    forcing%CO2PP = forcinggrid%CO2PP(ix,iy) 
    forcing%UU = forcinggrid%UU(ix,iy)
    forcing%VV = forcinggrid%VV(ix,iy)
    forcing%TBOT = forcinggrid%TBOT(ix,iy)
    forcing%UR = forcinggrid%UR(ix,iy)
    forcing%THAIR = forcinggrid%THAIR(ix,iy)
    forcing%QAIR = forcinggrid%QAIR(ix,iy)
    forcing%EAIR = forcinggrid%EAIR(ix,iy)
    forcing%RHOAIR = forcinggrid%RHOAIR(ix,iy)
    forcing%FPICE = forcinggrid%FPICE(ix,iy)
    forcing%SWDOWN = forcinggrid%SWDOWN(ix,iy)
    forcing%SOLAD(:) = forcinggrid%SOLAD(ix,iy,:)
    forcing%SOLAI(:) = forcinggrid%SOLAI(ix,iy,:)

  end subroutine

  subroutine ForcingVarOutTransfer(forcing, forcinggrid, ix, iy)

    implicit none

    type(forcinggrid_type), intent(inout)    :: forcinggrid
    type(forcing_type),     intent(in)       :: forcing
    integer,                intent(in)       :: ix
    integer,                intent(in)       :: iy

    !Nothing to do

  end subroutine ForcingVarOutTransfer

end module ForcingTypeTransfer
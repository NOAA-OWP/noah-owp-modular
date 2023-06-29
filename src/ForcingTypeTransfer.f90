module ForcingTypeTransfer

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine ForcingVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(in)    :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowp%forcing%SFCPRS = noahowpgrid%SFCPRS(ix,iy)
    noahowp%forcing%SFCTMP = noahowpgrid%SFCTMP(ix,iy)
    noahowp%forcing%Q2 = noahowpgrid%Q2(ix,iy)
    noahowp%forcing%PRCP = noahowpgrid%PRCP(ix,iy)
    noahowp%forcing%PRCPCONV = noahowpgrid%PRCPCONV(ix,iy)
    noahowp%forcing%PRCPNONC = noahowpgrid%PRCPNONC(ix,iy)
    noahowp%forcing%PRCPSHCV = noahowpgrid%PRCPSHCV(ix,iy)
    noahowp%forcing%PRCPSNOW = noahowpgrid%PRCPSNOW(ix,iy)
    noahowp%forcing%PRCPGRPL = noahowpgrid%PRCPGRPL(ix,iy)
    noahowp%forcing%PRCPHAIL = noahowpgrid%PRCPHAIL(ix,iy)            
    noahowp%forcing%SOLDN = noahowpgrid%SOLDN(ix,iy)
    noahowp%forcing%LWDN = noahowpgrid%LWDN(ix,iy)
    noahowp%forcing%FOLN = noahowpgrid%FOLN(ix,iy)
    noahowp%forcing%O2PP = noahowpgrid%O2PP(ix,iy)
    noahowp%forcing%CO2PP = noahowpgrid%CO2PP(ix,iy) 
    noahowp%forcing%UU = noahowpgrid%UU(ix,iy)
    noahowp%forcing%VV = noahowpgrid%VV(ix,iy)
    noahowp%forcing%TBOT = noahowpgrid%TBOT(ix,iy)
    noahowp%forcing%UR = noahowpgrid%UR(ix,iy)
    noahowp%forcing%THAIR = noahowpgrid%THAIR(ix,iy)
    noahowp%forcing%QAIR = noahowpgrid%QAIR(ix,iy)
    noahowp%forcing%EAIR = noahowpgrid%EAIR(ix,iy)
    noahowp%forcing%RHOAIR = noahowpgrid%RHOAIR(ix,iy)
    noahowp%forcing%FPICE = noahowpgrid%FPICE(ix,iy)
    noahowp%forcing%SWDOWN = noahowpgrid%SWDOWN(ix,iy)
    noahowp%forcing%JULIAN = noahowpgrid%JULIAN
    noahowp%forcing%YEARLEN = noahowpgrid%YEARLEN
    noahowp%forcing%SOLAD(:) = noahowpgrid%SOLAD(ix,iy,:)
    noahowp%forcing%SOLAI(:) = noahowpgrid%SOLAI(ix,iy,:)

    end associate

  end subroutine

  subroutine ForcingVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(in)    :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowpgrid%SFCPRS(ix,iy) = noahowp%forcing%SFCPRS
    noahowpgrid%SFCTMP(ix,iy) = noahowp%forcing%SFCTMP
    noahowpgrid%Q2(ix,iy) = noahowp%forcing%Q2
    noahowpgrid%PRCP(ix,iy) = noahowp%forcing%PRCP
    noahowpgrid%PRCPCONV(ix,iy) = noahowp%forcing%PRCPCONV
    noahowpgrid%PRCPNONC(ix,iy) = noahowp%forcing%PRCPNONC
    noahowpgrid%PRCPSHCV(ix,iy) = noahowp%forcing%PRCPSHCV
    noahowpgrid%PRCPSNOW(ix,iy) = noahowp%forcing%PRCPSNOW
    noahowpgrid%PRCPGRPL(ix,iy) = noahowp%forcing%PRCPGRPL
    noahowpgrid%PRCPHAIL(ix,iy) = noahowp%forcing%PRCPHAIL
    noahowpgrid%SOLDN(ix,iy) = noahowp%forcing%SOLDN
    noahowpgrid%LWDN(ix,iy) = noahowp%forcing%LWDN
    noahowpgrid%FOLN(ix,iy) = noahowp%forcing%FOLN
    noahowpgrid%O2PP(ix,iy) = noahowp%forcing%O2PP
    noahowpgrid%CO2PP(ix,iy) = noahowp%forcing%CO2PP
    noahowpgrid%UU(ix,iy) = noahowp%forcing%UU
    noahowpgrid%VV(ix,iy) = noahowp%forcing%VV
    noahowpgrid%TBOT(ix,iy) = noahowp%forcing%TBOT
    noahowpgrid%UR(ix,iy) = noahowp%forcing%UR
    noahowpgrid%THAIR(ix,iy) = noahowp%forcing%THAIR
    noahowpgrid%QAIR(ix,iy) = noahowp%forcing%QAIR
    noahowpgrid%EAIR(ix,iy) = noahowp%forcing%EAIR
    noahowpgrid%RHOAIR(ix,iy) = noahowp%forcing%RHOAIR
    noahowpgrid%FPICE(ix,iy) = noahowp%forcing%FPICE
    noahowpgrid%SWDOWN(ix,iy) = noahowp%forcing%SWDOWN
    noahowpgrid%SOLAD(ix,iy,:) = noahowp%forcing%SOLAD(:)
    noahowpgrid%SOLAI(ix,iy,:) = noahowp%forcing%SOLAI(:)

    end associate

  end subroutine ForcingVarOutTransfer

end module ForcingTypeTransfer
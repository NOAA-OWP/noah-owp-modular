module ForcingVarTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine ForcingVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%forcing%SFCPRS = NoahowpmpIO%SFCPRS(ix,iy)
    noahowpmp%forcing%SFCTMP = NoahowpmpIO%SFCTMP(ix,iy)
    noahowpmp%forcing%Q2 = NoahowpmpIO%Q2(ix,iy)
    noahowpmp%forcing%PRCP = NoahowpmpIO%PRCP(ix,iy)
    noahowpmp%forcing%PRCPCONV = NoahowpmpIO%PRCPCONV(ix,iy)
    noahowpmp%forcing%PRCPNONC = NoahowpmpIO%PRCPNONC(ix,iy)
    noahowpmp%forcing%PRCPSHCV = NoahowpmpIO%PRCPSHCV(ix,iy)
    noahowpmp%forcing%PRCPSNOW = NoahowpmpIO%PRCPSNOW(ix,iy)
    noahowpmp%forcing%PRCPGRPL = NoahowpmpIO%PRCPGRPL(ix,iy)
    noahowpmp%forcing%PRCPHAIL = NoahowpmpIO%PRCPHAIL(ix,iy)            
    noahowpmp%forcing%SOLDN = NoahowpmpIO%SOLDN(ix,iy)
    noahowpmp%forcing%LWDN = NoahowpmpIO%LWDN(ix,iy)
    noahowpmp%forcing%FOLN = NoahowpmpIO%FOLN(ix,iy)
    noahowpmp%forcing%O2PP = NoahowpmpIO%O2PP(ix,iy)
    noahowpmp%forcing%CO2PP = NoahowpmpIO%CO2PP(ix,iy) 
    noahowpmp%forcing%UU = NoahowpmpIO%UU(ix,iy)
    noahowpmp%forcing%VV = NoahowpmpIO%VV(ix,iy)
    noahowpmp%forcing%TBOT = NoahowpmpIO%TBOT(ix,iy)
    noahowpmp%forcing%UR = NoahowpmpIO%UR(ix,iy)
    noahowpmp%forcing%THAIR = NoahowpmpIO%THAIR(ix,iy)
    noahowpmp%forcing%QAIR = NoahowpmpIO%QAIR(ix,iy)
    noahowpmp%forcing%EAIR = NoahowpmpIO%EAIR(ix,iy)
    noahowpmp%forcing%RHOAIR = NoahowpmpIO%RHOAIR(ix,iy)
    noahowpmp%forcing%FPICE = NoahowpmpIO%FPICE(ix,iy)
    noahowpmp%forcing%SWDOWN = NoahowpmpIO%SWDOWN(ix,iy)
    noahowpmp%forcing%JULIAN = NoahowpmpIO%JULIAN
    noahowpmp%forcing%YEARLEN = NoahowpmpIO%YEARLEN
    noahowpmp%forcing%SOLAD(:) = NoahowpmpIO%SOLAD(ix,iy,:)
    noahowpmp%forcing%SOLAI(:) = NoahowpmpIO%SOLAI(ix,iy,:)

    end associate

  end subroutine

  subroutine ForcingVarOutTransfer(Noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    NoahowpmpIO%SFCPRS(ix,iy) = Noahowpmp%forcing%SFCPRS
    NoahowpmpIO%SFCTMP(ix,iy) = Noahowpmp%forcing%SFCTMP
    NoahowpmpIO%Q2(ix,iy) = Noahowpmp%forcing%Q2
    NoahowpmpIO%PRCP(ix,iy) = Noahowpmp%forcing%PRCP
    NoahowpmpIO%PRCPCONV(ix,iy) = Noahowpmp%forcing%PRCPCONV
    NoahowpmpIO%PRCPNONC(ix,iy) = Noahowpmp%forcing%PRCPNONC
    NoahowpmpIO%PRCPSHCV(ix,iy) = Noahowpmp%forcing%PRCPSHCV
    NoahowpmpIO%PRCPSNOW(ix,iy) = Noahowpmp%forcing%PRCPSNOW
    NoahowpmpIO%PRCPGRPL(ix,iy) = Noahowpmp%forcing%PRCPGRPL
    NoahowpmpIO%PRCPHAIL(ix,iy) = Noahowpmp%forcing%PRCPHAIL
    NoahowpmpIO%SOLDN(ix,iy) = Noahowpmp%forcing%SOLDN
    NoahowpmpIO%LWDN(ix,iy) = Noahowpmp%forcing%LWDN
    NoahowpmpIO%FOLN(ix,iy) = Noahowpmp%forcing%FOLN
    NoahowpmpIO%O2PP(ix,iy) = Noahowpmp%forcing%O2PP
    NoahowpmpIO%CO2PP(ix,iy) = Noahowpmp%forcing%CO2PP
    NoahowpmpIO%UU(ix,iy) = Noahowpmp%forcing%UU
    NoahowpmpIO%VV(ix,iy) = Noahowpmp%forcing%VV
    NoahowpmpIO%TBOT(ix,iy) = Noahowpmp%forcing%TBOT
    NoahowpmpIO%UR(ix,iy) = Noahowpmp%forcing%UR
    NoahowpmpIO%THAIR(ix,iy) = Noahowpmp%forcing%THAIR
    NoahowpmpIO%QAIR(ix,iy) = Noahowpmp%forcing%QAIR
    NoahowpmpIO%EAIR(ix,iy) = Noahowpmp%forcing%EAIR
    NoahowpmpIO%RHOAIR(ix,iy) = Noahowpmp%forcing%RHOAIR
    NoahowpmpIO%FPICE(ix,iy) = Noahowpmp%forcing%FPICE
    NoahowpmpIO%SWDOWN(ix,iy) = Noahowpmp%forcing%SWDOWN
    NoahowpmpIO%SOLAD(ix,iy,:) = Noahowpmp%forcing%SOLAD(:)
    NoahowpmpIO%SOLAI(ix,iy,:) = Noahowpmp%forcing%SOLAI(:)

    end associate

  end subroutine ForcingVarOutTransfer

end module ForcingVarTransferModule
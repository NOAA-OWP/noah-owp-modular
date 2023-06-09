module ForcingVarInTransferModule

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

end module ForcingVarInTransferModule
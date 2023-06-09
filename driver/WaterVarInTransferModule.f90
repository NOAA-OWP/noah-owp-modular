module WaterVarInTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine WaterVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%water%qinsur = NoahowpmpIO%qinsur(ix,iy)
    noahowpmp%water%qseva = NoahowpmpIO%qseva(ix,iy)
    noahowpmp%water%EVAPOTRANS = NoahowpmpIO%EVAPOTRANS(ix,iy)
    noahowpmp%water%runsrf = NoahowpmpIO%runsrf(ix,iy)
    noahowpmp%water%runsub = NoahowpmpIO%runsub(ix,iy)
    noahowpmp%water%qdrain = NoahowpmpIO%qdrain(ix,iy)
    noahowpmp%water%zwt = NoahowpmpIO%zwt(ix,iy)
    noahowpmp%water%smcwtd = NoahowpmpIO%smcwtd(ix,iy)
    noahowpmp%water%deeprech = NoahowpmpIO%deeprech(ix,iy)
    noahowpmp%water%fcrmax = NoahowpmpIO%fcrmax(ix,iy)
    noahowpmp%water%snoflow = NoahowpmpIO%snoflow(ix,iy)
    noahowpmp%water%pddum = NoahowpmpIO%pddum(ix,iy)
    noahowpmp%water%FACC = NoahowpmpIO%FACC(ix,iy)
    noahowpmp%water%sicemax = NoahowpmpIO%sicemax(ix,iy)
    noahowpmp%water%FB_snow = NoahowpmpIO%FB_snow(ix,iy)
    noahowpmp%water%rain = NoahowpmpIO%rain(ix,iy)
    noahowpmp%water%snow = NoahowpmpIO%snow(ix,iy)
    noahowpmp%water%bdfall = NoahowpmpIO%bdfall(ix,iy)
    noahowpmp%water%FP = NoahowpmpIO%FP(ix,iy)
    noahowpmp%water%canliq = NoahowpmpIO%canliq(ix,iy)
    noahowpmp%water%canice = NoahowpmpIO%canice(ix,iy)
    noahowpmp%water%FWET = NoahowpmpIO%FWET(ix,iy)
    noahowpmp%water%CMC = NoahowpmpIO%CMC(ix,iy)
    noahowpmp%water%QINTR = NoahowpmpIO%QINTR(ix,iy)
    noahowpmp%water%QDRIPR = NoahowpmpIO%QDRIPR(ix,iy)
    noahowpmp%water%QTHROR = NoahowpmpIO%QTHROR(ix,iy)
    noahowpmp%water%QINTS = NoahowpmpIO%QINTS(ix,iy)
    noahowpmp%water%QDRIPS = NoahowpmpIO%QDRIPS(ix,iy)
    noahowpmp%water%QTHROS = NoahowpmpIO%QTHROS(ix,iy)
    noahowpmp%water%QRAIN = NoahowpmpIO%QRAIN(ix,iy)
    noahowpmp%water%QSNOW = NoahowpmpIO%QSNOW(ix,iy)
    noahowpmp%water%SNOWHIN = NoahowpmpIO%SNOWHIN(ix,iy)
    noahowpmp%water%ECAN = NoahowpmpIO%ECAN(ix,iy)
    noahowpmp%water%ETRAN = NoahowpmpIO%ETRAN(ix,iy)
    noahowpmp%water%QSNFRO = NoahowpmpIO%QSNFRO(ix,iy)
    noahowpmp%water%QSNSUB = NoahowpmpIO%QSNSUB(ix,iy)
    noahowpmp%water%SNOWH = NoahowpmpIO%SNOWH(ix,iy)
    noahowpmp%water%SNEQV = NoahowpmpIO%SNEQV(ix,iy)
    noahowpmp%water%SNEQVO = NoahowpmpIO%SNEQVO(ix,iy)
    noahowpmp%water%BDSNO = NoahowpmpIO%BDSNO(ix,iy)
    noahowpmp%water%QSNBOT = NoahowpmpIO%QSNBOT(ix,iy)
    noahowpmp%water%PONDING = NoahowpmpIO%PONDING(ix,iy)
    noahowpmp%water%PONDING1 = NoahowpmpIO%PONDING1(ix,iy)
    noahowpmp%water%PONDING2 = NoahowpmpIO%PONDING2(ix,iy)
    noahowpmp%water%QVAP = NoahowpmpIO%QVAP(ix,iy)
    noahowpmp%water%QDEW = NoahowpmpIO%QDEW(ix,iy)
    noahowpmp%water%QSDEW = NoahowpmpIO%QSDEW(ix,iy)
    noahowpmp%water%WSLAKE = NoahowpmpIO%WSLAKE(ix,iy)
    noahowpmp%water%runsrf_dt = NoahowpmpIO%runsrf_dt(ix,iy)
    noahowpmp%water%ASAT = NoahowpmpIO%ASAT(ix,iy)
    noahowpmp%water%ISNOW = NoahowpmpIO%ISNOW(ix,iy)
    noahowpmp%water%smc(:) = NoahowpmpIO%smc(ix,iy,:)
    noahowpmp%water%smc_init(:) = NoahowpmpIO%smc_init(ix,iy,:)
    noahowpmp%water%sice(:) = NoahowpmpIO%sice(ix,iy,:)
    noahowpmp%water%sh2o(:) = NoahowpmpIO%sh2o(ix,iy,:)
    noahowpmp%water%etrani(:) = NoahowpmpIO%etrani(ix,iy,:)
    noahowpmp%water%BTRANI(:) = NoahowpmpIO%BTRANI(ix,iy,:)
    noahowpmp%water%wcnd(:) = NoahowpmpIO%wcnd(ix,iy,:)
    noahowpmp%water%fcr(:) = NoahowpmpIO%fcr(ix,iy,:)
    noahowpmp%water%FICEOLD(:) = NoahowpmpIO%FICEOLD(ix,iy,:)
    noahowpmp%water%SNICE(:) = NoahowpmpIO%SNICE(ix,iy,:)
    noahowpmp%water%SNLIQ(:) = NoahowpmpIO%SNLIQ(ix,iy,:)
    noahowpmp%water%SNICEV(:) = NoahowpmpIO%SNICEV(ix,iy,:)
    noahowpmp%water%SNLIQV(:) = NoahowpmpIO%SNLIQV(ix,iy,:)
    noahowpmp%water%FICE(:) = NoahowpmpIO%FICE(ix,iy,:)
    noahowpmp%water%EPORE(:) = NoahowpmpIO%EPORE(ix,iy,:)
    noahowpmp%water%FSNO = NoahowpmpIO%FSNO(ix,iy)
    noahowpmp%water%BTRAN = NoahowpmpIO%BTRAN(ix,iy)
            
    end associate

  end subroutine WaterVarInTransfer

end module WaterVarInTransferModule
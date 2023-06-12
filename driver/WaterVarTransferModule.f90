module WaterVarTransferModule

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

  subroutine WaterVarOutTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    NoahowpmpIO%qinsur(ix,iy) = Noahowpmp%water%qinsur
    NoahowpmpIO%qseva(ix,iy) = Noahowpmp%water%qseva
    NoahowpmpIO%EVAPOTRANS(ix,iy) = Noahowpmp%water%EVAPOTRANS
    NoahowpmpIO%runsrf(ix,iy) = Noahowpmp%water%runsrf
    NoahowpmpIO%runsub(ix,iy) = Noahowpmp%water%runsub
    NoahowpmpIO%qdrain(ix,iy) = Noahowpmp%water%qdrain
    NoahowpmpIO%zwt(ix,iy) = Noahowpmp%water%zwt
    NoahowpmpIO%smcwtd(ix,iy) = Noahowpmp%water%smcwtd
    NoahowpmpIO%deeprech(ix,iy) = Noahowpmp%water%deeprech
    NoahowpmpIO%fcrmax(ix,iy) = Noahowpmp%water%fcrmax
    NoahowpmpIO%snoflow(ix,iy) = Noahowpmp%water%snoflow
    NoahowpmpIO%pddum(ix,iy) = Noahowpmp%water%pddum
    NoahowpmpIO%FACC(ix,iy) = Noahowpmp%water%FACC
    NoahowpmpIO%sicemax(ix,iy) = Noahowpmp%water%sicemax
    NoahowpmpIO%FB_snow(ix,iy) = Noahowpmp%water%FB_snow
    NoahowpmpIO%rain(ix,iy) = Noahowpmp%water%rain
    NoahowpmpIO%snow(ix,iy) = Noahowpmp%water%snow
    NoahowpmpIO%bdfall(ix,iy) = Noahowpmp%water%bdfall
    NoahowpmpIO%FP(ix,iy) = Noahowpmp%water%FP
    NoahowpmpIO%canliq(ix,iy) = Noahowpmp%water%canliq
    NoahowpmpIO%canice(ix,iy) = Noahowpmp%water%canice
    NoahowpmpIO%FWET(ix,iy) = Noahowpmp%water%FWET
    NoahowpmpIO%CMC(ix,iy) = Noahowpmp%water%CMC
    NoahowpmpIO%QINTR(ix,iy) = Noahowpmp%water%QINTR
    NoahowpmpIO%QDRIPR(ix,iy) = Noahowpmp%water%QDRIPR
    NoahowpmpIO%QTHROR(ix,iy) = Noahowpmp%water%QTHROR
    NoahowpmpIO%QINTS(ix,iy) = Noahowpmp%water%QINTS
    NoahowpmpIO%QDRIPS(ix,iy) = Noahowpmp%water%QDRIPS
    NoahowpmpIO%QTHROS(ix,iy) = Noahowpmp%water%QTHROS
    NoahowpmpIO%QRAIN(ix,iy) = Noahowpmp%water%QRAIN
    NoahowpmpIO%QSNOW(ix,iy) = Noahowpmp%water%QSNOW
    NoahowpmpIO%SNOWHIN(ix,iy) = Noahowpmp%water%SNOWHIN
    NoahowpmpIO%ECAN(ix,iy) = Noahowpmp%water%ECAN
    NoahowpmpIO%ETRAN(ix,iy) = Noahowpmp%water%ETRAN
    NoahowpmpIO%QSNFRO(ix,iy) = Noahowpmp%water%QSNFRO
    NoahowpmpIO%QSNSUB(ix,iy) = Noahowpmp%water%QSNSUB
    NoahowpmpIO%SNOWH(ix,iy) = Noahowpmp%water%SNOWH
    NoahowpmpIO%SNEQV(ix,iy) = Noahowpmp%water%SNEQV
    NoahowpmpIO%SNEQVO(ix,iy) = Noahowpmp%water%SNEQVO
    NoahowpmpIO%BDSNO(ix,iy) = Noahowpmp%water%BDSNO
    NoahowpmpIO%QSNBOT(ix,iy) = Noahowpmp%water%QSNBOT
    NoahowpmpIO%PONDING(ix,iy) = Noahowpmp%water%PONDING
    NoahowpmpIO%PONDING1(ix,iy) = Noahowpmp%water%PONDING1
    NoahowpmpIO%PONDING2(ix,iy) = Noahowpmp%water%PONDING2
    NoahowpmpIO%QVAP(ix,iy) = Noahowpmp%water%QVAP
    NoahowpmpIO%QDEW(ix,iy) = Noahowpmp%water%QDEW
    NoahowpmpIO%QSDEW(ix,iy) = Noahowpmp%water%QSDEW
    NoahowpmpIO%WSLAKE(ix,iy) = Noahowpmp%water%WSLAKE
    NoahowpmpIO%runsrf_dt(ix,iy) = Noahowpmp%water%runsrf_dt
    NoahowpmpIO%ASAT(ix,iy) = Noahowpmp%water%ASAT
    NoahowpmpIO%ISNOW(ix,iy) = Noahowpmp%water%ISNOW
    NoahowpmpIO%smc(ix,iy,:) = Noahowpmp%water%smc(:)
    NoahowpmpIO%smc_init(ix,iy,:) = Noahowpmp%water%smc_init(:)
    NoahowpmpIO%sice(ix,iy,:) = Noahowpmp%water%sice(:)
    NoahowpmpIO%sh2o(ix,iy,:) = Noahowpmp%water%sh2o(:)
    NoahowpmpIO%etrani(ix,iy,:) = Noahowpmp%water%etrani(:)
    NoahowpmpIO%BTRANI(ix,iy,:) = Noahowpmp%water%BTRANI(:)
    NoahowpmpIO%wcnd(ix,iy,:) = Noahowpmp%water%wcnd(:)
    NoahowpmpIO%fcr(ix,iy,:) = Noahowpmp%water%fcr(:)
    NoahowpmpIO%FICEOLD(ix,iy,:) = Noahowpmp%water%FICEOLD(:)
    NoahowpmpIO%SNICE(ix,iy,:) = Noahowpmp%water%SNICE(:)
    NoahowpmpIO%SNLIQ(ix,iy,:) = Noahowpmp%water%SNLIQ(:)
    NoahowpmpIO%SNICEV(ix,iy,:) = Noahowpmp%water%SNICEV(:)
    NoahowpmpIO%SNLIQV(ix,iy,:) = Noahowpmp%water%SNLIQV(:)
    NoahowpmpIO%FICE(ix,iy,:) = Noahowpmp%water%FICE(:)
    NoahowpmpIO%EPORE(ix,iy,:) = Noahowpmp%water%EPORE(:)
    NoahowpmpIO%FSNO(ix,iy) = Noahowpmp%water%FSNO
    NoahowpmpIO%BTRAN(ix,iy) = Noahowpmp%water%BTRAN

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarTransferModule
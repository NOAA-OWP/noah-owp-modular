module WaterVarTransferModule

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine WaterVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowp%water%qinsur = noahowpgrid%qinsur(ix,iy)
    noahowp%water%qseva = noahowpgrid%qseva(ix,iy)
    noahowp%water%EVAPOTRANS = noahowpgrid%EVAPOTRANS(ix,iy)
    noahowp%water%runsrf = noahowpgrid%runsrf(ix,iy)
    noahowp%water%runsub = noahowpgrid%runsub(ix,iy)
    noahowp%water%qdrain = noahowpgrid%qdrain(ix,iy)
    noahowp%water%zwt = noahowpgrid%zwt(ix,iy)
    noahowp%water%smcwtd = noahowpgrid%smcwtd(ix,iy)
    noahowp%water%deeprech = noahowpgrid%deeprech(ix,iy)
    noahowp%water%fcrmax = noahowpgrid%fcrmax(ix,iy)
    noahowp%water%snoflow = noahowpgrid%snoflow(ix,iy)
    noahowp%water%pddum = noahowpgrid%pddum(ix,iy)
    noahowp%water%FACC = noahowpgrid%FACC(ix,iy)
    noahowp%water%sicemax = noahowpgrid%sicemax(ix,iy)
    noahowp%water%FB_snow = noahowpgrid%FB_snow(ix,iy)
    noahowp%water%rain = noahowpgrid%rain(ix,iy)
    noahowp%water%snow = noahowpgrid%snow(ix,iy)
    noahowp%water%bdfall = noahowpgrid%bdfall(ix,iy)
    noahowp%water%FP = noahowpgrid%FP(ix,iy)
    noahowp%water%canliq = noahowpgrid%canliq(ix,iy)
    noahowp%water%canice = noahowpgrid%canice(ix,iy)
    noahowp%water%FWET = noahowpgrid%FWET(ix,iy)
    noahowp%water%CMC = noahowpgrid%CMC(ix,iy)
    noahowp%water%QINTR = noahowpgrid%QINTR(ix,iy)
    noahowp%water%QDRIPR = noahowpgrid%QDRIPR(ix,iy)
    noahowp%water%QTHROR = noahowpgrid%QTHROR(ix,iy)
    noahowp%water%QINTS = noahowpgrid%QINTS(ix,iy)
    noahowp%water%QDRIPS = noahowpgrid%QDRIPS(ix,iy)
    noahowp%water%QTHROS = noahowpgrid%QTHROS(ix,iy)
    noahowp%water%QRAIN = noahowpgrid%QRAIN(ix,iy)
    noahowp%water%QSNOW = noahowpgrid%QSNOW(ix,iy)
    noahowp%water%SNOWHIN = noahowpgrid%SNOWHIN(ix,iy)
    noahowp%water%ECAN = noahowpgrid%ECAN(ix,iy)
    noahowp%water%ETRAN = noahowpgrid%ETRAN(ix,iy)
    noahowp%water%QSNFRO = noahowpgrid%QSNFRO(ix,iy)
    noahowp%water%QSNSUB = noahowpgrid%QSNSUB(ix,iy)
    noahowp%water%SNOWH = noahowpgrid%SNOWH(ix,iy)
    noahowp%water%SNEQV = noahowpgrid%SNEQV(ix,iy)
    noahowp%water%SNEQVO = noahowpgrid%SNEQVO(ix,iy)
    noahowp%water%BDSNO = noahowpgrid%BDSNO(ix,iy)
    noahowp%water%QSNBOT = noahowpgrid%QSNBOT(ix,iy)
    noahowp%water%PONDING = noahowpgrid%PONDING(ix,iy)
    noahowp%water%PONDING1 = noahowpgrid%PONDING1(ix,iy)
    noahowp%water%PONDING2 = noahowpgrid%PONDING2(ix,iy)
    noahowp%water%QVAP = noahowpgrid%QVAP(ix,iy)
    noahowp%water%QDEW = noahowpgrid%QDEW(ix,iy)
    noahowp%water%QSDEW = noahowpgrid%QSDEW(ix,iy)
    noahowp%water%WSLAKE = noahowpgrid%WSLAKE(ix,iy)
    noahowp%water%runsrf_dt = noahowpgrid%runsrf_dt(ix,iy)
    noahowp%water%ASAT = noahowpgrid%ASAT(ix,iy)
    noahowp%water%ISNOW = noahowpgrid%ISNOW(ix,iy)
    noahowp%water%smc(:) = noahowpgrid%smc(ix,iy,:)
    noahowp%water%smc_init(:) = noahowpgrid%smc_init(ix,iy,:)
    noahowp%water%sice(:) = noahowpgrid%sice(ix,iy,:)
    noahowp%water%sh2o(:) = noahowpgrid%sh2o(ix,iy,:)
    noahowp%water%etrani(:) = noahowpgrid%etrani(ix,iy,:)
    noahowp%water%BTRANI(:) = noahowpgrid%BTRANI(ix,iy,:)
    noahowp%water%wcnd(:) = noahowpgrid%wcnd(ix,iy,:)
    noahowp%water%fcr(:) = noahowpgrid%fcr(ix,iy,:)
    noahowp%water%FICEOLD(:) = noahowpgrid%FICEOLD(ix,iy,:)
    noahowp%water%SNICE(:) = noahowpgrid%SNICE(ix,iy,:)
    noahowp%water%SNLIQ(:) = noahowpgrid%SNLIQ(ix,iy,:)
    noahowp%water%SNICEV(:) = noahowpgrid%SNICEV(ix,iy,:)
    noahowp%water%SNLIQV(:) = noahowpgrid%SNLIQV(ix,iy,:)
    noahowp%water%FICE(:) = noahowpgrid%FICE(ix,iy,:)
    noahowp%water%EPORE(:) = noahowpgrid%EPORE(ix,iy,:)
    noahowp%water%FSNO = noahowpgrid%FSNO(ix,iy)
    noahowp%water%BTRAN = noahowpgrid%BTRAN(ix,iy)
            
    end associate

  end subroutine WaterVarInTransfer

  subroutine WaterVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowpgrid%qinsur(ix,iy) = noahowp%water%qinsur
    noahowpgrid%qseva(ix,iy) = noahowp%water%qseva
    noahowpgrid%EVAPOTRANS(ix,iy) = noahowp%water%EVAPOTRANS
    noahowpgrid%runsrf(ix,iy) = noahowp%water%runsrf
    noahowpgrid%runsub(ix,iy) = noahowp%water%runsub
    noahowpgrid%qdrain(ix,iy) = noahowp%water%qdrain
    noahowpgrid%zwt(ix,iy) = noahowp%water%zwt
    noahowpgrid%smcwtd(ix,iy) = noahowp%water%smcwtd
    noahowpgrid%deeprech(ix,iy) = noahowp%water%deeprech
    noahowpgrid%fcrmax(ix,iy) = noahowp%water%fcrmax
    noahowpgrid%snoflow(ix,iy) = noahowp%water%snoflow
    noahowpgrid%pddum(ix,iy) = noahowp%water%pddum
    noahowpgrid%FACC(ix,iy) = noahowp%water%FACC
    noahowpgrid%sicemax(ix,iy) = noahowp%water%sicemax
    noahowpgrid%FB_snow(ix,iy) = noahowp%water%FB_snow
    noahowpgrid%rain(ix,iy) = noahowp%water%rain
    noahowpgrid%snow(ix,iy) = noahowp%water%snow
    noahowpgrid%bdfall(ix,iy) = noahowp%water%bdfall
    noahowpgrid%FP(ix,iy) = noahowp%water%FP
    noahowpgrid%canliq(ix,iy) = noahowp%water%canliq
    noahowpgrid%canice(ix,iy) = noahowp%water%canice
    noahowpgrid%FWET(ix,iy) = noahowp%water%FWET
    noahowpgrid%CMC(ix,iy) = noahowp%water%CMC
    noahowpgrid%QINTR(ix,iy) = noahowp%water%QINTR
    noahowpgrid%QDRIPR(ix,iy) = noahowp%water%QDRIPR
    noahowpgrid%QTHROR(ix,iy) = noahowp%water%QTHROR
    noahowpgrid%QINTS(ix,iy) = noahowp%water%QINTS
    noahowpgrid%QDRIPS(ix,iy) = noahowp%water%QDRIPS
    noahowpgrid%QTHROS(ix,iy) = noahowp%water%QTHROS
    noahowpgrid%QRAIN(ix,iy) = noahowp%water%QRAIN
    noahowpgrid%QSNOW(ix,iy) = noahowp%water%QSNOW
    noahowpgrid%SNOWHIN(ix,iy) = noahowp%water%SNOWHIN
    noahowpgrid%ECAN(ix,iy) = noahowp%water%ECAN
    noahowpgrid%ETRAN(ix,iy) = noahowp%water%ETRAN
    noahowpgrid%QSNFRO(ix,iy) = noahowp%water%QSNFRO
    noahowpgrid%QSNSUB(ix,iy) = noahowp%water%QSNSUB
    noahowpgrid%SNOWH(ix,iy) = noahowp%water%SNOWH
    noahowpgrid%SNEQV(ix,iy) = noahowp%water%SNEQV
    noahowpgrid%SNEQVO(ix,iy) = noahowp%water%SNEQVO
    noahowpgrid%BDSNO(ix,iy) = noahowp%water%BDSNO
    noahowpgrid%QSNBOT(ix,iy) = noahowp%water%QSNBOT
    noahowpgrid%PONDING(ix,iy) = noahowp%water%PONDING
    noahowpgrid%PONDING1(ix,iy) = noahowp%water%PONDING1
    noahowpgrid%PONDING2(ix,iy) = noahowp%water%PONDING2
    noahowpgrid%QVAP(ix,iy) = noahowp%water%QVAP
    noahowpgrid%QDEW(ix,iy) = noahowp%water%QDEW
    noahowpgrid%QSDEW(ix,iy) = noahowp%water%QSDEW
    noahowpgrid%WSLAKE(ix,iy) = noahowp%water%WSLAKE
    noahowpgrid%runsrf_dt(ix,iy) = noahowp%water%runsrf_dt
    noahowpgrid%ASAT(ix,iy) = noahowp%water%ASAT
    noahowpgrid%ISNOW(ix,iy) = noahowp%water%ISNOW
    noahowpgrid%smc(ix,iy,:) = noahowp%water%smc(:)
    noahowpgrid%smc_init(ix,iy,:) = noahowp%water%smc_init(:)
    noahowpgrid%sice(ix,iy,:) = noahowp%water%sice(:)
    noahowpgrid%sh2o(ix,iy,:) = noahowp%water%sh2o(:)
    noahowpgrid%etrani(ix,iy,:) = noahowp%water%etrani(:)
    noahowpgrid%BTRANI(ix,iy,:) = noahowp%water%BTRANI(:)
    noahowpgrid%wcnd(ix,iy,:) = noahowp%water%wcnd(:)
    noahowpgrid%fcr(ix,iy,:) = noahowp%water%fcr(:)
    noahowpgrid%FICEOLD(ix,iy,:) = noahowp%water%FICEOLD(:)
    noahowpgrid%SNICE(ix,iy,:) = noahowp%water%SNICE(:)
    noahowpgrid%SNLIQ(ix,iy,:) = noahowp%water%SNLIQ(:)
    noahowpgrid%SNICEV(ix,iy,:) = noahowp%water%SNICEV(:)
    noahowpgrid%SNLIQV(ix,iy,:) = noahowp%water%SNLIQV(:)
    noahowpgrid%FICE(ix,iy,:) = noahowp%water%FICE(:)
    noahowpgrid%EPORE(ix,iy,:) = noahowp%water%EPORE(:)
    noahowpgrid%FSNO(ix,iy) = noahowp%water%FSNO
    noahowpgrid%BTRAN(ix,iy) = noahowp%water%BTRAN

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarTransferModule
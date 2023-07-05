module WaterTypeTransfer

    use WaterType
    use WaterGridType

    implicit none
  
  contains
  
  subroutine WaterVarInTransfer(water, watergrid, ix, iy)

    implicit none

    type(water_type),     intent(inout) :: water
    type(watergrid_type), intent(in)    :: watergrid
    integer,              intent(in)    :: ix
    integer,              intent(in)    :: iy

    water%qinsur = watergrid%qinsur(ix,iy)
    water%qseva = watergrid%qseva(ix,iy)
    water%EVAPOTRANS = watergrid%EVAPOTRANS(ix,iy)
    water%runsrf = watergrid%runsrf(ix,iy)
    water%runsub = watergrid%runsub(ix,iy)
    water%qdrain = watergrid%qdrain(ix,iy)
    water%zwt = watergrid%zwt(ix,iy)
    water%smcwtd = watergrid%smcwtd(ix,iy)
    water%deeprech = watergrid%deeprech(ix,iy)
    water%fcrmax = watergrid%fcrmax(ix,iy)
    water%snoflow = watergrid%snoflow(ix,iy)
    water%pddum = watergrid%pddum(ix,iy)
    water%FACC = watergrid%FACC(ix,iy)
    water%sicemax = watergrid%sicemax(ix,iy)
    water%FB_snow = watergrid%FB_snow(ix,iy)
    water%rain = watergrid%rain(ix,iy)
    water%snow = watergrid%snow(ix,iy)
    water%bdfall = watergrid%bdfall(ix,iy)
    water%FP = watergrid%FP(ix,iy)
    water%canliq = watergrid%canliq(ix,iy)
    water%canice = watergrid%canice(ix,iy)
    water%FWET = watergrid%FWET(ix,iy)
    water%CMC = watergrid%CMC(ix,iy)
    water%QINTR = watergrid%QINTR(ix,iy)
    water%QDRIPR = watergrid%QDRIPR(ix,iy)
    water%QTHROR = watergrid%QTHROR(ix,iy)
    water%QINTS = watergrid%QINTS(ix,iy)
    water%QDRIPS = watergrid%QDRIPS(ix,iy)
    water%QTHROS = watergrid%QTHROS(ix,iy)
    water%QRAIN = watergrid%QRAIN(ix,iy)
    water%QSNOW = watergrid%QSNOW(ix,iy)
    water%SNOWHIN = watergrid%SNOWHIN(ix,iy)
    water%ECAN = watergrid%ECAN(ix,iy)
    water%ETRAN = watergrid%ETRAN(ix,iy)
    water%QSNFRO = watergrid%QSNFRO(ix,iy)
    water%QSNSUB = watergrid%QSNSUB(ix,iy)
    water%SNOWH = watergrid%SNOWH(ix,iy)
    water%SNEQV = watergrid%SNEQV(ix,iy)
    water%SNEQVO = watergrid%SNEQVO(ix,iy)
    water%BDSNO = watergrid%BDSNO(ix,iy)
    water%QSNBOT = watergrid%QSNBOT(ix,iy)
    water%PONDING = watergrid%PONDING(ix,iy)
    water%PONDING1 = watergrid%PONDING1(ix,iy)
    water%PONDING2 = watergrid%PONDING2(ix,iy)
    water%QVAP = watergrid%QVAP(ix,iy)
    water%QDEW = watergrid%QDEW(ix,iy)
    water%QSDEW = watergrid%QSDEW(ix,iy)
    water%WSLAKE = watergrid%WSLAKE(ix,iy)
    water%runsrf_dt = watergrid%runsrf_dt(ix,iy)
    water%ASAT = watergrid%ASAT(ix,iy)
    water%ISNOW = watergrid%ISNOW(ix,iy)
    water%smc(:) = watergrid%smc(ix,iy,:)
    water%smc_init(:) = watergrid%smc_init(ix,iy,:)
    water%sice(:) = watergrid%sice(ix,iy,:)
    water%sh2o(:) = watergrid%sh2o(ix,iy,:)
    water%etrani(:) = watergrid%etrani(ix,iy,:)
    water%BTRANI(:) = watergrid%BTRANI(ix,iy,:)
    water%wcnd(:) = watergrid%wcnd(ix,iy,:)
    water%fcr(:) = watergrid%fcr(ix,iy,:)
    water%FICEOLD(:) = watergrid%FICEOLD(ix,iy,:)
    water%SNICE(:) = watergrid%SNICE(ix,iy,:)
    water%SNLIQ(:) = watergrid%SNLIQ(ix,iy,:)
    water%SNICEV(:) = watergrid%SNICEV(ix,iy,:)
    water%SNLIQV(:) = watergrid%SNLIQV(ix,iy,:)
    water%FICE(:) = watergrid%FICE(ix,iy,:)
    water%EPORE(:) = watergrid%EPORE(ix,iy,:)
    water%FSNO = watergrid%FSNO(ix,iy)
    water%BTRAN = watergrid%BTRAN(ix,iy)

  end subroutine WaterVarInTransfer

  subroutine WaterVarOutTransfer(water, watergrid, ix, iy)

    implicit none

    type(water_type),     intent(in)    :: water
    type(watergrid_type), intent(inout) :: watergrid
    integer,              intent(in)    :: ix
    integer,              intent(in)    :: iy

    watergrid%qinsur(ix,iy) = water%qinsur
    watergrid%qseva(ix,iy) = water%qseva
    watergrid%EVAPOTRANS(ix,iy) = water%EVAPOTRANS
    watergrid%runsrf(ix,iy) = water%runsrf
    watergrid%runsub(ix,iy) = water%runsub
    watergrid%qdrain(ix,iy) = water%qdrain
    watergrid%zwt(ix,iy) = water%zwt
    watergrid%smcwtd(ix,iy) = water%smcwtd
    watergrid%deeprech(ix,iy) = water%deeprech
    watergrid%fcrmax(ix,iy) = water%fcrmax
    watergrid%snoflow(ix,iy) = water%snoflow
    watergrid%pddum(ix,iy) = water%pddum
    watergrid%FACC(ix,iy) = water%FACC
    watergrid%sicemax(ix,iy) = water%sicemax
    watergrid%FB_snow(ix,iy) = water%FB_snow
    watergrid%rain(ix,iy) = water%rain
    watergrid%snow(ix,iy) = water%snow
    watergrid%bdfall(ix,iy) = water%bdfall
    watergrid%FP(ix,iy) = water%FP
    watergrid%canliq(ix,iy) = water%canliq
    watergrid%canice(ix,iy) = water%canice
    watergrid%FWET(ix,iy) = water%FWET
    watergrid%CMC(ix,iy) = water%CMC
    watergrid%QINTR(ix,iy) = water%QINTR
    watergrid%QDRIPR(ix,iy) = water%QDRIPR
    watergrid%QTHROR(ix,iy) = water%QTHROR
    watergrid%QINTS(ix,iy) = water%QINTS
    watergrid%QDRIPS(ix,iy) = water%QDRIPS
    watergrid%QTHROS(ix,iy) = water%QTHROS
    watergrid%QRAIN(ix,iy) = water%QRAIN
    watergrid%QSNOW(ix,iy) = water%QSNOW
    watergrid%SNOWHIN(ix,iy) = water%SNOWHIN
    watergrid%ECAN(ix,iy) = water%ECAN
    watergrid%ETRAN(ix,iy) = water%ETRAN
    watergrid%QSNFRO(ix,iy) = water%QSNFRO
    watergrid%QSNSUB(ix,iy) = water%QSNSUB
    watergrid%SNOWH(ix,iy) = water%SNOWH
    watergrid%SNEQV(ix,iy) = water%SNEQV
    watergrid%SNEQVO(ix,iy) = water%SNEQVO
    watergrid%BDSNO(ix,iy) = water%BDSNO
    watergrid%QSNBOT(ix,iy) = water%QSNBOT
    watergrid%PONDING(ix,iy) = water%PONDING
    watergrid%PONDING1(ix,iy) = water%PONDING1
    watergrid%PONDING2(ix,iy) = water%PONDING2
    watergrid%QVAP(ix,iy) = water%QVAP
    watergrid%QDEW(ix,iy) = water%QDEW
    watergrid%QSDEW(ix,iy) = water%QSDEW
    watergrid%WSLAKE(ix,iy) = water%WSLAKE
    watergrid%runsrf_dt(ix,iy) = water%runsrf_dt
    watergrid%ASAT(ix,iy) = water%ASAT
    watergrid%ISNOW(ix,iy) = water%ISNOW
    watergrid%smc(ix,iy,:) = water%smc(:)
    watergrid%smc_init(ix,iy,:) = water%smc_init(:)
    watergrid%sice(ix,iy,:) = water%sice(:)
    watergrid%sh2o(ix,iy,:) = water%sh2o(:)
    watergrid%etrani(ix,iy,:) = water%etrani(:)
    watergrid%BTRANI(ix,iy,:) = water%BTRANI(:)
    watergrid%wcnd(ix,iy,:) = water%wcnd(:)
    watergrid%fcr(ix,iy,:) = water%fcr(:)
    watergrid%FICEOLD(ix,iy,:) = water%FICEOLD(:)
    watergrid%SNICE(ix,iy,:) = water%SNICE(:)
    watergrid%SNLIQ(ix,iy,:) = water%SNLIQ(:)
    watergrid%SNICEV(ix,iy,:) = water%SNICEV(:)
    watergrid%SNLIQV(ix,iy,:) = water%SNLIQV(:)
    watergrid%FICE(ix,iy,:) = water%FICE(:)
    watergrid%EPORE(ix,iy,:) = water%EPORE(:)
    watergrid%FSNO(ix,iy) = water%FSNO
    watergrid%BTRAN(ix,iy) = water%BTRAN

  end subroutine WaterVarOutTransfer

end module WaterTypeTransfer
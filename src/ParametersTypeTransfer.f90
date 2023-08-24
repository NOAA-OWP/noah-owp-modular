module ParametersTypeTransfer

    use ParametersType
    use ParametersGridType

    implicit none
  
  contains
  
  subroutine ParametersVarInTransfer(parameters, parametersgrid, ix, iy)

    implicit none

    type(parameters_type),      intent(inout) :: parameters
    type(parametersgrid_type),  intent(in)    :: parametersgrid
    integer,                    intent(in)    :: ix
    integer,                    intent(in)    :: iy

    parameters%bexp(:) = parametersgrid%bexp(ix,iy,:)
    parameters%smcmax(:) = parametersgrid%smcmax(ix,iy,:)
    parameters%smcwlt(:) = parametersgrid%smcwlt(ix,iy,:)
    parameters%smcref(:) = parametersgrid%smcref(ix,iy,:)
    parameters%dksat(:) = parametersgrid%dksat(ix,iy,:)
    parameters%dwsat(:) = parametersgrid%dwsat(ix,iy,:)
    parameters%psisat(:) = parametersgrid%psisat(ix,iy,:)
    parameters%bvic = parametersgrid%bvic(ix,iy)
    parameters%AXAJ = parametersgrid%AXAJ(ix,iy)
    parameters%BXAJ = parametersgrid%BXAJ(ix,iy)
    parameters%XXAJ = parametersgrid%XXAJ(ix,iy)
    parameters%BBVIC = parametersgrid%BBVIC(ix,iy)
    parameters%G = parametersgrid%G(ix,iy)
    parameters%QUARTZ = parametersgrid%QUARTZ(ix,iy)
    parameters%kdt = parametersgrid%kdt(ix,iy)
    parameters%refkdt = parametersgrid%refkdt(ix,iy)
    parameters%refdk = parametersgrid%refdk(ix,iy)
    parameters%csoil = parametersgrid%csoil(ix,iy)
    parameters%Z0 = parametersgrid%Z0(ix,iy)
    parameters%CZIL = parametersgrid%CZIL(ix,iy)
    parameters%ZBOT = parametersgrid%ZBOT(ix,iy)
    parameters%frzx = parametersgrid%frzx(ix,iy)
    parameters%slope = parametersgrid%slope(ix,iy)
    parameters%timean = parametersgrid%timean(ix,iy)
    parameters%fsatmx = parametersgrid%fsatmx(ix,iy)
    parameters%ZWT_INIT = parametersgrid%ZWT_INIT(ix,iy)
    parameters%urban_flag = parametersgrid%urban_flag(ix,iy)
    parameters%LAIM(:) = parametersgrid%LAIM(ix,iy,:)
    parameters%SAIM(:) = parametersgrid%SAIM(ix,iy,:)
    parameters%LAI = parametersgrid%LAI(ix,iy)
    parameters%SAI = parametersgrid%SAI(ix,iy)
    parameters%CH2OP = parametersgrid%CH2OP(ix,iy)
    parameters%NROOT = parametersgrid%NROOT(ix,iy)
    parameters%HVT = parametersgrid%HVT(ix,iy)
    parameters%HVB = parametersgrid%HVB(ix,iy)
    parameters%TMIN = parametersgrid%TMIN(ix,iy)
    parameters%SHDFAC = parametersgrid%SHDFAC(ix,iy)
    parameters%SHDMAX = parametersgrid%SHDMAX(ix,iy)
    parameters%Z0MVT = parametersgrid%Z0MVT(ix,iy)
    parameters%RC = parametersgrid%RC(ix,iy)
    parameters%XL = parametersgrid%XL(ix,iy)
    parameters%BP = parametersgrid%BP(ix,iy)
    parameters%FOLNMX = parametersgrid%FOLNMX(ix,iy)
    parameters%QE25 = parametersgrid%QE25(ix,iy)
    parameters%VCMX25 = parametersgrid%VCMX25(ix,iy)
    parameters%MP = parametersgrid%MP(ix,iy)
    parameters%RGL = parametersgrid%RGL(ix,iy)
    parameters%RSMIN = parametersgrid%RSMIN(ix,iy)
    parameters%HS = parametersgrid%HS(ix,iy)
    parameters%AKC = parametersgrid%AKC(ix,iy)
    parameters%AKO = parametersgrid%AKO(ix,iy)
    parameters%AVCMX = parametersgrid%AVCMX(ix,iy)
    parameters%RSMAX = parametersgrid%RSMAX(ix,iy)
    parameters%CWP = parametersgrid%CWP(ix,iy)
    parameters%C3PSN = parametersgrid%C3PSN(ix,iy)
    parameters%DLEAF = parametersgrid%DLEAF(ix,iy)
    parameters%KC25 = parametersgrid%KC25(ix,iy)
    parameters%KO25 = parametersgrid%KO25(ix,iy)
    parameters%ELAI = parametersgrid%ELAI(ix,iy)
    parameters%ESAI = parametersgrid%ESAI(ix,iy)
    parameters%VAI = parametersgrid%VAI(ix,iy)
    parameters%VEG = parametersgrid%VEG(ix,iy)
    parameters%FVEG = parametersgrid%FVEG(ix,iy)
    parameters%RHOL(:) = parametersgrid%RHOL(ix,iy,:)
    parameters%RHOS(:) = parametersgrid%RHOS(ix,iy,:)
    parameters%TAUL(:) = parametersgrid%TAUL(ix,iy,:)
    parameters%TAUS(:) = parametersgrid%TAUS(ix,iy,:)
    parameters%SSI = parametersgrid%SSI(ix,iy)
    parameters%MFSNO = parametersgrid%MFSNO(ix,iy)
    parameters%Z0SNO = parametersgrid%Z0SNO(ix,iy)
    parameters%RSURF_SNOW = parametersgrid%RSURF_SNOW(ix,iy)
    parameters%RSURF_EXP = parametersgrid%RSURF_EXP(ix,iy)
    parameters%ALBSAT(:) = parametersgrid%ALBSAT(ix,iy,:)
    parameters%ALBDRY(:) = parametersgrid%ALBDRY(ix,iy,:)
    parameters%ALBICE(:) = parametersgrid%ALBICE(:)
    parameters%ALBLAK(:) = parametersgrid%ALBLAK(:)
    parameters%OMEGAS(:) = parametersgrid%OMEGAS(:)
    parameters%EG(:) = parametersgrid%EG(ix,iy,:)
    parameters%WSLMAX = parametersgrid%WSLMAX(ix,iy)
    parameters%max_liq_mass_fraction = parametersgrid%max_liq_mass_fraction(ix,iy)
    parameters%SNOW_RET_FAC = parametersgrid%SNOW_RET_FAC(ix,iy)
    parameters%TOPT = parametersgrid%TOPT(ix,iy)
    parameters%PSIWLT = parametersgrid%PSIWLT(ix,iy)
    parameters%TBOT = parametersgrid%TBOT(ix,iy)
    parameters%rain_snow_thresh = parametersgrid%rain_snow_thresh(ix,iy)

  end subroutine

  subroutine ParametersVarOutTransfer(parameters, parametersgrid, ix, iy)

    implicit none

    type(parameters_type),      intent(in)    :: parameters
    type(parametersgrid_type),  intent(inout) :: parametersgrid
    integer,                    intent(in)    :: ix
    integer,                    intent(in)    :: iy

    parametersgrid%bexp(ix,iy,:) = parameters%bexp(:)
    parametersgrid%smcmax(ix,iy,:) = parameters%smcmax(:)
    parametersgrid%smcwlt(ix,iy,:) = parameters%smcwlt(:)
    parametersgrid%smcref(ix,iy,:) = parameters%smcref(:)
    parametersgrid%dksat(ix,iy,:) = parameters%dksat(:)
    parametersgrid%dwsat(ix,iy,:) = parameters%dwsat(:)
    parametersgrid%psisat(ix,iy,:) = parameters%psisat(:)
    parametersgrid%bvic(ix,iy) = parameters%bvic
    parametersgrid%AXAJ(ix,iy) = parameters%AXAJ
    parametersgrid%BXAJ(ix,iy) = parameters%BXAJ
    parametersgrid%XXAJ(ix,iy) = parameters%XXAJ
    parametersgrid%BBVIC(ix,iy) = parameters%BBVIC
    parametersgrid%G(ix,iy) = parameters%G
    parametersgrid%QUARTZ(ix,iy) = parameters%QUARTZ
    parametersgrid%kdt(ix,iy) = parameters%kdt
    parametersgrid%refkdt(ix,iy) = parameters%refkdt
    parametersgrid%refdk(ix,iy) = parameters%refdk
    parametersgrid%csoil(ix,iy) = parameters%csoil
    parametersgrid%Z0(ix,iy) = parameters%Z0
    parametersgrid%CZIL(ix,iy) = parameters%CZIL
    parametersgrid%ZBOT(ix,iy) = parameters%ZBOT
    parametersgrid%frzx(ix,iy) = parameters%frzx
    parametersgrid%slope(ix,iy) = parameters%slope
    parametersgrid%timean(ix,iy) = parameters%timean
    parametersgrid%fsatmx(ix,iy) = parameters%fsatmx
    parametersgrid%ZWT_INIT(ix,iy) = parameters%ZWT_INIT
    parametersgrid%urban_flag(ix,iy) = parameters%urban_flag
    parametersgrid%LAIM(ix,iy,:) = parameters%LAIM(:)
    parametersgrid%SAIM(ix,iy,:) = parameters%SAIM(:)
    parametersgrid%LAI(ix,iy) = parameters%LAI
    parametersgrid%SAI(ix,iy) = parameters%SAI
    parametersgrid%CH2OP(ix,iy) = parameters%CH2OP
    parametersgrid%NROOT(ix,iy) = parameters%NROOT
    parametersgrid%HVT(ix,iy) = parameters%HVT
    parametersgrid%HVB(ix,iy) = parameters%HVB
    parametersgrid%TMIN(ix,iy) = parameters%TMIN
    parametersgrid%SHDFAC(ix,iy) = parameters%SHDFAC
    parametersgrid%SHDMAX(ix,iy) = parameters%SHDMAX
    parametersgrid%Z0MVT(ix,iy) = parameters%Z0MVT
    parametersgrid%RC(ix,iy) = parameters%RC
    parametersgrid%XL(ix,iy) = parameters%XL
    parametersgrid%BP(ix,iy) = parameters%BP
    parametersgrid%FOLNMX(ix,iy) = parameters%FOLNMX
    parametersgrid%QE25(ix,iy) = parameters%QE25
    parametersgrid%VCMX25(ix,iy) = parameters%VCMX25
    parametersgrid%MP(ix,iy) = parameters%MP
    parametersgrid%RGL(ix,iy) = parameters%RGL
    parametersgrid%RSMIN(ix,iy) = parameters%RSMIN
    parametersgrid%HS(ix,iy) = parameters%HS
    parametersgrid%AKC(ix,iy) = parameters%AKC
    parametersgrid%AKO(ix,iy) = parameters%AKO
    parametersgrid%AVCMX(ix,iy) = parameters%AVCMX
    parametersgrid%RSMAX(ix,iy) = parameters%RSMAX
    parametersgrid%CWP(ix,iy) = parameters%CWP
    parametersgrid%C3PSN(ix,iy) = parameters%C3PSN
    parametersgrid%DLEAF(ix,iy) = parameters%DLEAF
    parametersgrid%KC25(ix,iy) = parameters%KC25
    parametersgrid%KO25(ix,iy) = parameters%KO25
    parametersgrid%ELAI(ix,iy) = parameters%ELAI
    parametersgrid%ESAI(ix,iy) = parameters%ESAI
    parametersgrid%VAI(ix,iy) = parameters%VAI
    parametersgrid%VEG(ix,iy) = parameters%VEG
    parametersgrid%FVEG(ix,iy) = parameters%FVEG
    parametersgrid%RHOL(ix,iy,:) = parameters%RHOL(:)
    parametersgrid%RHOS(ix,iy,:) = parameters%RHOS(:)
    parametersgrid%TAUL(ix,iy,:) = parameters%TAUL(:)
    parametersgrid%TAUS(ix,iy,:) = parameters%TAUS(:)
    parametersgrid%SSI(ix,iy) = parameters%SSI
    parametersgrid%MFSNO(ix,iy) = parameters%MFSNO
    parametersgrid%Z0SNO(ix,iy) = parameters%Z0SNO
    parametersgrid%RSURF_SNOW(ix,iy) = parameters%RSURF_SNOW
    parametersgrid%RSURF_EXP(ix,iy) = parameters%RSURF_EXP
    parametersgrid%ALBSAT(ix,iy,:) = parameters%ALBSAT(:)
    parametersgrid%ALBDRY(ix,iy,:) = parameters%ALBDRY(:)
    parametersgrid%ALBICE(:) = parameters%ALBICE(:)
    parametersgrid%ALBLAK(:) = parameters%ALBLAK(:)
    parametersgrid%OMEGAS(:) = parameters%OMEGAS(:)
    parametersgrid%EG(ix,iy,:) = parameters%EG(:)
    parametersgrid%WSLMAX(ix,iy) = parameters%WSLMAX
    parametersgrid%max_liq_mass_fraction(ix,iy) = parameters%max_liq_mass_fraction
    parametersgrid%SNOW_RET_FAC(ix,iy) = parameters%SNOW_RET_FAC
    parametersgrid%TOPT(ix,iy) = parameters%TOPT
    parametersgrid%PSIWLT(ix,iy) = parameters%PSIWLT
    parametersgrid%rain_snow_thresh(ix,iy) = parameters%rain_snow_thresh

  end subroutine ParametersVarOutTransfer

end module ParametersTypeTransfer
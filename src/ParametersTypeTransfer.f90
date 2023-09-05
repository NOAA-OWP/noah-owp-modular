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

    parametersgrid%LAI(ix,iy) = parameters%LAI
    parametersgrid%SAI(ix,iy) = parameters%SAI
    parametersgrid%ELAI(ix,iy) = parameters%ELAI
    parametersgrid%ESAI(ix,iy) = parameters%ESAI
    parametersgrid%VAI(ix,iy) = parameters%VAI
    parametersgrid%VEG(ix,iy) = parameters%VEG
    parametersgrid%FVEG(ix,iy) = parameters%FVEG

  end subroutine ParametersVarOutTransfer

end module ParametersTypeTransfer
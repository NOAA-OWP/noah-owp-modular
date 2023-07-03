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

    parameters%bexp(:)    = parametersgrid%bexp(ix,iy,:)
    parameters%smcmax(:)  = parametersgrid%smcmax(ix,iy,:)
    parameters%smcwlt(:)  = parametersgrid%smcwlt(ix,iy,:)
    parameters%smcref(:)  = parametersgrid%smcref(ix,iy,:)
    parameters%dksat(:)   = parametersgrid%dksat(ix,iy,:)
    parameters%dwsat(:)   = parametersgrid%dwsat(ix,iy,:)
    parameters%psisat(:)  = parametersgrid%psisat(ix,iy,:)
    parameters%bvic       = parametersgrid%bvic(ix,iy)
    parameters%AXAJ       = parametersgrid%AXAJ(ix,iy)
    parameters%BXAJ       = parametersgrid%BXAJ(ix,iy)
    parameters%XXAJ       = parametersgrid%XXAJ(ix,iy)
    parameters%BBVIC      = parametersgrid%BBVIC(ix,iy)
    parameters%G          = parametersgrid%G(ix,iy)
    parameters%QUARTZ     = parametersgrid%QUARTZ(ix,iy)
    parameters%LAIM(:)    = parametersgrid%LAIM(ix,iy,:)
    parameters%SAIM(:)    = parametersgrid%SAIM(ix,iy,:)
    parameters%CH2OP      = parametersgrid%CH2OP(ix,iy)
    parameters%NROOT      = parametersgrid%NROOT(ix,iy)
    parameters%HVT        = parametersgrid%HVT(ix,iy) 
    parameters%HVB        = parametersgrid%HVB(ix,iy) 
    parameters%TMIN       = parametersgrid%TMIN(ix,iy)
    parameters%SHDFAC     = parametersgrid%SHDFAC(ix,iy) ! this used to be in VEGPARM.TBL, but now somewhere else for hrldas. this is temporarily in MPTABLE.TBL.
    parameters%SHDMAX     = parametersgrid%SHDFAC(ix,iy) 
    parameters%Z0MVT      = parametersgrid%Z0MVT(ix,iy) 
    parameters%RC         = parametersgrid%RC(ix,iy) 
    parameters%XL         = parametersgrid%XL(ix,iy) 
    parameters%BP         = parametersgrid%BP(ix,iy) 
    parameters%FOLNMX     = parametersgrid%FOLNMX(ix,iy) 
    parameters%QE25       = parametersgrid%QE25(ix,iy) 
    parameters%VCMX25     = parametersgrid%VCMX25(ix,iy) 
    parameters%MP         = parametersgrid%MP(ix,iy) 
    parameters%RGL        = parametersgrid%RGL(ix,iy) 
    parameters%RSMIN      = parametersgrid%RSMIN(ix,iy)
    parameters%HS         = parametersgrid%HS(ix,iy)
    parameters%AKC        = parametersgrid%AKC(ix,iy) 
    parameters%AKO        = parametersgrid%AKO(ix,iy) 
    parameters%AVCMX      = parametersgrid%AVCMX(ix,iy) 
    parameters%RSMAX      = parametersgrid%RSMAX(ix,iy) 
    parameters%CWP        = parametersgrid%CWP(ix,iy) 
    parameters%C3PSN      = parametersgrid%C3PSN(ix,iy) 
    parameters%DLEAF      = parametersgrid%DLEAF(ix,iy) 
    parameters%KC25       = parametersgrid%KC25(ix,iy)
    parameters%KO25       = parametersgrid%KO25(ix,iy) 

    parameters%RHOL(1)    = parametersgrid%RHOL(ix,iy,1)
    parameters%RHOL(2)    = parametersgrid%RHOL(ix,iy,2)
    parameters%RHOS(1)    = parametersgrid%RHOS(ix,iy,1)
    parameters%RHOS(2)    = parametersgrid%RHOS(ix,iy,2)
    parameters%TAUL(1)    = parametersgrid%TAUL(ix,iy,1)
    parameters%TAUL(2)    = parametersgrid%TAUL(ix,iy,2)
    parameters%TAUS(1)    = parametersgrid%TAUS(ix,iy,1)
    parameters%TAUS(2)    = parametersgrid%TAUS(ix,iy,2)

    parameters%refkdt       = parametersgrid%refkdt
    parameters%refdk        = parametersgrid%refdk
    parameters%kdt          = parametersgrid%kdt(ix,iy)  
    parameters%Z0           = parametersgrid%Z0     ! bare soil roughness length (m). in GENPARM.TBL.  NOTE: This is hard-coded in hrldas version of noah-mp
    parameters%CZIL         = parametersgrid%CZIL
    parameters%ZBOT         = parametersgrid%ZBOT
    parameters%frzx         = parametersgrid%frzx(ix,iy)
    parameters%SSI          = parametersgrid%SSI
    parameters%MFSNO        = parametersgrid%MFSNO(ix,iy)
    parameters%Z0SNO        = parametersgrid%Z0SNO
    parameters%SWEMX        = parametersgrid%SWEMX
    parameters%TAU0         = parametersgrid%TAU0
    parameters%GRAIN_GROWTH = parametersgrid%GRAIN_GROWTH
    parameters%EXTRA_GROWTH = parametersgrid%EXTRA_GROWTH
    parameters%DIRT_SOOT    = parametersgrid%DIRT_SOOT
    parameters%BATS_COSZ    = parametersgrid%BATS_COSZ
    parameters%BATS_VIS_NEW = parametersgrid%BATS_VIS_NEW
    parameters%BATS_NIR_NEW = parametersgrid%BATS_NIR_NEW
    parameters%BATS_VIS_AGE = parametersgrid%BATS_VIS_AGE
    parameters%BATS_NIR_AGE = parametersgrid%BATS_NIR_AGE
    parameters%BATS_VIS_DIR = parametersgrid%BATS_VIS_DIR
    parameters%BATS_NIR_DIR = parametersgrid%BATS_NIR_DIR
    parameters%RSURF_SNOW   = parametersgrid%RSURF_SNOW
    parameters%RSURF_EXP    = parametersgrid%RSURF_EXP

    parameters%ALBSAT(1)    = parametersgrid%ALBSAT(ix,iy,1)
    parameters%ALBSAT(2)    = parametersgrid%ALBSAT(ix,iy,2)
    parameters%ALBDRY(1)    = parametersgrid%ALBDRY(ix,iy,1)
    parameters%ALBDRY(2)    = parametersgrid%ALBDRY(ix,iy,2)
    parameters%ALBICE(:)    = parametersgrid%ALBICE(ix,iy,:)
    parameters%ALBLAK(:)    = parametersgrid%ALBLAK(ix,iy,:)
    parameters%OMEGAS(:)    = parametersgrid%OMEGAS(ix,iy,:)
    parameters%BETADS       = parametersgrid%BETADS
    parameters%BETAIS       = parametersgrid%BETAIS
    parameters%EG(:)        = parametersgrid%EG(ix,iy,:)
    parameters%slope        = parametersgrid%slope

    parameters%ISURBAN                   = parametersgrid%ISURBAN
    parameters%ISWATER                   = parametersgrid%ISWATER
    parameters%ISBARREN                  = parametersgrid%ISBARREN
    parameters%ISICE                     = parametersgrid%ISICE
    parameters%ISCROP                    = parametersgrid%ISCROP
    parameters%EBLFOREST                 = parametersgrid%EBLFOREST
    parameters%NATURAL                   = parametersgrid%NATURAL
    parameters%LOW_DENSITY_RESIDENTIAL   = parametersgrid%LOW_DENSITY_RESIDENTIAL  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_1
    parameters%HIGH_DENSITY_RESIDENTIAL  = parametersgrid%HIGH_DENSITY_RESIDENTIAL  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_2
    parameters%HIGH_INTENSITY_INDUSTRIAL = parametersgrid%HIGH_INTENSITY_INDUSTRIAL  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_3

    parameters%urban_flag = parametersgrid%urban_flag(ix,iy)
    parameters%timean     = parametersgrid%timean
    parameters%fsatmx     = parametersgrid%fsatmx
    parameters%GRAV       = parametersgrid%GRAV
    parameters%SB         = parametersgrid%SB
    parameters%VKC        = parametersgrid%VKC
    parameters%TFRZ       = parametersgrid%TFRZ
    parameters%HSUB       = parametersgrid%HSUB
    parameters%HVAP       = parametersgrid%HVAP
    parameters%HFUS       = parametersgrid%HFUS
    parameters%CWAT       = parametersgrid%CWAT
    parameters%CICE       = parametersgrid%CICE
    parameters%CPAIR      = parametersgrid%CPAIR
    parameters%TKWAT      = parametersgrid%TKWAT
    parameters%TKICE      = parametersgrid%TKICE
    parameters%TKAIR      = parametersgrid%TKAIR
    parameters%RAIR       = parametersgrid%RAIR
    parameters%RW         = parametersgrid%RW
    parameters%DENH2O     = parametersgrid%DENH2O
    parameters%DENICE     = parametersgrid%DENICE
    parameters%THKW       = parametersgrid%THKW
    parameters%THKO       = parametersgrid%THKO
    parameters%THKQTZ     = parametersgrid%THKQTZ
    parameters%WSLMAX     = parametersgrid%WSLMAX
    parameters%max_liq_mass_fraction = parametersgrid%max_liq_mass_fraction
    parameters%SNOW_RET_FAC = parametersgrid%SNOW_RET_FAC
    parameters%NBAND        = parametersgrid%NBAND       ! do not change
    parameters%MPE          = parametersgrid%MPE  ! do not change ! need to make this a parameter
    parameters%TOPT         = parametersgrid%TOPT  ! Optimum transpiration air temperature [K]

    parameters%CO2       =  parametersgrid%CO2   ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    parameters%O2        =  parametersgrid%O2      ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    parameters%PSIWLT    = parametersgrid%PSIWLT      ! originally a fixed parameter set in ENERGY()
    parameters%TBOT      = parametersgrid%TBOT       ! (K) can be updated depending on option OPT_TBOT
    parameters%rain_snow_thresh = parametersgrid%rain_snow_thresh
    parameters%zwt_init = parametersgrid%zwt_init(ix,iy)

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
    parametersgrid%refkdt = parameters%refkdt
    parametersgrid%refdk = parameters%refdk
    parametersgrid%csoil = parameters%csoil
    parametersgrid%Z0 = parameters%Z0
    parametersgrid%CZIL = parameters%CZIL
    parametersgrid%ZBOT = parameters%ZBOT
    parametersgrid%frzx(ix,iy) = parameters%frzx
    parametersgrid%slope = parameters%slope
    parametersgrid%timean = parameters%timean
    parametersgrid%fsatmx = parameters%fsatmx
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
    parametersgrid%ISURBAN = parameters%ISURBAN
    parametersgrid%ISWATER = parameters%ISWATER
    parametersgrid%ISBARREN = parameters%ISBARREN
    parametersgrid%ISICE = parameters%ISICE
    parametersgrid%ISCROP = parameters%ISCROP
    parametersgrid%EBLFOREST = parameters%EBLFOREST
    parametersgrid%NATURAL = parameters%NATURAL
    parametersgrid%LOW_DENSITY_RESIDENTIAL = parameters%LOW_DENSITY_RESIDENTIAL
    parametersgrid%HIGH_DENSITY_RESIDENTIAL = parameters%HIGH_DENSITY_RESIDENTIAL
    parametersgrid%HIGH_INTENSITY_INDUSTRIAL = parameters%HIGH_INTENSITY_INDUSTRIAL
    parametersgrid%SB = parameters%SB
    parametersgrid%VKC = parameters%VKC
    parametersgrid%TFRZ = parameters%TFRZ
    parametersgrid%HSUB = parameters%HSUB
    parametersgrid%HVAP = parameters%HVAP
    parametersgrid%HFUS = parameters%HFUS
    parametersgrid%CWAT = parameters%CWAT
    parametersgrid%CICE = parameters%CICE
    parametersgrid%CPAIR = parameters%CPAIR
    parametersgrid%TKWAT = parameters%TKWAT
    parametersgrid%TKICE = parameters%TKICE
    parametersgrid%TKAIR = parameters%TKAIR
    parametersgrid%RAIR = parameters%RAIR
    parametersgrid%RW = parameters%RW
    parametersgrid%DENH2O = parameters%DENH2O
    parametersgrid%DENICE = parameters%DENICE
    parametersgrid%THKW = parameters%THKW
    parametersgrid%THKO = parameters%THKO
    parametersgrid%THKQTZ = parameters%THKQTZ
    parametersgrid%SSI = parameters%SSI
    parametersgrid%MFSNO(ix,iy) = parameters%MFSNO
    parametersgrid%Z0SNO = parameters%Z0SNO
    parametersgrid%SWEMX = parameters%SWEMX
    parametersgrid%TAU0 = parameters%TAU0
    parametersgrid%GRAIN_GROWTH = parameters%GRAIN_GROWTH
    parametersgrid%EXTRA_GROWTH = parameters%EXTRA_GROWTH
    parametersgrid%DIRT_SOOT = parameters%DIRT_SOOT
    parametersgrid%BATS_COSZ = parameters%BATS_COSZ
    parametersgrid%BATS_VIS_NEW = parameters%BATS_VIS_NEW
    parametersgrid%BATS_NIR_NEW = parameters%BATS_NIR_NEW
    parametersgrid%BATS_VIS_AGE = parameters%BATS_VIS_AGE
    parametersgrid%BATS_NIR_AGE = parameters%BATS_NIR_AGE
    parametersgrid%BATS_VIS_DIR = parameters%BATS_VIS_DIR
    parametersgrid%BATS_NIR_DIR = parameters%BATS_NIR_DIR
    parametersgrid%RSURF_SNOW = parameters%RSURF_SNOW
    parametersgrid%RSURF_EXP = parameters%RSURF_EXP
    parametersgrid%ALBSAT(ix,iy,:) = parameters%ALBSAT(:)
    parametersgrid%ALBDRY(ix,iy,:) = parameters%ALBDRY(:)
    parametersgrid%ALBICE(ix,iy,:) = parameters%ALBICE(:)
    parametersgrid%ALBLAK(ix,iy,:) = parameters%ALBLAK(:)
    parametersgrid%OMEGAS(ix,iy,:) = parameters%OMEGAS(:)
    parametersgrid%BETADS = parameters%BETADS
    parametersgrid%BETAIS = parameters%BETAIS
    parametersgrid%EG(ix,iy,:) = parameters%EG(:)
    parametersgrid%WSLMAX = parameters%WSLMAX
    parametersgrid%max_liq_mass_fraction = parameters%max_liq_mass_fraction
    parametersgrid%SNOW_RET_FAC = parameters%SNOW_RET_FAC
    parametersgrid%NBAND = parameters%NBAND
    parametersgrid%MPE = parameters%MPE
    parametersgrid%TOPT = parameters%TOPT
    parametersgrid%O2 = parameters%O2
    parametersgrid%CO2 = parameters%CO2
    parametersgrid%PSIWLT = parameters%PSIWLT
    parametersgrid%GRAV = parameters%GRAV
    !parametersgrid%rain_snow_thresh = parameters%rain_snow_thresh

  end subroutine ParametersVarOutTransfer

end module ParametersTypeTransfer
module ParametersVarInTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine ParametersVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),   intent(inout) :: noahowpmp
    integer                            :: ii

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy, &
              isltyp => NoahowpmpIO%isltyp(NoahowpmpIO%ix,NoahowpmpIO%iy), &
              vegtyp => NoahowpmpIO%vegtyp(NoahowpmpIO%ix,NoahowpmpIO%iy), &
              soilcolor => NoahowpmpIO%soilcolor(NoahowpmpIO%ix,NoahowpmpIO%iy) &
              )

    noahowpmp%parameters%bexp    = NoahowpmpIO%BEXP_TABLE(isltyp)
    noahowpmp%parameters%smcmax  = NoahowpmpIO%SMCMAX_TABLE(isltyp)
    noahowpmp%parameters%smcwlt  = NoahowpmpIO%SMCWLT_TABLE(isltyp)
    noahowpmp%parameters%smcref  = NoahowpmpIO%SMCREF_TABLE(isltyp)
    noahowpmp%parameters%dksat   = NoahowpmpIO%DKSAT_TABLE(isltyp)
    noahowpmp%parameters%dwsat   = NoahowpmpIO%DWSAT_TABLE(isltyp)
    noahowpmp%parameters%psisat  = NoahowpmpIO%PSISAT_TABLE(isltyp)
    noahowpmp%parameters%bvic    = NoahowpmpIO%BVIC_table(isltyp)
    noahowpmp%parameters%AXAJ    = NoahowpmpIO%AXAJ_table(isltyp)
    noahowpmp%parameters%BXAJ    = NoahowpmpIO%BXAJ_table(isltyp)
    noahowpmp%parameters%XXAJ    = NoahowpmpIO%XXAJ_table(isltyp)
    noahowpmp%parameters%BBVIC   = NoahowpmpIO%BBVIC_table(isltyp)
    noahowpmp%parameters%G       = NoahowpmpIO%GDVIC_table(isltyp)
    noahowpmp%parameters%QUARTZ  = NoahowpmpIO%QUARTZ_table(isltyp)

    do ii = 1,12
      noahowpmp%parameters%LAIM(ii) = NoahowpmpIO%LAIM_TABLE(vegtyp, ii)
      noahowpmp%parameters%SAIM(ii) = NoahowpmpIO%SAIM_TABLE(vegtyp, ii)
    end do

    noahowpmp%parameters%CH2OP   = NoahowpmpIO%CH2OP_TABLE(vegtyp)
    noahowpmp%parameters%NROOT   = NoahowpmpIO%NROOT_TABLE(vegtyp)
    noahowpmp%parameters%HVT     = NoahowpmpIO%HVT_TABLE(vegtyp)
    noahowpmp%parameters%HVB     = NoahowpmpIO%HVB_TABLE(vegtyp)
    noahowpmp%parameters%TMIN    = NoahowpmpIO%TMIN_TABLE(vegtyp)
    noahowpmp%parameters%SHDFAC  = NoahowpmpIO%SHDFAC_TABLE(vegtyp) ! this used to be in VEGPARM.TBL, but now somewhere else for hrldas. this is temporarily in MPTABLE.TBL.
    noahowpmp%parameters%SHDMAX  = NoahowpmpIO%SHDFAC_TABLE(vegtyp)
    noahowpmp%parameters%Z0MVT   = NoahowpmpIO%Z0MVT_TABLE(vegtyp)
    noahowpmp%parameters%RC      = NoahowpmpIO%RC_TABLE(vegtyp)
    noahowpmp%parameters%XL      = NoahowpmpIO%XL_TABLE(vegtyp)
    noahowpmp%parameters%BP      = NoahowpmpIO%BP_TABLE(vegtyp)
    noahowpmp%parameters%FOLNMX  = NoahowpmpIO%FOLNMX_TABLE(vegtyp)
    noahowpmp%parameters%QE25    = NoahowpmpIO%QE25_TABLE(vegtyp)
    noahowpmp%parameters%VCMX25  = NoahowpmpIO%VCMX25_TABLE(vegtyp)
    noahowpmp%parameters%MP      = NoahowpmpIO%MP_TABLE(vegtyp)
    noahowpmp%parameters%RGL     = NoahowpmpIO%RGL_TABLE(vegtyp)
    noahowpmp%parameters%RSMIN   = NoahowpmpIO%RS_TABLE(vegtyp)
    noahowpmp%parameters%HS      = NoahowpmpIO%HS_TABLE(vegtyp)
    noahowpmp%parameters%AKC     = NoahowpmpIO%AKC_TABLE(vegtyp)
    noahowpmp%parameters%AKO     = NoahowpmpIO%AKO_TABLE(vegtyp)
    noahowpmp%parameters%AVCMX   = NoahowpmpIO%AVCMX_TABLE(vegtyp)
    noahowpmp%parameters%RSMAX   = NoahowpmpIO%RSMAX_TABLE(vegtyp)
    noahowpmp%parameters%CWP     = NoahowpmpIO%CWPVT_TABLE(vegtyp)
    noahowpmp%parameters%C3PSN   = NoahowpmpIO%C3PSN_TABLE(vegtyp)
    noahowpmp%parameters%DLEAF   = NoahowpmpIO%DLEAF_TABLE(vegtyp)
    noahowpmp%parameters%KC25    = NoahowpmpIO%KC25_TABLE(vegtyp)
    noahowpmp%parameters%KO25    = NoahowpmpIO%KO25_TABLE(vegtyp)

    noahowpmp%parameters%RHOL(1) = NoahowpmpIO%RHOL_TABLE(vegtyp, 1)
    noahowpmp%parameters%RHOL(2) = NoahowpmpIO%RHOL_TABLE(vegtyp, 2)
    noahowpmp%parameters%RHOS(1) = NoahowpmpIO%RHOS_TABLE(vegtyp, 1)
    noahowpmp%parameters%RHOS(2) = NoahowpmpIO%RHOS_TABLE(vegtyp, 2)
    noahowpmp%parameters%TAUL(1) = NoahowpmpIO%TAUL_TABLE(vegtyp, 1)
    noahowpmp%parameters%TAUL(2) = NoahowpmpIO%TAUL_TABLE(vegtyp, 2)
    noahowpmp%parameters%TAUS(1) = NoahowpmpIO%TAUS_TABLE(vegtyp, 1)
    noahowpmp%parameters%TAUS(2) = NoahowpmpIO%TAUS_TABLE(vegtyp, 2)

    noahowpmp%parameters%refkdt       = NoahowpmpIO%REFKDT_TABLE
    noahowpmp%parameters%refdk        = NoahowpmpIO%REFDK_TABLE
    noahowpmp%parameters%kdt          = NoahowpmpIO%refkdt * NoahowpmpIO%dksat(ix,iy,1) / NoahowpmpIO%refdk
    noahowpmp%parameters%csoil        = NoahowpmpIO%CSOIL_TABLE
    noahowpmp%parameters%Z0           = NoahowpmpIO%Z0_TABLE     ! bare soil roughness length (m). in GENPARM.TBL.  NOTE: This is hard-coded in hrldas version of noah-mp
    noahowpmp%parameters%CZIL         = NoahowpmpIO%CZIL_TABLE
    noahowpmp%parameters%ZBOT         = NoahowpmpIO%ZBOT_TABLE
    noahowpmp%parameters%frzx         = 0.15 * (NoahowpmpIO%smcmax(ix,iy,1) / NoahowpmpIO%smcref(ix,iy,1)) * (0.412 / 0.468)
    noahowpmp%parameters%SSI          = NoahowpmpIO%SSI_TABLE
    noahowpmp%parameters%MFSNO        = NoahowpmpIO%MFSNO_TABLE(vegtyp)
    noahowpmp%parameters%Z0SNO        = NoahowpmpIO%Z0SNO_TABLE
    noahowpmp%parameters%SWEMX        = NoahowpmpIO%SWEMX_TABLE
    noahowpmp%parameters%TAU0         = NoahowpmpIO%TAU0_TABLE
    noahowpmp%parameters%GRAIN_GROWTH = NoahowpmpIO%GRAIN_GROWTH_TABLE
    noahowpmp%parameters%EXTRA_GROWTH = NoahowpmpIO%EXTRA_GROWTH_TABLE
    noahowpmp%parameters%DIRT_SOOT    = NoahowpmpIO%DIRT_SOOT_TABLE
    noahowpmp%parameters%BATS_COSZ    = NoahowpmpIO%BATS_COSZ_TABLE
    noahowpmp%parameters%BATS_VIS_NEW = NoahowpmpIO%BATS_VIS_NEW_TABLE
    noahowpmp%parameters%BATS_NIR_NEW = NoahowpmpIO%BATS_NIR_NEW_TABLE
    noahowpmp%parameters%BATS_VIS_AGE = NoahowpmpIO%BATS_VIS_AGE_TABLE
    noahowpmp%parameters%BATS_NIR_AGE = NoahowpmpIO%BATS_NIR_AGE_TABLE
    noahowpmp%parameters%BATS_VIS_DIR = NoahowpmpIO%BATS_VIS_DIR_TABLE
    noahowpmp%parameters%BATS_NIR_DIR = NoahowpmpIO%BATS_NIR_DIR_TABLE
    noahowpmp%parameters%RSURF_SNOW   = NoahowpmpIO%RSURF_SNOW_TABLE
    noahowpmp%parameters%RSURF_EXP    = NoahowpmpIO%RSURF_EXP_TABLE

    noahowpmp%parameters%ALBSAT(1)    = NoahowpmpIO%ALBSAT_TABLE(soilcolor, 1)
    noahowpmp%parameters%ALBSAT(2)    = NoahowpmpIO%ALBSAT_TABLE(soilcolor, 2)
    noahowpmp%parameters%ALBDRY(1)    = NoahowpmpIO%ALBDRY_TABLE(soilcolor, 1)
    noahowpmp%parameters%ALBDRY(2)    = NoahowpmpIO%ALBDRY_TABLE(soilcolor, 2)
    noahowpmp%parameters%ALBICE       = NoahowpmpIO%ALBICE_TABLE
    noahowpmp%parameters%ALBLAK       = NoahowpmpIO%ALBLAK_TABLE
    noahowpmp%parameters%OMEGAS       = NoahowpmpIO%OMEGAS_TABLE
    noahowpmp%parameters%BETADS       = NoahowpmpIO%BETADS_TABLE
    noahowpmp%parameters%BETAIS       = NoahowpmpIO%BETAIS_TABLE
    noahowpmp%parameters%EG           = NoahowpmpIO%EG_TABLE
    noahowpmp%parameters%slope        = NoahowpmpIO%SLOPE_TABLE(1)

    noahowpmp%parameters%ISURBAN                   = NoahowpmpIO%ISURBAN_TABLE
    noahowpmp%parameters%ISWATER                   = NoahowpmpIO%ISWATER_TABLE
    noahowpmp%parameters%ISBARREN                  = NoahowpmpIO%ISBARREN_TABLE
    noahowpmp%parameters%ISICE                     = NoahowpmpIO%ISICE_TABLE
    noahowpmp%parameters%ISCROP                    = NoahowpmpIO%ISCROP_TABLE
    noahowpmp%parameters%EBLFOREST                 = NoahowpmpIO%EBLFOREST_TABLE
    noahowpmp%parameters%NATURAL                   = NoahowpmpIO%NATURAL_TABLE
    noahowpmp%parameters%LOW_DENSITY_RESIDENTIAL   = NoahowpmpIO%LCZ_1_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_1
    noahowpmp%parameters%HIGH_DENSITY_RESIDENTIAL  = NoahowpmpIO%LCZ_2_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_2
    noahowpmp%parameters%HIGH_INTENSITY_INDUSTRIAL = NoahowpmpIO%LCZ_3_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_3

    noahowpmp%parameters%urban_flag = .false.
    noahowpmp%parameters%timean     = 10.5
    noahowpmp%parameters%fsatmx     = 0.38
    noahowpmp%parameters%GRAV       = 9.80616
    noahowpmp%parameters%SB         = 5.67E-08
    noahowpmp%parameters%VKC        = 0.40
    noahowpmp%parameters%TFRZ       = 273.16
    noahowpmp%parameters%HSUB       = 2.8440E06
    noahowpmp%parameters%HVAP       = 2.5104E06
    noahowpmp%parameters%HFUS       = 0.3336E06
    noahowpmp%parameters%CWAT       = 4.188E06
    noahowpmp%parameters%CICE       = 2.094E06
    noahowpmp%parameters%CPAIR      = 1004.64
    noahowpmp%parameters%TKWAT      = 0.6
    noahowpmp%parameters%TKICE      = 2.2
    noahowpmp%parameters%TKAIR      = 0.023
    noahowpmp%parameters%RAIR       = 287.04
    noahowpmp%parameters%RW         = 461.269
    noahowpmp%parameters%DENH2O     = 1000.0
    noahowpmp%parameters%DENICE     = 917.0
    noahowpmp%parameters%THKW       = 0.57
    noahowpmp%parameters%THKO       = 2.0
    noahowpmp%parameters%THKQTZ     = 7.7
    noahowpmp%parameters%WSLMAX     = 5000.0
    noahowpmp%parameters%max_liq_mass_fraction = 0.4
    noahowpmp%parameters%SNOW_RET_FAC = 5.e-5
    noahowpmp%parameters%NBAND        = 2       ! do not change
    noahowpmp%parameters%MPE          = 1.E-06  ! do not change ! need to make this a parameter
    noahowpmp%parameters%TOPT         = 1.E-06  ! Optimum transpiration air temperature [K]

    noahowpmp%parameters%CO2       =  395.e-06   ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    noahowpmp%parameters%O2        =  0.209      ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    noahowpmp%parameters%PSIWLT    = -150.0      ! originally a fixed parameter set in ENERGY()
    noahowpmp%parameters%TBOT      = 263.0       ! (K) can be updated depending on option OPT_TBOT

    ! Assign rain-snow threshold based on option
    IF(NoahowpmpIO%opt_snf(ix,iy) == 2) THEN
      NoahowpmpIO%rain_snow_thresh = NoahowpmpIO%TFRZ + 2.2
    ELSE IF(NoahowpmpIO%opt_snf(ix,iy) == 3) THEN
      NoahowpmpIO%rain_snow_thresh = NoahowpmpIO%TFRZ
    ELSE IF(NoahowpmpIO%opt_snf(ix,iy) == 5 .or. NoahowpmpIO%opt_snf(ix,iy) == 6) THEN
      NoahowpmpIO%rain_snow_thresh = NoahowpmpIO%TFRZ + NoahowpmpIO%rain_snow_thresh
    ELSE 
      NoahowpmpIO%rain_snow_thresh = NoahowpmpIO%TFRZ ! set to TFRZ as a backup
    ENDIF

    ! Assign initial soil moisture based on variable or uniform initial conditions
    NoahowpmpIO%zwt_init = NoahowpmpIO%zwt

    end associate

  end subroutine

  subroutine ParametersVarOutTransfer(Noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    NoahowpmpIO%bexp(ix,iy,:) = Noahowpmp%parameters%bexp(:)
    NoahowpmpIO%smcmax(ix,iy,:) = Noahowpmp%parameters%smcmax(:)
    NoahowpmpIO%smcwlt(ix,iy,:) = Noahowpmp%parameters%smcwlt(:)
    NoahowpmpIO%smcref(ix,iy,:) = Noahowpmp%parameters%smcref(:)
    NoahowpmpIO%dksat(ix,iy,:) = Noahowpmp%parameters%dksat(:)
    NoahowpmpIO%dwsat(ix,iy,:) = Noahowpmp%parameters%dwsat(:)
    NoahowpmpIO%psisat(ix,iy,:) = Noahowpmp%parameters%psisat(:)
    NoahowpmpIO%bvic(ix,iy) = Noahowpmp%parameters%bvic
    NoahowpmpIO%AXAJ(ix,iy) = Noahowpmp%parameters%AXAJ
    NoahowpmpIO%BXAJ(ix,iy) = Noahowpmp%parameters%BXAJ
    NoahowpmpIO%XXAJ(ix,iy) = Noahowpmp%parameters%XXAJ
    NoahowpmpIO%BBVIC(ix,iy) = Noahowpmp%parameters%BBVIC
    NoahowpmpIO%G(ix,iy) = Noahowpmp%parameters%G
    NoahowpmpIO%QUARTZ(ix,iy) = Noahowpmp%parameters%QUARTZ
    NoahowpmpIO%kdt(ix,iy) = Noahowpmp%parameters%kdt
    NoahowpmpIO%refkdt = Noahowpmp%parameters%refkdt
    NoahowpmpIO%refdk = Noahowpmp%parameters%refdk
    NoahowpmpIO%csoil = Noahowpmp%parameters%csoil
    NoahowpmpIO%Z0 = Noahowpmp%parameters%Z0
    NoahowpmpIO%CZIL = Noahowpmp%parameters%CZIL
    NoahowpmpIO%ZBOT = Noahowpmp%parameters%ZBOT
    NoahowpmpIO%frzx(ix,iy) = Noahowpmp%parameters%frzx
    NoahowpmpIO%slope = Noahowpmp%parameters%slope
    NoahowpmpIO%timean = Noahowpmp%parameters%timean
    NoahowpmpIO%fsatmx = Noahowpmp%parameters%fsatmx
    NoahowpmpIO%ZWT_INIT(ix,iy) = Noahowpmp%parameters%ZWT_INIT
    NoahowpmpIO%urban_flag(ix,iy) = Noahowpmp%parameters%urban_flag
    NoahowpmpIO%LAIM(ix,iy,:) = Noahowpmp%parameters%LAIM(:)
    NoahowpmpIO%SAIM(ix,iy,:) = Noahowpmp%parameters%SAIM(:)
    NoahowpmpIO%LAI(ix,iy) = Noahowpmp%parameters%LAI
    NoahowpmpIO%SAI(ix,iy) = Noahowpmp%parameters%SAI
    NoahowpmpIO%CH2OP(ix,iy) = Noahowpmp%parameters%CH2OP
    NoahowpmpIO%NROOT(ix,iy) = Noahowpmp%parameters%NROOT
    NoahowpmpIO%HVT(ix,iy) = Noahowpmp%parameters%HVT
    NoahowpmpIO%HVB(ix,iy) = Noahowpmp%parameters%HVB
    NoahowpmpIO%TMIN(ix,iy) = Noahowpmp%parameters%TMIN
    NoahowpmpIO%SHDFAC(ix,iy) = Noahowpmp%parameters%SHDFAC
    NoahowpmpIO%SHDMAX(ix,iy) = Noahowpmp%parameters%SHDMAX
    NoahowpmpIO%Z0MVT(ix,iy) = Noahowpmp%parameters%Z0MVT
    NoahowpmpIO%RC(ix,iy) = Noahowpmp%parameters%RC
    NoahowpmpIO%XL(ix,iy) = Noahowpmp%parameters%XL
    NoahowpmpIO%BP(ix,iy) = Noahowpmp%parameters%BP
    NoahowpmpIO%FOLNMX(ix,iy) = Noahowpmp%parameters%FOLNMX
    NoahowpmpIO%QE25(ix,iy) = Noahowpmp%parameters%QE25
    NoahowpmpIO%VCMX25(ix,iy) = Noahowpmp%parameters%VCMX25
    NoahowpmpIO%MP(ix,iy) = Noahowpmp%parameters%MP
    NoahowpmpIO%RGL(ix,iy) = Noahowpmp%parameters%RGL
    NoahowpmpIO%RSMIN(ix,iy) = Noahowpmp%parameters%RSMIN
    NoahowpmpIO%HS(ix,iy) = Noahowpmp%parameters%HS
    NoahowpmpIO%AKC(ix,iy) = Noahowpmp%parameters%AKC
    NoahowpmpIO%AKO(ix,iy) = Noahowpmp%parameters%AKO
    NoahowpmpIO%AVCMX(ix,iy) = Noahowpmp%parameters%AVCMX
    NoahowpmpIO%RSMAX(ix,iy) = Noahowpmp%parameters%RSMAX
    NoahowpmpIO%CWP(ix,iy) = Noahowpmp%parameters%CWP
    NoahowpmpIO%C3PSN(ix,iy) = Noahowpmp%parameters%C3PSN
    NoahowpmpIO%DLEAF(ix,iy) = Noahowpmp%parameters%DLEAF
    NoahowpmpIO%KC25(ix,iy) = Noahowpmp%parameters%KC25
    NoahowpmpIO%KO25(ix,iy) = Noahowpmp%parameters%KO25
    NoahowpmpIO%ELAI(ix,iy) = Noahowpmp%parameters%ELAI
    NoahowpmpIO%ESAI(ix,iy) = Noahowpmp%parameters%ESAI
    NoahowpmpIO%VAI(ix,iy) = Noahowpmp%parameters%VAI
    NoahowpmpIO%VEG(ix,iy) = Noahowpmp%parameters%VEG
    NoahowpmpIO%FVEG(ix,iy) = Noahowpmp%parameters%FVEG
    NoahowpmpIO%RHOL(ix,iy,:) = Noahowpmp%parameters%RHOL(:)
    NoahowpmpIO%RHOS(ix,iy,:) = Noahowpmp%parameters%RHOS(:)
    NoahowpmpIO%TAUL(ix,iy,:) = Noahowpmp%parameters%TAUL(:)
    NoahowpmpIO%TAUS(ix,iy,:) = Noahowpmp%parameters%TAUS(:)
    NoahowpmpIO%ISURBAN = Noahowpmp%parameters%ISURBAN
    NoahowpmpIO%ISWATER = Noahowpmp%parameters%ISWATER
    NoahowpmpIO%ISBARREN = Noahowpmp%parameters%ISBARREN
    NoahowpmpIO%ISICE = Noahowpmp%parameters%ISICE
    NoahowpmpIO%ISCROP = Noahowpmp%parameters%ISCROP
    NoahowpmpIO%EBLFOREST = Noahowpmp%parameters%EBLFOREST
    NoahowpmpIO%NATURAL = Noahowpmp%parameters%NATURAL
    NoahowpmpIO%LOW_DENSITY_RESIDENTIAL = Noahowpmp%parameters%LOW_DENSITY_RESIDENTIAL
    NoahowpmpIO%HIGH_DENSITY_RESIDENTIAL = Noahowpmp%parameters%HIGH_DENSITY_RESIDENTIAL
    NoahowpmpIO%HIGH_INTENSITY_INDUSTRIAL = Noahowpmp%parameters%HIGH_INTENSITY_INDUSTRIAL
    NoahowpmpIO%SB = Noahowpmp%parameters%SB
    NoahowpmpIO%VKC = Noahowpmp%parameters%VKC
    NoahowpmpIO%TFRZ = Noahowpmp%parameters%TFRZ
    NoahowpmpIO%HSUB = Noahowpmp%parameters%HSUB
    NoahowpmpIO%HVAP = Noahowpmp%parameters%HVAP
    NoahowpmpIO%HFUS = Noahowpmp%parameters%HFUS
    NoahowpmpIO%CWAT = Noahowpmp%parameters%CWAT
    NoahowpmpIO%CICE = Noahowpmp%parameters%CICE
    NoahowpmpIO%CPAIR = Noahowpmp%parameters%CPAIR
    NoahowpmpIO%TKWAT = Noahowpmp%parameters%TKWAT
    NoahowpmpIO%TKICE = Noahowpmp%parameters%TKICE
    NoahowpmpIO%TKAIR = Noahowpmp%parameters%TKAIR
    NoahowpmpIO%RAIR = Noahowpmp%parameters%RAIR
    NoahowpmpIO%RW = Noahowpmp%parameters%RW
    NoahowpmpIO%DENH2O = Noahowpmp%parameters%DENH2O
    NoahowpmpIO%DENICE = Noahowpmp%parameters%DENICE
    NoahowpmpIO%THKW = Noahowpmp%parameters%THKW
    NoahowpmpIO%THKO = Noahowpmp%parameters%THKO
    NoahowpmpIO%THKQTZ = Noahowpmp%parameters%THKQTZ
    NoahowpmpIO%SSI = Noahowpmp%parameters%SSI
    NoahowpmpIO%MFSNO(ix,iy) = Noahowpmp%parameters%MFSNO
    NoahowpmpIO%Z0SNO = Noahowpmp%parameters%Z0SNO
    NoahowpmpIO%SWEMX = Noahowpmp%parameters%SWEMX
    NoahowpmpIO%TAU0 = Noahowpmp%parameters%TAU0
    NoahowpmpIO%GRAIN_GROWTH = Noahowpmp%parameters%GRAIN_GROWTH
    NoahowpmpIO%EXTRA_GROWTH = Noahowpmp%parameters%EXTRA_GROWTH
    NoahowpmpIO%DIRT_SOOT = Noahowpmp%parameters%DIRT_SOOT
    NoahowpmpIO%BATS_COSZ = Noahowpmp%parameters%BATS_COSZ
    NoahowpmpIO%BATS_VIS_NEW = Noahowpmp%parameters%BATS_VIS_NEW
    NoahowpmpIO%BATS_NIR_NEW = Noahowpmp%parameters%BATS_NIR_NEW
    NoahowpmpIO%BATS_VIS_AGE = Noahowpmp%parameters%BATS_VIS_AGE
    NoahowpmpIO%BATS_NIR_AGE = Noahowpmp%parameters%BATS_NIR_AGE
    NoahowpmpIO%BATS_VIS_DIR = Noahowpmp%parameters%BATS_VIS_DIR
    NoahowpmpIO%BATS_NIR_DIR = Noahowpmp%parameters%BATS_NIR_DIR
    NoahowpmpIO%RSURF_SNOW = Noahowpmp%parameters%RSURF_SNOW
    NoahowpmpIO%RSURF_EXP = Noahowpmp%parameters%RSURF_EXP
    NoahowpmpIO%ALBSAT(ix,iy,:) = Noahowpmp%parameters%ALBSAT(:)
    NoahowpmpIO%ALBDRY(ix,iy,:) = Noahowpmp%parameters%ALBDRY(:)
    NoahowpmpIO%ALBICE(ix,iy,:) = Noahowpmp%parameters%ALBICE(:)
    NoahowpmpIO%ALBLAK(ix,iy,:) = Noahowpmp%parameters%ALBLAK(:)
    NoahowpmpIO%OMEGAS(ix,iy,:) = Noahowpmp%parameters%OMEGAS(:)
    NoahowpmpIO%BETADS = Noahowpmp%parameters%BETADS
    NoahowpmpIO%BETAIS = Noahowpmp%parameters%BETAIS
    NoahowpmpIO%EG(ix,iy,:) = Noahowpmp%parameters%EG(:)
    NoahowpmpIO%WSLMAX = Noahowpmp%parameters%WSLMAX
    NoahowpmpIO%max_liq_mass_fraction = Noahowpmp%parameters%max_liq_mass_fraction
    NoahowpmpIO%SNOW_RET_FAC = Noahowpmp%parameters%SNOW_RET_FAC
    NoahowpmpIO%NBAND = Noahowpmp%parameters%NBAND
    NoahowpmpIO%MPE = Noahowpmp%parameters%MPE
    NoahowpmpIO%TOPT(ix,iy) = Noahowpmp%parameters%TOPT
    NoahowpmpIO%O2 = Noahowpmp%parameters%O2
    NoahowpmpIO%CO2 = Noahowpmp%parameters%CO2
    NoahowpmpIO%PSIWLT = Noahowpmp%parameters%PSIWLT
    NoahowpmpIO%GRAV = Noahowpmp%parameters%GRAV
    NoahowpmpIO%rain_snow_thresh = Noahowpmp%parameters%rain_snow_thresh

  end associate

  end subroutine ParametersVarOutTransfer

end module ParametersVarInTransferModule
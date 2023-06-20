module ParametersVarTransferModule

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine ParametersVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(in)    :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp
    integer                               :: ii

    associate(ix        => noahowpgrid%ix, &
              iy        => noahowpgrid%iy, &
              isltyp    => noahowpgrid%isltyp(noahowpgrid%ix,noahowpgrid%iy),   &
              vegtyp    => noahowpgrid%vegtyp(noahowpgrid%ix,noahowpgrid%iy),   &
              soilcolor => noahowpgrid%soilcolor(noahowpgrid%ix,noahowpgrid%iy) &
              )

    noahowp%parameters%bexp    = noahowpgrid%BEXP_TABLE(isltyp)
    noahowp%parameters%smcmax  = noahowpgrid%SMCMAX_TABLE(isltyp)
    noahowp%parameters%smcwlt  = noahowpgrid%SMCWLT_TABLE(isltyp)
    noahowp%parameters%smcref  = noahowpgrid%SMCREF_TABLE(isltyp)
    noahowp%parameters%dksat   = noahowpgrid%DKSAT_TABLE(isltyp)
    noahowp%parameters%dwsat   = noahowpgrid%DWSAT_TABLE(isltyp)
    noahowp%parameters%psisat  = noahowpgrid%PSISAT_TABLE(isltyp)
    noahowp%parameters%bvic    = noahowpgrid%BVIC_table(isltyp)
    noahowp%parameters%AXAJ    = noahowpgrid%AXAJ_table(isltyp)
    noahowp%parameters%BXAJ    = noahowpgrid%BXAJ_table(isltyp)
    noahowp%parameters%XXAJ    = noahowpgrid%XXAJ_table(isltyp)
    noahowp%parameters%BBVIC   = noahowpgrid%BBVIC_table(isltyp)
    noahowp%parameters%G       = noahowpgrid%GDVIC_table(isltyp)
    noahowp%parameters%QUARTZ  = noahowpgrid%QUARTZ_table(isltyp)

    do ii = 1,12
      noahowp%parameters%LAIM(ii) = noahowpgrid%LAIM_TABLE(vegtyp, ii)
      noahowp%parameters%SAIM(ii) = noahowpgrid%SAIM_TABLE(vegtyp, ii)
    end do

    noahowp%parameters%CH2OP   = noahowpgrid%CH2OP_TABLE(vegtyp)
    noahowp%parameters%NROOT   = noahowpgrid%NROOT_TABLE(vegtyp)
    noahowp%parameters%HVT     = noahowpgrid%HVT_TABLE(vegtyp)
    noahowp%parameters%HVB     = noahowpgrid%HVB_TABLE(vegtyp)
    noahowp%parameters%TMIN    = noahowpgrid%TMIN_TABLE(vegtyp)
    noahowp%parameters%SHDFAC  = noahowpgrid%SHDFAC_TABLE(vegtyp) ! this used to be in VEGPARM.TBL, but now somewhere else for hrldas. this is temporarily in MPTABLE.TBL.
    noahowp%parameters%SHDMAX  = noahowpgrid%SHDFAC_TABLE(vegtyp)
    noahowp%parameters%Z0MVT   = noahowpgrid%Z0MVT_TABLE(vegtyp)
    noahowp%parameters%RC      = noahowpgrid%RC_TABLE(vegtyp)
    noahowp%parameters%XL      = noahowpgrid%XL_TABLE(vegtyp)
    noahowp%parameters%BP      = noahowpgrid%BP_TABLE(vegtyp)
    noahowp%parameters%FOLNMX  = noahowpgrid%FOLNMX_TABLE(vegtyp)
    noahowp%parameters%QE25    = noahowpgrid%QE25_TABLE(vegtyp)
    noahowp%parameters%VCMX25  = noahowpgrid%VCMX25_TABLE(vegtyp)
    noahowp%parameters%MP      = noahowpgrid%MP_TABLE(vegtyp)
    noahowp%parameters%RGL     = noahowpgrid%RGL_TABLE(vegtyp)
    noahowp%parameters%RSMIN   = noahowpgrid%RS_TABLE(vegtyp)
    noahowp%parameters%HS      = noahowpgrid%HS_TABLE(vegtyp)
    noahowp%parameters%AKC     = noahowpgrid%AKC_TABLE(vegtyp)
    noahowp%parameters%AKO     = noahowpgrid%AKO_TABLE(vegtyp)
    noahowp%parameters%AVCMX   = noahowpgrid%AVCMX_TABLE(vegtyp)
    noahowp%parameters%RSMAX   = noahowpgrid%RSMAX_TABLE(vegtyp)
    noahowp%parameters%CWP     = noahowpgrid%CWPVT_TABLE(vegtyp)
    noahowp%parameters%C3PSN   = noahowpgrid%C3PSN_TABLE(vegtyp)
    noahowp%parameters%DLEAF   = noahowpgrid%DLEAF_TABLE(vegtyp)
    noahowp%parameters%KC25    = noahowpgrid%KC25_TABLE(vegtyp)
    noahowp%parameters%KO25    = noahowpgrid%KO25_TABLE(vegtyp)

    noahowp%parameters%RHOL(1) = noahowpgrid%RHOL_TABLE(vegtyp, 1)
    noahowp%parameters%RHOL(2) = noahowpgrid%RHOL_TABLE(vegtyp, 2)
    noahowp%parameters%RHOS(1) = noahowpgrid%RHOS_TABLE(vegtyp, 1)
    noahowp%parameters%RHOS(2) = noahowpgrid%RHOS_TABLE(vegtyp, 2)
    noahowp%parameters%TAUL(1) = noahowpgrid%TAUL_TABLE(vegtyp, 1)
    noahowp%parameters%TAUL(2) = noahowpgrid%TAUL_TABLE(vegtyp, 2)
    noahowp%parameters%TAUS(1) = noahowpgrid%TAUS_TABLE(vegtyp, 1)
    noahowp%parameters%TAUS(2) = noahowpgrid%TAUS_TABLE(vegtyp, 2)

    noahowp%parameters%refkdt       = noahowpgrid%REFKDT_TABLE
    noahowp%parameters%refdk        = noahowpgrid%REFDK_TABLE
    noahowp%parameters%kdt          = noahowp%parameters%refkdt * noahowp%parameters%dksat(1) / noahowp%parameters%refdk
    noahowp%parameters%csoil        = noahowpgrid%CSOIL_TABLE
    noahowp%parameters%Z0           = noahowpgrid%Z0_TABLE     ! bare soil roughness length (m). in GENPARM.TBL.  NOTE: This is hard-coded in hrldas version of noah-mp
    noahowp%parameters%CZIL         = noahowpgrid%CZIL_TABLE
    noahowp%parameters%ZBOT         = noahowpgrid%ZBOT_TABLE
    noahowp%parameters%frzx         = 0.15 * (noahowp%parameters%smcmax(1) / noahowp%parameters%smcref(1)) * (0.412 / 0.468)
    noahowp%parameters%SSI          = noahowpgrid%SSI_TABLE
    noahowp%parameters%MFSNO        = noahowpgrid%MFSNO_TABLE(vegtyp)
    noahowp%parameters%Z0SNO        = noahowpgrid%Z0SNO_TABLE
    noahowp%parameters%SWEMX        = noahowpgrid%SWEMX_TABLE
    noahowp%parameters%TAU0         = noahowpgrid%TAU0_TABLE
    noahowp%parameters%GRAIN_GROWTH = noahowpgrid%GRAIN_GROWTH_TABLE
    noahowp%parameters%EXTRA_GROWTH = noahowpgrid%EXTRA_GROWTH_TABLE
    noahowp%parameters%DIRT_SOOT    = noahowpgrid%DIRT_SOOT_TABLE
    noahowp%parameters%BATS_COSZ    = noahowpgrid%BATS_COSZ_TABLE
    noahowp%parameters%BATS_VIS_NEW = noahowpgrid%BATS_VIS_NEW_TABLE
    noahowp%parameters%BATS_NIR_NEW = noahowpgrid%BATS_NIR_NEW_TABLE
    noahowp%parameters%BATS_VIS_AGE = noahowpgrid%BATS_VIS_AGE_TABLE
    noahowp%parameters%BATS_NIR_AGE = noahowpgrid%BATS_NIR_AGE_TABLE
    noahowp%parameters%BATS_VIS_DIR = noahowpgrid%BATS_VIS_DIR_TABLE
    noahowp%parameters%BATS_NIR_DIR = noahowpgrid%BATS_NIR_DIR_TABLE
    noahowp%parameters%RSURF_SNOW   = noahowpgrid%RSURF_SNOW_TABLE
    noahowp%parameters%RSURF_EXP    = noahowpgrid%RSURF_EXP_TABLE

    noahowp%parameters%ALBSAT(1)    = noahowpgrid%ALBSAT_TABLE(soilcolor, 1)
    noahowp%parameters%ALBSAT(2)    = noahowpgrid%ALBSAT_TABLE(soilcolor, 2)
    noahowp%parameters%ALBDRY(1)    = noahowpgrid%ALBDRY_TABLE(soilcolor, 1)
    noahowp%parameters%ALBDRY(2)    = noahowpgrid%ALBDRY_TABLE(soilcolor, 2)
    noahowp%parameters%ALBICE       = noahowpgrid%ALBICE_TABLE
    noahowp%parameters%ALBLAK       = noahowpgrid%ALBLAK_TABLE
    noahowp%parameters%OMEGAS       = noahowpgrid%OMEGAS_TABLE
    noahowp%parameters%BETADS       = noahowpgrid%BETADS_TABLE
    noahowp%parameters%BETAIS       = noahowpgrid%BETAIS_TABLE
    noahowp%parameters%EG           = noahowpgrid%EG_TABLE
    noahowp%parameters%slope        = noahowpgrid%SLOPE_TABLE(1)

    noahowp%parameters%ISURBAN                   = noahowpgrid%ISURBAN_TABLE
    noahowp%parameters%ISWATER                   = noahowpgrid%ISWATER_TABLE
    noahowp%parameters%ISBARREN                  = noahowpgrid%ISBARREN_TABLE
    noahowp%parameters%ISICE                     = noahowpgrid%ISICE_TABLE
    noahowp%parameters%ISCROP                    = noahowpgrid%ISCROP_TABLE
    noahowp%parameters%EBLFOREST                 = noahowpgrid%EBLFOREST_TABLE
    noahowp%parameters%NATURAL                   = noahowpgrid%NATURAL_TABLE
    noahowp%parameters%LOW_DENSITY_RESIDENTIAL   = noahowpgrid%LCZ_1_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_1
    noahowp%parameters%HIGH_DENSITY_RESIDENTIAL  = noahowpgrid%LCZ_2_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_2
    noahowp%parameters%HIGH_INTENSITY_INDUSTRIAL = noahowpgrid%LCZ_3_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_3

    noahowp%parameters%urban_flag = .false.
    noahowp%parameters%timean     = 10.5
    noahowp%parameters%fsatmx     = 0.38
    noahowp%parameters%GRAV       = 9.80616
    noahowp%parameters%SB         = 5.67E-08
    noahowp%parameters%VKC        = 0.40
    noahowp%parameters%TFRZ       = 273.16
    noahowp%parameters%HSUB       = 2.8440E06
    noahowp%parameters%HVAP       = 2.5104E06
    noahowp%parameters%HFUS       = 0.3336E06
    noahowp%parameters%CWAT       = 4.188E06
    noahowp%parameters%CICE       = 2.094E06
    noahowp%parameters%CPAIR      = 1004.64
    noahowp%parameters%TKWAT      = 0.6
    noahowp%parameters%TKICE      = 2.2
    noahowp%parameters%TKAIR      = 0.023
    noahowp%parameters%RAIR       = 287.04
    noahowp%parameters%RW         = 461.269
    noahowp%parameters%DENH2O     = 1000.0
    noahowp%parameters%DENICE     = 917.0
    noahowp%parameters%THKW       = 0.57
    noahowp%parameters%THKO       = 2.0
    noahowp%parameters%THKQTZ     = 7.7
    noahowp%parameters%WSLMAX     = 5000.0
    noahowp%parameters%max_liq_mass_fraction = 0.4
    noahowp%parameters%SNOW_RET_FAC = 5.e-5
    noahowp%parameters%NBAND        = 2       ! do not change
    noahowp%parameters%MPE          = 1.E-06  ! do not change ! need to make this a parameter
    noahowp%parameters%TOPT         = 1.E-06  ! Optimum transpiration air temperature [K]

    noahowp%parameters%CO2       =  395.e-06   ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    noahowp%parameters%O2        =  0.209      ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    noahowp%parameters%PSIWLT    = -150.0      ! originally a fixed parameter set in ENERGY()
    noahowp%parameters%TBOT      = 263.0       ! (K) can be updated depending on option OPT_TBOT

    ! Assign rain-snow threshold based on option
    IF(noahowpgrid%opt_snf(ix,iy) == 2) THEN
      noahowp%parameters%rain_snow_thresh = noahowpgrid%TFRZ + 2.2
    ELSE IF(noahowpgrid%opt_snf(ix,iy) == 3) THEN
      noahowp%parameters%rain_snow_thresh = noahowpgrid%TFRZ
    ELSE IF(noahowpgrid%opt_snf(ix,iy) == 5 .or. noahowpgrid%opt_snf(ix,iy) == 6) THEN
      noahowp%parameters%rain_snow_thresh = noahowpgrid%TFRZ + noahowpgrid%rain_snow_thresh
    ELSE 
      noahowp%parameters%rain_snow_thresh = noahowpgrid%TFRZ ! set to TFRZ as a backup
    ENDIF

    ! Assign initial soil moisture based on variable or uniform initial conditions
    noahowp%parameters%zwt_init = noahowpgrid%zwt(ix,iy)

    end associate

  end subroutine

  subroutine ParametersVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(in)    :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowpgrid%bexp(ix,iy,:) = noahowp%parameters%bexp(:)
    noahowpgrid%smcmax(ix,iy,:) = noahowp%parameters%smcmax(:)
    noahowpgrid%smcwlt(ix,iy,:) = noahowp%parameters%smcwlt(:)
    noahowpgrid%smcref(ix,iy,:) = noahowp%parameters%smcref(:)
    noahowpgrid%dksat(ix,iy,:) = noahowp%parameters%dksat(:)
    noahowpgrid%dwsat(ix,iy,:) = noahowp%parameters%dwsat(:)
    noahowpgrid%psisat(ix,iy,:) = noahowp%parameters%psisat(:)
    noahowpgrid%bvic(ix,iy) = noahowp%parameters%bvic
    noahowpgrid%AXAJ(ix,iy) = noahowp%parameters%AXAJ
    noahowpgrid%BXAJ(ix,iy) = noahowp%parameters%BXAJ
    noahowpgrid%XXAJ(ix,iy) = noahowp%parameters%XXAJ
    noahowpgrid%BBVIC(ix,iy) = noahowp%parameters%BBVIC
    noahowpgrid%G(ix,iy) = noahowp%parameters%G
    noahowpgrid%QUARTZ(ix,iy) = noahowp%parameters%QUARTZ
    noahowpgrid%kdt(ix,iy) = noahowp%parameters%kdt
    noahowpgrid%refkdt = noahowp%parameters%refkdt
    noahowpgrid%refdk = noahowp%parameters%refdk
    noahowpgrid%csoil = noahowp%parameters%csoil
    noahowpgrid%Z0 = noahowp%parameters%Z0
    noahowpgrid%CZIL = noahowp%parameters%CZIL
    noahowpgrid%ZBOT = noahowp%parameters%ZBOT
    noahowpgrid%frzx(ix,iy) = noahowp%parameters%frzx
    noahowpgrid%slope = noahowp%parameters%slope
    noahowpgrid%timean = noahowp%parameters%timean
    noahowpgrid%fsatmx = noahowp%parameters%fsatmx
    noahowpgrid%ZWT_INIT(ix,iy) = noahowp%parameters%ZWT_INIT
    noahowpgrid%urban_flag(ix,iy) = noahowp%parameters%urban_flag
    noahowpgrid%LAIM(ix,iy,:) = noahowp%parameters%LAIM(:)
    noahowpgrid%SAIM(ix,iy,:) = noahowp%parameters%SAIM(:)
    noahowpgrid%LAI(ix,iy) = noahowp%parameters%LAI
    noahowpgrid%SAI(ix,iy) = noahowp%parameters%SAI
    noahowpgrid%CH2OP(ix,iy) = noahowp%parameters%CH2OP
    noahowpgrid%NROOT(ix,iy) = noahowp%parameters%NROOT
    noahowpgrid%HVT(ix,iy) = noahowp%parameters%HVT
    noahowpgrid%HVB(ix,iy) = noahowp%parameters%HVB
    noahowpgrid%TMIN(ix,iy) = noahowp%parameters%TMIN
    noahowpgrid%SHDFAC(ix,iy) = noahowp%parameters%SHDFAC
    noahowpgrid%SHDMAX(ix,iy) = noahowp%parameters%SHDMAX
    noahowpgrid%Z0MVT(ix,iy) = noahowp%parameters%Z0MVT
    noahowpgrid%RC(ix,iy) = noahowp%parameters%RC
    noahowpgrid%XL(ix,iy) = noahowp%parameters%XL
    noahowpgrid%BP(ix,iy) = noahowp%parameters%BP
    noahowpgrid%FOLNMX(ix,iy) = noahowp%parameters%FOLNMX
    noahowpgrid%QE25(ix,iy) = noahowp%parameters%QE25
    noahowpgrid%VCMX25(ix,iy) = noahowp%parameters%VCMX25
    noahowpgrid%MP(ix,iy) = noahowp%parameters%MP
    noahowpgrid%RGL(ix,iy) = noahowp%parameters%RGL
    noahowpgrid%RSMIN(ix,iy) = noahowp%parameters%RSMIN
    noahowpgrid%HS(ix,iy) = noahowp%parameters%HS
    noahowpgrid%AKC(ix,iy) = noahowp%parameters%AKC
    noahowpgrid%AKO(ix,iy) = noahowp%parameters%AKO
    noahowpgrid%AVCMX(ix,iy) = noahowp%parameters%AVCMX
    noahowpgrid%RSMAX(ix,iy) = noahowp%parameters%RSMAX
    noahowpgrid%CWP(ix,iy) = noahowp%parameters%CWP
    noahowpgrid%C3PSN(ix,iy) = noahowp%parameters%C3PSN
    noahowpgrid%DLEAF(ix,iy) = noahowp%parameters%DLEAF
    noahowpgrid%KC25(ix,iy) = noahowp%parameters%KC25
    noahowpgrid%KO25(ix,iy) = noahowp%parameters%KO25
    noahowpgrid%ELAI(ix,iy) = noahowp%parameters%ELAI
    noahowpgrid%ESAI(ix,iy) = noahowp%parameters%ESAI
    noahowpgrid%VAI(ix,iy) = noahowp%parameters%VAI
    noahowpgrid%VEG(ix,iy) = noahowp%parameters%VEG
    noahowpgrid%FVEG(ix,iy) = noahowp%parameters%FVEG
    noahowpgrid%RHOL(ix,iy,:) = noahowp%parameters%RHOL(:)
    noahowpgrid%RHOS(ix,iy,:) = noahowp%parameters%RHOS(:)
    noahowpgrid%TAUL(ix,iy,:) = noahowp%parameters%TAUL(:)
    noahowpgrid%TAUS(ix,iy,:) = noahowp%parameters%TAUS(:)
    noahowpgrid%ISURBAN = noahowp%parameters%ISURBAN
    noahowpgrid%ISWATER = noahowp%parameters%ISWATER
    noahowpgrid%ISBARREN = noahowp%parameters%ISBARREN
    noahowpgrid%ISICE = noahowp%parameters%ISICE
    noahowpgrid%ISCROP = noahowp%parameters%ISCROP
    noahowpgrid%EBLFOREST = noahowp%parameters%EBLFOREST
    noahowpgrid%NATURAL = noahowp%parameters%NATURAL
    noahowpgrid%LOW_DENSITY_RESIDENTIAL = noahowp%parameters%LOW_DENSITY_RESIDENTIAL
    noahowpgrid%HIGH_DENSITY_RESIDENTIAL = noahowp%parameters%HIGH_DENSITY_RESIDENTIAL
    noahowpgrid%HIGH_INTENSITY_INDUSTRIAL = noahowp%parameters%HIGH_INTENSITY_INDUSTRIAL
    noahowpgrid%SB = noahowp%parameters%SB
    noahowpgrid%VKC = noahowp%parameters%VKC
    noahowpgrid%TFRZ = noahowp%parameters%TFRZ
    noahowpgrid%HSUB = noahowp%parameters%HSUB
    noahowpgrid%HVAP = noahowp%parameters%HVAP
    noahowpgrid%HFUS = noahowp%parameters%HFUS
    noahowpgrid%CWAT = noahowp%parameters%CWAT
    noahowpgrid%CICE = noahowp%parameters%CICE
    noahowpgrid%CPAIR = noahowp%parameters%CPAIR
    noahowpgrid%TKWAT = noahowp%parameters%TKWAT
    noahowpgrid%TKICE = noahowp%parameters%TKICE
    noahowpgrid%TKAIR = noahowp%parameters%TKAIR
    noahowpgrid%RAIR = noahowp%parameters%RAIR
    noahowpgrid%RW = noahowp%parameters%RW
    noahowpgrid%DENH2O = noahowp%parameters%DENH2O
    noahowpgrid%DENICE = noahowp%parameters%DENICE
    noahowpgrid%THKW = noahowp%parameters%THKW
    noahowpgrid%THKO = noahowp%parameters%THKO
    noahowpgrid%THKQTZ = noahowp%parameters%THKQTZ
    noahowpgrid%SSI = noahowp%parameters%SSI
    noahowpgrid%MFSNO(ix,iy) = noahowp%parameters%MFSNO
    noahowpgrid%Z0SNO = noahowp%parameters%Z0SNO
    noahowpgrid%SWEMX = noahowp%parameters%SWEMX
    noahowpgrid%TAU0 = noahowp%parameters%TAU0
    noahowpgrid%GRAIN_GROWTH = noahowp%parameters%GRAIN_GROWTH
    noahowpgrid%EXTRA_GROWTH = noahowp%parameters%EXTRA_GROWTH
    noahowpgrid%DIRT_SOOT = noahowp%parameters%DIRT_SOOT
    noahowpgrid%BATS_COSZ = noahowp%parameters%BATS_COSZ
    noahowpgrid%BATS_VIS_NEW = noahowp%parameters%BATS_VIS_NEW
    noahowpgrid%BATS_NIR_NEW = noahowp%parameters%BATS_NIR_NEW
    noahowpgrid%BATS_VIS_AGE = noahowp%parameters%BATS_VIS_AGE
    noahowpgrid%BATS_NIR_AGE = noahowp%parameters%BATS_NIR_AGE
    noahowpgrid%BATS_VIS_DIR = noahowp%parameters%BATS_VIS_DIR
    noahowpgrid%BATS_NIR_DIR = noahowp%parameters%BATS_NIR_DIR
    noahowpgrid%RSURF_SNOW = noahowp%parameters%RSURF_SNOW
    noahowpgrid%RSURF_EXP = noahowp%parameters%RSURF_EXP
    noahowpgrid%ALBSAT(ix,iy,:) = noahowp%parameters%ALBSAT(:)
    noahowpgrid%ALBDRY(ix,iy,:) = noahowp%parameters%ALBDRY(:)
    noahowpgrid%ALBICE(ix,iy,:) = noahowp%parameters%ALBICE(:)
    noahowpgrid%ALBLAK(ix,iy,:) = noahowp%parameters%ALBLAK(:)
    noahowpgrid%OMEGAS(ix,iy,:) = noahowp%parameters%OMEGAS(:)
    noahowpgrid%BETADS = noahowp%parameters%BETADS
    noahowpgrid%BETAIS = noahowp%parameters%BETAIS
    noahowpgrid%EG(ix,iy,:) = noahowp%parameters%EG(:)
    noahowpgrid%WSLMAX = noahowp%parameters%WSLMAX
    noahowpgrid%max_liq_mass_fraction = noahowp%parameters%max_liq_mass_fraction
    noahowpgrid%SNOW_RET_FAC = noahowp%parameters%SNOW_RET_FAC
    noahowpgrid%NBAND = noahowp%parameters%NBAND
    noahowpgrid%MPE = noahowp%parameters%MPE
    noahowpgrid%TOPT(ix,iy) = noahowp%parameters%TOPT
    noahowpgrid%O2 = noahowp%parameters%O2
    noahowpgrid%CO2 = noahowp%parameters%CO2
    noahowpgrid%PSIWLT = noahowp%parameters%PSIWLT
    noahowpgrid%GRAV = noahowp%parameters%GRAV
    !noahowpgrid%rain_snow_thresh = noahowp%parameters%rain_snow_thresh

    end associate

  end subroutine ParametersVarOutTransfer

end module ParametersVarTransferModule
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

end module ParametersVarInTransferModule
module ParametersVarInTransferModule

    use NoahowpmpIOVarType
    use NoahowpType

    implicit none
  
  contains
  
  subroutine ParametersVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahmp_type),   intent(inout) :: noahowpmp
    integer                            :: ii

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy, &
              isltyp => NoahowpmpIO%isltyp(NoahowpmpIO%ix,NoahowpmpIO%iy)
              vegtyp => NoahowpmpIO%vegtyp(NoahowpmpIO%ix,NoahowpmpIO%iy)
              soilcolor => NoahowpmpIO%soilcolor(NoahowpmpIO%ix,NoahowpmpIO%iy)
              )

    noahowpmp%bexp    = NoahowpmpIO%BEXP_TABLE(isltyp)
    noahowpmp%smcmax  = NoahowpmpIO%SMCMAX_TABLE(isltyp)
    noahowpmp%smcwlt  = NoahowpmpIO%SMCWLT_TABLE(isltyp)
    noahowpmp%smcref  = NoahowpmpIO%SMCREF_TABLE(isltyp)
    noahowpmp%dksat   = NoahowpmpIO%DKSAT_TABLE(isltyp)
    noahowpmp%dwsat   = NoahowpmpIO%DWSAT_TABLE(isltyp)
    noahowpmp%psisat  = NoahowpmpIO%PSISAT_TABLE(isltyp)
    noahowpmp%bvic    = NoahowpmpIO%BVIC_table(isltyp)
    noahowpmp%AXAJ    = NoahowpmpIO%AXAJ_table(isltyp)
    noahowpmp%BXAJ    = NoahowpmpIO%BXAJ_table(isltyp)
    noahowpmp%XXAJ    = NoahowpmpIO%XXAJ_table(isltyp)
    noahowpmp%BBVIC   = NoahowpmpIO%BBVIC_table(isltyp)
    noahowpmp%G       = NoahowpmpIO%GDVIC_table(isltyp)
    noahowpmp%QUARTZ  = NoahowpmpIO%QUARTZ_table(isltyp)

    do ii = 1,12
      noahowpmp%LAIM(ii) = NoahowpmpIO%LAIM_TABLE(vegtyp, ii)
      noahowpmp%SAIM(ii) = NoahowpmpIO%SAIM_TABLE(vegtyp, ii)
    end do

    noahowpmp%CH2OP   = NoahowpmpIO%CH2OP_TABLE(vegtyp)
    noahowpmp%NROOT   = NoahowpmpIO%NROOT_TABLE(vegtyp)
    noahowpmp%HVT     = NoahowpmpIO%HVT_TABLE(vegtyp)
    noahowpmp%HVB     = NoahowpmpIO%HVB_TABLE(vegtyp)
    noahowpmp%TMIN    = NoahowpmpIO%TMIN_TABLE(vegtyp)
    noahowpmp%SHDFAC  = NoahowpmpIO%SHDFAC_TABLE(vegtyp) ! this used to be in VEGPARM.TBL, but now somewhere else for hrldas. this is temporarily in MPTABLE.TBL.
    noahowpmp%SHDMAX  = NoahowpmpIO%SHDFAC_TABLE(vegtyp)
    noahowpmp%Z0MVT   = NoahowpmpIO%Z0MVT_TABLE(vegtyp)
    noahowpmp%RC      = NoahowpmpIO%RC_TABLE(vegtyp)
    noahowpmp%XL      = NoahowpmpIO%XL_TABLE(vegtyp)
    noahowpmp%BP      = NoahowpmpIO%BP_TABLE(vegtyp)
    noahowpmp%FOLNMX  = NoahowpmpIO%FOLNMX_TABLE(vegtyp)
    noahowpmp%QE25    = NoahowpmpIO%QE25_TABLE(vegtyp)
    noahowpmp%VCMX25  = NoahowpmpIO%VCMX25_TABLE(vegtyp)
    noahowpmp%MP      = NoahowpmpIO%MP_TABLE(vegtyp)
    noahowpmp%RGL     = NoahowpmpIO%RGL_TABLE(vegtyp)
    noahowpmp%RSMIN   = NoahowpmpIO%RS_TABLE(vegtyp)
    noahowpmp%HS      = NoahowpmpIO%HS_TABLE(vegtyp)
    noahowpmp%AKC     = NoahowpmpIO%AKC_TABLE(vegtyp)
    noahowpmp%AKO     = NoahowpmpIO%AKO_TABLE(vegtyp)
    noahowpmp%AVCMX   = NoahowpmpIO%AVCMX_TABLE(vegtyp)
    noahowpmp%RSMAX   = NoahowpmpIO%RSMAX_TABLE(vegtyp)
    noahowpmp%CWP     = NoahowpmpIO%CWPVT_TABLE(vegtyp)
    noahowpmp%C3PSN   = NoahowpmpIO%C3PSN_TABLE(vegtyp)
    noahowpmp%DLEAF   = NoahowpmpIO%DLEAF_TABLE(vegtyp)
    noahowpmp%KC25    = NoahowpmpIO%KC25_TABLE(vegtyp)
    noahowpmp%KO25    = NoahowpmpIO%KO25_TABLE(vegtyp)

    noahowpmp%RHOL(1) = NoahowpmpIO%RHOL_TABLE(vegtyp, 1)
    noahowpmp%RHOL(2) = NoahowpmpIO%RHOL_TABLE(vegtyp, 2)
    noahowpmp%RHOS(1) = NoahowpmpIO%RHOS_TABLE(vegtyp, 1)
    noahowpmp%RHOS(2) = NoahowpmpIO%RHOS_TABLE(vegtyp, 2)
    noahowpmp%TAUL(1) = NoahowpmpIO%TAUL_TABLE(vegtyp, 1)
    noahowpmp%TAUL(2) = NoahowpmpIO%TAUL_TABLE(vegtyp, 2)
    noahowpmp%TAUS(1) = NoahowpmpIO%TAUS_TABLE(vegtyp, 1)
    noahowpmp%TAUS(2) = NoahowpmpIO%TAUS_TABLE(vegtyp, 2)

    noahowpmp%refkdt       = NoahowpmpIO%REFKDT_TABLE
    noahowpmp%refdk        = NoahowpmpIO%REFDK_TABLE
    noahowpmp%kdt          = NoahowpmpIO%refkdt * NoahowpmpIO%dksat(ix,iy,1) / NoahowpmpIO%refdk
    noahowpmp%csoil        = NoahowpmpIO%CSOIL_TABLE
    noahowpmp%Z0           = NoahowpmpIO%Z0_TABLE     ! bare soil roughness length (m). in GENPARM.TBL.  NOTE: This is hard-coded in hrldas version of noah-mp
    noahowpmp%CZIL         = NoahowpmpIO%CZIL_TABLE
    noahowpmp%ZBOT         = NoahowpmpIO%ZBOT_TABLE
    noahowpmp%frzx         = 0.15 * (NoahowpmpIO%smcmax(ix,iy,1) / NoahowpmpIO%smcref(ix,iy,1)) * (0.412 / 0.468)
    noahowpmp%SSI          = NoahowpmpIO%SSI_TABLE
    noahowpmp%MFSNO        = NoahowpmpIO%MFSNO_TABLE(vegtyp)
    noahowpmp%Z0SNO        = NoahowpmpIO%Z0SNO_TABLE
    noahowpmp%SWEMX        = NoahowpmpIO%SWEMX_TABLE
    noahowpmp%TAU0         = NoahowpmpIO%TAU0_TABLE
    noahowpmp%GRAIN_GROWTH = NoahowpmpIO%GRAIN_GROWTH_TABLE
    noahowpmp%EXTRA_GROWTH = NoahowpmpIO%EXTRA_GROWTH_TABLE
    noahowpmp%DIRT_SOOT    = NoahowpmpIO%DIRT_SOOT_TABLE
    noahowpmp%BATS_COSZ    = NoahowpmpIO%BATS_COSZ_TABLE
    noahowpmp%BATS_VIS_NEW = NoahowpmpIO%BATS_VIS_NEW_TABLE
    noahowpmp%BATS_NIR_NEW = NoahowpmpIO%BATS_NIR_NEW_TABLE
    noahowpmp%BATS_VIS_AGE = NoahowpmpIO%BATS_VIS_AGE_TABLE
    noahowpmp%BATS_NIR_AGE = NoahowpmpIO%BATS_NIR_AGE_TABLE
    noahowpmp%BATS_VIS_DIR = NoahowpmpIO%BATS_VIS_DIR_TABLE
    noahowpmp%BATS_NIR_DIR = NoahowpmpIO%BATS_NIR_DIR_TABLE
    noahowpmp%RSURF_SNOW   = NoahowpmpIO%RSURF_SNOW_TABLE
    noahowpmp%RSURF_EXP    = NoahowpmpIO%RSURF_EXP_TABLE

    noahowpmp%ALBSAT(1)    = NoahowpmpIO%ALBSAT_TABLE(soilcolor, 1)
    noahowpmp%ALBSAT(2)    = NoahowpmpIO%ALBSAT_TABLE(soilcolor, 2)
    noahowpmp%ALBDRY(1)    = NoahowpmpIO%ALBDRY_TABLE(soilcolor, 1)
    noahowpmp%ALBDRY(2)    = NoahowpmpIO%ALBDRY_TABLE(soilcolor, 2)
    noahowpmp%ALBICE       = NoahowpmpIO%ALBICE_TABLE
    noahowpmp%ALBLAK       = NoahowpmpIO%ALBLAK_TABLE
    noahowpmp%OMEGAS       = NoahowpmpIO%OMEGAS_TABLE
    noahowpmp%BETADS       = NoahowpmpIO%BETADS_TABLE
    noahowpmp%BETAIS       = NoahowpmpIO%BETAIS_TABLE
    noahowpmp%EG           = NoahowpmpIO%EG_TABLE
    noahowpmp%slope        = NoahowpmpIO%SLOPE_TABLE(1)

    noahowpmp%ISURBAN                   = NoahowpmpIO%ISURBAN_TABLE
    noahowpmp%ISWATER                   = NoahowpmpIO%ISWATER_TABLE
    noahowpmp%ISBARREN                  = NoahowpmpIO%ISBARREN_TABLE
    noahowpmp%ISICE                     = NoahowpmpIO%ISICE_TABLE
    noahowpmp%ISCROP                    = NoahowpmpIO%ISCROP_TABLE
    noahowpmp%EBLFOREST                 = NoahowpmpIO%EBLFOREST_TABLE
    noahowpmp%NATURAL                   = NoahowpmpIO%NATURAL_TABLE
    noahowpmp%LOW_DENSITY_RESIDENTIAL   = NoahowpmpIO%LCZ_1_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_1
    noahowpmp%HIGH_DENSITY_RESIDENTIAL  = NoahowpmpIO%LCZ_2_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_2
    noahowpmp%HIGH_INTENSITY_INDUSTRIAL = NoahowpmpIO%LCZ_3_TABLE  ! TO-DO: rename LOW_DENSITY_RESIDENTIAL -> LCZ_3

    noahowpmp%urban_flag = .false.
    noahowpmp%timean     = 10.5
    noahowpmp%fsatmx     = 0.38
    noahowpmp%GRAV       = 9.80616
    noahowpmp%SB         = 5.67E-08
    noahowpmp%VKC        = 0.40
    noahowpmp%TFRZ       = 273.16
    noahowpmp%HSUB       = 2.8440E06
    noahowpmp%HVAP       = 2.5104E06
    noahowpmp%HFUS       = 0.3336E06
    noahowpmp%CWAT       = 4.188E06
    noahowpmp%CICE       = 2.094E06
    noahowpmp%CPAIR      = 1004.64
    noahowpmp%TKWAT      = 0.6
    noahowpmp%TKICE      = 2.2
    noahowpmp%TKAIR      = 0.023
    noahowpmp%RAIR       = 287.04
    noahowpmp%RW         = 461.269
    noahowpmp%DENH2O     = 1000.0
    noahowpmp%DENICE     = 917.0
    noahowpmp%THKW       = 0.57
    noahowpmp%THKO       = 2.0
    noahowpmp%THKQTZ     = 7.7
    noahowpmp%WSLMAX     = 5000.0
    noahowpmp%max_liq_mass_fraction = 0.4
    noahowpmp%SNOW_RET_FAC = 5.e-5
    noahowpmp%NBAND        = 2       ! do not change
    noahowpmp%MPE          = 1.E-06  ! do not change ! need to make this a parameter
    noahowpmp%TOPT         = 1.E-06  ! Optimum transpiration air temperature [K]

    noahowpmp%CO2       =  395.e-06   ! co2 partial pressure, from CO2_TABLE var (set in MPTABLE.TBL)
    noahowpmp%O2        =  0.209      ! o2 partial pressure, from O2_TABLE var (set in MPTABLE.TBL)
    noahowpmp%PSIWLT    = -150.0      ! originally a fixed parameter set in ENERGY()
    noahowpmp%TBOT      = 263.0       ! (K) can be updated depending on option OPT_TBOT

    ! Assign rain-snow threshold based on option
    IF(NoahowpmpIO%precip_phase_option(ix,iy) == 2) THEN
      noahowpmp%rain_snow_thresh = NoahowpmpIO%TFRZ + 2.2
    ELSE IF(NoahowpmpIO%precip_phase_option(ix,iy) == 3) THEN
      noahowpmp%rain_snow_thresh = NoahowpmpIO%TFRZ
    ELSE IF(NoahowpmpIO%precip_phase_option(ix,iy) == 5 .or. NoahowpmpIO%precip_phase_option(ix,iy) == 6) THEN
      noahowpmp%rain_snow_thresh = NoahowpmpIO%TFRZ + NoahowpmpIO%rain_snow_thresh
    ELSE 
      noahowpmp%rain_snow_thresh = NoahowpmpIO%TFRZ ! set to TFRZ as a backup
    ENDIF
    
    ! Assign initial soil moisture based on variable or uniform initial conditions
    noahowpmp%zwt_init = NoahowpmpIO%zwt

    end associate

  end subroutine

end module ParametersVarInTransferModule
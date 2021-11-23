module AtmProcessing

  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains

  !== begin combine ==================================================================================

  SUBROUTINE ATM (options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (parameters_type), intent(in) :: parameters
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    real                    :: PAIR              !atm bottom level pressure (pa)
    !real                    :: PRCP              !total precipitation (mm/s)
    real                    :: PRCP_FROZEN       !total frozen precipitation [mm/s] ! MB/AN : v3.7
    real                    :: QPRECC            !total convective precipitation [mm/s] (used to compute FP-maybe delete)
    real                    :: QPRECL            !total non-convective precipitation [mm/s] (used to compute FP-maybe delete)
    real, parameter         :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
    real, parameter         :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7
    real                    :: QV_CURR           ! water vapor mixing ratio (kg/kg)
    ! --------------------------------------------------------------------------------------------------

    ! Compute derived variables from forcing data
    PAIR           = forcing%SFCPRS                   ! atm bottom level pressure (pa)
    forcing%THAIR  = forcing%SFCTMP * (forcing%SFCPRS / PAIR)**(parameters%RAIR / parameters%CPAIR) 
    forcing%QAIR   = forcing%Q2                       ! In WRF, driver converts to specific humidity
    forcing%EAIR   = forcing%QAIR * forcing%SFCPRS / (0.622 + (0.378 * forcing%QAIR))
    forcing%RHOAIR = (forcing%SFCPRS - (0.378 * forcing%EAIR)) / (parameters%RAIR * forcing%SFCTMP)
    
    ! O2 and C02 partial pressures
    forcing%O2PP  = parameters%O2 * PAIR   ! atmospheric co2 concentration partial pressure (Pa)
    forcing%CO2PP = parameters%CO2 * PAIR  ! atmospheric o2 concentration partial pressure (Pa)

    ! Starting canopy temp and vapor pressure
    energy%TAH = forcing%SFCTMP                         ! assign canopy temp with forcing air temp (K)
    QV_CURR    = forcing%Q2 / (1 - forcing%Q2)          ! mixing ratio, assuming input forcing Q2 is specific hum.
    energy%EAH = forcing%SFCPRS*QV_CURR/(0.622+QV_CURR) ! Initial guess only. (Pa)

    ! Set incoming shortwave to 0 if cosine of solar zenith angle < 0
    IF(energy%COSZ <= 0.0) THEN
      forcing%SWDOWN = 0.0
    ELSE
      forcing%SWDOWN = forcing%SOLDN
    END IF

    ! Split incoming solar in direct and diffuse visible and near infrared
    forcing%SOLAD(1) = forcing%SWDOWN*0.7*0.5  ! direct  vis
    forcing%SOLAD(2) = forcing%SWDOWN*0.7*0.5  ! direct  nir
    forcing%SOLAI(1) = forcing%SWDOWN*0.3*0.5  ! diffuse vis
    forcing%SOLAI(2) = forcing%SWDOWN*0.3*0.5  ! diffuse nir

    ! sum different precip types to get total
    !PRCP = forcing%PRCPCONV + forcing%PRCPNONC + forcing%PRCPSHCV
    forcing%PRCPNONC = forcing%PRCP    ! this assumption on type of prcp is from noah-mp driver

    !---------------- TO DO 2021-03-23 ---------------------
    !---------------- I think we need to get rid of FP because it is
    !---------------- designed for LSMs with massive grid spacing
    !---------------- not hydrologic models at 1 km
    ! fractional area that receives precipitation (see, Niu et al. 2005)
    ! A simple TOPMODEL‐based runoff parameterization (SIMTOP) for use in global climate models

    ! Split precip into convective and large-scale fractions (used for computing FP below)
    IF(options%OPT_SNF == 4) THEN
      QPRECC = forcing%PRCPCONV + forcing%PRCPSHCV
      QPRECL = forcing%PRCPNONC
    ELSE
      QPRECC = 0.10 * forcing%PRCP          ! should be from the atmospheric model
      QPRECL = 0.90 * forcing%PRCP          ! should be from the atmospheric model
    END IF
        
    ! Fraction of grid cell receiving precipitation
    water%FP = 0.0
    IF(QPRECC + QPRECL > 0.) & 
       water%FP = (QPRECC + QPRECL) / (10. * QPRECC + QPRECL)
    !---------------- END TO DO

    ! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7
    
    ! OPT_SNF == 1: Jordan (1991)
    IF(options%OPT_SNF == 1) THEN
      IF(forcing%SFCTMP > parameters%TFRZ + 2.5) THEN
        forcing%FPICE = 0.
        ELSE 
          IF(forcing%SFCTMP <= parameters%TFRZ + 0.5) THEN
            forcing%FPICE = 1.0
          ELSE IF(forcing%SFCTMP <= parameters%TFRZ + 2.) THEN
            forcing%FPICE = 1. - (-54.632 + 0.2 * forcing%SFCTMP)
          ELSE
            forcing%FPICE = 0.6
        ENDIF
      ENDIF
    ENDIF

    ! OPT_SNF == 2: rain-snow air temperature threshold of 2.2°C
    IF(options%OPT_SNF == 2) THEN
      IF(forcing%SFCTMP >= parameters%TFRZ + 2.2) THEN
        forcing%FPICE = 0.
      ELSE
        forcing%FPICE = 1.0
      ENDIF
    ENDIF
        
    ! OPT_SNF == 3: rain-snow air temperature threshold of 0°C
    IF(options%OPT_SNF == 3) THEN
      IF(forcing%SFCTMP >= parameters%TFRZ) THEN
        forcing%FPICE = 0.
      ELSE
        forcing%FPICE = 1.0
      ENDIF
    ENDIF

    ! OPT_SNF == 4: precipitation phase from weather model
    IF(options%OPT_SNF == 4) THEN
      prcp_frozen = forcing%PRCPSNOW + forcing%PRCPGRPL + forcing%PRCPHAIL
      IF(forcing%PRCPNONC > 0. .and. prcp_frozen > 0.) THEN
        forcing%FPICE = MIN(1.0, prcp_frozen / forcing%PRCPNONC)
        forcing%FPICE = MAX(0.0, forcing%FPICE)
      ELSE
        forcing%FPICE = 0.0
      ENDIF
    ENDIF

    ! Calculate rain and snow as function of FPICE
    water%RAIN   = forcing%PRCP * (1. - forcing%FPICE)
    water%SNOW   = forcing%PRCP * forcing%FPICE

    ! Compute the density of new snow

    ! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
    water%BDFALL = MIN(120., 67.92 + 51.25 * EXP((forcing%SFCTMP - parameters%TFRZ)/2.59))       !MB/AN: change to MIN  

    ! Modify bulk density if other frozen hydrometeors from weather model
    IF(options%OPT_SNF == 4) THEN
      water%BDFALL = water%BDFALL * (forcing%PRCPSNOW / PRCP_FROZEN) + &
                     RHO_GRPL * (forcing%PRCPGRPL / PRCP_FROZEN) + &
                     RHO_HAIL * (forcing%PRCPHAIL / PRCP_FROZEN)
    ENDIF

    ! Compute wind speed at reference height: ur >= 1
    ! Moved from main level of ENERGY, KSJ 2021-04-06

    forcing%UR = MAX( SQRT(forcing%UU**2. + forcing%VV**2.), 1. )

  END SUBROUTINE ATM
  
end module AtmProcessing
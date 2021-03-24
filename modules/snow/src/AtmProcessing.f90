module AtmProcessing

    use LevelsType
    use DomainType
    use ParametersType
    use WaterType
    use EnergyType
    use ForcingType

    implicit none

    contains

  !== begin combine ==================================================================================

    SUBROUTINE ATM (domain, levels, parameters, energy, water, forcing)

        type (    levels_type), intent(in) :: levels
        type (parameters_type), intent(in) :: parameters
        type (    domain_type)             :: domain
        type (    energy_type)             :: energy
        type (     water_type)             :: water
        type (   forcing_type)             :: forcing

  ! ------------------------ local variables ---------------------------

        real                                        :: PAIR              !atm bottom level pressure (pa)
        real                                        :: PRCP_FROZEN       !total frozen precipitation [mm/s] ! MB/AN : v3.7
        real                                        :: QPRECC            !total convective precipitation [mm/s] (used to compute FP-maybe delete)
        real                                        :: QPRECL            !total non-convective precipitation [mm/s] (used to compute FP-maybe delete)
        real, parameter                             :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
        real, parameter                             :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7
! --------------------------------------------------------------------------------------------------

        ! jref: seems like PAIR should be P1000mb??
        PAIR   = forcing%SFCPRS                   ! atm bottom level pressure (pa)
        forcing%THAIR  = forcing%SFCTMP * (forcing%SFCPRS / PAIR)**(parameters%RAIR / parameters%CPAIR) 
        forcing%QAIR   = forcing%Q2                       ! In WRF, driver converts to specific humidity
        forcing%EAIR   = forcing%QAIR * forcing%SFCPRS / (0.622 + (0.378 * forcing%QAIR))
        forcing%RHOAIR = (forcing%SFCPRS - (0.378 * forcing%EAIR)) / (parameters%RAIR * forcing%SFCTMP)
	   
!---------------- TO DO 2021-03-22 ---------------------
!---------------- COMPUTATION OF COSZ FOR ENERGY MODULE -------------

       !IF(COSZ <= 0.) THEN 
       !   SWDOWN = 0.
       !ELSE
       !   SWDOWN = SOLDN
       !END IF 

       !SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
       !SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
       !SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
       !SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir

!---------------- END TO DO

        ! sum different precip types to get total
        water%PRCP = forcing%PRCPCONV + forcing%PRCPNONC + forcing%PRCPSHCV
        

        !---------------- TO DO 2021-03-23 ---------------------
        !---------------- I think we need to get rid of FP because it is
        !---------------- designed for LSMs with massive grid spacing
        !---------------- not hydrologic models at 1 km
        ! fractional area that receives precipitation (see, Niu et al. 2005)
        ! A simple TOPMODEL‐based runoff parameterization (SIMTOP) for use in global climate models
        
        IF(options%OPT_SNF == 4) THEN
            forcing%QPRECC = forcing%PRCPCONV + forcing%PRCPSHCV
            forcing%QPRECL = forcing%PRCPNONC
        ELSE
            forcing%QPRECC = 0.10 * water%PRCP          ! should be from the atmospheric model
            forcing%QPRECL = 0.90 * water%PRCP          ! should be from the atmospheric model
        END IF
        
        water%FP = 0.0
        IF(forcing%QPRECC + forcing%QPRECL > 0.) & 
           water%FP = (forcing%QPRECC + forcing%QPRECL) / (10. * forcing%QPRECC + forcing%QPRECL)
        !---------------- END TO DO
        
        ! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7
        ! OPT_SNF == 1: Jordan (1991)

        IF(options%OPT_SNF == 1) THEN
            IF(forcing%SFCTMP > parameters%TFRZ + 2.5) THEN
                forcing%FPICE = 0.
            ELSE IF(forcing%SFCTMP <= parameters%TFRZ + 0.5) THEN
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
            forcing%PRCP_FROZEN = forcing%PRCPSNOW + forcing%PRCPGRPL + forcing%PRCPHAIL
            IF(forcing%PRCPNONC > 0. .and. forcing%PRCP_FROZEN > 0.) THEN
                forcing%FPICE = MIN(1.0, forcing%PRCP_FROZEN / forcing%PRCPNONC)
                forcing%FPICE = MAX(0.0, forcing%FPICE)
            ELSE
                forcing%FPICE = 0.0
            ENDIF
        ENDIF

        ! Calculate rain and snow as function of FPICE
        water%RAIN   = water%PRCP * (1. - forcing%FPICE)
        water%SNOW   = water%PRCP * forcing%FPICE

        ! Compute the density of new snow
        
        ! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
        water%BDFALL = MIN(120., 67.92 + 51.25 * EXP((forcing%SFCTMP - parameters%TFRZ)/2.59))       !MB/AN: change to MIN  
        
        ! Modify bulk density if other frozen hydrometeors from weather model
        IF(options%OPT_SNF == 4) THEN
            water%BDFALL = water%BDFALL * (forcing%PRCPSNOW / forcing%PRCP_FROZEN) + &
                           RHO_GRPL * (forcing%PRCPGRPL / forcing%PRCP_FROZEN) + &
                           RHO_HAIL * (forcing%PRCPHAIL / forcing%PRCP_FROZEN)
        ENDIF


  END SUBROUTINE ATM
  
end module AtmProcessing
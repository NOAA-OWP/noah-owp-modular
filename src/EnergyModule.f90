module EnergyModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use ThermalPropertiesModule
  use PrecipHeatModule
  use ShortwaveRadiationModule
  use EtFluxModule
  use SnowSoilTempModule
  implicit none

contains

  !== begin energy subroutine ================================================================================
  SUBROUTINE EnergyMain (domain, levels, options, parameters, forcing, energy, water)
    !---------------------------------------------------------------------
    ! Main module for all energy components
    !---------------------------------------------------------------------
    IMPLICIT NONE

    ! ------------------------ in, inout, out variables ----------------------------------------------------
    type (levels_type),     intent(in)   :: levels
    type (domain_type)                   :: domain
    type (parameters_type)               :: parameters
    type (options_type),    intent(in)   :: options
    type (water_type)                    :: water
    type (forcing_type),    intent(in)   :: forcing 
    type (energy_type)                   :: energy

    ! ------------------------ local variables ---------------------------
    INTEGER                              :: IZ     ! do-loop index
    REAL                                 :: FMELT  ! melting factor for snow cover frac
    REAL                                 :: ZPDG   ! zero plane displacement, ground (m)
      
    REAL                                 :: GX       ! temporary variable -- prev. undeclared in ENERGY())
    REAL                                 :: PSI      ! surface layer soil matrix potential (m)
    
    REAL                                 :: BEVAP    ! soil water evaporation factor (0-1)
    REAL                                 :: L_RSURF  ! Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
    REAL                                 :: D_RSURF  ! Reduced vapor diffusivity in soil for computing RSURF (SZ09)
    
    REAL                                 :: FIRE   !emitted IR (w/m2)
    !---------------------------------------------------------------------

    ! Initialize the the fluxes from the vegetated fraction
    energy%TAUXV     = 0.
    energy%TAUYV     = 0.
    energy%IRC       = 0.
    energy%SHC       = 0.
    energy%IRG       = 0.
    energy%SHG       = 0.
    energy%EVG       = 0.
    energy%EVC       = 0.
    energy%TR        = 0.
    energy%GHV       = 0.
    energy%PSNSUN    = 0.
    energy%PSNSHA    = 0.
    energy%T2MV      = 0.
    energy%Q2V       = 0.
    energy%CHV       = 0.
    energy%CHLEAF    = 0.
    energy%CHUC      = 0.
    energy%CHV2      = 0.
    energy%RB        = 0.

    ! Determine whether grid cell is vegetated or not
    parameters%VAI = parameters%ELAI + parameters%ESAI
    IF(parameters%VAI > 0.0) THEN
      parameters%VEG = .TRUE.
    ELSE
      parameters%VEG = .FALSE.
    ENDIF

    ! Compute fraction of grid cell with snow cover [Niu and Yang, 2007, JGR]
    ! TO DO: MFSNO (m in Niu and Yang) is set to 2.5 for all vegtypes
    ! Reference paper indicates MFSNO varies in space (values of 1.0, 1.6, 1.8)
    ! KSJ 2021-04-06

    water%FSNO = 0.0
    IF(water%SNOWH > 0.0)  THEN
      water%BDSNO  = water%SNEQV / water%SNOWH
      FMELT        = (water%BDSNO / 100.)**parameters%MFSNO
      water%FSNO   = TANH( water%SNOWH /(2.5 * parameters%Z0 * FMELT)) ! eq. 4 from Niu and Yang (2007)
    ENDIF

    ! Compute ground roughness length
    IF(domain%IST == 2) THEN
      IF(energy%TG <= parameters%TFRZ) THEN
        energy%Z0MG = (0.01 * (1.0 - water%FSNO)) + (water%FSNO * parameters%Z0SNO)
      ELSE
        energy%Z0MG = 0.01  
      END IF
    ELSE
      energy%Z0MG = (parameters%Z0 * (1.0 - water%FSNO)) + (water%FSNO * parameters%Z0SNO)
    END IF

    ! Compute roughness length and displacement height
    ZPDG  = water%SNOWH
    IF(parameters%VEG) THEN
      energy%Z0M  = parameters%Z0MVT
      energy%ZPD  = 0.65 * parameters%HVT
      IF(water%SNOWH > energy%ZPD) energy%ZPD = water%SNOWH
    ELSE
      energy%Z0M  = energy%Z0MG
      energy%ZPD  = ZPDG
    END IF

    ! special case for urban
    IF (parameters%urban_flag) THEN
      energy%Z0MG = parameters%Z0MVT
      ZPDG = 0.65 * parameters%HVT
      energy%Z0M  = energy%Z0MG
      energy%ZPD  = ZPDG
    END IF

    energy%ZLVL = MAX(energy%ZPD, parameters%HVT) + domain%ZREF
    IF(ZPDG >= energy%ZLVL) energy%ZLVL = ZPDG + domain%ZREF

    ! Compute snow and soil thermodynamic properties
    call THERMOPROP(domain, levels, options, parameters, forcing, energy, water)

    ! Compute the heat advected by precipitation
    call PRECIP_HEAT(parameters, forcing, energy, water)

    ! Compute net solar radiation
    ! Subroutine was formerly called RADIATION
    ! Changed name because it only computes shortwave
    ! KSJ 2021-04-20
    call ShortwaveRadiationMain (domain, levels, options, parameters, forcing, energy, water)

    ! vegetation and ground emissivity
    energy%EMV = 1. - EXP(-(parameters%ELAI + parameters%ESAI)/1.0)
    IF (energy%ICE == 1) THEN
      energy%EMG = 0.98*(1.-water%FSNO) + 1.0*water%FSNO
    ELSE
      energy%EMG = parameters%EG(domain%IST)*(1.-water%FSNO) + 1.0*water%FSNO
    END IF

    ! calculate soil moisture stress factor controlling stomatal resistance
    water%BTRAN = 0.

    IF(domain%IST ==1 ) THEN
      DO IZ = 1, parameters%NROOT
        IF(options%OPT_BTR == 1) then                  ! Noah
          GX = (water%SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
        END IF
        IF(options%OPT_BTR == 2) then                  ! CLM
          IF(options%OPT_SUB == 1) then                ! Noah-MP style subsurface
            PSI = MAX(parameters%PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,water%SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
          END IF
          IF(options%OPT_SUB == 2) then                ! one-way coupled subsurface
            PSI = water%ZWT - (domain%zsoil(IZ) + (domain%dzsnso(IZ) / 2)) ! set PSI to be midpoint of layer above water table
          END IF
          GX = (1.-PSI/parameters%PSIWLT)/(1.+parameters%PSISAT(IZ)/parameters%PSIWLT)
        END IF
        IF(options%OPT_BTR == 3) then                  ! SSiB
          IF(options%OPT_SUB == 1) then                ! Noah-MP style subsurface
            PSI = MAX(parameters%PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,water%SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
          END IF
          IF(options%OPT_SUB == 2) then                ! one-way coupled subsurface
            PSI = water%ZWT - (domain%zsoil(IZ) + (domain%dzsnso(IZ) / 2)) ! set PSI to be midpoint of layer above water table
          END IF
          GX = 1.-EXP(-5.8*(LOG(parameters%PSIWLT/PSI)))
        END IF
        GX = MIN(1.,MAX(0.,GX))
        water%BTRANI(IZ) = MAX(parameters%MPE, domain%DZSNSO(IZ) / (-domain%ZSOIL(parameters%NROOT)) * GX)
        water%BTRAN      = water%BTRAN + water%BTRANI(IZ)
      END DO
      water%BTRAN = MAX(parameters%MPE, water%BTRAN)
      water%BTRANI(1:parameters%NROOT) = water%BTRANI(1:parameters%NROOT)/water%BTRAN
    END IF
    
    ! calculate soil surface resistance for ground evap.
    BEVAP = MAX(0.0, water%SH2O(1)/parameters%SMCMAX(1) )
    IF(domain%IST == 2) THEN
      energy%RSURF = 1.0         ! avoid being divided by 0
      energy%RHSUR = 1.0
    ELSE
      IF(options%OPT_RSF == 1 .OR. options%OPT_RSF == 4) THEN
        ! RSURF based on Sakaguchi and Zeng, 2009
        ! taking the "residual water content" to be the wilting point,
        ! and correcting the exponent on the D term (typo in SZ09 ?)
        L_RSURF = (-domain%ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,water%SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 )
        D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** &
                    (2.0+3.0/parameters%BEXP(1))
        energy%RSURF = L_RSURF / D_RSURF
      ELSEIF(options%OPT_RSF == 2) THEN
        energy%RSURF = water%FSNO * 1. + (1.-water%FSNO) * EXP(8.25-4.225 * BEVAP)       ! Sellers (1992) ! Older RSURF computations
      ELSEIF(options%OPT_RSF == 3) THEN
        energy%RSURF = water%FSNO * 1. + (1.-water%FSNO) * EXP(8.25-6.0   * BEVAP)       ! adjusted to decrease RSURF for wet soil
    ENDIF
    IF(options%OPT_RSF == 4) THEN                                     ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
      energy%RSURF = 1. / (water%FSNO * (1./parameters%RSURF_SNOW) + (1.-water%FSNO) * (1./max(energy%RSURF, 0.001)))
    ENDIF
    IF(water%SH2O(1) < 0.01 .and. water%SNOWH == 0.) energy%RSURF = 1.E6
      PSI   = -parameters%PSISAT(1) * (MAX(0.01, water%SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))
      energy%RHSUR = water%FSNO + (1.-water%FSNO) * EXP(PSI * parameters%GRAV/(parameters%RW * energy%TG))
    END IF

    ! urban - jref
    IF (parameters%urban_flag .and. water%SNOWH == 0. ) THEN
      energy%RSURF = 1.E6
    ENDIF

    ! set psychrometric constant
    IF (energy%TV .GT. parameters%TFRZ) THEN           ! Barlage: add distinction between ground and
      energy%LATHEAV = parameters%HVAP          !          vegetation in v3.6
      energy%frozen_canopy = .false.
    ELSE
      energy%LATHEAV = parameters%HSUB
      energy%frozen_canopy = .true.
    END IF
    energy%GAMMAV = parameters%CPAIR*forcing%SFCPRS/(0.622*energy%LATHEAV)

    IF (energy%TG .GT. parameters%TFRZ) THEN
     energy%LATHEAG = parameters%HVAP
     energy%frozen_ground = .false.
    ELSE
      energy%LATHEAG = parameters%HSUB
      energy%frozen_ground = .true.
    END IF
    energy%GAMMAG = parameters%CPAIR*forcing%SFCPRS/(0.622*energy%LATHEAG)

    ! next block commented out in orig code
    !IF (SFCTMP .GT. parameters%TFRZ) THEN
    !  energy%LATHEA = parameters%HVAP
    !ELSE
    !  energy%LATHEA = parameters%HSUB
    !END IF
    !energy%GAMMA = CPAIR*SFCPRS/(0.622*energy%LATHEA)

    ! Calculate surface temperatures of the ground and canopy and energy fluxes
    IF (parameters%VEG .AND. parameters%FVEG > 0) THEN
      energy%TGV = energy%TG
      energy%CMV = energy%CM
      energy%CHV = energy%CH

      ! Calculate canopy energy fluxes
      CALL VegeFluxMain (domain, levels, options, parameters, forcing, energy, water)

    END IF

    energy%TGB = energy%TG
    energy%CMB = energy%CM
    energy%CHB = energy%CH

    CALL BareFluxMain (domain, levels, options, parameters, forcing, energy, water)

    !energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG
    !energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
    !energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (parameters%VEG .AND. parameters%FVEG > 0) THEN
      energy%TAUX  = parameters%FVEG * energy%TAUXV     + (1.0 - parameters%FVEG) * energy%TAUXB
      energy%TAUY  = parameters%FVEG * energy%TAUYV     + (1.0 - parameters%FVEG) * energy%TAUYB
      energy%FIRA  = parameters%FVEG * energy%IRG       + (1.0 - parameters%FVEG) * energy%IRB       + energy%IRC
      energy%FSH   = parameters%FVEG * energy%SHG       + (1.0 - parameters%FVEG) * energy%SHB       + energy%SHC
      energy%FGEV  = parameters%FVEG * energy%EVG       + (1.0 - parameters%FVEG) * energy%EVB
      energy%SSOIL = parameters%FVEG * energy%GHV       + (1.0 - parameters%FVEG) * energy%GHB
      energy%GH    = parameters%FVEG * energy%GHV       + (1.0 - parameters%FVEG) * energy%GHB ! FVEG = 1. Hence, this is a weighted average
      energy%FCEV  = energy%EVC
      energy%FCTR  = energy%TR
      energy%PAH   = parameters%FVEG * energy%PAHG      + (1.0 - parameters%FVEG) * energy%PAHB   + energy%PAHV
      energy%TG    = parameters%FVEG * energy%TGV       + (1.0 - parameters%FVEG) * energy%TGB
      energy%T2M   = parameters%FVEG * energy%T2MV      + (1.0 - parameters%FVEG) * energy%T2MB
      energy%TS    = parameters%FVEG * energy%TV        + (1.0 - parameters%FVEG) * energy%TGB
      energy%CM    = parameters%FVEG * energy%CMV       + (1.0 - parameters%FVEG) * energy%CMB      ! better way to average?
      energy%CH    = parameters%FVEG * energy%CHV       + (1.0 - parameters%FVEG) * energy%CHB
      energy%Q1    = parameters%FVEG *(energy%EAH*0.622/(forcing%SFCPRS - 0.378*energy%EAH)) + (1.0 - parameters%FVEG)*energy%QSFC
      energy%Q2E   = parameters%FVEG * energy%Q2V       + (1.0 - parameters%FVEG) * energy%Q2B
      energy%Z0WRF = energy%Z0M
    ELSE
      energy%TAUX  = energy%TAUXB
      energy%TAUY  = energy%TAUYB
      energy%FIRA  = energy%IRB
      energy%FSH   = energy%SHB
      energy%FGEV  = energy%EVB
      energy%SSOIL = energy%GHB
      energy%TG    = energy%TGB      ! could use more associated variables to unclutter the code
      energy%T2M   = energy%T2MB
      energy%FCEV  = 0.
      energy%FCTR  = 0.
      energy%PAH   = energy%PAHB
      energy%TS    = energy%TG
      energy%CM    = energy%CMB
      energy%CH    = energy%CHB
      energy%Q1    = energy%QSFC
      energy%Q2E   = energy%Q2B
      energy%RSSUN = 0.0
      energy%RSSHA = 0.0
      energy%TGV   = energy%TGB
      energy%CHV   = energy%CHB
      energy%Z0WRF = energy%Z0MG
    END IF

    FIRE = forcing%LWDN + energy%FIRA
    IF(FIRE <=0.) THEN
      WRITE(*,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
      WRITE(*,*) 'input of SHDFAC with LAI'
      WRITE(*,*) domain%ILOC, domain%JLOC, 'SHDFAC=',parameters%FVEG,'parameters%VAI=',parameters%VAI,'TV=',energy%TV,'TG=',energy%TG
      WRITE(*,*) 'LWDN=',forcing%LWDN,'energy%FIRA=',energy%FIRA,'water%SNOWH=',water%SNOWH
      WRITE(*,*) 'Exiting ...'
      STOP
    END IF

    ! Compute a net emissivity
    energy%EMISSI = parameters%FVEG * (energy%EMG*(1-energy%EMV) + energy%EMV + energy%EMV*(1-energy%EMV)*(1-energy%EMG)) +&
                    (1-parameters%FVEG) * energy%EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    energy%TRAD = ( ( FIRE - (1 - energy%EMISSI) * forcing%LWDN ) / (energy%EMISSI * parameters%SB) ) ** 0.25

    energy%APAR = energy%PARSUN * energy%LAISUN + energy%PARSHA * energy%LAISHA
    energy%PSN  = energy%PSNSUN * energy%LAISUN + energy%PSNSHA * energy%LAISHA

    ! calculate 3L snow & 4L soil temperatures
    CALL TSNOSOI (parameters, levels, domain, options, forcing,                   & !in
                  energy%ICE, water%ISNOW, energy%SSOIL, energy%DF, energy%HCPCT, & !in
                  energy%SAG, water%SNOWH, energy%TG,                             & !in
                  energy%STC     )                                                  !inout

    ! AW:  need to decide what to do with STC if no subsurface will be simulated
    !      ie, should soil layers be 0C if there is snow and TGB if not?

    ! adjusting snow surface temperature
    IF(options%OPT_STC == 2) THEN
      IF (water%SNOWH > 0.05 .AND. energy%TG > parameters%TFRZ) THEN
        energy%TGV = parameters%TFRZ
        energy%TGB = parameters%TFRZ
        IF (parameters%VEG .AND. parameters%FVEG > 0) THEN
          energy%TG = parameters%FVEG * energy%TGV  + (1.0 - parameters%FVEG) * energy%TGB
          energy%TS = parameters%FVEG * energy%TV   + (1.0 - parameters%FVEG) * energy%TGB
        ELSE
          energy%TG = energy%TGB
          energy%TS = energy%TGB
        END IF
      END IF
    END IF

    ! Energy released or consumed by snow & frozen soil
    CALL PHASECHANGE (parameters, domain, energy, water, options, levels%NSNOW, levels%NSOIL)
    
    ! derived diagnostic variables
    energy%LH = energy%FCEV + energy%FGEV + energy%FCTR
    
    ! Compute TGS, or ground surface temperature for passing to a subsurface module
    ! TGS is equal to TG when SNOWH <= 0.05 and equal to the temperature of the bottom snow
    ! element when SNOWH > 0.05
    IF (water%SNOWH > 0.05) THEN
      energy%TGS = energy%STC(0)
    ELSE
      energy%TGS = energy%TG
    END IF

  END SUBROUTINE EnergyMain   

END module EnergyModule

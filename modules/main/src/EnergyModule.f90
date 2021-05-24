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
    INTEGER                              :: IZ       ! do-loop index
    REAL                                 :: FMELT    ! melting factor for snow cover frac
    REAL                                 :: Z0MG     ! z0 momentum, ground (m)
    REAL                                 :: Z0M      ! z0 momentum (m)
    REAL                                 :: ZPDG     ! zero plane displacement, ground (m)
    REAL                                 :: ZPD      ! zero plane displacement (m)
    REAL                                 :: ZLVL     ! reference height (m)
    REAL                                 :: GX       ! temporary variable -- prev. undeclared in ENERGY())
    REAL                                 :: PSI      ! surface layer soil matrix potential (m)
    REAL                                 :: BEVAP    ! soil water evaporation factor (0-1)
    REAL                                 :: RSURF    ! ground surface resistance (s/m)
    REAL                                 :: L_RSURF  ! Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
    REAL                                 :: D_RSURF  ! Reduced vapor diffusivity in soil for computing RSURF (SZ09)
    REAL                                 :: RHSUR    ! relative humidity in surface soil/snow air space (-)  
    REAL                                 :: CMV      ! momentum drag coefficient
    REAL                                 :: CMB      ! momentum drag coefficient
    REAL                                 :: PSNSUN   ! sunlit photosynthesis (umolco2/m2/s)
    REAL                                 :: PSNSHA   ! shaded photosynthesis (umolco2/m2/s)
    REAL                                 :: FIRE     ! emitted IR (w/m2)
    ! ---------------------------------------------------------------------

    ! associate common variables to keep variable names intact in the code below  
    associate(&
      ! examples
      SFCTMP   => forcing%SFCTMP     ,&   ! intent(in)    : real  air temperature at reference height (K)  
      EAH      => energy%EAH         ,&   ! intent(in)    : real  canopy air vapor pressure (Pa) 
      TV       => energy%TV          ,&   ! intent(in)    : real  vegetation temperature (K)  
      TG       => energy%TG          ,&   ! real, intent(inout) : ground temperature (K)  
      ZLVL     => energy%ZLVL        ,&   ! intent(in)    : reference height (m) 
      EMV      => energy%EMV         ,&   ! intent(in)    : vegetation emissivity
      EMG      => energy%EMG         ,&   ! intent(in)    : ground emissivity
      LWDN     => energy%LWDN        ,&   ! intent(inout) : atmospheric longwave radiation (w/m2)
      FVEG     => parameters%FVEG    ,&   ! intent(in)    : greeness vegetation fraction (-)
      CPAIR    => parameters%CPAIR   ,&   ! intent(in)    : heat capacity dry air at const pres (j/kg/k)
      FSNO     => water%FSNO         ,&   ! REAL, INTENT(OUT)   : fraction of grid cell with snow cover
    )  
    ! ---- end associate block --------------------------------------------------------------------

    ! Determine whether grid cell is vegetated or not
    parameters%VAI = parameters%ELAI + parameters%ESAI
    IF(parameters%VAI > 0.0) THEN
      parameters%VEG = .TRUE.
    ELSE
      parameters%VEG = .FALSE.
    ENDIF

    ! Compute fraction of grid cell with snow cover [Niu and Yang, 2007, JGR]
    ! Note: MFSNO (m in Niu and Yang) is set to 2.5 for all vegtypes in NOAH-MP
    ! Reference paper indicates MFSNO varies in space (values of 1.0, 1.6, 1.8)
    ! KSJ 2021-04-06
    FSNO = 0.0
    IF(water%SNOWH > 0.0)  THEN
      water%BDSNO  = water%SNEQV / water%SNOWH
      FMELT        = (water%BDSNO / 100.)**parameters%MFSNO
      FSNO   = TANH( water%SNOWH /(2.5 * parameters%Z0 * FMELT)) ! eq. 4 from Niu and Yang (2007)
    ENDIF

    ! Compute ground roughness length
    IF(domain%IST == 2) THEN
      IF(TG <= parameters%TFRZ) THEN
        Z0MG = (0.01 * (1.0 - FSNO)) + (FSNO * parameters%Z0SNO)
      ELSE
        Z0MG = 0.01  
      END IF
    ELSE
      Z0MG = (parameters%Z0 * (1.0 - FSNO)) + (FSNO * parameters%Z0SNO)
    END IF

    ! Compute roughness length and displacement height
    ZPDG  = water%SNOWH
    IF(parameters%VEG) THEN
      Z0M  = parameters%Z0MVT
      ZPD  = 0.65 * parameters%HVT
      IF(water%SNOWH > ZPD) ZPD = water%SNOWH
    ELSE
      Z0M  = Z0MG
      ZPD  = ZPDG
    END IF

    ! special case for urban
    IF (parameters%urban_flag) THEN
      Z0MG = parameters%Z0MVT
      ZPDG = 0.65 * parameters%HVT
      Z0M  = Z0MG
      ZPD  = ZPDG
    END IF

    ZLVL = MAX(ZPD, parameters%HVT) + domain%ZREF
    IF(ZPDG >= ZLVL) ZLVL = ZPDG + domain%ZREF

    ! Compute snow and soil thermodynamic properties
    call THERMOPROP(domain, levels, options, parameters, forcing, energy, water)

    ! Compute the heat advected by precipitation
    call PRECIP_HEAT(parameters, forcing, energy, water)

    ! Compute net solar radiation
    ! Subroutine was formerly called RADIATION
    ! Changed name because it only computes shortwave -- KSJ 2021-04-20
    call ShortwaveRadiationMain (domain, levels, options, parameters, forcing, energy, water)

    ! vegetation and ground emissivity
    EMV = 1. - EXP(-(parameters%ELAI + parameters%ESAI)/1.0)
    IF (energy%ICE == 1) THEN
      EMG = 0.98*(1.-FSNO) + 1.0*FSNO
    ELSE
      EMG = parameters%EG(domain%IST)*(1.-FSNO) + 1.0*FSNO
    END IF

    ! calculate soil moisture stress factor controlling stomatal resistance
    water%BTRAN = 0.

    IF(domain%IST ==1 ) THEN
      DO IZ = 1, parameters%NROOT
        IF(options%OPT_BTR == 1) then                  ! Noah
          GX    = (water%SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
        END IF
        IF(options%OPT_BTR == 2) then                  ! CLM
          PSI   = MAX(parameters%PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,water%SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
          GX    = (1.-PSI/parameters%PSIWLT)/(1.+parameters%PSISAT(IZ)/parameters%PSIWLT)
        END IF
        IF(options%OPT_BTR == 3) then                  ! SSiB
          PSI   = MAX(parameters%PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,water%SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
          GX    = 1.-EXP(-5.8*(LOG(parameters%PSIWLT/PSI)))
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
      RSURF = 1.0         ! avoid being divided by 0
      RHSUR = 1.0
    ELSE
      IF(options%OPT_RSF == 1 .OR. options%OPT_RSF == 4) THEN
        ! RSURF based on Sakaguchi and Zeng, 2009
        ! taking the "residual water content" to be the wilting point,
        ! and correcting the exponent on the D term (typo in SZ09 ?)
        L_RSURF = (-domain%ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,water%SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 )
        D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** &
                    (2.0+3.0/parameters%BEXP(1))
        RSURF = L_RSURF / D_RSURF
      ELSEIF(options%OPT_RSF == 2) THEN
        RSURF = FSNO * 1. + (1.-FSNO) * EXP(8.25-4.225 * BEVAP)       ! Sellers (1992) ! Older RSURF computations
      ELSEIF(options%OPT_RSF == 3) THEN
        RSURF = FSNO * 1. + (1.-FSNO) * EXP(8.25-6.0   * BEVAP)       ! adjusted to decrease RSURF for wet soil
    ENDIF
    IF(options%OPT_RSF == 4) THEN                                     ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
      RSURF = 1. / (FSNO * (1./parameters%RSURF_SNOW) + (1.-FSNO) * (1./max(RSURF, 0.001)))
    ENDIF
    IF(water%SH2O(1) < 0.01 .and. water%SNOWH == 0.) RSURF = 1.E6
      PSI   = -parameters%PSISAT(1) * (MAX(0.01, water%SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))
      RHSUR = FSNO + (1.-FSNO) * EXP(PSI * parameters%GRAV/(parameters%RW * TG))
    END IF

    ! urban - jref
    IF (parameters%urban_flag .and. water%SNOWH == 0. ) THEN
      RSURF = 1.E6
    ENDIF

    ! set psychrometric constant
    IF (TV .GT. parameters%TFRZ) THEN           ! Barlage: add distinction between ground and
      energy%LATHEAV = parameters%HVAP          !          vegetation in v3.6
      energy%frozen_canopy = .false.
    ELSE
      energy%LATHEAV = parameters%HSUB
      energy%frozen_canopy = .true.
    END IF
    energy%GAMMAV = CPAIR*SFCPRS/(0.622*energy%LATHEAV)

    IF (TG .GT. parameters%TFRZ) THEN
     energy%LATHEAG = parameters%HVAP
     energy%frozen_ground = .false.
    ELSE
      energy%LATHEAG = parameters%HSUB
      energy%frozen_ground = .true.
    END IF
    energy%GAMMAG = CPAIR*SFCPRS/(0.622*energy%LATHEAG)

    ! next block commented out in orig code
    !IF (SFCTMP .GT. parameters%TFRZ) THEN
    !  energy%LATHEA = parameters%HVAP
    !ELSE
    !  energy%LATHEA = parameters%HSUB
    !END IF
    !energy%GAMMA = CPAIR*SFCPRS/(0.622*energy%LATHEA)

    ! Calculate surface temperatures of the ground and canopy and energy fluxes
    IF (parameters%VEG .AND. FVEG > 0) THEN
      energy%TGV = TG
      CMV        = energy%CM
      energy%CHV = energy%CH

      ! Calculate canopy energy fluxes
      CALL VegeFluxMain (domain, levels, options, parameters, forcing, energy, water) 

    ELSE
      energy%TAUXV     = 0.
      energy%TAUYV     = 0.
      energy%IRC       = 0.
      energy%SHC       = 0.
      energy%IRG       = 0.
      energy%SHG       = 0.
      EVG              = 0.
      EVC              = 0.
      energy%TR        = 0.
      energy%GHV       = 0.
      PSNSUN           = 0.
      PSNSHA           = 0.
      energy%T2MV      = 0.
      energy%Q2V       = 0.
      energy%CHV       = 0.
      energy%CHLEAF    = 0.
      energy%CHUC      = 0.
      energy%CHV2      = 0.
      energy%RB        = 0.
    END IF

    TGB = TG
    CMB = energy%CM
    CHB = energy%CH

    CALL BareFluxMain (domain, levels, options, parameters, forcing, energy, water)

    !energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG
    !energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
    !energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (parameters%VEG .AND. FVEG > 0) THEN
      energy%TAUX  = FVEG * energy%TAUXV     + (1.0 - FVEG) * energy%TAUXB
      energy%TAUY  = FVEG * energy%TAUYV     + (1.0 - FVEG) * energy%TAUYB
      energy%FIRA  = FVEG * energy%IRG       + (1.0 - FVEG) * energy%IRB       + energy%IRC
      energy%FSH   = FVEG * energy%SHG       + (1.0 - FVEG) * energy%SHB       + energy%SHC
      energy%FGEV  = FVEG * energy%EVG       + (1.0 - FVEG) * energy%EVB
      energy%SSOIL = FVEG * energy%GHV       + (1.0 - FVEG) * energy%GHB
      energy%FCEV  = energy%EVC
      energy%FCTR  = energy%TR
      energy%PAH   = FVEG * energy%PAHG      + (1.0 - FVEG) * energy%PAHB   + energy%PAHV
      TG    = FVEG * energy%TGV       + (1.0 - FVEG) * energy%TGB
      energy%T2M   = FVEG * energy%T2MV      + (1.0 - FVEG) * energy%T2MB
      energy%TS    = FVEG * energy%TV        + (1.0 - FVEG) * energy%TGB
      energy%CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
      energy%CH    = FVEG * energy%CHV       + (1.0 - FVEG) * energy%CHB
      energy%Q1    = FVEG * (EAH*0.622/(energy%SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*energy%QSFC
      energy%Q2E   = FVEG * energy%Q2V       + (1.0 - FVEG) * energy%Q2B
      energy%Z0WRF = Z0M
    ELSE
      energy%TAUX  = energy%TAUXB
      energy%TAUY  = energy%TAUYB
      energy%FIRA  = energy%IRB
      energy%FSH   = energy%SHB
      energy%FGEV  = energy%EVB
      energy%SSOIL = energy%GHB
      TG           = energy%TGB      ! could use more associated variables to unclutter the code
      energy%T2M   = energy%T2MB
      energy%FCEV  = 0.
      energy%FCTR  = 0.
      energy%PAH   = energy%PAHB
      energy%TS    = TG
      energy%CM    = CMB
      energy%CH    = energy%CHB
      energy%Q1    = energy%QSFC
      energy%Q2E   = energy%Q2B
      energy%RSSUN = 0.0
      energy%RSSHA = 0.0
      energy%TGV   = energy%TGB
      energy%CHV   = energy%CHB
      energy%Z0WRF = Z0MG
    END IF

    FIRE = LWDN + energy%FIRA
    IF(FIRE <=0.) THEN
      !WRITE(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
      !WRITE(6,*) 'input of SHDFAC with LAI'
      !WRITE(6,*) domain%ILOC, domain%JLOC, 'SHDFAC=',FVEG,'parameters%VAI=',parameters%VAI,'TV=',TV,'TG=',TG
      !WRITE(6,*) 'LWDN=',LWDN,'energy%FIRA=',energy%FIRA,'water%SNOWH=',water%SNOWH
      ! call wrf_error_fatal("STOP in Noah-MP")
      WRITE(*,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
      WRITE(*,*) 'input of SHDFAC with LAI'
      WRITE(*,*) domain%ILOC, domain%JLOC, 'SHDFAC=',FVEG,'parameters%VAI=',parameters%VAI,'TV=',TV,'TG=',TG
      WRITE(*,*) 'LWDN=',LWDN,'energy%FIRA=',energy%FIRA,'water%SNOWH=',water%SNOWH
      WRITE(*,*) 'Exiting ...'
      STOP
    END IF

    ! Compute a net emissivity
    energy%EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + (1-FVEG) * EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    energy%TRAD = ( ( FIRE - (1 - energy%EMISSI) * LWDN ) / (energy%EMISSI * parameters%SB) ) ** 0.25
    !energy%TRAD = (FIRE/SB)**0.25          Old TRAD calculation not taking into account Emissivity

    energy%APAR = energy%PARSUN * energy%LAISUN + energy%PARSHA * energy%LAISHA
    energy%PSN  = energy%PSNSUN * energy%LAISUN + energy%PSNSHA * energy%LAISHA
    
    ! calculate 3L snow & 4L soil temperatures
    CALL TSNOSOI (parameters, levels, domain, energy%ICE, water%ISNOW   , & ! in
                  energy%SSOIL, energy%DF, energy%HCPCT, & !in
                  energy%SAG, water%SNOWH, TG, & !in
                  energy%STC     )                                          ! inout
    
    ! AW:  need to decide what to do with STC if no subsurface will be simulated
    !      ie, should soil layers be 0C if there is snow and TGB if not? 
                  
    ! adjusting snow surface temperature
    IF(options%OPT_STC == 2) THEN
      IF (water%SNOWH > 0.05 .AND. TG > parameters%TFRZ) THEN
        energy%TGV = parameters%TFRZ
        energy%TGB = parameters%TFRZ
        IF (parameters%VEG .AND. FVEG > 0) THEN
          TG           = FVEG * energy%T    + (1.0 - FVEG) * energy%TGB
          energy%TS    = FVEG * TV          + (1.0 - FVEG) * energy%TGB
        ELSE
          TG           = energy%TGB
          energy%TS    = energy%TGB
        END IF
      END IF
    END IF

    ! Energy released or consumed by snow & frozen soil
    CALL PHASECHANGE (parameters, domain, energy, water, levels%NSNOW, levels%NSOIL)
                      
  END SUBROUTINE EnergyMain   



END module EnergyModule

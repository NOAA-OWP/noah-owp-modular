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

  implicit none

contains

!== begin energy subroutine ================================================================================

  SUBROUTINE EnergyMain (domain, levels, options, parameters, forcing, energy, water)
!---------------------------------------------------------------------
! Main module for all water components
!---------------------------------------------------------------------

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
    REAL                                 :: Z0MG   ! z0 momentum, ground (m)
    REAL                                 :: Z0M    ! z0 momentum (m)
    REAL                                 :: ZPDG   ! zero plane displacement, ground (m)
    REAL                                 :: ZPD    ! zero plane displacement (m)
    REAL                                 :: ZLVL   ! reference height (m)
    
    !---------------------------------------------------------------------

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

    water%FSNO = 0.0
    IF(water%SNOWH > 0.0)  THEN
      water%BDSNO  = water%SNEQV / water%SNOWH
      FMELT        = (water%BDSNO / 100.)**parameters%MFSNO
      water%FSNO   = TANH( water%SNOWH /(2.5 * parameters%Z0 * FMELT)) ! eq. 4 from Niu and Yang (2007)
    ENDIF

    ! Compute ground roughness length
    IF(domain%IST == 2) THEN
      IF(energy%TG <= parameters%TFRZ) THEN
        Z0MG = (0.01 * (1.0 - water%FSNO)) + (water%FSNO * parameters%Z0SNO)
      ELSE
        Z0MG = 0.01  
      END IF
    ELSE
      Z0MG = (parameters%Z0 * (1.0 - water%FSNO)) + (water%FSNO * parameters%Z0SNO)
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
    call ShortwaveRadiationMain (domain, levels, options, parameters, forcing, energy, water)
    
!
!     ! vegetation and ground emissivity
!
!     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
!     IF (ICE == 1) THEN
!       EMG = 0.98*(1.-FSNO) + 1.0*FSNO
!     ELSE
!       EMG = parameters%EG(IST)*(1.-FSNO) + 1.0*FSNO
!     END IF
!
!     ! soil moisture factor controlling stomatal resistance
!
!     BTRAN = 0.
!
!     IF(IST ==1 ) THEN
!       DO IZ = 1, parameters%NROOT
!         IF(OPT_BTR == 1) then                  ! Noah
!           GX    = (SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
!         END IF
!         IF(OPT_BTR == 2) then                  ! CLM
!           PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
!           GX    = (1.-PSI/PSIWLT)/(1.+parameters%PSISAT(IZ)/PSIWLT)
!         END IF
!         IF(OPT_BTR == 3) then                  ! SSiB
!           PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
!           GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI)))
!         END IF
!         GX = MIN(1.,MAX(0.,GX))
!         BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(parameters%NROOT)) * GX)
!         BTRAN      = BTRAN + BTRANI(IZ)
!       END DO
!       BTRAN = MAX(MPE,BTRAN)
!       BTRANI(1:parameters%NROOT) = BTRANI(1:parameters%NROOT)/BTRAN
!     END IF
!
!   ! soil surface resistance for ground evap.
!
!     BEVAP = MAX(0.0,SH2O(1)/parameters%SMCMAX(1))
!     IF(IST == 2) THEN
!       RSURF = 1.          ! avoid being divided by 0
!       RHSUR = 1.0
!     ELSE
!       IF(OPT_RSF == 1 .OR. OPT_RSF == 4) THEN
!         ! RSURF based on Sakaguchi and Zeng, 2009
!         ! taking the "residual water content" to be the wilting point,
!         ! and correcting the exponent on the D term (typo in SZ09 ?)
!         L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 )
!         D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** (2.0+3.0/parameters%BEXP(1))
!         RSURF = L_RSURF / D_RSURF
!       ELSEIF(OPT_RSF == 2) THEN
!         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992) ! Older RSURF computations
!       ELSEIF(OPT_RSF == 3) THEN
!         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil
!     ENDIF
!     IF(OPT_RSF == 4) THEN  ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
!       RSURF = 1. / (FSNO * (1./parameters%RSURF_SNOW) + (1.-FSNO) * (1./max(RSURF, 0.001)))
!     ENDIF
!     IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
!       PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))
!       RHSUR = FSNO + (1.-FSNO) * EXP(PSI*GRAV/(RW*TG))
!     END IF
!
!     ! urban - jref
!     IF (parameters%urban_flag .and. SNOWH == 0. ) THEN
!       RSURF = 1.E6
!     ENDIF
!
!   ! set psychrometric constant
!
!     IF (TV .GT. TFRZ) THEN           ! Barlage: add distinction between ground and
!       LATHEAV = HVAP                ! vegetation in v3.6
!       frozen_canopy = .false.
!     ELSE
!       LATHEAV = HSUB
!       frozen_canopy = .true.
!     END IF
!     GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)
!
!     IF (TG .GT. TFRZ) THEN
!       LATHEAG = HVAP
!       frozen_ground = .false.
!     ELSE
!       LATHEAG = HSUB
!       frozen_ground = .true.
!     END IF
!     GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)
!
!   !     IF (SFCTMP .GT. TFRZ) THEN
!   !        LATHEA = HVAP
!   !     ELSE
!   !        LATHEA = HSUB
!   !     END IF
!   !     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)
!
!   ! Surface temperatures of the ground and canopy and energy fluxes
!
!     IF (parameters%VEG .AND. FVEG > 0) THEN
!       TGV = TG
!       CMV = CM
!       CHV = CH
!       CALL VEGE_FLUX
!     ELSE
!       TAUXV     = 0.
!       TAUYV     = 0.
!       IRC       = 0.
!       SHC       = 0.
!       IRG       = 0.
!       SHG       = 0.
!       EVG       = 0.
!       EVC       = 0.
!       TR        = 0.
!       GHV       = 0.
!       PSNSUN    = 0.
!       PSNSHA    = 0.
!       T2MV      = 0.
!       Q2V       = 0.
!       CHV       = 0.
!       CHLEAF    = 0.
!       CHUC      = 0.
!       CHV2      = 0.
!       RB        = 0.
!     END IF
!
!     TGB = TG
!     CMB = CM
!     CHB = CH
!     CALL BARE_FLUX
!
!   !energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG
!   !energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
!   !energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG
!
!     IF (parameters%VEG .AND. FVEG > 0) THEN
!       TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
!       TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
!       FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
!       FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
!       FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
!       SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
!       FCEV  = EVC
!       FCTR  = TR
!       PAH   = FVEG * PAHG      + (1.0 - FVEG) * PAHB   + PAHV
!       TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
!       T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
!       TS    = FVEG * TV        + (1.0 - FVEG) * TGB
!       CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
!       CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
!       Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
!       Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
!       Z0WRF = Z0M
!     ELSE
!       TAUX  = TAUXB
!       TAUY  = TAUYB
!       FIRA  = IRB
!       FSH   = SHB
!       FGEV  = EVB
!       SSOIL = GHB
!       TG    = TGB
!       T2M   = T2MB
!       FCEV  = 0.
!       FCTR  = 0.
!       PAH   = PAHB
!       TS    = TG
!       CM    = CMB
!       CH    = CHB
!       Q1    = QSFC
!       Q2E   = Q2B
!       RSSUN = 0.0
!       RSSHA = 0.0
!       TGV   = TGB
!       CHV   = CHB
!       Z0WRF = Z0MG
!     END IF
!
!     FIRE = LWDN + FIRA
!
!     IF(FIRE <=0.) THEN
!        WRITE(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
!        WRITE(6,*) 'input of SHDFAC with LAI'
!        WRITE(6,*) ILOC, JLOC, 'SHDFAC=',FVEG,'parameters%VAI=',parameters%VAI,'TV=',TV,'TG=',TG
!        WRITE(6,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
!        ! call wrf_error_fatal("STOP in Noah-MP")
!     END IF
!
!     ! Compute a net emissivity
!     EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
!          (1-FVEG) * EMG
!
!     ! When we're computing a TRAD, subtract from the emitted IR the
!     ! reflected portion of the incoming LWDN, so we're just
!     ! considering the IR originating in the canopy/ground system.
!
!     TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25
!
!     ! Old TRAD calculation not taking into account Emissivity:
!     ! TRAD = (FIRE/SB)**0.25
!
!     APAR = PARSUN*LAISUN + PARSHA*LAISHA
!     PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA
!
!     ! 3L snow & 4L soil temperatures
!
!     CALL TSNOSOI
!
!     ! adjusting snow surface temperature
!     IF(OPT_STC == 2) THEN
!       IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
!         TGV = TFRZ
!         TGB = TFRZ
!         IF (parameters%VEG .AND. FVEG > 0) THEN
!           TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
!           TS    = FVEG * TV        + (1.0 - FVEG) * TGB
!         ELSE
!           TG    = TGB
!           TS    = TGB
!         END IF
!       END IF
!     END IF
!
!     ! Energy released or consumed by snow & frozen soil
!
!     CALL PHASECHANGE

  END SUBROUTINE EnergyMain   

end module EnergyModule

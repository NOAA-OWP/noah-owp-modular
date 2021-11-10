! NOTES
!   opt_crop==2 is not supported [gecros]

! contains
!   VegeFluxMain, BareFluxMain

!   and subroutines in FluxUtilityModule:
!     RAGRB, STOMATA, CANRES, CALHUM, ESAT, SFCDIF1, SFCDIF2
!     in addition to rewritten old statement functions (at end)

module EtFluxModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType
  implicit none
  
  !REAL, PARAMETER      :: CP = 1004.5, RD = 287.04, SIGMA = 5.67E-8,    &
  !                        CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
  !                        LSUBF = 3.335E+5
  
  public  ::  VEGEFLUXMAIN   ! major routine, orig VEGE_FLUX
  public  ::  BAREFLUXMAIN   ! major routine, orig BARE_FLUX
  private ::    SFCDIF1      ! subs called by vege & bare flux routines                
  private ::    SFCDIF2                
  private ::    STOMATA                  
  private ::    CANRES                  
  private ::    ESAT
  private ::    RAGRB
  private ::    TDC, F1, PSLMU, PSLMS, PSLHU, PSLHS, PSPMU, PSPMS, PSPHU, PSPHS  ! statement functions

contains
  
  ! == begin VegeFluxMain ==================================================================================
  SUBROUTINE VegeFluxMain (domain, levels, options, parameters, forcing, energy, water)
    ! --------------------------------------------------------------------------------------------------
    ! use newton-raphson iteration to solve for vegetation (tv) and
    ! ground (tg) temperatures that balance the surface energy budgets

    ! vegetated:
    ! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
    ! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    ! ------------------------ in, inout, out variables ----------------------------------------------------
    type (    levels_type), intent(in)    :: levels
    type (parameters_type), intent(in)    :: parameters
    type (    domain_type), intent(in)    :: domain
    type (   forcing_type), intent(in)    :: forcing
    type (   options_type), intent(in)    :: options
    type (    energy_type)                :: energy
    type (     water_type)                :: water
    
    ! ------------------------ local variables ----------------------------------------------------
    REAL :: CW           ! water vapor exchange coefficient
    REAL :: FV           ! friction velocity (m/s)
    REAL :: WSTAR        ! friction velocity n vertical direction (m/s) (only for SFCDIF2)
    REAL :: Z0H          ! roughness length, sensible heat (m)
    REAL :: Z0HG         ! roughness length, sensible heat (m)
    REAL :: RB           ! bulk leaf boundary layer resistance (s/m)
    REAL :: RAMC         ! aerodynamic resistance for momentum (s/m)
    REAL :: RAHC         ! aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWC         ! aerodynamic resistance for water vapor (s/m)
    REAL :: RAMG         ! aerodynamic resistance for momentum (s/m)
    REAL :: RAHG         ! aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWG         ! aerodynamic resistance for water vapor (s/m)

    REAL :: MOL          ! Monin-Obukhov length (m)
    REAL :: DTV          ! change in tv, last iteration (k)
    REAL :: DTG          ! change in tg, last iteration (k)

    REAL :: AIR,CIR      !coefficients for ir as function of ts**4
    REAL :: CSH          !coefficients for sh as function of ts
    REAL :: CEV          !coefficients for ev as function of esat[ts]
    REAL :: CGH          !coefficients for st as function of ts
    REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
    REAL :: ATA,BTA      !coefficients for tah as function of ts
    REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]

    REAL :: ESTV         !saturation vapor pressure at tv (pa)
    REAL :: ESTG         !saturation vapor pressure at tg (pa)
    REAL :: DESTV        !d(es)/dt at ts (pa/k)
    REAL :: DESTG        !d(es)/dt at tg (pa/k)
    REAL :: ESATW        !es for water
    REAL :: ESATI        !es for ice
    REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
    REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice

    REAL :: FM           !momentum stability correction, weighted by prior iters
    REAL :: FH           !sen heat stability correction, weighted by prior iters
    REAL :: FHG          !sen heat stability correction, ground
    REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]

    REAL :: A            !temporary calculation
    REAL :: B            !temporary calculation
    REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
    REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
    REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
    REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
    REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
    REAL :: COND         !sum of conductances (s/m)
    REAL :: UC           !wind speed at top of canopy (m/s)
    REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
    REAL :: H            !temporary sensible heat flux (w/m2)
    REAL :: HG           !temporary sensible heat flux (w/m2)
    REAL :: MOZ          !Monin-Obukhov stability parameter
    REAL :: MOZG         !Monin-Obukhov stability parameter
    REAL :: MOZOLD       !Monin-Obukhov stability parameter from prior iteration
    REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
    REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
    REAL :: CH2          !Surface exchange at 2m
    REAL :: THSTAR       !Surface exchange at 2m

    REAL :: THVAIR
    REAL :: THAH 
    REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
    REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
    REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
    REAL :: EAH2         !2m vapor pressure over canopy
    REAL :: QFX        !moisture flux
    REAL :: E1           

    REAL :: VAIE         !total leaf area index + stem area index,effective
    REAL :: LAISUNE      !sunlit leaf area index, one-sided (m2/m2),effective
    REAL :: LAISHAE      !shaded leaf area index, one-sided (m2/m2),effective

    INTEGER :: K         !index
    INTEGER :: ITER      !iteration index
    
    REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
    
    !jref - NITERC test from 5 to 20  
    INTEGER, PARAMETER :: NITERC = 20   !number of iterations for surface temperature
    !jref - NITERG test from 3-5
    INTEGER, PARAMETER :: NITERG = 5   ! number of iterations for ground temperature
    INTEGER            :: MOZSGN       ! number of times MOZ changes sign

    INTEGER :: LITER     ! Last iteration
    REAL    :: ROOTD, WUL, WLL, Thickness, TLAIE, GLAIE, TLAI, GLAI, FRSU
    INTEGER :: NROOT, J

    REAL    :: T  !, TDC        ! Kelvin to degree Celsius with limit -50 to +50

    character(len=80) ::  message
    ! ---------------------------------------------------------------------------------------------
  
    !TDC(T)   = MIN( 50., MAX(-50.,(T-parameters%TFRZ)) )     ! function declaration
      
    ! associate variables to keep variable names intact in the code below  
    associate(&
      SFCTMP   => forcing%SFCTMP     ,&   ! intent(in)    : real  air temperature at reference height (K)  
      TAH      => energy%TAH         ,&   ! intent(in)    : real  canopy temperature (K)  
      EAH      => energy%EAH         ,&   ! intent(in)    : real  canopy air vapor pressure (Pa) 
      TV       => energy%TV          ,&   ! intent(in)    : real  vegetation temperature (K)  
      RHOAIR   => forcing%RHOAIR     ,&   ! intent(in)    : real  density air (kg/m3)
      PSFC     => forcing%SFCPRS     ,&   ! intent(in)    : pressure at lowest model layer (Pa)  
      Z0M      => energy%Z0M         ,&   ! intent(in)    : real  vegetation temperature (K)  
      ZLVL     => energy%ZLVL        ,&   ! intent(in)    : reference height (m) 
      QSFC     => energy%QSFC        ,&   ! intent(inout  : mixing ratio at lowest model layer (g/g)  
      EMV      => energy%EMV         ,&   ! intent(in)    : vegetation emissivity
      EMG      => energy%EMG         ,&   ! intent(in)    : ground emissivity
      LWDN     => forcing%LWDN       ,&   ! intent(in) : atmospheric longwave radiation (w/m2)
      RSSHA    => energy%RSSHA       ,&   ! intent(in)    : shaded leaf stomatal resistance (s/m)
      RHSUR    => energy%RHSUR       ,&   ! intent(in)    : relative humidity in surface soil/snow air space (-)  
      FVEG     => parameters%FVEG    ,&   ! intent(in)    : greeness vegetation fraction (-)
      CPAIR    => parameters%CPAIR   ,&   ! intent(in)    : heat capacity dry air at const pres (j/kg/k)
      UR       => forcing%UR         )    ! intent(in)    : roughness length, momentum (m)  
      ! ---- end associate block --------------------------------------------------------------------

    LITER = 0      ! last iteration
    FV = 0.1

    ! ---------------------------------------------------------------------------------------------
    ! initialization variables that do not depend on stability iteration
    ! ---------------------------------------------------------------------------------------------
    DTV = 0.; DTG    = 0.
    MOZ = 0.; MOZSGN = 0; MOZOLD = 0.
    FH2 = 0.; HG     = 0.; H     = 0.
    QFX = 0.

    ! limit LAI
    VAIE    = MIN(6., parameters%VAI)
    LAISUNE = MIN(6., energy%LAISUN)
    LAISHAE = MIN(6., energy%LAISHA)

    ! saturation vapor pressure at ground temperature
    T = TDC(energy%TG, parameters%TFRZ)
    CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
    IF (T .GT. 0.) THEN
      ESTG = ESATW
    ELSE
      ESTG = ESATI
    END IF

    !jref - consistent surface specific humidity for sfcdif3 and sfcdif4
    QSFC = 0.622 * forcing%EAIR / (PSFC - 0.378 * forcing%EAIR)  

    ! canopy height
    HCAN = parameters%HVT
    UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
    UC = UR*LOG((HCAN-energy%ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7
    IF((HCAN-energy%ZPD) <= 0.) THEN
      WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
      !call wrf_message ( message )
      WRITE(message,*) 'i,j point=',domain%ILOC, domain%JLOC
      !call wrf_message ( message )
      WRITE(message,*) 'HCAN  =', HCAN
      !call wrf_message ( message )
      WRITE(message,*) 'ZPD   =', energy%ZPD
      !call wrf_message ( message )
      write (message, *) 'SNOWH =', water%SNOWH
      !call wrf_message ( message )
      !call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
      WRITE(*,*) "CRITICAL PROBLEM: HCAN <= ZPD"
      WRITE(*,*) 'i,j point=', domain%ILOC, domain%JLOC
      WRITE(*,*) 'HCAN  = ', HCAN
      WRITE(*,*) 'ZPD   = ', energy%ZPD
      write(*,*) 'SNOWH = ', water%SNOWH      
      write(*,*) "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX"
    END IF

    ! prepare for longwave rad.
    AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*parameters%SB*energy%TG**4  
    CIR = (2.-EMV*(1.-EMG))*EMV*parameters%SB

    ! ---------------------------------------------------------------------------------------------
    loop1: DO ITER = 1, NITERC    !  begin stability iteration

      IF(ITER == 1) THEN
        Z0H  = Z0M  
        Z0HG = energy%Z0MG
      ELSE
        Z0H  = Z0M           !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        Z0HG = energy%Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
      END IF

      ! aerodyn resistances between heights zlvl and d+z0v
      IF(options%OPT_SFC == 1) THEN
        CALL SFCDIF1(parameters, ITER, SFCTMP, RHOAIR, H, forcing%QAIR,     &  ! in
                     ZLVL, energy%ZPD, Z0M, Z0H, UR,                        &  ! in
                     MOZ, MOZSGN, FM, FH, FM2, FH2,                         &  ! inout
                     energy%CM, energy%CH, FV, CH2)                            ! out
        ! note, local vars:  ITER H ZOH MOZ MOZSGN FM FH FM2 FH2 FV CH2
      ENDIF
     
      IF(options%OPT_SFC == 2) THEN
        CALL SFCDIF2(parameters, ITER, Z0M, TAH, forcing%THAIR, UR,         & ! in
                     ZLVL, energy%CM, energy%CH, MOZ, WSTAR,                & ! in
                     FV )                                                     ! out
        ! Undo the multiplication by windspeed that SFCDIF2 
        ! applies to exchange coefficients CH and CM:
        energy%CH = energy%CH / UR
        energy%CM = energy%CM / UR
      ENDIF

      RAMC = MAX(1.,1./(energy%CM*UR))
      RAHC = MAX(1.,1./(energy%CH*UR))
      RAWC = RAHC

      ! calculate aerodynamic resistance between heights z0g and d+z0v, RAG, and leaf
      ! boundary layer resistance, RB
      CALL RAGRB(parameters, ITER, VAIE, RHOAIR, HG, TAH, energy%ZPD,     &  ! in
                 energy%Z0MG, Z0HG, HCAN, UC, Z0H, FV, domain%VEGTYP,     &  ! in
                 TV, MOZG, FHG,                                           &  ! inout
                 RAMG, RAHG, RAWG, RB)                                       ! out                  
      ! note, local vars:  ITER VAIE HG ZOHG HCAN UC Z0H FV MOZH FHG RAMG, RAHG, RAWG, RB
       
      ! es and d(es)/dt evaluated at tv
      T = TDC(TV, parameters%TFRZ)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTV  = ESATW
        DESTV = DSATW
      ELSE
        ESTV  = ESATI
        DESTV = DSATI
      END IF

      ! calculate stomatal resistance and photosynthesis (two options available)
      IF(ITER == 1) THEN
        IF (options%OPT_CRS == 1) then  ! Ball-Berry
          ! sun
          CALL STOMATA (parameters, domain%VEGTYP, energy%PARSUN, forcing%FOLN, TV, &  ! in
                        ESTV, EAH, SFCTMP, forcing%SFCPRS, forcing%O2PP,            &  ! in
                        forcing%CO2PP, energy%IGS, water%BTRAN, RB,                 &  ! in
                        energy%RSSUN , energy%PSNSUN)                                  ! out
          ! shade
          CALL STOMATA (parameters, domain%VEGTYP, energy%PARSHA, forcing%FOLN, TV, &  ! in
                        ESTV, EAH, SFCTMP, forcing%SFCPRS, forcing%O2PP,            &  ! in
                        forcing%CO2PP, energy%IGS, water%BTRAN, RB,                 &  ! in
                        energy%RSSHA , energy%PSNSHA)                                  ! out           
        END IF

        ! calculate sunlit and shaded resistances and leaf photosynthesis
        IF (options%OPT_CRS == 2) then  ! Jarvis
          ! sun
          CALL  CANRES (parameters, energy%PARSUN, TV, water%BTRAN, EAH, forcing%SFCPRS, &  ! in
                        energy%RSSUN, energy%PSNSUN )                                       ! out
          ! shade
          CALL  CANRES (parameters, energy%PARSHA, TV, water%BTRAN, EAH, forcing%SFCPRS, &  ! in
                        energy%RSSHA, energy%PSNSHA )                                       ! out                   
        END IF
        
        ! Call GECROS
        ! Note:  GECROS option (opt_crop == 2) is not currently supported. 
       
      END IF

      ! prepare for sensible heat flux above veg.
      CAH  = 1./RAHC
      CVH  = 2.*VAIE/RB
      CGH  = 1./RAHG
      COND = CAH + CVH + CGH
      ATA  = (SFCTMP*CAH + energy%TG*CGH) / COND
      BTA  = CVH/COND
      CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH

      ! prepare for latent heat flux above veg.
      CAW  = 1./RAWC
      CEW  = water%FWET*VAIE/RB

      IF (options%OPT_CROP /= 2) THEN
        CTW  = (1.-water%FWET)*(LAISUNE/(RB+energy%RSSUN) + LAISHAE/(RB+RSSHA))
      ELSE
        ! RSSUN and RSSHA are in resistance per unit LAI in the Jarvis and Ball-Berry!. RSSUN and RSSHA of Gecros are in s/m
        CTW  = (1.-water%FWET)*(1./(RB/(FRSU*GLAIE)+energy%RSSUN) + 1./(RB/((1.-FRSU)*GLAIE)+energy%RSSHA)) ! transpiration conductance leaf to canopy air
      ENDIF
      
      ! comment needed
      CGW  = 1./(RAWG+energy%RSURF)
      COND = CAW + CEW + CTW + CGW
      AEA  = (forcing%EAIR * CAW + ESTG * CGW) / COND
      BEA  = (CEW+CTW)/COND
      CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/energy%GAMMAV   ! Barlage: change to vegetation v3.6
      CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/energy%GAMMAV

      ! evaluate surface fluxes with current temperature and solve for dts
      TAH = ATA + BTA*TV               ! canopy air T.
      EAH = AEA + BEA*ESTV             ! canopy air e

      energy%IRC = FVEG*(AIR + CIR*TV**4)
      energy%SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
      energy%EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / energy%GAMMAV ! Barlage: change to v in v3.6
      energy%TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / energy%GAMMAV
      
      IF (TV > parameters%TFRZ) THEN
        energy%EVC = MIN(water%CANLIQ*energy%LATHEAV/domain%DT, energy%EVC)    ! Barlage: add if block for canice in v3.6
      ELSE
        energy%EVC = MIN(water%CANICE*energy%LATHEAV/domain%DT, energy%EVC)
      END IF

      B   = energy%SAV-energy%IRC-energy%SHC-energy%EVC-energy%TR+energy%PAHV                          !additional w/m2
      A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) !volumetric heat capacity
      DTV = B/A
      energy%IRC = energy%IRC + FVEG*4.*CIR*TV**3*DTV
      energy%SHC = energy%SHC + FVEG*CSH*DTV
      energy%EVC = energy%EVC + FVEG*CEV*DESTV*DTV
      energy%TR  = energy%TR  + FVEG*CTR*DESTV*DTV                               

      ! update vegetation surface temperature
      TV  = TV + DTV
      !TAH = ATA + BTA*TV               ! canopy air T; update here for consistency

      ! for computing M-O length in the next iteration
      H  = RHOAIR * CPAIR * (TAH - SFCTMP) /RAHC        
      HG = RHOAIR * CPAIR * (energy%TG  - TAH)   /RAHG

      ! consistent specific humidity from canopy air vapor pressure
      QSFC = (0.622 * EAH) / (forcing%SFCPRS - 0.378 * EAH)

      IF (LITER == 1) THEN
        exit loop1 
      ENDIF
      IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
        LITER = 1
      ENDIF

    END DO loop1 ! end stability iteration
    ! ---------------------------------------------------------------------------

    ! under-canopy fluxes and tg
    AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*parameters%SB*TV**4
    CIR = EMG*parameters%SB
    CSH = RHOAIR*CPAIR/RAHG
    CEV = RHOAIR*CPAIR / (energy%GAMMAG*(RAWG+energy%RSURF))  ! Barlage: change to ground v3.6
    CGH = 2. * energy%DF(water%ISNOW+1)/domain%DZSNSO(water%ISNOW+1)

    ! ========= LOOP 2 ========================
    loop2: DO ITER = 1, NITERG

      T = TDC(energy%TG, parameters%TFRZ)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTG  = ESATW
        DESTG = DSATW
      ELSE
        ESTG  = ESATI
        DESTG = DSATI
      END IF

      energy%IRG = CIR * energy%TG**4 + AIR
      energy%SHG = CSH * (energy%TG         - TAH         )
      energy%EVG = CEV * (ESTG*energy%RHSUR - EAH         )
      energy%GHV  = CGH * (energy%TG         - energy%STC(water%ISNOW+1))

      B = energy%SAG-energy%IRG-energy%SHG-energy%EVG-energy%GHV+energy%PAHG
      A = 4.*CIR*energy%TG**3+CSH+CEV*DESTG+CGH
      DTG = B/A

      energy%IRG = energy%IRG + 4.*CIR*energy%TG**3*DTG
      energy%SHG = energy%SHG + CSH*DTG
      energy%EVG = energy%EVG + CEV*DESTG*DTG
      energy%GHV = energy%GHV + CGH*DTG
      energy%TG  = energy%TG  + DTG

    END DO loop2
    ! ---------------------------------------------------------------------------
     
    !TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)

    ! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
    IF(options%OPT_STC == 1 .OR. options%OPT_STC == 3) THEN
      IF (water%SNOWH > 0.05 .AND. energy%TG > parameters%TFRZ) THEN
        IF(options%OPT_STC == 1) energy%TG = parameters%TFRZ
        IF(options%OPT_STC == 3) energy%TG = (1.-water%FSNO)*energy%TG + water%FSNO*parameters%TFRZ   ! MB: allow TG>0C during melt v3.7
        energy%IRG = CIR * (energy%TG)**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*parameters%SB*TV**4
        energy%SHG = CSH * (energy%TG  - TAH)
        energy%EVG = CEV * (ESTG*RHSUR - EAH)
        energy%GHV = energy%SAG + energy%PAHG - (energy%IRG+energy%SHG+energy%EVG)
      END IF
    END IF

    ! wind stresses
    energy%TAUXV = -RHOAIR*energy%CM*UR*forcing%UU
    energy%TAUYV = -RHOAIR*energy%CM*UR*forcing%VV

    ! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
    ! calculation.
    ! Commented out in original code
    !     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
    !     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
    !     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
    !     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG

    ! 2m temperature over vegetation ( corrected for low CQ2V values )
    IF (options%OPT_SFC == 1 .OR. options%OPT_SFC == 2) THEN
      !      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      energy%CAH2 = FV * parameters%VKC/LOG((2.+Z0H)/Z0H)
      energy%CAH2 = FV * parameters%VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = energy%CAH2
      IF (energy%CAH2 .LT. 1.E-5 ) THEN
        energy%T2MV = TAH
        ! Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
        energy%Q2V  = QSFC
      ELSE
        energy%T2MV = TAH - (energy%SHG+energy%SHC/FVEG)/(RHOAIR*CPAIR) * 1./energy%CAH2
        !Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
        energy%Q2V = QSFC - ((energy%EVC+energy%TR)/FVEG + energy%EVG) / (energy%LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
    ENDIF

    ! update CH for output
    energy%CH     = CAH
    energy%CHLEAF = CVH
    energy%CHUC   = 1./RAHG
  
    end associate ! end the associated variables
  END SUBROUTINE VegeFluxMain


  ! == begin BareFluxMain ==================================================================================
  SUBROUTINE BareFluxMain (domain, levels, options, parameters, forcing, energy, water)
    ! --------------------------------------------------------------------------------------------------
    ! use newton-raphson iteration to solve ground (tg) temperature
    ! that balances the surface energy budgets for bare soil fraction.
    !    bare soil:
    !      -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
    ! ----------------------------------------------------------------------
    IMPLICIT NONE

    ! ------------------------ in, inout, out variables ----------------------------------------------------
    type (    levels_type), intent(in)    :: levels
    type (parameters_type), intent(in)    :: parameters
    type (    domain_type), intent(in)    :: domain
    type (   forcing_type), intent(in)    :: forcing
    type (   options_type), intent(in)    :: options
    type (    energy_type)                :: energy
    type (     water_type)                :: water

    ! ------------------------ local variables ---------------------------
    REAL :: EHB     !bare ground heat conductance
    REAL :: U10B    !10 m wind speed in eastward dir (m/s)
    REAL :: V10B    !10 m wind speed in eastward dir (m/s)
    REAL :: WSPD

    !REAL :: TAUX       !wind stress: e-w (n/m2)
    !REAL :: TAUY       !wind stress: n-s (n/m2)
    !REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
    REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
    REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
    REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
    REAL :: FIRE       !emitted ir (w/m2)
    REAL :: TRAD       !radiative temperature (k)
    REAL :: TAH        !"surface" temperature at height z0h+zpd (k)

    REAL :: CW         !water vapor exchange coefficient
    REAL :: FV         !friction velocity (m/s)
    REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
    REAL :: Z0H        !roughness length, sensible heat, ground (m)
    REAL :: RB         !bulk leaf boundary layer resistance (s/m)
    REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
    REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
    REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
    REAL :: MOL        !Monin-Obukhov length (m)
    REAL :: DTG        !change in tg, last iteration (k)

    REAL :: CIR        !coefficients for ir as function of ts**4
    REAL :: CSH        !coefficients for sh as function of ts
    REAL :: CEV        !coefficients for ev as function of esat[ts]
    REAL :: CGH        !coefficients for st as function of ts

    REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
    REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
    REAL :: CH2B       !exchange coefficient for 2m temp.
    REAL :: CQ2B       !exchange coefficient for 2m temp.
    REAL :: THVAIR     !virtual potential air temp
    REAL :: THGH       !potential ground temp
    REAL :: EMB        !momentum conductance
    REAL :: QFX        !moisture flux
    REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
    INTEGER :: VEGTYP     !vegetation type set to isbarren
    REAL :: E1

    REAL :: ESTG       !saturation vapor pressure at tg (pa)
    REAL :: DESTG      !d(es)/dt at tg (pa/K)
    REAL :: ESATW      !es for water
    REAL :: ESATI      !es for ice
    REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
    REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice

    REAL :: A, B       !temporary calculation
    REAL :: H          !temporary sensible heat flux (w/m2)
    REAL :: MOZ        !Monin-Obukhov stability parameter
    REAL :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
    REAL :: FM         !momentum stability correction, weighted by prior iters
    REAL :: FH         !sen heat stability correction, weighted by prior iters
    INTEGER :: MOZSGN  !number of times MOZ changes sign
    REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
    REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
    REAL :: CH2          !Surface exchange at 2m

    INTEGER :: ITER    !iteration index
    INTEGER :: NITERB  !number of iterations for surface temperature
    REAL    :: MPE     !prevents overflow error if division by zero
    DATA NITERB /5/
    SAVE NITERB
    REAL :: T  !, TDC     !Kelvin to degree Celsius with limit -50 to +50
    ! -----------------------------------------------------------------
    
    !TDC(T)   = MIN( 50., MAX(-50.,(T-parameters%TFRZ)) )  ! struct ref needed?
    
    ! ---- associate variables to keep variable names intact in the code below  
    associate(&
      ! used in resistance calculations
      SFCTMP   => forcing%SFCTMP     ,&   ! intent(in)    : real  air temperature at reference height (K)  
      TGB      => energy%TGB         ,&   ! intent(in)    : real  ground temperature (K)  
      RHOAIR   => forcing%RHOAIR     ,&   ! intent(in)    : real  density air (kg/m3)
      PSFC     => forcing%SFCPRS     ,&   ! intent(in)    : real pressure at lowest model layer (Pa)  
      Z0M      => energy%Z0M         ,&   ! intent(in)    : real  vegetation temperature (K)  
      ZLVL     => energy%ZLVL        ,&   ! intent(in)    : real reference height (m) 
      QSFC     => energy%QSFC        ,&   ! intent(inout  : real mixing ratio at lowest model layer (g/g)  
      EMG      => energy%EMG         ,&   ! intent(in)    : real ground emissivity
      LWDN     => forcing%LWDN       ,&   ! intent(in) : real atmospheric longwave radiation (w/m2)
      CPAIR    => parameters%CPAIR   ,&   ! intent(in)    : real heat capacity dry air at const pres (j/kg/k)
      UR       => forcing%UR          &   ! intent(in)    : real roughness length, momentum (m)  
    ) ! ---- end associate block --------------------------------------------------------------------
    
    ! -----------------------------------------------------------------
    ! initialization variables that do not depend on stability iteration
    ! -----------------------------------------------------------------
    !MPE = 1E-6
    DTG    = 0.
    MOZ    = 0.
    MOZSGN = 0
    MOZOLD = 0.
    FH2    = 0.
    H      = 0.
    QFX    = 0.
    FV     = 0.1

    CIR = EMG*parameters%SB
    CGH = 2. * energy%DF(water%ISNOW+1)/domain%DZSNSO(water%ISNOW+1)

    ! -----------------------------------------------------------------
    loop3: DO ITER = 1, NITERB  ! begin stability iteration

      IF(ITER == 1) THEN
        Z0H = Z0M 
      ELSE
        Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
      END IF

      ! aerodyn resistances 
      IF(options%OPT_SFC == 1) THEN
        CALL SFCDIF1(parameters, ITER, SFCTMP, RHOAIR, H, forcing%QAIR,     &  ! in
                     energy%ZLVL, energy%ZPD, energy%Z0M, Z0H, UR,  &  ! in
                     MOZ, MOZSGN, FM, FH, FM2, FH2,                         &  ! inout
                     energy%CM, energy%CH, FV, CH2)                            ! out
        ENDIF

      IF(options%OPT_SFC == 2) THEN
        CALL SFCDIF2(parameters, ITER, energy%Z0M, TGB, forcing%THAIR,    & ! in and in/out
                     UR, energy%ZLVL, energy%CM, energy%CH, MOZ, WSTAR,  & ! in
                     FV )       
        ! Undo the multiplication by windspeed that SFCDIF2 
        ! applies to exchange coefficients CH and CM:
        energy%CH = energy%CH / UR
        energy%CM = energy%CM / UR
        IF(water%SNOWH > 0.) THEN
          energy%CM = MIN(0.01,energy%CM)   ! CM & CH are too large, causing
          energy%CH = MIN(0.01,energy%CH)   ! computational instability
        END IF

      ENDIF

      RAMB = MAX(1.,1./(energy%CM*UR))
      RAHB = MAX(1.,1./(energy%CH*UR))
      RAWB = RAHB

      ! variables for diagnostics          
      EMB = 1./RAMB
      EHB = 1./RAHB

      ! es and d(es)/dt evaluated at tg
      T = TDC(TGB, parameters%TFRZ)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
         ESTG  = ESATW
         DESTG = DSATW
      ELSE
         ESTG  = ESATI
         DESTG = DSATI
      END IF

      CSH = RHOAIR*CPAIR/RAHB
      CEV = RHOAIR*CPAIR/energy%GAMMAG/(energy%RSURF+RAWB)

      ! surface fluxes and dtg
      energy%IRB = CIR * TGB**4 - EMG*LWDN
      energy%SHB = CSH * (TGB - SFCTMP)
      energy%EVB = CEV * (ESTG*energy%RHSUR - forcing%EAIR)
      energy%GHB = CGH * (TGB - energy%STC(water%ISNOW+1))

      B   = energy%SAG-energy%IRB-energy%SHB-energy%EVB-energy%GHB + energy%PAHB
      A   = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
      DTG = B/A

      energy%IRB = energy%IRB + 4.*CIR*TGB**3*DTG
      energy%SHB = energy%SHB + CSH*DTG
      energy%EVB = energy%EVB + CEV*DESTG*DTG
      energy%GHB = energy%GHB + CGH*DTG

      ! update ground surface temperature
      TGB = TGB + DTG

      ! for M-O length
      H = CSH * (TGB - SFCTMP)

      T = TDC(TGB, parameters%TFRZ)
      CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
      IF (T .GT. 0.) THEN
        ESTG  = ESATW
      ELSE
        ESTG  = ESATI
      END IF
      QSFC = 0.622*(ESTG*energy%RHSUR)/(PSFC-0.378*(ESTG*energy%RHSUR))
      QFX  = (QSFC-forcing%QAIR)*CEV*energy%GAMMAG/CPAIR

    END DO loop3 ! end stability iteration
    ! -----------------------------------------------------------------

    ! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
    IF(options%OPT_STC == 1 .OR. options%OPT_STC == 3) THEN
      IF (water%SNOWH > 0.05 .AND. TGB > parameters%TFRZ) THEN
        IF(options%OPT_STC == 1) TGB = parameters%TFRZ
        IF(options%OPT_STC == 3) TGB = (1.-water%FSNO)*TGB + water%FSNO*parameters%TFRZ  ! MB: allow TG>0C during melt v3.7
        energy%IRB = CIR * TGB**4 - EMG*LWDN
        energy%SHB = CSH * (TGB        - SFCTMP)
        energy%EVB = CEV * (ESTG*energy%RHSUR - forcing%EAIR )          !ESTG reevaluate ?
        energy%GHB = energy%SAG + energy%PAHB - (energy%IRB+energy%SHB+energy%EVB)
      END IF
    END IF

    ! wind stresses
    energy%TAUXB = -RHOAIR*energy%CM*UR*forcing%UU
    energy%TAUYB = -RHOAIR*energy%CM*UR*forcing%VV

    ! 2m air temperature
    IF(options%OPT_SFC == 1 .OR. options%OPT_SFC ==2) THEN
      energy%EHB2  = FV* parameters%VKC/LOG((2.+Z0H)/Z0H)
      energy%EHB2  = FV* parameters%VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2B  = energy%EHB2
      IF (energy%EHB2.lt.1.E-5 ) THEN
        energy%T2MB  = TGB
        energy%Q2B   = QSFC
      ELSE
        energy%T2MB  = TGB - energy%SHB/(RHOAIR*CPAIR) * 1./energy%EHB2
        energy%Q2B   = QSFC - energy%EVB/(energy%LATHEA*RHOAIR)*(1./CQ2B + energy%RSURF)
      ENDIF
      IF (parameters%urban_flag) energy%Q2B = QSFC
    END IF

    ! update CH 
    energy%CH = EHB
    
    end associate ! end the associated variables
  END SUBROUTINE BareFluxMain
  
  
  ! == begin RAGRB ====================================================================================
  SUBROUTINE RAGRB(parameters, ITER, VAI, RHOAIR, HG, TAH, ZPD, Z0MG, & ! in
                   Z0HG, HCAN, UC, Z0H, FV, VEGTYP, &         ! in
                   TV, MOZG, FHG, &                                     ! inout
                   RAMG, RAHG, RAWG, RB)                                ! out
    ! --------------------------------------------------------------------------------------------------
    ! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
    ! resistance RB
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type(parameters_type), intent(in)   :: parameters ! parameters data structure
    !INTEGER,              INTENT(IN)    :: ILOC       ! grid index (not used)
    !INTEGER,              INTENT(IN)    :: JLOC       ! grid index (not used)
    INTEGER,              INTENT(IN)    :: ITER       ! iteration index
    INTEGER,              INTENT(IN)    :: VEGTYP     ! vegetation physiology type
    REAL,                 INTENT(IN)    :: VAI        ! total LAI + stem area index, one sided
    REAL,                 INTENT(IN)    :: RHOAIR     ! density air (kg/m3)
    REAL,                 INTENT(IN)    :: HG         ! ground sensible heat flux (w/m2)
    REAL,                 INTENT(IN)    :: TV         ! vegetation temperature (k)
    REAL,                 INTENT(IN)    :: TAH        ! air temperature at height z0h+zpd (k)
    REAL,                 INTENT(IN)    :: ZPD        ! zero plane displacement (m)
    REAL,                 INTENT(IN)    :: Z0MG       ! roughness length, momentum, ground (m)
    REAL,                 INTENT(IN)    :: HCAN       ! canopy height (m) [note: hcan >= z0mg]
    REAL,                 INTENT(IN)    :: UC         ! wind speed at top of canopy (m/s)
    REAL,                 INTENT(IN)    :: Z0H        ! roughness length, sensible heat (m)
    REAL,                 INTENT(IN)    :: Z0HG       ! roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN)    :: FV         ! friction velocity (m/s)
    ! in & out
    REAL,              INTENT(INOUT)    :: MOZG       ! Monin-Obukhov stability parameter
    REAL,              INTENT(INOUT)    :: FHG        ! stability correction
    ! outputs
    REAL,                INTENT(OUT)    :: RAMG       ! aerodynamic resistance for momentum (s/m)
    REAL,                INTENT(OUT)    :: RAHG       ! aerodynamic resistance for sensible heat (s/m)
    REAL,                INTENT(OUT)    :: RAWG       ! aerodynamic resistance for water vapor (s/m)
    REAL,                INTENT(OUT)    :: RB         ! bulk leaf boundary layer resistance (s/m)

    ! local
    REAL :: KH                   ! turbulent transfer coefficient, sensible heat, (m2/s)
    REAL :: TMP1                 ! temporary calculation
    REAL :: TMP2                 ! temporary calculation
    REAL :: TMPRAH2              ! temporary calculation for aerodynamic resistances
    REAL :: TMPRB                ! temporary calculation for rb
    real :: MOLG, FHGNEW, CWPC
    ! --------------------------------------------------------------------------------------------------

    ! stability correction to below canopy resistance
    MOZG = 0.; MOLG = 0.

    IF(ITER > 1) THEN
      TMP1 = parameters%VKC * (parameters%GRAV/TAH) * HG/(RHOAIR*parameters%CPAIR)
      IF (ABS(TMP1) .LE. parameters%MPE) TMP1 = parameters%MPE
      MOLG = -1. * FV**3 / TMP1
      MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
    END IF

    IF (MOZG < 0.) THEN
      FHGNEW  = (1. - 15.*MOZG)**(-0.25)
    ELSE
      FHGNEW  = 1.+ 4.7*MOZG
    ENDIF

    IF (ITER == 1) THEN
      FHG = FHGNEW
    ELSE
      FHG = 0.5 * (FHG+FHGNEW)
    ENDIF

    CWPC = (parameters%CWP * VAI * HCAN * FHG)**0.5
    !CWPC = (CWP*FHG)**0.5

    TMP1 = EXP( -CWPC*Z0HG/HCAN )
    TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
    TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)

    ! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.
    KH = MAX ( parameters%VKC*FV*(HCAN-ZPD), parameters%MPE )
    RAMG = 0.
    RAHG = TMPRAH2 / KH
    RAWG = RAHG

    ! leaf boundary layer resistance
    TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
    RB     = TMPRB * SQRT(parameters%DLEAF/UC)
    RB     = MAX(RB,100.0)
    !RB = 200
    
  END SUBROUTINE RAGRB


  ! == begin stomata ==================================================================================

  SUBROUTINE STOMATA (parameters, VEGTYP, APAR, FOLN, TV, &         ! in
                      EI, EA, SFCTMP, SFCPRS, O2, &                 ! in
                      CO2, IGS, BTRAN, RB,    &                     ! in
                      RS, PSN)                                      ! out
    IMPLICIT NONE

    ! --------------------------------------------------------------------------------------------------
    ! input
    type (parameters_type), intent(in) :: parameters
    INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type
    REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
    REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
    REAL, INTENT(IN)    :: TV     !foliage temperature (k)
    REAL, INTENT(IN)    :: EI     !vapor pressure inside leaf (sat vapor press at tv) (pa)
    REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
    REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
    REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
    REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa) -- partial pressures, from parameters type
    REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
    REAL, INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)
    REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
    REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)
    ! output
    REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
    REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]

    ! ------------------------ local variables ----------------------------------------------------
    INTEGER :: ITER     !iteration index
    INTEGER :: NITER    !number of iterations
    DATA NITER /3/
    SAVE NITER

    REAL :: RLB         !boundary layer resistance (s m2 / umol)
    REAL :: AB          !used in statement functions
    REAL :: BC          !used in statement functions
    REAL :: TC          !foliage temperature (degree Celsius)
    REAL :: CS          !co2 concentration at leaf surface (pa)
    REAL :: KC          !co2 Michaelis-Menten constant (pa)
    REAL :: KO          !o2 Michaelis-Menten constant (pa)
    REAL :: A,B,C,Q     !intermediate calculations for RS
    REAL :: R1,R2       !roots for RS
    REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
    REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
    REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
    REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
    REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
    REAL :: CP          !co2 compensation point (pa)
    REAL :: CI          !internal co2 (pa)
    REAL :: AWC         !intermediate calculation for wc
    REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
    REAL :: J           !electron transport (umol co2/m2/s)
    REAL :: CEA         !constrain ea or else model blows up
    REAL :: CF          !s m2/umol -> s/m
    REAL :: T
    ! ---------------------------------------------------------------------------------------------

    ! initialize RS=RSMAX and PSN=0 because will only do calculations
    ! for APAR > 0, in which case RS <= RSMAX and PSN >= 0
    CF = SFCPRS/(8.314*SFCTMP)*1.e06
    RS = 1./parameters%BP * CF
    PSN = 0.

    IF (APAR .LE. 0.) RETURN

    FNF = MIN( FOLN/MAX(parameters%MPE,parameters%FOLNMX), 1.0 )
    TC  = TV-parameters%TFRZ
    PPF = 4.6*APAR
    J   = PPF*parameters%QE25
    KC  = parameters%KC25 * F1(parameters%AKC, TC)
    KO  = parameters%KO25 * F1(parameters%AKO, TC)
    AWC = KC * (1.+O2/KO)
    CP  = 0.5*KC/KO*O2*0.21
    VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX, TC)

    ! first guess ci
    CI = 0.7*CO2*parameters%C3PSN + 0.4*CO2*(1.-parameters%C3PSN)

    ! rb: s/m -> s m**2 / umol
    RLB = RB/CF

    ! constrain ea
    CEA = MAX(0.25*EI*parameters%C3PSN+0.40*EI*(1.-parameters%C3PSN), MIN(EA,EI) )

    ! ci iteration
    !jref: C3PSN is equal to 1 for all veg types.
    DO ITER = 1, NITER
      WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*parameters%C3PSN  + J*(1.-parameters%C3PSN)
      WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*parameters%C3PSN + VCMX*(1.-parameters%C3PSN)
      WE = 0.5*VCMX*parameters%C3PSN + 4000.*VCMX*CI/SFCPRS*(1.-parameters%C3PSN)
      PSN = MIN(WJ,WC,WE) * IGS

      CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, parameters%MPE )
      A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
      B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.
      C = -RLB
      IF (B .GE. 0.) THEN
        Q = -0.5*( B + SQRT(B*B-4.*A*C) )
      ELSE
        Q = -0.5*( B - SQRT(B*B-4.*A*C) )
      END IF
      R1 = Q/A
      R2 = C/Q
      RS = MAX(R1,R2)
      CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
    END DO 

    ! rs, rb:  s m**2 / umol -> s/m
    RS = RS*CF

  END SUBROUTINE STOMATA

  ! == begin canres ===================================================================================
  SUBROUTINE CANRES (parameters, PAR, TV, BTRAN ,EAH, SFCPRS, & ! in
                        RS , PSN )                              ! out
    ! --------------------------------------------------------------------------------------------------
    ! calculate canopy resistance which depends on incoming solar radiation,
    ! air temperature, atmospheric water vapor pressure deficit at the
    ! lowest model level, and soil moisture (preferably unfrozen soil
    ! moisture rather than total)
    ! --------------------------------------------------------------------------------------------------
    ! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), Jacquemin and
    ! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
    ! eqns 12-14 and table 2 of sec. 3.1.2
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type (parameters_type), intent(in)    :: parameters
    REAL,                     INTENT(IN)  :: PAR    ! par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: TV     ! canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS ! surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    ! water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: BTRAN ! soil moisture stress factor (orig RCSOIL in this subr.)

    ! outputs
    REAL,                     INTENT(OUT) :: RS     ! canopy resistance per unit LAI (formerly RC)
    REAL,                     INTENT(OUT) :: PSN    ! foliage photosynthesis (umolco2/m2/s)

    ! local params
    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     ! water vapor mixing ratio (kg/kg)
    REAL                                  :: Q2SAT  ! saturation Q2
    REAL                                  :: DQSDT2 ! d(Q2SAT)/d(T)  (calculated but not used)

    ! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set in REDPRM
    ! ----------------------------------------------------------------------
    ! initialize canopy resistance multiplier terms.
    ! ----------------------------------------------------------------------
    RS     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

    !  compute Q2 and Q2SAT
    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixing ratio [kg/kg]

    !CALL CALHUM(parameters, SFCTMP, SFCPRS, Q2SAT, DQSDT2) orig
    CALL CALHUM(TV, SFCPRS, Q2SAT, DQSDT2)

    ! contribution due to incoming solar radiation
    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

    ! contribution due to air temperature
    RCT = 1.0- 0.0016* ( (parameters%TOPT - TV)**2.0)
    RCT = MAX (RCT,0.0001)

    ! contribution due to vapor pressure deficit
    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0., Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

    ! determine canopy resistance due to all factors
    RS  = parameters%RSMIN / (RCS * RCT * RCQ * BTRAN)   ! note BTRAN was originally RCSOIL
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES


  ! == begin calhum ===================================================================================
  SUBROUTINE CALHUM (TV, SFCPRS, Q2SAT, DQSDT2)
    ! --------------------------------------------------------------------------------------------------
    ! calculate saturated mixing ratio and its derivative with temperature
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    !  type (noahmp_parameters), intent(in) :: parameters   (not used)
    REAL, INTENT(IN)       :: TV        ! canopy air temperature
    REAL, INTENT(IN)       :: SFCPRS    ! surface temperature and pressure (from forcing%SFCTMP, forcing%SFCPRS)
    REAL, INTENT(OUT)      :: Q2SAT     ! saturated mixing ratio (g/g)
    REAL, INTENT(OUT)      :: DQSDT2    ! derivative of Q2SAT with Temperature (is not used anywhere in NOAHMP)
    
    ! local
    REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                              A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                              EPSILON=0.622
    REAL                   :: ES, SFCPRSX

    ! Q2SAT: saturated mixing ratio
    ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./TV) )
    ! convert SFCPRS from Pa to KPa
    SFCPRSX = SFCPRS*1.E-3
    Q2SAT = EPSILON * ES / (SFCPRSX-ES)
    
    ! AW: replace the following (mult/div inline instead)
    ! convert from  g/g to g/kg
    !Q2SAT = Q2SAT * 1.E3         ! note: Q2SAT is currently a 'mixing ratio'
    ! DQSDT2 is calculated assuming Q2SAT is a specific humidity
    !DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(TV-A4)**2
    ! DG Q2SAT needs to be in g/g when returned for SFLX
    !Q2SAT = Q2SAT / 1.E3
    
    ! DQSDT2 is calculated using Q2SAT converted to specific humidity (g/kg) from mix. ratio (g/g)
    DQSDT2 = ((Q2SAT*1.E3)/(1+(Q2SAT*1.E3))) * A23M4/(TV-A4)**2

  END SUBROUTINE CALHUM
  
  
  ! == begin sfcdif1 ==================================================================================
  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
    ! -------------------------------------------------------------------------------------------------
    ! computing surface drag coefficient CM for momentum and CH for heat
    ! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! inputs
    type (parameters_type), intent(in) :: parameters
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)
    ! in & out
    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters
    ! outputs
    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

    ! locals
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
    ! -------------------------------------------------------------------------------------------------

    ! Monin-Obukhov stability parameter moz for next iteration
    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       !call wrf_error_fatal("STOP in Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = parameters%VKC * (parameters%GRAV/TVIR) * H/(RHOAIR*parameters%CPAIR)
       IF (ABS(TMP1) .LE. parameters%MPE) TMP1 = parameters%MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF

    ! accumulate number of times moz changes sign.
    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF

    ! evaluate stability-dependent variables using moz from prior iteration
    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2

       ! 2-meter
       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF

    ! except for first iteration, weight stability factors for previous
    ! iteration to help avoid flip-flops from one iteration to the next
    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF

    ! exchange coefficients
    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= parameters%MPE) CMFM = parameters%MPE
    IF(ABS(CHFH) <= parameters%MPE) CHFH = parameters%MPE
    IF(ABS(CM2FM2) <= parameters%MPE) CM2FM2 = parameters%MPE
    IF(ABS(CH2FH2) <= parameters%MPE) CH2FH2 = parameters%MPE
    CM  = parameters%VKC*parameters%VKC/(CMFM*CMFM)
    CH  = parameters%VKC*parameters%VKC/(CMFM*CHFH)
    CH2  = parameters%VKC*parameters%VKC/(CM2FM2*CH2FH2)
        
    ! friction velocity
    FV = UR * SQRT(CM)
    CH2  = parameters%VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1


  ! == begin sfcdif2 ==================================================================================
  SUBROUTINE SFCDIF2(parameters, ITER, Z0, THZ0, THLM, SFCSPD, ZLM, & ! in
                     AKMS, AKHS, RLMO, WSTAR2,                      & ! inout
                     USTAR  )                                         ! out
    ! -------------------------------------------------------------------------------------------------
    ! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
    ! -------------------------------------------------------------------------------------------------
    ! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
    ! SEE CHEN ET AL (1997, BLM)
    ! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    ! in, inout, out
    type (parameters_type), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR

    ! local
    REAL     ZZ !,PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, YY !, PSPMU, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN, RLMA
    REAL     BTG, ELFC    ! BTG orig a const. parameter
    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    !REAL,    PARAMETER :: BTG    = BETA * GRAV  ! can't assign of GRAV not a constant
    !REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )
    
    BTG  = BETA * parameters%GRAV  ! BTG orig. a const. parameter
    ELFC = VKRM * BTG              ! ELFC orig. a const. parameter
  
    ! ----------------------------------------------------------------------
    ! NOTE: a lot of f77 statement functions moved to end of module
    ! ----------------------------------------------------------------------

    ! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
    ! OVER SOLID SURFACE (LAND, SEA-ICE).
    ! ----------------------------------------------------------------------
    !     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
    !     C......ZTFC=0.1
    !     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
    ! ----------------------------------------------------------------------
    ILECH = 0

    ! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0

    ! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
    ! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)

    ! ----------------------------------------------------------------------
    ! 1./MONIN-OBUKKHOV LENGTH-SCALE
    ! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU, PIHF)
          SIMM = PSPMU (XLU, PIHF) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
    ! ----------------------------------------------------------------------
    ! LECH'S FUNCTIONS
    ! ----------------------------------------------------------------------
    ELSE
      IF (RLMO .lt. 0.)THEN
        PSMZ = PSLMU (ZETAU)
        SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
        PSHZ = PSLHU (ZETAT)
        SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
      ELSE
        ZETALU = MIN (ZETALU,ZTMAX)
        ZETALT = MIN (ZETALT,ZTMAX)
        PSMZ = PSLMS (ZETAU, RRIC)
        SIMM = PSLMS (ZETALU, RRIC) - PSMZ + RLOGU
        PSHZ = PSLHS (ZETAT, RFAC)
        SIMH = PSLHS (ZETALT, RFAC) - PSHZ + RLOGT
      END IF

    END IF

    ! ----------------------------------------------------------------------
    ! BELJAARS CORRECTION FOR USTAR
    ! ----------------------------------------------------------------------
    USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

    ! ZILITINKEVITCH FIX FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLT = ZLM + ZT
    !-----------------------------------------------------------------------
    RLOGT = log (ZSLT / ZT)
    USTARK = USTAR * VKRM
    IF(SIMM < 1.e-6) SIMM = 1.e-6        ! Limit stability function
    AKMS = MAX (USTARK / SIMM,CXCH)
    !-----------------------------------------------------------------------
    ! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
    !-----------------------------------------------------------------------
    IF(SIMH < 1.e-6) SIMH = 1.e-6        ! Limit stability function
    AKHS = MAX (USTARK / SIMH,CXCH)

    IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
      WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
    ELSE
      WSTAR2 = 0.0
    END IF
    !-----------------------------------------------------------------------
    RLMN = ELFC * AKHS * DTHV / USTAR **3
    !-----------------------------------------------------------------------
    !     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
    !-----------------------------------------------------------------------
    RLMA = RLMO * WOLD+ RLMN * WNEW
    !-----------------------------------------------------------------------
    RLMO = RLMA

  END SUBROUTINE SFCDIF2

  
  ! == begin ESAT =====================================================================================
  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
    !---------------------------------------------------------------------------------------------------
    ! use polynomials to calculate saturation vapor pressure and derivative with
    ! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
    !---------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! in, out
    REAL, intent(in)  :: T              ! temperature
    REAL, intent(out) :: ESW            ! saturation vapor pressure over water (pa)
    REAL, intent(out) :: ESI            ! saturation vapor pressure over ice (pa)
    REAL, intent(out) :: DESW           ! d(esat)/dt over water (pa/K)
    REAL, intent(out) :: DESI           ! d(esat)/dt over ice (pa/K)

    ! local
    REAL :: A0,A1,A2,A3,A4,A5,A6        ! coefficients for esat over water
    REAL :: B0,B1,B2,B3,B4,B5,B6        ! coefficients for esat over ice
    REAL :: C0,C1,C2,C3,C4,C5,C6        ! coefficients for dsat over water
    REAL :: D0,D1,D2,D3,D4,D5,D6        ! coefficients for dsat over ice

    PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
               A2=1.428945805E-02, A3=2.650648471E-04,  &
               A4=3.031240396E-06, A5=2.034080948E-08,  &
               A6=6.136820929E-11)

    PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
               B2=1.886013408E-02, B3=4.176223716E-04,  &
               B4=5.824720280E-06, B5=4.838803174E-08,  &
               B6=1.838826904E-10)

    PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
               C2= 7.938054040E-04, C3=1.215215065E-05,  &
               C4= 1.036561403E-07, C5=3.532421810e-10,  &
               C6=-7.090244804E-13)

    PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
               D2=1.267995369E-03, D3=2.477563108E-05,  &
               D4=3.005693132E-07, D5=2.158542548E-09,  &
               D6=7.131097725E-12)

    ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
    ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
    DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
    DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT 
  
  ! f77-style statement function pulled out of vege_flux and bare_flux
  function TDC(T, TFRZ)
    real, intent(in)     :: T, TFRZ
    real                 :: TDC
    TDC = MIN( 50., MAX(-50.,(T - TFRZ)) )
    return
  end function TDC
  
  ! generic temperature response (statement function)
  function F1(AB,BC)
    real, intent(in)     :: AB, BC
    real                 :: F1
    F1 = AB**((BC-25.)/10.)
  end function 
  
  ! generic temperature inhibition (statement function)
  function F2(AB)
    real, intent(in)     :: AB
    real                 :: F2   
    F2 = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
    return
  end function 
  
  ! ----------------------------------------------------------------------
  ! DEFINE FUNCTIONS FOR SFCDIF2
  ! ----------------------------------------------------------------------
  ! LECH'S SURFACE FUNCTIONS
  function PSLMU (ZZ)
    real, intent(in)     :: ZZ
    real                 :: PSLMU
    PSLMU = -0.96* log (1.0-4.5* ZZ)
    return
  end function
  function PSLMS (ZZ, RRIC)
    real, intent(in)     :: ZZ
    real, intent(in)     :: RRIC
    real                 :: PSLMS
    PSLMS = ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    return
  end function
  function PSLHU (ZZ)
    real, intent(in)     :: ZZ
    real                 :: PSLHU
    PSLHU = -0.96* log (1.0-4.5* ZZ)
    return
  end function
  function PSLHS (ZZ, RFAC)
    real, intent(in)     :: ZZ
    real, intent(in)     :: RFAC
    real                 :: PSLHS
    PSLHS = ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
    return
  end function
  ! PAULSON'S SURFACE FUNCTIONS
  function PSPMU (XX, PIHF)
    real, intent(in)     :: XX
    real, intent(in)     :: PIHF
    real                 :: PSPMU
    PSPMU = -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)+2.* ATAN (XX) - PIHF     
    return
  end function
  function PSPMS (YY)
    real, intent(in)     :: YY
    real                 :: PSPMS
    PSPMS = 5.* YY
    return
  end function
  function PSPHU (XX)
    real, intent(in)     :: XX
    real                 :: PSPHU
    PSPHU = -2.* log ( (XX * XX +1.)*0.5)
    return
  end function
  function PSPHS (YY)
    real, intent(in)     :: YY
    real                 :: PSPHS
    PSPHS = 5.* YY
    return
  end function
  
end module EtFluxModule
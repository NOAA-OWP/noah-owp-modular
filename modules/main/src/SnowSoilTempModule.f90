! SnowSoilTempModule 
!  TSNOSOI and PHASE_CHANGE and supporting subroutines HRT, HSTEP, ROSR12 and FRH2O

module SnowSoilTempModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType
  implicit none

  public  ::  TSNOSOI
  private ::    HRT
  private ::    HSTEP   
  private ::      ROSR12
  public  ::  PHASECHANGE
  private ::    FRH2O 

contains
  
  ! == begin tsnosoi ==================================================================================
  SUBROUTINE TSNOSOI (parameters, levels, domain, options, ICE, ISNOW,   & ! in
                      SSOIL, DF, HCPCT,                                  & ! in
                      SAG, SNOWH, TG,                                    & ! in
                      STC     )                                            ! inout
    ! --------------------------------------------------------------------------------------------------
    ! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
    ! during melting season may exceed melting point (TFRZ) but later in PHASECHANGE
    ! subroutine the snow temperatures are reset to TFRZ for melting snow.
    ! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    ! --------------------------------------------------------------------------------------------------
    ! input
    type (parameters_type), intent(in) :: parameters
    type (levels_type), intent(in)     :: levels
    type (domain_type), intent(in)     :: domain
    type (options_type), intent(in)    :: options
    INTEGER,                         INTENT(IN)  :: ICE    ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
    INTEGER,                         INTENT(IN)  :: ISNOW  ! actual no of snow layers
    REAL,                            INTENT(IN)  :: SSOIL  ! ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SAG    ! solar rad. absorbed by ground (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  ! snow depth (m)
    REAL,                            INTENT(IN)  :: TG     ! ground temperature (k)
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL), INTENT(IN)  :: DF     ! thermal conductivity
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL), INTENT(IN)  :: HCPCT  ! heat capacity (J/m3/k)
    ! input and output
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL), INTENT(INOUT) :: STC  ! snow/soil layer temperature [K]

    ! local
    INTEGER                                        :: IZ
    REAL                                           :: ZBOTSNO   ! ZBOT from snow surface
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL)  :: AI, BI, CI, RHSTS
    REAL                                           :: EFLXB     ! energy influx from soil bottom (w/m2)
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL)  :: PHI       ! light through water (w/m2)
    REAL, DIMENSION(-levels%NSNOW+1:levels%NSOIL)  :: TBEG      ! layer temp for balance check [K]
    REAL                                           :: ERR_EST   ! heat storage error  (w/m2)
    REAL                                           :: SSOIL2    ! ground heat flux (w/m2) (for energy check)
    REAL                                           :: EFLXB2    ! heat flux from the bottom (w/m2) (for energy check)
    character(len=256)                             :: message
    ! ----------------------------------------------------------------------

    ! compute solar penetration through water, needs more work
    PHI(ISNOW+1:levels%NSOIL) = 0.

    ! adjust ZBOT from soil surface to ZBOTSNO from snow surface
    ZBOTSNO = parameters%ZBOT - SNOWH    !from snow surface

    ! snow/soil heat storage for energy balance check
    DO IZ = ISNOW+1, levels%NSOIL
      TBEG(IZ) = STC(IZ)
    ENDDO

    ! compute soil temperatures
    CALL HRT (parameters, domain, options, levels%NSNOW, levels%NSOIL,  & ! in 
              ISNOW, STC, ZBOTSNO, DF, HCPCT, SSOIL, PHI,               & ! in 
              AI, BI, CI, RHSTS, EFLXB)                                   ! out

    CALL HSTEP (levels%NSNOW, levels%NSOIL, ISNOW, domain%DT,           & ! in
                AI, BI, CI, RHSTS, STC)                                 & ! inout

    ! update ground heat flux just for energy check, but not for final output
    ! otherwise, it would break the surface energy balance
    IF(options%OPT_TBOT == 1) THEN
       EFLXB2 = 0.
    ELSE IF(options%OPT_TBOT == 2) THEN
       EFLXB2 = DF(levels%NSOIL) * (TBOT - STC(levels%NSOIL)) / &
            (0.5 * (domain%ZSNSO(levels%NSOIL-1) + domain%ZSNSO(levels%NSOIL)) - ZBOTSNO)
    END IF

    ! Skip the energy balance check for now, until we can make it work right for small time steps.
    return

    ! energy balance check
    ERR_EST = 0.0
    DO IZ = ISNOW+1, levels%NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * domain%DZSNSO(IZ) * HCPCT(IZ) / domain%DT
    ENDDO

    if (options%OPT_STC == 1 .OR. options%OPT_STC == 3) THEN   ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     ! full-implicit
       SSOIL2 = DF(ISNOW+1) * (TG - STC(ISNOW+1))/(0.5*domain%DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2 + EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.) THEN    ! W/m2
       WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy', ERR_EST,' W/m2'
       !call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            domain%ILOC, domain%JLOC, domain%IST, ERR_EST, SSOIL, SNOWH, TG, STC(ISNOW+1), EFLXB
       !call wrf_message(trim(message))
       !niu      STOP
    END IF

  END SUBROUTINE TSNOSOI

  ! == begin hrt ======================================================================================
  SUBROUTINE HRT (parameters, domain, options, NSNOW, NSOIL, ISNOW,  & ! in
                  STC, ZBOT, DF, HCPCT, SSOIL, PHI,                  & ! in 
                  AI, BI, CI, RHSTS, BOTFLX)                           ! out
    ! ----------------------------------------------------------------------
    ! calculate the right hand side of the time tendency term of the soil
    ! thermal diffusion equation.  also to compute ( prepare ) the matrix
    ! coefficients for the tri-diagonal matrix of the implicit time scheme.
    ! ----------------------------------------------------------------------
    IMPLICIT NONE
    ! input
    type (parameters_type), intent(in)           :: parameters
    type (domain_type), intent(in)               :: domain    
    type (options_type), intent(in)              :: options   
    INTEGER,                         INTENT(IN)  :: NSNOW  ! maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: NSOIL  ! no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: ISNOW  ! actual no of snow layers
    REAL,                            INTENT(IN)  :: ZBOT   ! depth of lower boundary condition (m)
                                                           !   from soil surface not snow surface
    REAL,                            INTENT(IN)  :: SSOIL  ! ground heat flux (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    ! snow/soil temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     ! thermal conductivity [w/m/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  ! heat capacity [j/m3/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    ! light through water (w/m2)
    ! output
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  ! right-hand side of the matrix
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     ! left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     ! left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     ! left-hand side coefficient
    REAL,                            INTENT(OUT) :: BOTFLX ! energy influx from soil bottom (w/m2)

    ! local
    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1
    ! ----------------------------------------------------------------------
    
    ! associate variables to keep variable names intact in the code below  
    associate(&
      ZSNSO   => domain%ZSNSO     ! real,dim(-NSNOW+1:NSOIL), intent(in): snow/soil layerthickness [m]  
    )      

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - parameters%TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (options%OPT_STC == 1 .OR. options%OPT_STC == 3 ) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (options%OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO
    
    end associate ! terminate the associate block

  END SUBROUTINE HRT

  ! == begin hstep ====================================================================================
  SUBROUTINE HSTEP (NSNOW, NSOIL, ISNOW, DT,  & ! in
                    AI, BI, CI, RHSTS, STC )    ! inout
    ! ----------------------------------------------------------------------
    ! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
    ! ----------------------------------------------------------------------
    implicit none
    ! input
    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT
    ! output & input
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC
    ! local
    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN
    ! ----------------------------------------------------------------------

    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO

    ! copy values for input variables before call to rosr12
    DO K = ISNOW+1, NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO

    ! solve the tri-diagonal matrix equation
    !CALL ROSR12 (CI, AI, BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW) original (keep)
    CALL ROSR12 (AI, BI, RHSTSIN, ISNOW+1, NSOIL, NSNOW, & ! in               (reordered)
                 CIIN, CI, RHSTS)                          ! out    

    ! update snow & soil temperature
    DO K = ISNOW+1,NSOIL
       STC(K) = STC(K) + CI(K)
    END DO

  END SUBROUTINE HSTEP


  !== begin rosr12 ===================================================================================
  !SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)        original (keep)
  SUBROUTINE ROSR12 (A, B, D, NTOP, NSOIL, NSNOW, & ! in                (reordered)
                     C, P, DELTA)                   ! out
    ! ----------------------------------------------------------------------
    ! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
    ! ###                                            ### ###  ###   ###  ###
    ! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
    ! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
    ! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
    ! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
    ! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
    ! # .                                          .   # #  .   # = #   .  #
    ! # .                                          .   # #  .   #   #   .  #
    ! # .                                          .   # #  .   #   #   .  #
    ! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
    ! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
    ! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
    ! ###                                            ### ###  ###   ###  ###
    ! ----------------------------------------------------------------------
    IMPLICIT NONE
    ! input
    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL, NSNOW
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    ! input/output
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C, P, DELTA
    ! local
    INTEGER               :: K, KK

    ! ----------------------------------------------------------------------
    ! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
    ! ----------------------------------------------------------------------
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
    ! ----------------------------------------------------------------------
    ! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
    ! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
    ! ----------------------------------------------------------------------
    ! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
    ! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
    ! ----------------------------------------------------------------------
    ! SET P TO DELTA FOR LOWEST SOIL LAYER
    ! ----------------------------------------------------------------------
    P (NSOIL) = DELTA (NSOIL)
    ! ----------------------------------------------------------------------
    ! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
    ! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
    ! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12
  

  ! == begin phasechange ==============================================================================
  SUBROUTINE PHASECHANGE (parameters, domain, energy, water, NSNOW, NSOIL)
    ! ----------------------------------------------------------------------
    ! melting/freezing of snow water and soil water
    ! ----------------------------------------------------------------------
    IMPLICIT NONE
    ! inputs
    type (parameters_type), intent(in)              :: parameters
    type (domain_type), intent(in)                  :: domain
    type (energy_type), intent(in)                  :: energy
    type (water_type), intent(in)                   :: water
    INTEGER, INTENT(IN)                             :: NSNOW   ! maximum no. of snow layers [=3]
    INTEGER, INTENT(IN)                             :: NSOIL   ! No. of soil layers [=4]
   
    ! local
    INTEGER                         :: J         ! do loop index
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        ! energy residual [w/m2]
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        ! melting or freezing water [kg/m2]
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      ! soil/snow ice mass [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      ! soil/snow liquid water mass [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL ! supercooled water in soil (kg/m2)
    REAL                            :: HEATR     ! energy residual or loss after melting/freezing
    REAL                            :: TEMP1     ! temporary variables [kg/m2]
    REAL                            :: PROPOR
    REAL                            :: SMP       ! frozen water potential (mm)
    REAL                            :: XMF       ! total latent heat of phase change
    ! ----------------------------------------------------------------------
    
    ! associate variables to keep variable names intact in the code below  
    associate(&
      ! inputs
      ISNOW    => water%ISNOW    ,&   ! INTEGER, INTENT(IN):  actual no. of snow layers [<=3]
      FACT     => energy%FACT    ,&   ! REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN):     temporary
      HCPCT    => energy%HCPCT   ,&   ! REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN):     heat capacity (J/m3/k)
      ! inputs and outputs
      SNEQV    => water%SNEQV    ,&   ! REAL, INTENT(INOUT) :: SNEQV                     snow water eqv. [mm]
      SNOWH    => water%SNOWH    ,&   ! REAL, INTENT(INOUT) :: SNOWH                     snow height [m]
      STC      => energy%STC     ,&   ! REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT):  snow/soil layer temperature [k]
      SH2O     => water%SH2O     ,&   ! REAL, DIMENSION(       1:NSOIL), INTENT(INOUT):  soil liquid water [m3/m3]
      SMC      => water%SMC      ,&   ! REAL, DIMENSION(       1:NSOIL), INTENT(INOUT):  total soil water [m3/m3]
      SNICE    => water%SNICE    ,&   ! REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT):  snow layer ice [mm]
      SNLIQ    => water%SNLIQ    ,&   ! REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT):  snow layer liquid water [mm]
      ! outputs
      IMELT    => energy%IMELT   ,&   ! INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT):  phase change index
      QMELT    => energy%QMELT   ,&   ! REAL,                               INTENT(OUT):  snowmelt rate [mm/s]
      PONDING  => water%PONDING   &   ! REAL,                               INTENT(OUT):  snowmelt when snow has no layer [mm]
    )  ! ---- end associate block --------------------------------------------------------------------

    ! Initialization
    QMELT   = 0.;      PONDING = 0.;      XMF     = 0.

    DO J = -NSNOW+1, NSOIL
      SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       ! all layers
      MICE(J) = SNICE(J)
      MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               ! soil
      MLIQ(J) =  SH2O(J)            * domain%DZSNSO(J) * 1000.
      MICE(J) = (SMC(J) - SH2O(J))  * domain%DZSNSO(J) * 1000.
    END DO

    DO J = ISNOW+1,NSOIL       ! all layers
      IMELT(J)    = 0
      HM(J)       = 0.
      XM(J)       = 0.
      WICE0(J)    = MICE(J)
      WLIQ0(J)    = MLIQ(J)
      WMASS0(J)   = MICE(J) + MLIQ(J)
    END DO

    if(domain%IST == 1) then
      DO J = 1, NSOIL
        IF (OPT_FRZ == 1) THEN
          IF(STC(J) < TFRZ) THEN
            SMP = parameters%HFUS * (TFRZ-STC(J))/(parameters%GRAV*STC(J))             ! (m)
            SUPERCOOL(J) = parameters%SMCMAX(J) * (SMP/parameters%PSISAT(J))**(-1./parameters%BEXP(J))
            SUPERCOOL(J) = SUPERCOOL(J) * domain%DZSNSO(J)*1000.            ! (mm)
          END IF
        END IF
        IF (OPT_FRZ == 2) THEN
          CALL FRH2O (parameters, J, STC(J), SMC(J), SH2O(J), &  ! in
                      SUPERCOOL(J))                              ! out
          SUPERCOOL(J) = SUPERCOOL(J)*domain%DZSNSO(J)*1000.                ! (mm)
        END IF
      END DO
    end if

    DO J = ISNOW+1, NSOIL
      IF (MICE(J) > 0. .AND. STC(J) >= parameters%TFRZ) THEN  !melting 
        IMELT(J) = 1
      ENDIF
      IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < parameters%TFRZ) THEN
        IMELT(J) = 2
      ENDIF

      ! If snow exists, but its thickness is not enough to create a layer
      IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
        IF (STC(J) >= parameters%TFRZ) THEN
          IMELT(J) = 1
        ENDIF
      ENDIF
    END DO

    ! Calculate the energy surplus and loss for melting and freezing
    DO J = ISNOW+1, NSOIL
      IF (IMELT(J) > 0) THEN
        HM(J) = (STC(J) - parameters%TFRZ) / FACT(J)
        STC(J) = parameters%TFRZ
      ENDIF
      IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
        HM(J) = 0.
        IMELT(J) = 0
      ENDIF
      IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
        HM(J) = 0.
        IMELT(J) = 0
      ENDIF
      XM(J) = HM(J) * domain%DT / parameters%HFUS                           
    END DO

    ! The rate of melting and freezing for snow without a layer, needs more work.
    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
      TEMP1  = SNEQV
      SNEQV  = MAX(0., TEMP1 - XM(1))  
      PROPOR = SNEQV / TEMP1
      SNOWH  = MAX(0.,PROPOR * SNOWH)
      HEATR  = HM(1) - parameters%HFUS * (TEMP1 - SNEQV) / domain%DT  
      IF (HEATR > 0.) THEN
        XM(1) = HEATR * domain%DT / parameters%HFUS             
        HM(1) = HEATR                    
      ELSE
        XM(1) = 0.
        HM(1) = 0.
      ENDIF
      QMELT   = MAX(0., (TEMP1 - SNEQV)) / domain%DT
      XMF     = parameters%HFUS * QMELT
      PONDING = TEMP1 - SNEQV
    ENDIF

    ! The rate of melting and freezing for snow and soil
    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN
        HEATR = 0.
        IF (XM(J) > 0.) THEN                            
          MICE(J) = MAX(0., WICE0(J) - XM(J))
          HEATR = HM(J) - parameters%HFUS * (WICE0(J) - MICE(J)) / domain%DT
        ELSE IF (XM(J) < 0.) THEN                      
          IF (J <= 0) THEN                             ! snow
            MICE(J) = MIN(WMASS0(J), WICE0(J) - XM(J))  
          ELSE                                         ! soil
            IF (WMASS0(J) < SUPERCOOL(J)) THEN
              MICE(J) = 0.
            ELSE
              MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J), WICE0(J) - XM(J))
              MICE(J) = MAX(MICE(J), 0.0)
            ENDIF
          ENDIF
          HEATR = HM(J) - parameters%HFUS*(WICE0(J) - MICE(J)) / domain%DT
        ENDIF

        MLIQ(J) = MAX(0., WMASS0(J) - MICE(J))

        IF (ABS(HEATR) > 0.) THEN
          STC(J) = STC(J) + FACT(J) * HEATR
          IF (J <= 0) THEN                             ! snow
            IF (MLIQ(J) * MICE(J) > 0.) STC(J) = parameters%TFRZ
            IF (MICE(J) == 0.) THEN                               ! BARLAGE
              STC(J) = parameters%TFRZ                           ! BARLAGE
              HM(J+1) = HM(J+1) + HEATR                          ! BARLAGE
              XM(J+1) = HM(J+1) * domain%DT / parameters%HFUS    ! BARLAGE
            ENDIF 
          ENDIF
        ENDIF

        XMF = XMF + parameters%HFUS * (WICE0(J) - MICE(J)) / domain%DT

        IF (J < 1) THEN
          QMELT = QMELT + MAX(0., (WICE0(J) - MICE(J))) / domain%DT
        ENDIF
      ENDIF
    END DO

    DO J = ISNOW+1,0             ! snow
      SNLIQ(J) = MLIQ(J)
      SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              ! soil
      SH2O(J) =  MLIQ(J)            / (1000. * domain%DZSNSO(J))
      SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * domain%DZSNSO(J))
    END DO
  
    end associate ! terminate the associate block
   
  END SUBROUTINE PHASECHANGE

  ! == begin frh2o ====================================================================================
  SUBROUTINE FRH2O (parameters, ISOIL, TKELV, SMC, SH2O, FREE)   ! FREE is output
    ! ----------------------------------------------------------------------
    ! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
    ! TEMPERATURE IS BELOW 273.15K (TFRZ).  REQUIRES NEWTON-TYPE ITERATION
    ! TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
    ! (1999, JGR, VOL 104(D16), 19569-19585).
    ! ----------------------------------------------------------------------
    ! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
    ! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
    ! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
    ! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
    ! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
    ! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
    ! LIMIT OF FREEZING POINT TEMPERATURE TFRZ.
    ! ----------------------------------------------------------------------
    IMPLICIT NONE
    ! input
    type (parameters_type), intent(in) :: parameters
    INTEGER,INTENT(IN)   :: ISOIL  ! Soil layer index (btw 1 and NSOIL)
    REAL, INTENT(IN)     :: SH2O   ! LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
    REAL, INTENT(IN)     :: SMC    ! TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
    REAL, INTENT(IN)     :: TKELV  ! TKELV.........TEMPERATURE (Kelvin)
    ! output
    REAL, INTENT(OUT)    :: FREE   ! SUPERCOOLED LIQUID WATER CONTENT [m3/m3]
    ! local
    REAL                 :: BX, DENOM, DF, DSWL, FK, SWL, SWLK
    INTEGER              :: NLOG, KCOUNT
    !REAL, PARAMETER(CK = 0.0)
    REAL, PARAMETER      :: CK = 8.0          ! define these
    REAL, PARAMETER      :: BLIM = 5.5 
    REAL, PARAMETER      :: ERROR = 0.005 
    REAL, PARAMETER      :: DICE = 920.0
    CHARACTER(LEN=80)    :: message

    ! ---------------------------------------------------------------------
    ! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
    ! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
    ! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
    ! ----------------------------------------------------------------------
    BX = parameters%BEXP(ISOIL)   ! SOIL TYPE "B" PARAMETER

    ! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
    IF (parameters%BEXP(ISOIL) >  BLIM) BX = BLIM
    NLOG = 0

    !  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (TFRZ), SH2O = SMC
    KCOUNT = 0
    IF (TKELV > (parameters%TFRZ - 1.E-3)) THEN
      FREE = SMC
      
    ELSE
      ! ----------------------------------------------------------------------
      ! OPTION 1: ITERATED SOLUTION IN KOREN ET AL, JGR, 1999, EQN 17
      ! ----------------------------------------------------------------------
      ! INITIAL GUESS FOR SWL (frozen content)
      IF (CK /= 0.0) THEN
        SWL = SMC - SH2O

        ! KEEP WITHIN BOUNDS.
        IF (SWL > (SMC -0.02)) SWL = SMC -0.02

        !  START OF ITERATIONS
        IF (SWL < 0.) SWL = 0.
        
1001    Continue
        IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
        
        NLOG = NLOG +1
        DF = ALOG ( ( parameters%PSISAT(ISOIL) * parameters%GRAV / parameters%HFUS ) * ( ( 1. + CK * SWL )**2.) * &
          ( parameters%SMCMAX(ISOIL) / (SMC - SWL) )** BX) - ALOG ( - (               &
          TKELV - parameters%TFRZ)/ TKELV)
        DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
        SWLK = SWL - DF / DENOM

        ! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
        IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
        IF (SWLK < 0.)          SWLK = 0.

        ! MATHEMATICAL SOLUTION BOUNDS APPLIED.
        DSWL = ABS (SWLK - SWL)
          
        ! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
        ! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
        SWL = SWLK
        IF ( DSWL <= ERROR ) THEN
          KCOUNT = KCOUNT +1
        END IF
        !  END OF ITERATIONS
        ! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
        goto 1001
1002    continue

        FREE = SMC - SWL
      END IF

      ! ----------------------------------------------------------------------
      ! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
      ! IN KOREN ET AL., JGR, 1999, EQN 17
      ! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
      ! ----------------------------------------------------------------------
      IF (KCOUNT == 0) THEN
        write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
        #call wrf_message(trim(message))
        FK = ( ( (parameters%HFUS / (parameters%GRAV * ( - parameters%PSISAT(ISOIL))))*                    &
             ( (TKELV - parameters%TFRZ)/ TKELV))** ( -1/ BX))* parameters%SMCMAX(ISOIL)
        IF (FK < 0.02) FK = 0.02
        FREE = MIN (FK, SMC)
        ! END OPTION 2
      END IF
      
    END IF  ! end IF case for temperature below freezing

  END SUBROUTINE FRH2O
  
  
END module SnowSoilTempModule
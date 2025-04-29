module SurfaceRunoffInfiltration

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use SoilWaterRetentionCoeff
  
  implicit none
  
contains

  SUBROUTINE INFIL (parameters, domain, levels, water)
! Originally called "INFIL" subroutine for free drainage
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (parameters_type), intent(in) :: parameters
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (     water_type)             :: water
! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR1
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND1
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL                     :: dt     !time step (sec)
  REAL, DIMENSION(1:levels%nsoil) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    dt = water%runsrf_dt

    IF (water%QINSUR >  0.0) THEN
       DT1 = dt /86400.
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum infiltration rate
       DMAX(1)= -domain%zsoil(1) * SMCAV
       DICE   = -domain%zsoil(1) * water%SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(water%SH2O(1) + water%SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,levels%nsoil
          DICE    = DICE + (domain%zsoil(K-1) - domain%zsoil(K) ) * water%SICE(K)
          DMAX(K) = (domain%zsoil(K-1) - domain%zsoil(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(water%SH2O(K) + water%SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,water%QINSUR * dt)
       INFMAX = (PX * (DDT / (PX + DDT)))/ dt

! impermeable fraction due to frozen soil
       FCR1 = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR1 = 1. - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation
       INFMAX = INFMAX * FCR1

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND1,water%SH2O(1),water%SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND1)
       INFMAX = MIN (INFMAX,PX)

       water%RUNSRF= MAX(0., water%QINSUR - INFMAX)
       water%PDDUM = water%QINSUR - water%RUNSRF

    END IF

  END SUBROUTINE INFIL

 SUBROUTINE COMPUTE_VIC_SURFRUNOFF(parameters,domain,levels,water)
! ----------------------------------------------------------------------
! Calculate the saturated area and runoff based on VIC runoff scheme.
! This scheme adopted from VIC model
! Author: PV
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs
  type (parameters_type), intent(in) :: parameters
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (     water_type)             :: water
!local 
  REAL    :: EX, I_0, I_MAX, BASIS, TOP_MOIST, TOP_MAX_MOIST, DT
  INTEGER :: IZ
! ----------------------------------------------------------------------

! Initialize Variables  
  DT    = water%runsrf_dt
  EX    = 0.0
  I_MAX = 0.0
  I_0   = 0.0
  BASIS = 0.0
  TOP_MOIST     = 0.0
  TOP_MAX_MOIST = 0.0
  water%RUNSRF = 0.0
  water%ASAT  = 0.0

  DO IZ=1,levels%nsoil-2
    TOP_MOIST     = TOP_MOIST + (water%SMC(IZ) * (-1) * domain%ZSOIL(IZ)) ! m
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*(-1)*domain%ZSOIL(IZ)) ! m  
  END DO

  ! Saturated area from soil moisture
  EX    = parameters%BVIC/(1+parameters%BVIC)
  water%ASAT  = 1.0 - (( 1.0 - (TOP_MOIST/TOP_MAX_MOIST))**EX)  ! 
  water%ASAT  = MAX(0.0, water%ASAT)
  water%ASAT  = MIN(1.0, water%ASAT)

  ! Infiltration for the previous time-step soil moisture based on ASAT
  I_MAX = (1.0 + parameters%BVIC)*TOP_MAX_MOIST ! m
  I_0   = I_MAX*(1.0 - (1.0 - water%ASAT)**(1.0/parameters%BVIC)) !m

  ! Solve for surface runoff
  IF(water%QINSUR .EQ. 0.0) THEN
     water%RUNSRF = 0.0
  ELSE IF(I_MAX .EQ. 0.0) THEN
     water%RUNSRF = water%QINSUR*DT
  ELSE IF( (I_0 + (water%QINSUR*DT)) .GT. I_MAX ) THEN
     water%RUNSRF = (water%QINSUR*DT) - TOP_MAX_MOIST + TOP_MOIST
  ELSE
     BASIS  = 1.0 - ((I_0 + (water%QINSUR*DT))/I_MAX)
     water%RUNSRF = (water%QINSUR*DT) - TOP_MAX_MOIST + TOP_MOIST + &
              TOP_MAX_MOIST*(basis**(1.0+parameters%BVIC))
  END IF

  water%RUNSRF = water%RUNSRF/(DT) ! m/s
  IF (water%RUNSRF .LT. 0.0) water%RUNSRF = 0.0
  IF (water%RUNSRF .GT. water%QINSUR) water%RUNSRF = water%QINSUR
  water%PDDUM = water%QINSUR - water%RUNSRF    ! m/s

 END SUBROUTINE COMPUTE_VIC_SURFRUNOFF

  SUBROUTINE COMPUTE_XAJ_SURFRUNOFF(parameters,domain,levels,water)
! ----------------------------------------------------------------------
! Calculate the saturated area and runoff based on Xinanjiag runoff scheme.
! Reference: Knoben, W. J., Freer, J. E., Fowler, K. J., Peel, M. C., & Woods, R. A. (2019). 
! Modular Assessment of Rainfall-Runoff Models Toolbox (MARRMoT) v1. 2: 
! an open-source, extendable framework providing implementations of 46 conceptual 
! hydrologic models as continuous state-space formulations.
! ----------------------------------------------------------------------
! Author: PV
! Date: August 03, 2020
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs
  type (parameters_type), intent(in) :: parameters
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (     water_type)             :: water
! local
    REAL    :: WM,WM_MAX,SM,SM_MAX,IRUNOFF,PRUNOFF,DT
    INTEGER :: IZ
!------------------------------------------------------------------------
!initialize  
    WM      = 0.0
    WM_MAX  = 0.0
    SM      = 0.0
    SM_MAX  = 0.0
    IRUNOFF = 0.0
    PRUNOFF = 0.0
    DT = water%runsrf_dt
    water%RUNSRF  = 0.0

    DO IZ=1,levels%nsoil-2
       IF ((water%SMC(IZ)-parameters%SMCREF(IZ)) .GT. 0.) THEN ! soil moisture greater than field capacity
          SM     = SM + (water%SMC(IZ) - parameters%SMCREF(IZ) )*(-1)*domain%ZSOIL(IZ) !m
          WM     = WM + (parameters%SMCREF(IZ)*(-1)*domain%ZSOIL(IZ))            !m  
       ELSE
          WM     = WM + (water%SMC(IZ)*(-1)*domain%ZSOIL(IZ))
       END IF
       WM_MAX = WM_MAX + (parameters%SMCREF(IZ)*(-1)*domain%ZSOIL(IZ))
       SM_MAX = SM_MAX + (parameters%SMCMAX(IZ) - parameters%SMCREF(IZ))*(-1)*domain%ZSOIL(IZ)
    END DO
    WM = MIN(WM,WM_MAX) ! tension water (m) 
    SM = MIN(SM,SM_MAX) ! free water (m)

! impervious surface runoff R_IMP    
    IRUNOFF = water%FCR(1)*water%QINSUR*DT
! solve pervious surface runoff (m) based on Eq. (310)
    IF ((WM/WM_MAX) .LE. (0.5-parameters%AXAJ))THEN
       PRUNOFF = (1-water%FCR(1))*water%QINSUR*DT* &
                 ((0.5-parameters%AXAJ)**(1-parameters%BXAJ))*((WM/WM_MAX)**parameters%BXAJ)
    ELSE
       PRUNOFF = (1-water%FCR(1))*water%QINSUR*DT* &
            (1-(((0.5+parameters%AXAJ)**(1-parameters%BXAJ))*((1-(WM/WM_MAX))**parameters%BXAJ)))
    END IF
! estimate surface runoff based on Eq. (313)
    IF(water%QINSUR .EQ. 0.0) THEN
      water%RUNSRF  = 0.0
    ELSE
      water%RUNSRF = PRUNOFF*(1-((1-(SM/SM_MAX))**parameters%XXAJ))+IRUNOFF
    END IF
    water%RUNSRF = water%RUNSRF/DT !m/s
    water%RUNSRF = MAX(0.0,    water%RUNSRF)
    water%RUNSRF = MIN(water%QINSUR, water%RUNSRF)
    water%PDDUM  = water%QINSUR - water%RUNSRF

  END SUBROUTINE COMPUTE_XAJ_SURFRUNOFF

  SUBROUTINE  DYNAMIC_VIC(parameters,options,domain,levels,water)
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and estimate surface runoff based on 
! Liang, X., & Xie, Z. (2001). A new surface runoff parameterization with subgrid-scale
! soil heterogeneity for land surface models. Advances in Water Resources, 24(9-10), 1173-1193.
! Author: PV
! Date  : August 3, 2020
! --------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (parameters_type), intent(in) :: parameters
  type (   options_type), intent(in) :: options
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (     water_type)             :: water
! locals
  REAL                                 :: BB            !B parameter for infiltration scaling curve
  REAL                                 :: TOP_MOIST     !actual water depth in top layers (m)
  REAL                                 :: TOP_MAX_MOIST !water depth in top layers (m)
  REAL                                 :: DP            !water input on soil surface (m)
  REAL                                 :: I_0           !initial water depth (m)
  REAL                                 :: I_MAX         !maximum water depth (m)
  REAL                                 :: FSUR          !surface infiltration rate (m/s)
  REAL                                 :: FMAX          !maximum infiltration rate (m/s)
  REAL                                 :: RUNOFFSAT     !saturation excess runoff (m/s)
  REAL                                 :: RUNOFFINF     !infiltration excess runoff (m/s)
  REAL                                 :: INFILTRTN     !infiltration (m/s)
  REAL                                 :: TEMPR1        !temporary saturation excess runoff (m/s)
  REAL                                 :: TEMPR2        !temporary infiltration excess runoff (m/s)
  REAL                                 :: R1            !saturation excess runoff (m/s)
  REAL                                 :: R2            !infiltration excess runoff (m/s)
  REAL                                 :: YD            !initial depth Y (m)
  REAL                                 :: YD_OLD        !initial depth Y (m)
  REAL                                 :: YD0           !initial depth Y (m)
  REAL                                 :: TEMP1, ERROR
  REAL                                 :: DT
  INTEGER                              :: IZ, IZMAX, INFLMAX
!---------------------------------------------------------------------------------
  TOP_MOIST     = 0.0
  TOP_MAX_MOIST = 0.0
  BB            = 1.0
  DP            = 0.0
  I_MAX         = 0.0
  I_0           = 0.0
  RUNOFFSAT     = 0.0
  RUNOFFINF     = 0.0
  INFILTRTN     = 0.0
  DT = water%runsrf_dt
  water%RUNSRF  = 0.0
  IZMAX         = 20
  ERROR         = 1.388889E-07*DT ! 0.5 mm per hour time step
  BB = parameters%BBVIC

  DO IZ=1,levels%nsoil-2
    TOP_MOIST     = TOP_MOIST + (water%SMC(IZ)*(-1)*domain%ZSOIL(IZ))                      ! actual moisture in top layers, [m]
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*(-1)*domain%ZSOIL(IZ))          ! maximum moisture in top layers, [m]  
  END DO
  IF(TOP_MOIST .GT. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
  DP     = water%QINSUR * DT                                                              ! precipitation depth, [m]
  I_MAX  = TOP_MAX_MOIST * (parameters%BVIC+1.0)                                         ! maximum infiltration capacity, im, [m], Eq. 14
  I_0    = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))        ! infiltration capacity, i [m] in the Eq. 1
  ! I_MAX = CAP_minf ; I_0 = A  
  INFLMAX = 0
  IF (options%opt_infdv .EQ. 1) THEN
     CALL PHILIP_INFIL(parameters,domain,levels,water,DT,FSUR,INFLMAX)
  ELSE IF (options%opt_infdv .EQ. 2) THEN
     CALL GREEN_AMPT_INFIL(parameters,domain,levels,water,FSUR,INFLMAX)
  ELSE IF (options%opt_infdv .EQ. 3) THEN
     CALL SMITH_PARLANGE_INFIL(parameters,domain,levels,water,FSUR,INFLMAX)
  END IF

  ! I_MM = FSUR; I_M = FMAX  
  FMAX = (BB+1.0)*FSUR
  IF(DP .LE. 0.0) THEN
    RUNOFFSAT = 0.0
    RUNOFFINF = 0.0
    INFILTRTN = 0.0
    GOTO 2001
  ELSE
    IF((TOP_MOIST .GE. TOP_MAX_MOIST) .AND. (I_0 .GE. I_MAX)) THEN
      TOP_MOIST = TOP_MAX_MOIST
      I_0       = I_MAX
      RUNOFFSAT = DP
      RUNOFFINF = 0.0
      INFILTRTN = 0.0
      GOTO 2001
    ELSE
      I_0 = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))
      IF((DP+I_0) .GT. I_MAX)THEN
        IF((FMAX*DT) .GE. DP) THEN
          YD     = I_MAX - I_0
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          TEMP1  = I_MAX-I_0-TEMPR1-((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
          IF(TEMP1 .LE. 0.0) THEN
            YD        = I_MAX - I_0
            INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
            RUNOFFSAT = DP - INFILTRTN
            RUNOFFINF = 0.0
            TOP_MOIST = TOP_MAX_MOIST
            I_0       = I_MAX
            GOTO 2001
          ELSE
            YD        = 0.0

            DO IZ = 1,IZMAX ! loop : IITERATION1
               YD_OLD = YD
               TEMPR1 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               YD     = TEMPR1 + ((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  GOTO 1001
               END IF
            END DO
          END IF
        ELSE
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          IF((TEMPR1+(FMAX*DT)) .LE. DP) THEN
            IF((I_MAX-I_0-TEMPR1-(FMAX*DT)) .LE. 0.0)THEN
              YD        = I_MAX - I_0
              INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
              RUNOFFSAT = DP - INFILTRTN
              RUNOFFINF = 0.0
              TOP_MOIST = TOP_MAX_MOIST
              I_0       = I_MAX
              GOTO 2001
            ELSE
              YD        = 0.0

              DO IZ = 1,IZMAX ! loop : IITERATION2
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = TEMPR1 + (FSUR*DT)
                 IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    GOTO 1001
                 END IF
              END DO
            END IF

          ELSE

            YD = DP/2.0
            DO IZ = 1,IZMAX ! loop : IITERATION30
               YD_OLD = YD
               TEMPR1 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               YD     = YD - TEMPR1 - (FSUR*DT) + DP
               IF (YD .LE. 0.0) YD = 0.0
               IF (YD .GE. DP) YD = DP
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  YD0 = YD
                  EXIT
               END IF
            END DO
            DO IZ = 1,IZMAX ! loop : IITERATION3
               YD_OLD = YD
               TEMPR1 = 0.0
               TEMPR2 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               CALL RR2(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
               YD     = DP - TEMPR2
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  GOTO 1001
               END IF
            END DO
1001        IF(YD .LE. 0.0) YD = 0.0
            IF(YD .GE. DP) YD = DP
            CALL RR1(parameters,I_0,I_MAX,YD,R1)
            RUNOFFSAT = R1
            RUNOFFINF = DP - YD
            INFILTRTN = YD - RUNOFFSAT
            TOP_MOIST = TOP_MOIST + INFILTRTN
            YD        = I_0+YD
            IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
            IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
            I_0 = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))
            GOTO 2001
          END IF
        END IF

      ELSE
        IF((FMAX*DT) .GE. DP) THEN
          YD = DP/2.0
          DO IZ = 1,IZMAX ! ITERATION1
             YD_OLD = YD
             TEMPR1 = 0.0
             CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
             YD = TEMPR1 + ((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
             IF ((ABS(YD - YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                GOTO 1002
             END IF
          END DO
        ELSE
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          IF((TEMPR1+(FMAX*DT)) .LE. DP)THEN
              YD = DP/2.0
              DO IZ = 1,IZMAX ! ITERATION2
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = TEMPR1+(FSUR*DT)
                 IF((ABS(YD - YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    GOTO 1002
                 END IF
              END DO
          ELSE
              YD = 0.0
              DO IZ = 1,IZMAX ! ITERATION30
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = (DP - (FMAX*DT)) + YD - TEMPR1
                 IF(YD .LE. 0.0) YD = 0.0
                 IF(YD .GE. DP) YD = DP
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 IF ((ABS(TEMPR1+(FMAX*DT)-DP) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    YD0 = YD
                    EXIT
                 END IF
              END DO
              DO  IZ = 1,IZMAX ! ITERATION3
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 TEMPR2 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 CALL RR2(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
                 YD     = DP - TEMPR2
                 IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                   GOTO 1002
                 END IF
              END DO
          END IF
        END IF
1002    IF(YD .LE. 0.0) YD = 0.0
        IF(YD .GE. DP)  YD = DP
        R1 = 0.0
        CALL RR1(parameters,I_0,I_MAX,YD,R1)
        RUNOFFSAT = R1
        RUNOFFINF = DP - YD
        INFILTRTN = YD - RUNOFFSAT
        TOP_MOIST = TOP_MOIST + INFILTRTN
        IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
        IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
        I_0       = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))
      END IF
    END IF
  END IF

2001 water%RUNSRF = (RUNOFFSAT + RUNOFFINF)/DT
     water%RUNSRF = MIN(water%RUNSRF,water%QINSUR)
     water%RUNSRF = MAX(water%RUNSRF,0.0)
     water%PDDUM  = water%QINSUR - water%RUNSRF

  END SUBROUTINE DYNAMIC_VIC

! ---------------------------  Runoff subroutines for dynamic VIC ----------------------------
  SUBROUTINE RR1 (parameters,I_0,I_MAX,YD,R1)
!---------------------------------------------------------------------------------------------
! This subroutine estimate saturation excess runoff, R1
! Author: PV
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   type (parameters_type),   intent(in) :: parameters
   REAL,                     INTENT(IN) :: I_0,I_MAX,YD
   REAL,                     INTENT(OUT):: R1
   REAL                                 :: TDEPTH
!------------------------------------------------------
   TDEPTH = I_0 + YD
   IF(TDEPTH .GT. I_MAX) TDEPTH = I_MAX

   !Saturation excess runoff , Eq 5.
   R1 = YD - ( (I_MAX/(parameters%BVIC+1.0)) * ( ((1 - (I_0/I_MAX))**(parameters%BVIC+1.0)) &
                                               - ((1 - (TDEPTH/I_MAX))**(parameters%BVIC+1.0))))

   IF (R1 .LT. 0.0) R1 = 0.0
  END SUBROUTINE RR1
!---------------------------------------------------------------------------------------------
  SUBROUTINE RR2 (YD,Y0,R1,FMAX,FSUR,DT,DP,BB,R2)
!---------------------------------------------------------------------------------------------
! This subroutine estimate infiltration excess runoff, R1
! Author: PV
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   REAL,                     INTENT(IN) :: YD,Y0,R1,FMAX,FSUR,DT,DP,BB
   REAL,                     INTENT(OUT):: R2
!------------------------------------------------------
   IF(YD .GE. Y0)THEN
     R2 = DP - R1 - (FMAX*DT* (1 - ((1 - (DP-R1)/(FMAX*DT))**(BB+1.0))))
   ELSE
     R2 = DP - R1 - (FMAX*DT)
   END IF

   IF(R2 .LT. 0.0) R2 =0.0
  END SUBROUTINE RR2

  SUBROUTINE SMITH_PARLANGE_INFIL(parameters,domain,levels,water,FSUR,INFLMAX)
!---------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Smith-Parlange equation. We use its three
! parameter version of the equation (Eq. 6.25) from Smith, R.E. (2002) Infiltration Theory for
! Hydrologic Applications, Water Resources Monograph 15, AGU. 
! Author: PV
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
! inputs
   type (parameters_type), intent(in) :: parameters
   type (    domain_type), intent(in) :: domain
   type (    levels_type), intent(in) :: levels
   type (     water_type)             :: water
   INTEGER,                INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT
! outputs
   REAL,                 INTENT(OUT)  :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                               :: WDF    ! soil water diffusivity       
   REAL                               :: WCND   ! soil water conductivity[m/s]
   REAL                               :: GAM    ! smith-parlang weighing parameter[-]
   REAL                               :: JJ     ! dummy variable
   INTEGER                            :: ISOIL
!---------------------------------------------------------------------------------
   ! smith-parlang weighing parameter, GAMMA
   GAM = 0.82
   ISOIL = 1

   ! check whether we are estimating infiltration for current SMC or SMCWLT
   IF (INFLMAX .EQ. 1)THEN ! not active for now as the maximum infiltration is estimated based on table values
      ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
      CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)
      ! Maximum infiltrability based on the Eq. 6.25. (m/s)
      JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * (-1) * domain%ZSOIL(ISOIL)
      FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND) / (EXP(GAM * 1E-05 / JJ) -1))
      ! infiltration rate at surface
      IF(parameters%DKSAT(ISOIL) .LT. water%QINSUR)THEN
        FSUR = MIN(water%QINSUR,FSUR)
      ELSE
        FSUR = water%QINSUR
      END IF
      IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE
      ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
      CALL WDFCND2 (parameters,WDF,WCND,water%SMC(ISOIL),water%SICE(ISOIL),ISOIL)
      ! Maximum infiltrability based on the Eq. 6.25. (m/s)
      JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - water%SMC(ISOIL)) * (-1) * domain%ZSOIL(ISOIL)
      FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND)/(EXP(GAM*water%FACC/JJ)-1))
      ! infiltration rate at surface  
      IF(parameters%DKSAT(ISOIL) .LT. water%QINSUR)THEN
        FSUR = MIN(water%QINSUR,FSUR)
      ELSE
        FSUR = water%QINSUR
      END IF

      ! accumulated infiltration function
      water%FACC = water%FACC + FSUR
   END IF

  END SUBROUTINE SMITH_PARLANGE_INFIL

  SUBROUTINE GREEN_AMPT_INFIL(parameters,domain,levels,water,FSUR,INFLMAX)
!-------------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Green-Ampt equation. We use its three
! parameter version of the smith-parlage equation (Eq. 6.25) from Smith, R.E. (2002) Infiltration Theory for
! Hydrologic Applications, Water Resources Monograph 15, AGU. Where gamma = 0, Eq 6.25 = Green-Ampt.
! Author: PV
!-------------------------------------------------------------------------------------------------
   IMPLICIT NONE
! ------------------------------------------------------------------------------------------------
! inputs
   type (parameters_type), intent(in) :: parameters
   type (    domain_type), intent(in) :: domain
   type (    levels_type), intent(in) :: levels
   type (     water_type)             :: water
   INTEGER,                INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT
! outputs
   REAL,                   INTENT(OUT)   :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                                    :: WDF    ! soil water diffusivity
   REAL                                    :: WCND   ! soil water conductivity[m/s]
   REAL                                    :: JJ     ! dummy variable 
   INTEGER                                 :: ISOIL
!---------------------------------------------------------------------------------
   ISOIL = 1

   IF(INFLMAX .EQ. 1)THEN
     ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
     CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)
     ! Maximum infiltrability based on the Eq. 6.25. (m/s)
     JJ   = parameters%G*(parameters%SMCMAX(ISOIL)-parameters%SMCWLT(ISOIL))*(-1)*domain%ZSOIL(ISOIL)
     FSUR = parameters%DKSAT(ISOIL) + ((JJ/1E-05) * (parameters%DKSAT(ISOIL) - WCND))
     !maximum infiltration rate at surface
     IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE
     ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
     CALL WDFCND2 (parameters,WDF,WCND,water%SMC(ISOIL),water%SICE(ISOIL),ISOIL)
     ! Maximum infiltrability based on the Eq. 6.25. (m/s)
     JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - water%SMC(ISOIL))*(-1)*domain%ZSOIL(ISOIL)
     FSUR = parameters%DKSAT(ISOIL) + ((JJ/water%FACC) * (parameters%DKSAT(ISOIL) - WCND))
     ! infiltration rate at surface
     IF(parameters%DKSAT(ISOIL) .LT. water%QINSUR)THEN
        FSUR = MIN(water%QINSUR,FSUR)
     ELSE
        FSUR = water%QINSUR
     END IF
     ! accumulated infiltration function
     water%FACC = water%FACC + FSUR
   END IF

  END SUBROUTINE GREEN_AMPT_INFIL

!== begin Philip's infiltration ====================================================================

  SUBROUTINE PHILIP_INFIL(parameters,domain,levels,water,DT,FSUR,INFLMAX)
!-------------------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Philip's two parameter equation (Eq. 2) presented in
! Valiantzas (2010). New linearized two-parameter infiltration equation for direct determination 
! of conductivity and sorptivity, J. Hydrology.
! Author: PV
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
! inputs
   type (parameters_type), intent(in) :: parameters
   type (    domain_type), intent(in) :: domain
   type (    levels_type), intent(in) :: levels
   type (     water_type)             :: water
   REAL,                     INTENT(IN) :: DT     !time-step (sec)
   INTEGER,                  INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT 
! outputs
   REAL,                     INTENT(OUT)   :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                                    :: WDF    ! soil water diffusivity (m2/s)
   REAL                                    :: WCND   ! soil water conductivity[m/s]
   REAL                                    :: SP     ! sorptivity (LT^-1/2)
   REAL                                    :: AP     ! intial hydraulic conductivity (m/s,L/T)
   REAL                                    :: FMAX   ! Maximum infiltration (m/s)
   INTEGER                                 :: ISOIL
!---------------------------------------------------------------------------------
   ISOIL = 1

   IF (INFLMAX .EQ. 1) THEN
     ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
     CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)
     ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986)
     ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
     SP = SQRT(2 * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * (parameters%DWSAT(ISOIL) - WDF))
     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2.0/3)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1.0/3)*parameters%DKSAT(ISOIL))
     ! Maximun infiltration rate, m
     FSUR = (1.0/2)*SP*(DT**(-1.0/2))+AP ! m/s
     IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE
     ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
     CALL WDFCND2 (parameters,WDF,WCND,water%SMC(ISOIL),water%SICE(ISOIL),ISOIL)
     ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986) 
     ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
     SP = SQRT(2 * (parameters%SMCMAX(ISOIL) - water%SMC(ISOIL)) * (parameters%DWSAT(ISOIL) - WDF))
     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2.0/3)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1.0/3)*parameters%DKSAT(ISOIL))
     ! Maximun infiltration rate, m
     FSUR = (1.0/2)*SP*(DT**(-1.0/2))+AP ! m/s
     ! infiltration rate at surface
     IF(parameters%DKSAT(ISOIL) .LT. water%QINSUR)THEN
       FSUR = MIN(water%QINSUR,FSUR)
     ELSE
       FSUR = water%QINSUR
     END IF
     ! accumulated infiltration function
     water%FACC = water%FACC + FSUR
   END IF

  END SUBROUTINE PHILIP_INFIL

end module SurfaceRunoffInfiltration

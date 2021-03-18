module SoilWaterMovement

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use SoilWaterRetentionCoeff
  
contains

!== begin srt ======================================================================================

  SUBROUTINE SRT (options,parameters,domain,levels,water,dt,RHSTT,AI,BI,CI)
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! water diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input
  type (parameters_type), intent(in) :: parameters
  type (options_type)   , intent(in) :: options
  type (domain_type), intent(in) :: domain
  type (levels_type), intent(in) :: levels
  type (     water_type)             :: water

  REAL,                     INTENT(IN)  :: dt

! output
    REAL, DIMENSION(1:levels%soil), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:levels%soil), INTENT(OUT) :: AI
    REAL, DIMENSION(1:levels%soil), INTENT(OUT) :: BI
    REAL, DIMENSION(1:levels%soil), INTENT(OUT) :: CI

! local
    INTEGER                               :: K
    REAL, DIMENSION(1:levels%soil)              :: DDZ
    REAL, DIMENSION(1:levels%soil)              :: DENOM
    REAL, DIMENSION(1:levels%soil)              :: DSMDZ
    REAL, DIMENSION(1:levels%soil)              :: WFLUX
    REAL, DIMENSION(1:levels%soil)              :: WDF
    REAL, DIMENSION(1:levels%soil)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD !soil moisture between bottom of the soil and water table
    REAL                                  :: SMXBOT  !soil moisture below bottom to calculate flux

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(options%OPT_INF == 1) THEN
      DO K = 1, levels%soil
        CALL WDFCND1 (parameters,WDF(K),water%WCND(K),water%SMC(K),water%FCR(K),K)
        SMX(K) = water%SMC(K)
      END DO
        IF(options%OPT_DRN == 5) SMXWTD = water%SMCWTD
    END IF

    IF(options%OPT_INF == 2) THEN
      DO K = 1, levels%soil
        CALL WDFCND2 (parameters,WDF(K),water%WCND(K),water%SH2O(K),water%SICEMAX,K)
        SMX(K) = water%SH2O(K)
      END DO
          IF(options%OPT_DRN == 5)SMXWTD=water%SMCWTD*water%SH2O(levels%soil)/water%SMC(levels%soil)  !same liquid fraction as in the bottom layer
    END IF

    DO K = 1, levels%soil
       IF(K == 1) THEN
          DENOM(K) = - domain%zsoil (K)
          TEMP1    = - domain%zsoil (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + water%WCND(K) - water%PDDUM + water%ETRANI(K) + water%QSEVA
       ELSE IF (K < levels%soil) THEN
          DENOM(k) = (domain%zsoil(K-1) - domain%zsoil(K))
          TEMP1    = (domain%zsoil(K-1) - domain%zsoil(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + water%WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - water%WCND(K-1) + water%ETRANI(K)
       ELSE
          DENOM(K) = (domain%zsoil(K-1) - domain%zsoil(K))
          IF(options%OPT_DRN == 1 .or. options%OPT_DRN == 2) THEN
             water%QDRAIN   = 0.
          END IF
          IF(options%OPT_DRN == 3 .or. options%OPT_DRN == 6  .or. &
             options%OPT_DRN == 7 .or. options%OPT_DRN == 8) THEN
             water%QDRAIN   = parameters%SLOPE*water%WCND(K)
          END IF
          IF(options%OPT_DRN == 4) THEN
             water%QDRAIN   = (1.0-water%FCRMAX)*water%WCND(K)
          END IF
          IF(options%OPT_DRN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(water%ZWT < domain%zsoil(levels%soil)-DENOM(levels%soil))THEN
!gmm interpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K)+domain%zsoil(K)-water%ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             water%QDRAIN   = WDF(K  ) * DSMDZ(K  ) + water%WCND(K  )
          END IF   
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-water%WCND(K-1)+water%ETRANI(K) + water%QDRAIN
       END IF
    END DO

    DO K = 1, levels%soil
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < levels%soil) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO

! ----------------------------------------------------------------------
  END SUBROUTINE SRT

!== begin sstep ====================================================================================

  SUBROUTINE SSTEP (options,parameters,domain,levels,water,dt,AI,BI,CI,RHSTT,WPLUS)

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input
  type (parameters_type), intent(in) :: parameters
  type (options_type)   , intent(in) :: options
  type (domain_type), intent(in) :: domain
  type (levels_type), intent(in) :: levels
  type (     water_type)             :: water
    REAL, INTENT(IN)                            :: dt

!output
    REAL, DIMENSION(1:levels%soil), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:levels%soil), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:levels%soil), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:levels%soil), INTENT(INOUT) :: RHSTT
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:levels%soil)                :: RHSTTIN
    REAL, DIMENSION(1:levels%soil)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS
! ----------------------------------------------------------------------
    WPLUS = 0.0

    DO K = 1,levels%soil
       RHSTT (K) =   RHSTT(K) * dt
       AI (K)    =      AI(K) * dt
       BI (K)    = 1. + BI(K) * dt
       CI (K)    =      CI(K) * dt
    END DO

! copy values for input variables before calling rosr12

    DO K = 1,levels%soil
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO

! call ROSR12 to solve the tri-diagonal matrix

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,domain,levels,0)

    DO K = 1,levels%soil
        water%SH2O(K) = water%SH2O(K) + CI(K)
    ENDDO

!  excessive water above saturation in a layer is moved to
!  its unsaturated layer like in a bucket

!gmmwith opt_drn=5 there is soil moisture below levels%soil, to the water table
  IF(options%OPT_DRN == 5) THEN
!update smcwtd

     IF(water%ZWT < domain%zsoil(levels%soil)-domain%dzsnso(levels%soil))THEN
!accumulate qdrain to update deep water table and soil moisture later
        water%DEEPRECH =  water%DEEPRECH + dt * water%QDRAIN
     ELSE
        water%SMCWTD = water%SMCWTD + dt * water%QDRAIN  / domain%dzsnso(levels%soil)
        WPLUS = MAX((water%SMCWTD-parameters%SMCMAX(levels%soil)), 0.0) * domain%dzsnso(levels%soil)
        WMINUS = MAX((1.E-4-water%SMCWTD), 0.0) * domain%dzsnso(levels%soil)

        water%SMCWTD = MAX( MIN(water%SMCWTD,parameters%SMCMAX(levels%soil)) , 1.E-4)
        water%SH2O(levels%soil) = water%SH2O(levels%soil) + WPLUS/domain%dzsnso(levels%soil)

!reduce fluxes at the bottom boundaries accordingly
        water%QDRAIN = water%QDRAIN - WPLUS/dt
        water%DEEPRECH = water%DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = levels%soil,2,-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - water%SICE(K) ) )
      WPLUS        = MAX((water%SH2O(K)-EPORE), 0.0) * domain%dzsnso(K)
      water%SH2O(K)  = MIN(EPORE,water%SH2O(K))
      water%SH2O(K-1) = water%SH2O(K-1) + WPLUS/domain%dzsnso(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(1) - water%SICE(1) ) )
    WPLUS        = MAX((water%SH2O(1)-EPORE), 0.0) * domain%dzsnso(1) 
    water%SH2O(1)      = MIN(EPORE,water%SH2O(1))

   IF(WPLUS > 0.0) THEN
    water%SH2O(2)      = water%SH2O(2) + WPLUS/domain%dzsnso(2)
    DO K = 2,levels%soil-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - water%SICE(K) ) )
      WPLUS        = MAX((water%SH2O(K)-EPORE), 0.0) * domain%dzsnso(K)
      water%SH2O(K)      = MIN(EPORE,water%SH2O(K))
      water%SH2O(K+1)    = water%SH2O(K+1) + WPLUS/domain%dzsnso(K+1)
    END DO

    EPORE = MAX ( 1.E-4 , ( parameters%SMCMAX(levels%soil) - water%SICE(levels%soil) ) )
    WPLUS = MAX((water%SH2O(levels%soil)-EPORE), 0.0) * domain%dzsnso(levels%soil) 
    water%SH2O(levels%soil) = MIN(EPORE,water%SH2O(levels%soil))
   END IF
   
    water%SMC = water%SH2O + water%SICE

  END SUBROUTINE SSTEP

!== begin rosr12 ===================================================================================

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,domain,levels,nnsnow)
! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
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

  type (domain_type), intent(in) :: domain
  type (levels_type), intent(in) :: levels
    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER               :: K, KK, nnsnow

    REAL, DIMENSION(nnsnow+1:levels%soil),INTENT(IN):: A, B, D
    REAL, DIMENSION(nnsnow+1:levels%soil),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    C (levels%soil) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,levels%soil
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    P (levels%soil) = DELTA (levels%soil)
! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,levels%soil
       KK = levels%soil - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12

end module SoilWaterMovement

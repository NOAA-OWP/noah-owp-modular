module SoilWaterModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use SoilWaterMovement
  use SubsurfaceRunoffModule
  use SurfaceRunoffModule
  
  implicit none
  
contains

!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (domain, levels, options, parameters, water)

! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (   options_type), intent(in) :: options
  type (parameters_type), intent(in) :: parameters
  type (     water_type)             :: water

! local
  INTEGER                                 :: K,IZ   !do-loop index
  INTEGER                                 :: ITER   !iteration index
  REAl                                    :: DTFINE !fine time step (s)
  REAL, DIMENSION(1:levels%nsoil)         :: RHSTT  !right-hand side term of the matrix
  REAL, DIMENSION(1:levels%nsoil)         :: AI     !left-hand side term
  REAL, DIMENSION(1:levels%nsoil)         :: BI     !left-hand side term
  REAL, DIMENSION(1:levels%nsoil)         :: CI     !left-hand side term
  REAL                                    :: FICE   !ice fraction in frozen soil
  REAL                                    :: WPLUS  !saturation excess of the total soil [m]
  REAL                                    :: RSAT   !accumulation of WPLUS (saturation excess) [m]
  REAL                                    :: SH2OMIN!minimum soil liquid water content (m3/m3)
  REAL                                    :: WTSUB  !sum of WCND(K)*DZSNSO(K)
  REAL                                    :: MH2O   !water mass removal (mm)
  REAL, DIMENSION(1:levels%nsoil)         :: MLIQ   !
  REAL                                    :: XS     !
  REAL                                    :: WATMIN !
  REAL                                    :: QDRAIN_SAVE !
  REAL                                    :: RUNSRF_SAVE !
  REAL                                    :: EPORE  !effective porosity [m3/m3]
  INTEGER                                 :: NITER  !iteration times soil moisture (-)
  REAL                                    :: SICE_SMC_RATIO ! Ratio of frozen soil to total water content when opt_sub == 2
  REAL, PARAMETER :: A = 4.0
! ----------------------------------------------------------------------

  ! Call the full Noah-MP style subsurface when opt_sub == 1
  IF (options%OPT_SUB == 1) THEN
  
    water%RUNSRF = 0.0
    water%RUNSUB = 0.0
    water%PDDUM  = 0.0
    water%runsrf_dt = domain%dt
    RSAT   = 0.0

   ! for the case when snowmelt water is too large

    DO K = 1,levels%nsoil
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - water%SICE(K) ) )
       RSAT    = RSAT + MAX(0.,water%SH2O(K)-EPORE)*domain%dzsnso(K)  
       water%SH2O(K) = MIN(EPORE,water%SH2O(K))             
    END DO

   !impermeable fraction due to frozen soil

    DO K = 1,levels%nsoil
       FICE    = MIN(1.0,water%SICE(K)/parameters%SMCMAX(K))
       water%FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

    ! maximum soil ice content and minimum liquid water of all layers

    water%SICEMAX = 0.0
    water%FCRMAX  = 0.0
    SH2OMIN = parameters%SMCMAX(1)
    DO K = 1,levels%nsoil
       IF (water%SICE(K) > water%SICEMAX) water%SICEMAX = water%SICE(K)
       IF (water%FCR(K)  > water%FCRMAX)  water%FCRMAX  = water%FCR(K)
       IF (water%SH2O(K) < SH2OMIN) SH2OMIN = water%SH2O(K)
    END DO

   !update ZWT for option 2, which are used in both surface and subsurface runoff
   ! other subsurface runoff options are done at the end

    IF(options%OPT_DRN == 2) THEN 
        call SubsurfaceRunoff(domain, levels, options, parameters, water)
    END IF

    !surface runoff and infiltration rate using different schemes
    IF ( parameters%urban_flag ) water%FCR(1)= 0.95
    water%FACC  = 1E-06
    call SurfaceRunoff(domain, levels, options, parameters, water)

    ! determine iteration times and finer time step

    NITER = 1

    !    IF(OPT_INF == 1) THEN    !OPT_INF =2 may cause water imbalance
       NITER = 3
       IF (water%PDDUM*domain%dt>domain%dzsnso(1)*parameters%SMCMAX(1) ) THEN
          NITER = NITER*2
       END IF
    !    END IF                 

    DTFINE  = domain%dt / NITER
    water%runsrf_dt = DTFINE

    ! solve soil moisture
    water%FACC  = 1E-06
    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0
    DO ITER = 1, NITER
       IF(water%QINSUR > 0. .and. (options%OPT_RUN == 3 .or. options%OPT_RUN == 6 &
          .or. options%OPT_RUN == 7 .or. options%OPT_RUN == 8) ) THEN
          CALL SurfaceRunoff(domain, levels, options, parameters, water)
       END IF

       CALL SRT(options,parameters,domain,levels,water,DTFINE,RHSTT,AI,BI,CI)

       CALL SSTEP(options,parameters,domain,levels,water,DTFINE,AI,BI,CI,RHSTT,WPLUS)

       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + water%QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + water%RUNSRF
    END DO

    water%QDRAIN = QDRAIN_SAVE/NITER
    water%RUNSRF = RUNSRF_SAVE/NITER

    water%RUNSRF = water%RUNSRF * 1000. + RSAT * 1000./domain%dt  ! m/s -> mm/s
    water%QDRAIN = water%QDRAIN * 1000.

    !WRF_HYDRO_DJG...
    !yw    INFXSRT = RUNSRF * domain%dt   !mm/s -> mm

    ! removal of soil water due to groundwater flow (option 2)
    IF(options%OPT_DRN == 2) THEN
         WTSUB = 0.
         DO K = 1, levels%nsoil
           WTSUB = WTSUB + water%WCND(K)*domain%dzsnso(K)
         END DO

         DO K = 1, levels%nsoil
           MH2O    = water%RUNSUB*domain%dt*(water%WCND(K)*domain%dzsnso(K))/WTSUB       ! mm
           water%SH2O(K) = water%SH2O(K) - MH2O/(domain%dzsnso(K)*1000.)
         END DO
    END IF

    ! Limit MLIQ to be greater than or equal to watmin.
    ! Get water needed to bring MLIQ equal WATMIN from lower layer.
    IF(options%OPT_DRN /= 1) THEN
      DO IZ = 1, levels%nsoil
         MLIQ(IZ) = water%SH2O(IZ)*domain%dzsnso(IZ)*1000.
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, levels%nsoil-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = levels%nsoil
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        water%RUNSUB   = water%RUNSUB - XS/domain%dt
        IF(options%OPT_DRN == 5) water%DEEPRECH = water%DEEPRECH - XS*1.E-3

      DO IZ = 1, levels%nsoil
        water%SH2O(IZ)     = MLIQ(IZ) / (domain%dzsnso(IZ)*1000.)
      END DO
    END IF

    !subsurface runoff using different schemes
    IF(options%OPT_DRN /= 2) THEN
      call SubsurfaceRunoff(domain, levels, options, parameters, water)
    END IF
  END IF
  
  ! Perform simplified subsurface checks when one-way coupling activated with opt_sub == 2
  IF (options%OPT_SUB == 2) THEN
    ! Loop through each soil level and check SMC is constant
    ! Preserve the ratios of SICE:SMC and SH2O:SMC
    DO K = 1,levels%nsoil
      IF (water%SMC(K) /= water%SMC_INIT(K)) THEN
        SICE_SMC_RATIO = water%SICE(K) / water%SMC(K)
        water%SMC(K)   = water%SMC_INIT(K)
        water%SICE(K)  = water%SMC(K) * SICE_SMC_RATIO
        water%SH2O(K)  = water%SMC(K) - water%SICE(K)
      ENDIF
    END DO
    
    ! Check to ensure ZWT is equal to initial water table height
    IF (water%ZWT /= parameters%ZWT_INIT) water%ZWT = parameters%ZWT_INIT
  END IF

  END SUBROUTINE SOILWATER

end module SoilWaterModule

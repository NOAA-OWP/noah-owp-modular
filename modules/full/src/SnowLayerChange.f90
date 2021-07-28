module SnowLayerChange

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType

  implicit none

contains

!== begin combine ==================================================================================

  SUBROUTINE COMBINE (domain, levels, parameters, energy, water)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water

! ------------------------ local variables ---------------------------
  INTEGER :: I,J,K,L               ! node indices
  INTEGER :: ISNOW_OLD             ! number of top snow layer
  INTEGER :: MSSI                  ! node index
  INTEGER :: NEIBOR                ! adjacent node selected for combination
  REAL    :: ZWICE                 ! total ice mass in snow
  REAL    :: ZWLIQ                 ! total liquid water in snow
  REAL    :: DZMIN(3)              ! minimum of top snow layer
!    DATA DZMIN /0.045, 0.05, 0.2/
  DATA DZMIN /0.025, 0.025, 0.1/  ! MB: change limit
!-----------------------------------------------------------------------

       ISNOW_OLD = water%ISNOW
       DO J = ISNOW_OLD+1,0
          IF (water%SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                water%SNLIQ(J+1)   = water%SNLIQ(J+1) + water%SNLIQ(J)
                water%SNICE(J+1)   = water%SNICE(J+1) + water%SNICE(J)
                domain%DZSNSO(J+1) = domain%DZSNSO(J+1) + domain%DZSNSO(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                 water%SNLIQ(J-1) = water%SNLIQ(J-1) + water%SNLIQ(J)
                 water%SNICE(J-1) = water%SNICE(J-1) + water%SNICE(J)
                 domain%DZSNSO(J-1) = domain%DZSNSO(J-1) + domain%DZSNSO(J) 
               ELSE
	         IF(water%SNICE(J) >= 0.) THEN
                  water%PONDING1 = water%SNLIQ(J)    ! ISNOW WILL GET SET TO ZERO BELOW; PONDING1 WILL GET 
                  water%SNEQV = water%SNICE(J)       ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                  water%SNOWH = domain%DZSNSO(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
		 ELSE   ! SNICE OVER-SUBLIMATED EARLIER
		  water%PONDING1 = water%SNLIQ(J) + water%SNICE(J)
		  IF(water%PONDING1 < 0.) THEN  ! IF SNICE AND SNLIQ SUBLIMATES REMOVE FROM SOIL
		   water%SICE(1) = MAX(0.0,water%SICE(1)+water%PONDING1/(domain%DZSNSO(1)*1000.))
                   water%PONDING1 = 0.0
		  END IF
                  water%SNEQV = 0.0
                  water%SNOWH = 0.0
		 END IF
                 water%SNLIQ(J) = 0.0
                 water%SNICE(J) = 0.0
                 domain%DZSNSO(J) = 0.0
               ENDIF
!                SH2O(1) = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
!                SICE(1) = SICE(1)+SNICE(J)/(DZSNSO(1)*1000.)
             ENDIF

             ! shift all elements above this down by one.
             IF (J > water%ISNOW+1 .AND. water%ISNOW < -1) THEN
                DO I = J, water%ISNOW+2, -1
                   energy%STC(I)   = energy%STC(I-1)
                   water%SNLIQ(I) = water%SNLIQ(I-1)
                   water%SNICE(I) = water%SNICE(I-1)
                   domain%DZSNSO(I)= domain%DZSNSO(I-1)
                END DO
             END IF
             water%ISNOW = water%ISNOW + 1
          END IF
       END DO

! to conserve water in case of too large surface sublimation
       IF(water%SICE(1) < 0.) THEN
          water%SH2O(1) = water%SH2O(1) + water%SICE(1)
          water%SICE(1) = 0.
       END IF

       IF(water%ISNOW ==0) RETURN   ! MB: get out if no longer multi-layer
       water%SNEQV  = 0.
       water%SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.
       DO J = water%ISNOW+1,0
             water%SNEQV = water%SNEQV + water%SNICE(J) + water%SNLIQ(J)
             water%SNOWH = water%SNOWH + domain%DZSNSO(J)
             ZWICE = ZWICE + water%SNICE(J)
             ZWLIQ = ZWLIQ + water%SNLIQ(J)
       END DO

! check the snow depth - all snow gone
! the liquid water assumes ponding on soil surface.
       IF (water%SNOWH < 0.025 .AND. water%ISNOW < 0 ) THEN ! MB: change limit
!       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          water%ISNOW  = 0
          water%SNEQV = ZWICE
          water%PONDING2 = ZWLIQ           ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
          IF(water%SNEQV <= 0.) water%SNOWH = 0. ! SHOULD BE ZERO; SEE ABOVE
       END IF
!       IF (SNOWH < 0.05 ) THEN
!          ISNOW  = 0
!          SNEQV = ZWICE
!          SH2O(1) = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)
!          IF(SNEQV <= 0.) SNOWH = 0.
!       END IF

! check the snow depth - snow layers combined
       IF (water%ISNOW < -1) THEN
          ISNOW_OLD = water%ISNOW
          MSSI     = 1
          DO I = ISNOW_OLD+1,0
             IF (domain%DZSNSO(I) < DZMIN(MSSI)) THEN
                IF (I == water%ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((domain%DZSNSO(I-1)+domain%DZSNSO(I)) < (domain%DZSNSO(I+1)+domain%DZSNSO(I))) NEIBOR = I-1
                END IF
                ! Node l and j are combined and stored as node j.
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF
                CALL COMBO (parameters,domain%DZSNSO(J), water%SNLIQ(J), water%SNICE(J), &
                   energy%STC(J), domain%DZSNSO(L), water%SNLIQ(L), water%SNICE(L), energy%STC(L) )
                ! Now shift all elements above this down one.
                IF (J-1 > water%ISNOW+1) THEN
                   DO K = J-1, water%ISNOW+2, -1
                      energy%STC(K) = energy%STC(K-1)
                      water%SNICE(K) = water%SNICE(K-1)
                      water%SNLIQ(K) = water%SNLIQ(K-1)
                      domain%DZSNSO(K) = domain%DZSNSO(K-1)
                   END DO
                END IF
                ! Decrease the number of snow layers
                water%ISNOW = water%ISNOW + 1
                IF (water%ISNOW >= -1) EXIT
             ELSE
                ! The layer thickness is greater than the prescribed minimum value
                MSSI = MSSI + 1
             END IF
          END DO
       END IF
  END SUBROUTINE COMBINE

!== begin divide ===================================================================================

  SUBROUTINE DIVIDE (domain, levels, parameters, energy, water)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water

! ------------------------ local variables ---------------------------
    INTEGER                                       :: J     !indices
    INTEGER                                       :: MSNO  !number of layer (top) to MSNO (bot)
    REAL                                          :: DRR   !thickness of the combined [m]
    REAL, DIMENSION(1:levels%nsnow)                :: DZ    !snow layer thickness [m]
    REAL, DIMENSION(1:levels%nsnow)                :: SWICE !partial volume of ice [m3/m3]
    REAL, DIMENSION(1:levels%nsnow)                :: SWLIQ !partial volume of liquid water [m3/m3]
    REAL, DIMENSION(1:levels%nsnow)                :: TSNO  !node temperature [k]
    REAL                                          :: ZWICE !temporary
    REAL                                          :: ZWLIQ !temporary
    REAL                                          :: PROPOR!temporary
    REAL                                          :: DTDZ  !temporary
! ----------------------------------------------------------------------

    DO J = 1,levels%nsnow
          IF (J <= ABS(water%ISNOW)) THEN
             DZ(J)    = domain%DZSNSO(J+water%ISNOW)
             SWICE(J) = water%SNICE(J+water%ISNOW)
             SWLIQ(J) = water%SNLIQ(J+water%ISNOW)
             TSNO(J)  = energy%STC(J+water%ISNOW)
          END IF
    END DO

       MSNO = ABS(water%ISNOW)
       IF (MSNO == 1) THEN
          ! Specify a new snow layer
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF
       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05
             CALL COMBO (parameters,DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))
             ! subdivide a new layer
             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  ! MB: change limit
!             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= parameters%TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF
             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO (parameters,DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF
       water%ISNOW = -MSNO

    DO J = water%ISNOW+1,0
             domain%DZSNSO(J) = DZ(J-water%ISNOW)
             water%SNICE(J) = SWICE(J-water%ISNOW)
             water%SNLIQ(J) = SWLIQ(J-water%ISNOW)
             energy%STC(J)   = TSNO(J-water%ISNOW)
    END DO
!    DO J = ISNOW+1,NSOIL
!    WRITE(*,'(I5,7F10.3)') J, DZSNSO(J), SNICE(J), SNLIQ(J),STC(J)
!    END DO

  END SUBROUTINE DIVIDE

!== begin combo ====================================================================================

  SUBROUTINE COMBO(parameters,DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------s
! input
    type (parameters_type), intent(in) :: parameters
    REAL, INTENT(IN)    :: DZ2   !nodal thickness of 2 elements being combined [m]
    REAL, INTENT(IN)    :: WLIQ2 !liquid water of element 2 [kg/m2]
    REAL, INTENT(IN)    :: WICE2 !ice of element 2 [kg/m2]
    REAL, INTENT(IN)    :: T2    !nodal temperature of element 2 [k]
    REAL, INTENT(INOUT) :: DZ    !nodal thickness of 1 elements being combined [m]
    REAL, INTENT(INOUT) :: WLIQ  !liquid water of element 1
    REAL, INTENT(INOUT) :: WICE  !ice of element 1 [kg/m2]
    REAL, INTENT(INOUT) :: T     !node temperature of element 1 [k]
! local 
    REAL                :: DZC   !total thickness of nodes 1 and 2 (DZC=DZ+DZ2).
    REAL                :: WLIQC !combined liquid water [kg/m2]
    REAL                :: WICEC !combined ice [kg/m2]
    REAL                :: TC    !combined node temperature [k]
    REAL                :: H     !enthalpy of element 1 [J/m2]
    REAL                :: H2    !enthalpy of element 2 [J/m2]
    REAL                :: HC    !temporary
!-----------------------------------------------------------------------
    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (parameters%CICE*WICE+parameters%CWAT*WLIQ) * (T-parameters%TFRZ)+parameters%HFUS*WLIQ
    H2= (parameters%CICE*WICE2+parameters%CWAT*WLIQ2) * (T2-parameters%TFRZ)+parameters%HFUS*WLIQ2
    HC = H + H2
    IF(HC < 0.)THEN
       TC = parameters%TFRZ + HC/(parameters%CICE*WICEC + parameters%CWAT*WLIQC)
    ELSE IF (HC.LE.parameters%HFUS*WLIQC) THEN
       TC = parameters%TFRZ
    ELSE
       TC = parameters%TFRZ + (HC - parameters%HFUS*WLIQC) / (parameters%CICE*WICEC + parameters%CWAT*WLIQC)
    END IF
    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC
  END SUBROUTINE COMBO

!== begin compact ==================================================================================

  SUBROUTINE COMPACT (domain, levels, parameters, energy, water)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water

! ------------------------ local variables ---------------------------
   REAL, PARAMETER     :: C2 = 21.e-3   ![m3/kg] ! default 21.e-3
   REAL, PARAMETER     :: C3 = 2.5e-6   ![1/s]  
   REAL, PARAMETER     :: C4 = 0.04     ![1/k]
   REAL, PARAMETER     :: C5 = 2.0      !
   REAL, PARAMETER     :: DM = 100.0    !upper Limit on destructive metamorphism compaction [kg/m3]
   REAL, PARAMETER     :: ETA0 = 0.8e+6 !viscosity coefficient [kg-s/m2] 
                                        !according to Anderson, it is between 0.52e6~1.38e6
   REAL :: BURDEN !pressure of overlying snow [kg/m2]
   REAL :: DDZ1   !rate of settling of snow pack due to destructive metamorphism.
   REAL :: DDZ2   !rate of compaction of snow pack due to overburden.
   REAL :: DDZ3   !rate of compaction of snow pack due to melt [1/s]
   REAL :: DEXPF  !EXPF=exp(-c4*(273.15-STC)).
   REAL :: TD     !STC - TFRZ [K]
   REAL :: PDZDTC !nodal rate of change in fractional-thickness due to compaction [fraction/s]
   REAL :: VOID   !void (1 - SNICE - SNLIQ)
   REAL :: WX     !water mass (ice + liquid) [kg/m2]
   REAL :: BI     !partial density of ice [kg/m3]
   INTEGER  :: J
! ----------------------------------------------------------------------

    BURDEN = 0.0
    DO J = water%ISNOW+1, 0
        WX      = water%SNICE(J) + water%SNLIQ(J)
        water%FICE(J) = water%SNICE(J) / WX
        VOID    = 1. - (water%SNICE(J)/parameters%DENICE + water%SNLIQ(J)/parameters%DENH2O) / domain%DZSNSO(J)
        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. water%SNICE(J) > 0.1) THEN
           BI = water%SNICE(J) / domain%DZSNSO(J)
           TD = MAX(0.,parameters%TFRZ-energy%STC(J))
           DEXPF = EXP(-C4*TD)

           ! Settling as a result of destructive metamorphism
           DDZ1 = -C3*DEXPF
           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))
           ! Liquid water term
           IF (water%SNLIQ(J) > 0.01*domain%DZSNSO(J)) DDZ1=DDZ1*C5

           ! Compaction due to overburden
           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 ! 0.5*WX -> self-burden

           ! Compaction occurring during melt
           IF (energy%IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(water%FICEOLD(J) - water%FICE(J))/MAX(1.E-6,water%FICEOLD(J)))
              DDZ3 = - DDZ3/domain%DT           ! sometimes too large
           ELSE
              DDZ3 = 0.
           END IF

           ! Time rate of fractional change in DZ (units of s-1)
           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*domain%DT
           PDZDTC = MAX(-0.5,PDZDTC)
           ! The change in DZ due to compaction
           domain%DZSNSO(J) = domain%DZSNSO(J)*(1.+PDZDTC)
           domain%DZSNSO(J)=max(domain%DZSNSO(J),water%SNICE(J)/parameters%DENICE+water%SNLIQ(J)/parameters%DENH2O)
        END IF
        ! Pressure of overlying snow
        BURDEN = BURDEN + WX
    END DO

  END SUBROUTINE COMPACT

end module SnowLayerChange

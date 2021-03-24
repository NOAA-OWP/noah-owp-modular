module SnowWaterRenew

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use SnowLayerChange

  implicit none

contains

!== begin snowfall =================================================================================

  SUBROUTINE SnowFall (domain, levels, parameters, energy, water, forcing)
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water
  type (   forcing_type)             :: forcing


! ------------------------ local variables ---------------------------
  INTEGER :: NEWNODE            ! 0-no new layers, 1-creating new layers
! ----------------------------------------------------------------------
    NEWNODE  = 0

! shallow snow / no layer
    IF(water%ISNOW == 0 .and. water%QSNOW > 0.)  THEN
      water%SNOWH = water%SNOWH + water%SNOWHIN * domain%DT
      water%SNEQV = water%SNEQV + water%QSNOW * domain%DT
    END IF

! creating a new layer 
    IF(water%ISNOW == 0  .AND. water%QSNOW>0. .AND. water%SNOWH >= 0.025) THEN !MB: change limit
!    IF(water%ISNOW == 0  .AND. water%QSNOW>0. .AND. water%SNOWH >= 0.05) THEN
      water%ISNOW    = -1
      NEWNODE  =  1
      domain%DZSNSO(0)= water%SNOWH
      water%SNOWH    = 0.
      energy%STC(0)   = MIN(273.16, forcing%sfctmp)   ! temporary setup
      water%SNICE(0) = water%SNEQV
      water%SNLIQ(0) = 0.
    END IF

! snow with layers
    IF(water%ISNOW <  0 .AND. NEWNODE == 0 .AND. water%QSNOW > 0.) then
         water%SNICE(water%ISNOW+1) = water%SNICE(water%ISNOW+1) + water%QSNOW * domain%DT
         domain%DZSNSO(water%ISNOW+1) = domain%DZSNSO(water%ISNOW+1) + water%SNOWHIN * domain%DT
    ENDIF

  END SUBROUTINE SnowFall

!== begin SnowRenew: originally snowh2o subroutine =============================================================

  SUBROUTINE SnowRenew (domain, levels, parameters, energy, water)
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resulting from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------dd

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water

! ------------------------ local variables ---------------------------
   INTEGER                     :: J         !do loop/array indices
   REAL                        :: QIN       !water flow into the element (mm/s)
   REAL                        :: QOUT      !water flow out of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after minus sublimation
   REAL, DIMENSION(-levels%snow+1:0) :: VOL_LIQ   !partial volume of liquid water in layer
   REAL, DIMENSION(-levels%snow+1:0) :: VOL_ICE   !partial volume of ice lens in layer
   REAL, DIMENSION(-levels%snow+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'
   IF(water%SNEQV == 0.) THEN
      water%SICE(1) = water%SICE(1) + (water%QSNFRO-water%QSNSUB)*domain%DT/(domain%DZSNSO(1)*1000.)  ! Barlage: SH2O->SICE v3.6
      IF(water%SICE(1) < 0.) THEN
         water%SH2O(1) = water%SH2O(1) + water%SICE(1)
         water%SICE(1) = 0.
      END IF
   END IF

! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.
   IF(water%ISNOW == 0 .and. water%SNEQV > 0.) THEN
      TEMP   = water%SNEQV
      water%SNEQV  = water%SNEQV - water%QSNSUB*domain%DT + water%QSNFRO*domain%DT
      PROPOR = water%SNEQV/TEMP
      water%SNOWH  = MAX(0.,PROPOR * water%SNOWH)
      water%SNOWH  = MIN(MAX(water%SNOWH,water%SNEQV/500.0),water%SNEQV/50.0) ! limit adjustment to a reasonable density
      IF(water%SNEQV < 0.) THEN
         water%SICE(1) = water%SICE(1) + water%SNEQV/(domain%DZSNSO(1)*1000.)
         water%SNEQV   = 0.
         water%SNOWH   = 0.
      END IF
      IF(water%SICE(1) < 0.) THEN
         water%SH2O(1) = water%SH2O(1) + water%SICE(1)
         water%SICE(1) = 0.
      END IF
   END IF

   IF(water%SNOWH <= 1.E-8 .OR. water%SNEQV <= 1.E-6) THEN
     water%SNOWH = 0.0
     water%SNEQV = 0.0
   END IF

! for deep snow
   IF ( water%ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
      WGDIF = water%SNICE(water%ISNOW+1) - water%QSNSUB*domain%DT + water%QSNFRO*domain%DT
      water%SNICE(water%ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. water%ISNOW <0) THEN
         CALL COMBINE (domain, levels, parameters, energy, water)
      ENDIF
      !KWM:  Subroutine COMBINE can change ISNOW to make it 0 again?
      IF ( water%ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
         water%SNLIQ(water%ISNOW+1) = water%SNLIQ(water%ISNOW+1) + water%QRAIN * domain%DT
         water%SNLIQ(water%ISNOW+1) = MAX(0., water%SNLIQ(water%ISNOW+1))
      ENDIF
      
   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the subroutine (Just set QSNBOT=0)?

! Porosity and partial volume
   DO J = water%ISNOW+1, 0
     VOL_ICE(J)      = MIN(1., water%SNICE(J)/(domain%DZSNSO(J)*parameters%DENICE))
     EPORE(J)        = 1. - VOL_ICE(J)
   END DO

   QIN = 0.
   QOUT = 0.
   DO J =  water%ISNOW+1, 0
     water%SNLIQ(J) = water%SNLIQ(J) + QIN
     VOL_LIQ(J) = water%SNLIQ(J)/(domain%DZSNSO(J)*parameters%DENH2O)
     QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*domain%DZSNSO(J))
!New snow water retention code
     IF(J == 0) THEN
       QOUT = MAX((VOL_LIQ(J)- EPORE(J))*domain%DZSNSO(J),parameters%SNOW_RET_FAC*domain%DT*QOUT)
     END IF
     QOUT = QOUT*parameters%DENH2O
     water%SNLIQ(J) = water%SNLIQ(J) - QOUT
!New snow water retention code
     IF((water%SNLIQ(J)/(water%SNICE(J)+water%SNLIQ(J))) > parameters%max_liq_mass_fraction) THEN
       QOUT = QOUT + (water%SNLIQ(J) - parameters%max_liq_mass_fraction/ &
                                       (1.0 - parameters%max_liq_mass_fraction)*water%SNICE(J))
       water%SNLIQ(J) = parameters%max_liq_mass_fraction/ &
                        (1.0 - parameters%max_liq_mass_fraction)*water%SNICE(J)
     ENDIF
     QIN = QOUT
   END DO
!New snow water retention code
   DO J = water%ISNOW+1, 0
     domain%DZSNSO(J) = MAX(domain%DZSNSO(J),water%SNLIQ(J)/parameters%DENH2O+water%SNICE(J)/parameters%DENICE)
   END DO

! Liquid water from snow bottom to soil
   water%QSNBOT = QOUT / domain%DT           ! mm/s

  END SUBROUTINE SnowRenew

end module SnowWaterRenew

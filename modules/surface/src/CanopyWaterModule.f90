module CanopyWaterModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType

  implicit none
  
contains

!== begin canwater =================================================================================

  SUBROUTINE CanopyHydrology (domain, levels, options, parameters, forcing, energy, water)
! ------------------------ code history ------------------------------
! canopy hydrology
! --------------------------------------------------------------------
  IMPLICIT NONE

! input
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (   options_type), intent(in) :: options
  type (parameters_type), intent(in) :: parameters
  type (     water_type)             :: water
  type (   forcing_type), intent(in) :: forcing
  type (    energy_type)             :: energy

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: QEVAC   !evaporation rate (mm/s)
  REAL                :: QDEWC   !dew rate (mm/s)
  REAL                :: QFROC   !frost rate (mm/s)
  REAL                :: QSUBC   !sublimation rate (mm/s)
  REAL                :: QMELTC  !melting rate of canopy snow (mm/s)
  REAL                :: QFRZC   !refreezing rate of canopy liquid water (mm/s)
  REAL                :: CANMAS  !total canopy mass (kg/m2)
! --------------------------------------------------------------------
! initialization

      water%ECAN    = 0.0

! --------------------------- liquid water ------------------------------

! maximum canopy water
      MAXLIQ =  parameters%CH2OP * (parameters%ELAI+ parameters%ESAI)

! evaporation, transpiration, and dew
      IF (.NOT. energy%FROZEN_CANOPY) THEN             ! Barlage: change to frozen_canopy
        water%ETRAN = MAX( energy%FCTR / parameters%HVAP, 0. )
        QEVAC = MAX( energy%FCEV / parameters%HVAP, 0. )
        QDEWC = ABS( MIN( energy%FCEV / parameters%HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
      ELSE
        water%ETRAN = MAX( energy%FCTR / parameters%HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( energy%FCEV / parameters%HSUB, 0. )
        QFROC = ABS( MIN( energy%FCEV / parameters%HSUB, 0. ) )
      ENDIF

! canopy water balance. for convenience allow dew to bring CANLIQ above
! maxh2o or else would have to re-adjust drip
       QEVAC = MIN(water%CANLIQ / domain%DT,QEVAC)
       water%CANLIQ=MAX(0.,water%CANLIQ + (QDEWC-QEVAC) * domain%DT)
       IF(water%CANLIQ <= 1.E-06) water%CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------

! for canopy ice
      MAXSNO = 6.6*(0.27+46./water%bdfall)*(parameters%ELAI+ parameters%ESAI)
      QSUBC = MIN(water%CANICE / domain%DT,QSUBC) 
      water%CANICE= MAX(0.,water%CANICE + (QFROC-QSUBC)*domain%DT)
      IF(water%CANICE.LE.1.E-6) water%CANICE = 0.
     
! wetted fraction of canopy
      IF(water%CANICE.GT.0.) THEN
           water%FWET = MAX(0.,water%CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           water%FWET = MAX(0.,water%CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      water%FWET = MIN(water%FWET, 1.) ** 0.667

! phase change
      QMELTC = 0.
      QFRZC = 0.

      IF(water%CANICE.GT.1.E-6.AND.energy%TV.GT.parameters%TFRZ) THEN
         QMELTC = MIN(water%CANICE / domain%DT,(energy%TV - parameters%TFRZ)* &
                  parameters%CICE * water%CANICE / parameters%DENICE/ &
                  (domain%DT * parameters%HFUS))
         water%CANICE = MAX(0.,water%CANICE - QMELTC * domain%DT)
         water%CANLIQ = MAX(0.,water%CANLIQ + QMELTC * domain%DT)
         energy%TV = water%FWET*parameters%TFRZ+(1.-water%FWET)*energy%TV
      ENDIF

      IF(water%CANLIQ.GT.1.E-6.AND.energy%TV.LT.parameters%TFRZ) THEN
         QFRZC  = MIN(water%CANLIQ / domain%DT,(parameters%TFRZ - energy%TV) * &
                  parameters%CWAT * water%CANLIQ / parameters%DENH2O / &
                  (domain%DT * parameters%HFUS))
         water%CANLIQ = MAX(0.,water%CANLIQ - QFRZC * domain%DT)
         water%CANICE = MAX(0.,water%CANICE + QFRZC * domain%DT)
         energy%TV = water%FWET * parameters%TFRZ + (1. - water%FWET)*energy%TV
      ENDIF

! total canopy water

      water%CMC = water%CANLIQ + water%CANICE

! total canopy evaporation

      water%ECAN = QEVAC + QSUBC - QDEWC - QFROC

  END SUBROUTINE CanopyHydrology

end module CanopyWaterModule

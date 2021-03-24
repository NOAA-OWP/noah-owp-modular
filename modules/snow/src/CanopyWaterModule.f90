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

!== begin precip_canopy_intercept  ==============================================================================

  SUBROUTINE CanopyWaterIntercept (domain, levels, options, parameters, forcing, energy, water)
! ----------------------------------------------------------------------
! calculate precip movement for canopy intercepted water 
! --------------------------------------------------------------------------------------------------

  IMPLICIT NONE
! input
  type (    domain_type), intent(in) :: domain
  type (    levels_type), intent(in) :: levels
  type (   options_type), intent(in) :: options
  type (parameters_type), intent(in) :: parameters
  type (     water_type)             :: water
  type (   forcing_type), intent(in) :: forcing
  type (    energy_type), intent(in) :: energy

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: FT      !temperature factor for unloading rate
  REAL                :: FV      !wind factor for unloading rate
  REAL                :: ICEDRIP !canice unloading
! --------------------------------------------------------------------

! initialization
      water%QINTR   = 0.
      water%QDRIPR  = 0.
      water%QTHROR  = 0.
      water%QINTR   = 0.
      water%QINTS   = 0.
      water%QDRIPS  = 0.
      water%QTHROS  = 0.
      water%QRAIN   = 0.0
      water%QSNOW   = 0.0
      water%SNOWHIN = 0.0
      ICEDRIP = 0.0

! --------------------------- liquid water ------------------------------

! maximum canopy water
      MAXLIQ =  parameters%CH2OP * (parameters%ELAI + parameters%ESAI)

! average interception and throughfall
      IF((parameters%ELAI + parameters%ESAI).GT.0.) THEN
         water%QINTR  = parameters%FVEG * water%RAIN * water%FP  ! interception capability
         water%QINTR  = MIN(water%QINTR, (MAXLIQ - water%CANLIQ)/domain%DT * &
                        (1.-EXP(-water%RAIN * domain%DT / MAXLIQ)) )
         water%QINTR  = MAX(water%QINTR, 0.)
         water%QDRIPR = parameters%FVEG * water%RAIN - water%QINTR
         water%QTHROR = (1. - parameters%FVEG) * water%RAIN
         water%CANLIQ=MAX(0.,water%CANLIQ + water%QINTR * domain%DT)
      ELSE
         water%QINTR  = 0.
         water%QDRIPR = 0.
         water%QTHROR = water%RAIN
	 IF(water%CANLIQ > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   water%QDRIPR = water%QDRIPR + water%CANLIQ / domain%DT
	   water%CANLIQ = 0.0
	 END IF
      END IF
      
! --------------------------- canopy ice ------------------------------

! for canopy ice
      MAXSNO = 6.6*(0.27+46./water%bdfall) * (parameters%ELAI+parameters%ESAI)

      IF((parameters%ELAI+parameters%ESAI).GT.0.) THEN
         water%QINTS = parameters%FVEG * water%SNOW * water%FP
         water%QINTS = MIN(water%QINTS, (MAXSNO - water%CANICE)/domain%DT * &
                      (1.-EXP(-water%SNOW * domain%DT / MAXSNO)) )
         water%QINTS = MAX(water%QINTS, 0.)
         FT = MAX(0.0,(energy%TV - 270.15) / 1.87E5)
         FV = SQRT(forcing%uwind*forcing%uwind+forcing%vwind*forcing%vwind) / 1.56E5
	 ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
	 ICEDRIP = MAX(0.,water%CANICE) * (FV+FT)    !MB: removed /DT
         water%QDRIPS = (parameters%FVEG * water%SNOW - water%QINTS) + ICEDRIP
         water%QTHROS = (1.0-parameters%FVEG) * water%SNOW
         water%CANICE= MAX(0.,water%CANICE + (water%QINTS - ICEDRIP) * domain%DT)
      ELSE
         water%QINTS  = 0.
         water%QDRIPS = 0.
         water%QTHROS = water%SNOW
	 IF(water%CANICE > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   water%QDRIPS = water%QDRIPS + water%CANICE / domain%DT
	   water%CANICE = 0.0
	 END IF
      ENDIF

! wetted fraction of canopy
      IF(water%CANICE.GT.0.) THEN
           water%FWET = MAX(0.,water%CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           water%FWET = MAX(0.,water%CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      water%FWET = MIN(water%FWET, 1.) ** 0.667

! total canopy water
      water%CMC = water%CANLIQ + water%CANICE
      
! rain or snow on the ground
      water%QRAIN   = water%QDRIPR + water%QTHROR
      water%QSNOW   = water%QDRIPS + water%QTHROS
      water%SNOWHIN = water%QSNOW / water%bdfall
      IF (domain%sfctyp == 2 .AND. energy%TG > parameters%TFRZ) THEN
         water%QSNOW   = 0.
         water%SNOWHIN = 0.
      END IF
      
  END SUBROUTINE CanopyWaterIntercept


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

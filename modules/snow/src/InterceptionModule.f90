module InterceptionModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains
  
  !== begin InterceptionMain ==================================================================================

  SUBROUTINE InterceptionMain (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in)    :: levels
    type (parameters_type)                :: parameters
    type (    domain_type)                :: domain
    type (    energy_type)                :: energy
    type (     water_type)                :: water
    type (   forcing_type)                :: forcing
    type (   options_type)                :: options
      
    ! Adjust LAI and SAI based on seasonality and snow depth
    call PHENOLOGY(domain, levels, options, parameters, forcing, energy, water)
    
    ! Compute interception and throughfall
    call CanopyWaterIntercept(domain, levels, options, parameters, forcing, energy, water)
      
  END SUBROUTINE InterceptionMain

  !== begin PHENOLOGY ==================================================================================

  SUBROUTINE PHENOLOGY (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type)             :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

  ! ------------------------ local variables ---------------------------
    real                                        :: DB      !thickness of canopy buried by snow (m)
    real                                        :: FB      !fraction of canopy buried by snow
    real                                        :: SNOWHC  !critical snow depth at which short vege fully covered by snow
    integer                                     :: K       !index
    integer                                     :: IT1,IT2 !interpolation months
    real                                        :: DAY     !current day of year ( 0 <= DAY < forcing%yearlen )
    real                                        :: WT1,WT2 !interpolation weights    
    real                                        :: T       !current month (1.00, ..., 12.00)
! --------------------------------------------------------------------------------------------------

! Adjust LAI and SAI depending on day of year
    IF (domain%croptype == 0) THEN
      IF ( options%dveg == 1 .or. options%dveg == 3 .or. options%dveg == 4 ) THEN
        IF (domain%lat >= 0.) THEN
          ! Northern Hemisphere
          DAY = forcing%julian
        ELSE
          ! Southern Hemisphere.  DAY is shifted by 1/2 year.
          DAY = MOD ( forcing%julian + ( 0.5 * forcing%yearlen ) , REAL(forcing%yearlen) )
        ENDIF
        T = 12. * DAY / REAL(forcing%yearlen)
        IT1 = T + 0.5
        IT2 = IT1 + 1
        WT1 = (IT1+0.5) - T
        WT2 = 1.-WT1
        IF (IT1 .LT.  1) IT1 = 12
        IF (IT2 .GT. 12) IT2 = 1
        parameters%LAI = (WT1 * parameters%LAIM(IT1)) + (WT2 * parameters%LAIM(IT2))
        parameters%SAI = (WT1 * parameters%SAIM(IT1)) + (WT2 * parameters%SAIM(IT2))
      ENDIF
      IF(options%dveg == 7 .or. options%dveg == 8 .or. options%dveg == 9) THEN
        parameters%SAI = MAX(0.05,0.1 * parameters%LAI)  ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
        IF (parameters%LAI < 0.05) parameters%SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
      ENDIF
      IF (parameters%SAI < 0.05) parameters%SAI = 0.0                    ! MB: SAI CHECK, change to 0.05 v3.6
      IF (parameters%LAI < 0.05 .OR. parameters%SAI == 0.0) parameters%LAI = 0.0  ! MB: LAI CHECK
      IF ( ( domain%vegtyp == parameters%ISWATER ) .OR. ( domain%vegtyp == parameters%ISBARREN ) .OR. &
        ( domain%vegtyp == parameters%ISICE   ) .or. ( parameters%urban_flag ) ) THEN
        parameters%LAI  = 0.
        parameters%SAI  = 0.
      ENDIF
    ENDIF   ! domain%croptype == 0
    
    ! Determine how much of canopy is buried by snow
    DB = MIN( MAX(water%SNOWH - parameters%HVB,0.), parameters%HVT - parameters%HVB )
    FB = DB / MAX(1.E-06, parameters%HVT - parameters%HVB)

    IF(parameters%HVT> 0. .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
      SNOWHC = parameters%HVT * EXP(-water%SNOWH/0.2)             !      changes to HVT in MPTABLE
      FB     = MIN(water%SNOWH, SNOWHC) / SNOWHC
    ENDIF
    ! Compute ELAI and ESAI based on fraction of canopy covered by snow
    parameters%ELAI = parameters%LAI * (1. - FB)
    parameters%ESAI = parameters%SAI * (1. - FB)
    IF (parameters%ESAI < 0.05 .and. domain%croptype == 0) parameters%ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
    IF ((parameters%ELAI < 0.05 .OR. parameters%ESAI == 0.0) .and. domain%croptype == 0) parameters%ELAI = 0.0  ! MB: LAI CHECK

    ! set growing season flag
    ! change from orginal code: 
    ! .or. (PGS > 2 .and. PGS < 7 .and. domain%croptype > 0) omitted because carbon_crop not implemented
    ! KSJ 2021-03-31
    IF ((energy%TV > parameters%TMIN .and. domain%croptype == 0)) THEN
      energy%IGS = 1.
    ELSE
      energy%IGS = 0.
    ENDIF

    ! Compute the fraction of each grid cell that is vegetated (FVEG)
    IF(options%dveg == 1 .or. options%dveg == 6 .or. options%dveg == 7) THEN
       parameters%FVEG = parameters%SHDFAC
       IF(parameters%FVEG <= 0.05) parameters%FVEG = 0.05
    ELSE IF (options%dveg == 2 .or. options%dveg == 3 .or. options%dveg == 8) THEN
       parameters%FVEG = 1. - EXP(-0.52 * (parameters%LAI + parameters%SAI))
       IF(parameters%FVEG <= 0.05) parameters%FVEG = 0.05
    ELSE IF (options%dveg == 4 .or. options%dveg == 5 .or. options%dveg == 9) THEN
       parameters%FVEG = parameters%SHDMAX
       IF(parameters%FVEG <= 0.05) parameters%FVEG = 0.05
    ENDIF
    IF(domain%CROPTYPE > 0) THEN
       parameters%FVEG = parameters%SHDMAX
       IF(parameters%FVEG <= 0.05) parameters%FVEG = 0.05
    ENDIF
    IF(parameters%urban_flag .OR. domain%VEGTYP == parameters%ISBARREN) parameters%FVEG = 0.0
    IF(parameters%ELAI + parameters%ESAI == 0.0) parameters%FVEG = 0.0


  END SUBROUTINE PHENOLOGY
  
  !== begin CanopyWaterIntercept  ==============================================================================

  SUBROUTINE CanopyWaterIntercept (domain, levels, options, parameters, forcing, energy, water)
  ! ----------------------------------------------------------------------
  ! calculate canopy interception, drip, and throughfall for snow and liquid water
  ! --------------------------------------------------------------------------------------------------

    IMPLICIT NONE
  ! input
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (   options_type), intent(in) :: options
    type (parameters_type)             :: parameters
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
    ICEDRIP       = 0.0

  ! --------------------------- liquid water ------------------------------

  ! Compute maximum storage of liquid water in canopy
    MAXLIQ =  parameters%CH2OP * (parameters%ELAI + parameters%ESAI)

  ! average interception and throughfall
    IF((parameters%ELAI + parameters%ESAI) > 0.) THEN
       water%QINTR  = parameters%FVEG * water%RAIN * water%FP  ! interception capability
       water%QINTR  = MIN(water%QINTR, (MAXLIQ - water%CANLIQ)/domain%DT * &
                      (1.-EXP(-water%RAIN * domain%DT / MAXLIQ)) )
       water%QINTR  = MAX(water%QINTR, 0.)
       water%QDRIPR = parameters%FVEG * water%RAIN - water%QINTR
       water%QTHROR = (1. - parameters%FVEG) * water%RAIN
       water%CANLIQ = MAX(0.,water%CANLIQ + water%QINTR * domain%DT)
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
  
end module InterceptionModule
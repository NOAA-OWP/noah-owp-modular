module WaterModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use ForcingType
  use EnergyType
  use SoilWaterModule
  use CanopyWaterModule
  use SnowWaterModule

  implicit none

contains

!== begin soilwater ================================================================================

  SUBROUTINE WaterMain (domain, levels, options, parameters, forcing, energy, water)
!---------------------------------------------------------------------
! Main module for all water components
!---------------------------------------------------------------------

  type (levels_type),     intent(in)   :: levels
  type (domain_type)                   :: domain
  type (parameters_type)               :: parameters
  type (options_type),    intent(in)   :: options
  type (water_type)                    :: water
  type (forcing_type),    intent(in)   :: forcing 
  type (energy_type)                   :: energy

! ------------------------ local variables ---------------------------
  real    :: dtheta_max = 0.0   ! maximum value of theta change in all levels
  real    :: totalwat   = 0.0   ! total soil water [mm]
  real    :: tw0        = 0.0   ! initial total soil water [mm]
  real    :: acsrf      = 0.0   ! accumulated surface runoff [mm]
  real    :: acsub      = 0.0   ! accumulated drainage [mm]
  real    :: acpcp      = 0.0   ! accumulated precipitation [mm]
  real    :: errwat     = 0.0   ! accumulated error [mm]
  logical :: done               ! logical check
  integer :: IZ 
  real, dimension(1:levels%nsoil) :: smcold        !previous timestep smc
!---------------------------------------------------------------------

! determine frozen canopy and/or ground
  IF (energy%TV .GT. parameters%TFRZ) THEN 
     energy%frozen_canopy = .false.
  ELSE
     energy%frozen_canopy = .true.
  END IF

  IF (energy%TG .GT. parameters%TFRZ) THEN
     energy%frozen_ground = .false.
  ELSE
     energy%frozen_ground = .true.
  END IF

  ! total soil  water at last timestep, used for soil water budget check
  tw0 = sum(domain%dzsnso(1:)*water%smc*1000.0) ! [mm]
  smcold = 0.0

  !---------------------------------------------------------------------
  ! call the canopy water routines
  !--------------------------------------------------------------------- 

    call CanopyHydrology (domain, levels, options, parameters, forcing, energy, water) ! original CANWATER

  !---------------------------------------------------------------------
  ! call the snow water routines
  !---------------------------------------------------------------------

  ! sublimation, frost, evaporation, and dew
     water%QSNSUB = 0.0
     IF (water%SNEQV > 0.) THEN
        water%QSNSUB = MIN(water%QVAP, water%SNEQV/domain%DT)
     ENDIF
     water%QSEVA = water%QVAP - water%QSNSUB
     water%QSNFRO = 0.0
     IF (water%SNEQV > 0.) THEN
        water%QSNFRO = water%QDEW
     ENDIF
     water%QSDEW = water%QDEW - water%QSNFRO

   CALL SnowWater (domain, levels, parameters, energy, water, forcing)

   IF(energy%FROZEN_GROUND) THEN
      water%SICE(1) =  water%SICE(1) + (water%QSDEW-water%QSEVA)*domain%DT/(domain%DZSNSO(1)*1000.)
      water%QSDEW = 0.0
      water%QSEVA = 0.0
      IF(water%SICE(1) < 0.) THEN
         water%SH2O(1) = water%SH2O(1) + water%SICE(1)
         water%SICE(1) = 0.
      END IF
   END IF

! convert units (mm/s -> m/s)
    !PONDING: melting water from snow when there is no layer
    water%QINSUR = (water%PONDING+water%PONDING1+water%PONDING2)/domain%DT * 0.001
!    QINSUR = PONDING/DT * 0.001
    IF(water%ISNOW == 0) THEN
       water%QINSUR = water%QINSUR+(water%QSNBOT + water%QSDEW + water%QRAIN) * 0.001
    ELSE
       water%QINSUR = water%QINSUR+(water%QSNBOT + water%QSDEW) * 0.001
    ENDIF
    water%QSEVA  = water%QSEVA * 0.001 

! For vegetation root
   DO IZ = 1, parameters%NROOT
       water%ETRANI(IZ) = water%ETRAN * water%BTRANI(IZ) * 0.001
    ENDDO

! #ifdef WRF_HYDRO
!       water%QINSUR = water%QINSUR+water%sfcheadrt/domain%DT*0.001  !sfcheadrt units (m)
! #endif

! For lake points
    IF (domain%IST == 2) THEN                                        ! lake
       water%RUNSRF = 0.
       IF(water%WSLAKE >= parameters%WSLMAX) water%RUNSRF = water%QINSUR*1000.             !mm/s
       water%WSLAKE=water%WSLAKE+(water%QINSUR-water%QSEVA)*1000.*domain%DT -water%RUNSRF*domain%DT   !mm
    ELSE                                                      ! soil

! For soil points
  !---------------------------------------------------------------------
  ! call the soil water routines
  !---------------------------------------------------------------------

    smcold = water%smc
    call SoilWater (domain, levels, options, parameters, water )   

!!!!! did not include groundwater part
    ENDIF

    water%smc = water%sh2o + water%sice
   
  !---------------------------------------------------------------------
  ! accumulate some fields and error checks
  !---------------------------------------------------------------------

    water%runsub = water%runsub + water%snoflow      ! add glacier outflow to subsurface runoff [mm/s]
    acsrf  = water%runsrf * domain%dt          ! accumulated surface runoff [mm]
    acsub  = water%runsub * domain%dt          ! accumulated drainage [mm]
    acpcp  = water%qinsur * domain%dt * 1000.0 ! accumulated precipitation [mm]
   
    dtheta_max = maxval(abs(water%smc-smcold))
!    if (dtheta_max .lt. 0.00001) done = .true.
   
    totalwat = sum(domain%dzsnso(1:levels%nsoil)*water%smc*1000.0)         ! total soil water [mm]
    errwat = acpcp - acsrf - acsub - (totalwat - tw0)  ! accum error [mm]

  END SUBROUTINE WaterMain   

end module WaterModule

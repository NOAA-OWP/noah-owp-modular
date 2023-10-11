module SnowWaterModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use SnowWaterRenew
  use SnowLayerChange

  implicit none
  
contains

!== begin snowwater main module ================================================================================

  SUBROUTINE SnowWater (domain, levels, parameters, energy, water, forcing)

! ----------------------------------------------------------------------
  IMPLICIT NONE

  type (    levels_type), intent(in) :: levels
  type (parameters_type), intent(in) :: parameters
  type (    domain_type)             :: domain
  type (    energy_type)             :: energy
  type (     water_type)             :: water
  type (   forcing_type)             :: forcing

! ------------------------ local variables ---------------------------
  INTEGER :: IZ,i
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------

! initialization
   water%SNOFLOW  = 0.0
   water%PONDING1 = 0.0
   water%PONDING2 = 0.0

   CALL SnowFall (domain, levels, parameters, energy, water, forcing)

! MB: do each if block separately
   IF(water%ISNOW < 0) &        ! when multi-layer
   CALL COMPACT (domain, levels, parameters, energy, water)

   IF(water%ISNOW < 0) &        !when multi-layer
   CALL COMBINE (domain, levels, parameters, energy, water)

   IF(water%ISNOW < 0) &        !when multi-layer
   CALL DIVIDE (domain, levels, parameters, energy, water)

   CALL SnowRenew (domain, levels, parameters, energy, water) ! originally named SNOWH2O

!set empty snow layers to zero
   do iz = -levels%nsnow+1, water%isnow
        water%snice(iz)  = 0.
        water%snliq(iz)  = 0.
        energy%stc(iz)   = 0.
        domain%dzsnso(iz)= 0.
        domain%zsnso(iz) = 0.
   enddo

!to obtain equilibrium state of snow in glacier region       
   IF( water%SNEQV > 5000.) THEN   ! 5000 mm -> maximum water depth
      BDSNOW             =  water%SNICE(0) / domain%DZSNSO(0)
      water%SNOFLOW      = water%SNEQV - 5000.
      water%SNICE(0)     = water%SNICE(0)  - water%SNOFLOW 
      domain%DZSNSO(0)   = domain%DZSNSO(0) - water%SNOFLOW/BDSNOW
      water%SNOFLOW      = water%SNOFLOW / domain%DT
   END IF

! sum up snow mass for layered snow
   IF(water%ISNOW < 0) THEN  ! MB: only do for multi-layer
       water%SNEQV = 0.
       DO IZ = water%ISNOW+1,0
             water%SNEQV = water%SNEQV + water%SNICE(IZ) + water%SNLIQ(IZ)
       ENDDO
   END IF

! Reset ZSNSO and layer thinkness DZSNSO
   DO IZ = water%ISNOW+1, 0
        domain%DZSNSO(IZ) = -domain%DZSNSO(IZ)
   END DO
   domain%DZSNSO(1) = domain%ZSOIL(1)
   DO IZ = 2,levels%nsoil
        domain%DZSNSO(IZ) = (domain%ZSOIL(IZ) - domain%ZSOIL(IZ-1))
   END DO
   domain%ZSNSO(water%ISNOW+1) = domain%DZSNSO(water%ISNOW+1)
   DO IZ = water%ISNOW+2 ,levels%nsoil
       domain%ZSNSO(IZ) = domain%ZSNSO(IZ-1) + domain%DZSNSO(IZ)
   ENDDO
   DO IZ = water%ISNOW+1 ,levels%nsoil
       domain%DZSNSO(IZ) = -domain%DZSNSO(IZ)
   END DO

   ! NWM3.0 parameter
   if (sum(water%SNICE(-levels%nsnow+1:0) + water%SNLIQ(-levels%nsnow+1:0)).gt.0.) then
      energy%SNOWT_AVG = SUM(energy%STC(-levels%nsnow+1:0)*(water%SNICE(-levels%nsnow+1:0)+water%SNLIQ(-levels%nsnow+1:0))) / &
                         SUM(water%SNICE(-levels%nsnow+1:0)+water%SNLIQ(-levels%nsnow+1:0))
   else
      energy%SNOWT_AVG = huge(1.)
   end if

  END SUBROUTINE SnowWater

end module SnowWaterModule

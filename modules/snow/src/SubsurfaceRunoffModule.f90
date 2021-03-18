module SubsurfaceRunoffModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType

  implicit none

  integer, parameter :: subsurface_runoff_groundwater_option      = 1
  integer, parameter :: subsurface_runoff_equiwatertable_option   = 2
  integer, parameter :: subsurface_runoff_freedrainage_option     = 3
  integer, parameter :: subsurface_runoff_BATS_option             = 4
  integer, parameter :: subsurface_runoff_MMFdrainage_option      = 5
  integer, parameter :: subsurface_runoff_VIC_option              = 6
  integer, parameter :: subsurface_runoff_XinAnJiang_option       = 7
  integer, parameter :: subsurface_runoff_Dynamic_VIC_option      = 8
 
contains

  subroutine SubsurfaceRunoff(domain, levels, options, parameters, water)

    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (   options_type), intent(in) :: options
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water

    select case (options%opt_drn)

      case(subsurface_runoff_groundwater_option)
        call subsurface_runoff_groundwater(parameters, water)

      case(subsurface_runoff_equiwatertable_option)
        call subsurface_runoff_equiwatertable(domain, levels, parameters, water)

      case(subsurface_runoff_freedrainage_option)
        call subsurface_runoff_freedrainage(water)

      case(subsurface_runoff_BATS_option)
        call subsurface_runoff_freedrainage(water)

      case(subsurface_runoff_MMFdrainage_option)
        call subsurface_runoff_MMFdrainage(water)

      case(subsurface_runoff_VIC_option)
        call subsurface_runoff_freedrainage(water)

      case(subsurface_runoff_XinAnJiang_option)
        call subsurface_runoff_freedrainage(water)

      case(subsurface_runoff_Dynamic_VIC_option)
        call subsurface_runoff_freedrainage(water)

    end select

  end subroutine SubsurfaceRunoff


  subroutine subsurface_runoff_groundwater(parameters, water)
!======== Subsurface runoff scheme from groundwater module Niu et al.2007 ====================
    implicit none
! input
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
! local
    real                               :: FFF !runoff decay factor (m-1)
    real                               :: RSBMX !baseflow coefficient [mm/s] 

! Groundwater discharge = subsurface runoff [mm/s]
! Extracted from GROUNDWATER subrountine
      FFF   = 6.0
      RSBMX = 5.0
      water%runsub = (1.0 - water%fcrmax) * RSBMX * EXP(-parameters%TIMEAN) * &
                                                    EXP(-FFF * (water%zwt - 2.0))
 
  end subroutine subsurface_runoff_groundwater

  subroutine subsurface_runoff_equiwatertable(domain, levels, parameters, water)
!======== Subsurface runoff scheme with an equilibrium water table Niu et al.2005 ============
    implicit none
! input
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
! local
    real                               :: FFF !runoff decay factor (m-1)
    real                               :: RSBMX !baseflow coefficient [mm/s] 
    real                               :: WTSUB  !sum of WCND(K)*DZSNSO(K)
    real                               :: MH2O   !water mass removal (mm)
    integer                            :: K   !do-loop index

    FFF   = 2.0
    RSBMX = 4.0
    CALL ZWTEQ (parameters, domain, levels, water)
    water%runsub = (1.0 - water%fcrmax) * RSBMX * EXP(-parameters%TIMEAN) * &
                                                  EXP(-FFF * water%zwt)   ! mm/s

  end subroutine subsurface_runoff_equiwatertable


  subroutine subsurface_runoff_freedrainage(water)
!======== Free drainage  ====================
    implicit none
! input
    type (     water_type)             :: water

    water%runsub = water%runsub + water%qdrain  !mm/s

  end subroutine subsurface_runoff_freedrainage


  subroutine subsurface_runoff_MMFdrainage(water)
!========  drainage with MMF shallowwatertable scheme  ====================
    implicit none
! input
    type (     water_type)             :: water

    water%runsub = water%runsub + water%qdrain  !mm/s

  end subroutine subsurface_runoff_MMFdrainage


  SUBROUTINE ZWTEQ (parameters, domain, levels, water)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (parameters_type), intent(in) :: parameters
  type (domain_type), intent(in) :: domain
  type (levels_type), intent(in) :: levels
  type (water_type)              :: water
! locals
  INTEGER :: K                      !do-loop index
  INTEGER, PARAMETER :: NFINE = 100 !no. of fine soil layers of 6m soil
  REAL    :: WD1                    !water deficit from coarse (4-L) soil moisture profile
  REAL    :: WD2                    !water deficit from fine (100-L) soil moisture profile
  REAL    :: DZFINE                 !layer thickness of the 100-L soil layers to 6.0 m
  REAL    :: TEMP                   !temporary variable
  REAL, DIMENSION(1:NFINE) :: ZFINE !layer-bottom depth of the 100-L soil layers to 6.0 m
! ----------------------------------------------------------------------

   WD1 = 0.
   DO K = 1,levels%soil
     WD1 = WD1 + (parameters%SMCMAX(1)-water%SH2O(K)) * domain%dzsnso(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-domain%zsoil(levels%soil)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   water%ZWT = -3.*domain%zsoil(levels%soil) - 0.001   ! initial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (water%ZWT-ZFINE(K))/parameters%PSISAT(1)
     WD2   = WD2 + parameters%SMCMAX(1)*(1.-TEMP**(-1./parameters%BEXP(1)))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        water%ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ

end module SubsurfaceRunoffModule

module ThermalPropertiesModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType

  implicit none

contains

  !== begin THERMOPROP ==================================================================================

  SUBROUTINE THERMOPROP (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    INTEGER                                      :: IZ      ! do-loop index
    ! ----------------------------------------------------------------------

    ! snow/soil layer thickness (m)
    DO IZ = water%ISNOW+1, levels%NSOIL
        IF(IZ == water%ISNOW+1) THEN
          domain%DZSNSO(IZ) = - domain%ZSNSO(IZ)
        ELSE
          domain%DZSNSO(IZ) = domain%ZSNSO(IZ-1) - domain%ZSNSO(IZ)
        END IF
    END DO
    
    ! compute snow thermal conductivity and heat capacity

    CALL CSNOW(domain, levels, options, parameters, forcing, energy, water)

    ! compute soil thermal properties
    
    CALL TDFCND(domain, levels, options, parameters, forcing, energy, water)

    ! Change for urban caase
    IF ( parameters%urban_flag ) THEN
       DO IZ = 1,levels%NSOIL
         energy%DF(IZ) = 3.24 ! where does this number come from? KSJ 2021-04-08
       END DO
    ENDIF

    ! compute lake thermal properties 
    ! (no consideration of turbulent mixing for this version)
    IF(domain%IST == 2) THEN
      DO IZ = 1, levels%NSOIL 
        IF(energy%STC(IZ) > parameters%TFRZ) THEN
          energy%HCPCT(IZ) = parameters%CWAT
          energy%DF(IZ)    = parameters%TKWAT  !+ KEDDY * CWAT 
        ELSE
          energy%HCPCT(IZ) = parameters%CICE
          energy%DF(IZ)    = parameters%TKICE 
        END IF
      END DO
    END IF

    ! combine a temporary variable used for melting/freezing of snow and frozen soil
    DO IZ = water%ISNOW+1,levels%NSOIL
      energy%FACT(IZ) = domain%DT / (energy%HCPCT(IZ) * domain%DZSNSO(IZ))
    END DO

    ! snow/soil interface
    IF(water%ISNOW == 0) THEN
       energy%DF(1) = (energy%DF(1) * domain%DZSNSO(1) + 0.35 * water%SNOWH) / &
                      (water%SNOWH + domain%DZSNSO(1)) 
    ELSE
       energy%DF(1) = (energy%DF(1) * domain%DZSNSO(1) + energy%DF(0) * domain%DZSNSO(0)) / &
                      (domain%DZSNSO(0) + domain%DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP
  
  !== begin CSNOW ==================================================================================

  SUBROUTINE CSNOW (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    INTEGER                            :: IZ      ! do-loop index 
    REAL, DIMENSION(-levels%NSNOW+1:0) :: BDSNOI  ! bulk density of snow(kg/m3) per layer
    REAL, DIMENSION(-levels%NSNOW+1:0) :: CVSNO   ! volumetric specific heat (j/m3/k)
    REAL, DIMENSION(-levels%NSNOW+1:0) :: TKSNO   ! snow thermal conductivity (j/m3/k)
    ! --------------------------------------------------------------------------------------------------

    DO IZ = water%ISNOW+1, 0
      ! Compute snowpack properties
      water%SNICEV(IZ)   = MIN(1.0, water%SNICE(IZ) / (domain%DZSNSO(IZ) * parameters%DENICE) )
      water%EPORE(IZ)    = 1.0 - water%SNICEV(IZ)
      water%SNLIQV(IZ)   = MIN(water%EPORE(IZ), water%SNLIQ(IZ) / (domain%DZSNSO(IZ) * parameters%DENH2O))
      BDSNOI(IZ) = (water%SNICE(IZ) + water%SNLIQ(IZ)) / domain%DZSNSO(IZ)
      
      ! volumetric specific heat
      CVSNO(IZ) = (parameters%CICE * water%SNICEV(IZ)) + (parameters%CWAT * water%SNLIQV(IZ))

      ! thermal conductivity of snow
      TKSNO(IZ) = 3.2217E-6 * BDSNOI(IZ)**2.0           ! Stieglitz(yen,1965)
    
      ! Assign DF and HCPCT to each snow layer
      energy%DF(IZ)    = TKSNO(IZ)
      energy%HCPCT(IZ) = CVSNO(IZ)
    END DO

  END SUBROUTINE CSNOW
  
  !== begin TDFCND ==================================================================================

  SUBROUTINE TDFCND (domain, levels, options, parameters, forcing, energy, water)
    IMPLICIT NONE

    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (    domain_type)             :: domain
    type (    energy_type)             :: energy
    type (     water_type)             :: water
    type (   forcing_type)             :: forcing
    type (   options_type)             :: options

    ! ------------------------ local variables ---------------------------
    INTEGER :: IZ       ! do-loop index
    REAL    :: AKE      ! kersten number? (undefined in previous versions)
    REAL    :: GAMMD    ! undefined in previous versions
    REAL    :: THKDRY   ! thermal conductivity for dry soil? (undefined in previous versions)
    REAL    :: THKSAT   ! undefined in previous versions
    REAL    :: THKS     ! thermal conductivity for the solids
    REAL    :: SATRATIO ! ratio of soil moisture content to max soil moisture
    REAL    :: XU       ! undefined in previous versions
    REAL    :: XUNFROZ  ! undefined in previous versions
    ! --------------------------------------------------------------------------------------------------

    ! --------------------------------------------------------------------------------------------------
    ! We now get quartz as an input argument (set in routine redprm):
    !      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,
    !     &             0.35, 0.60, 0.40, 0.82/
    ! --------------------------------------------------------------------------------------------------
    ! If the soil has any moisture content compute a partial sum/product
    ! otherwise use a constant value which works well with most soils
    ! --------------------------------------------------------------------------------------------------
    !  QUARTZ ....QUARTZ CONTENT (SOIL TYPE DEPENDENT)
    ! --------------------------------------------------------------------------------------------------
    ! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).

    !                                  PABLO GRUNMANN, 08/17/98
    ! Refs.:
    !      Farouki, O.T.,1986: Thermal properties of soils. Series on Rock
    !              and Soil Mechanics, Vol. 11, Trans Tech, 136 pp.
    !      Johansen, O., 1975: Thermal conductivity of soils. PH.D. Thesis,
    !              University of Trondheim,
    !      Peters-Lidard, C. D., et al., 1998: The effect of soil thermal
    !              conductivity parameterization on surface energy fluxes
    !              and temperatures. Journal of The Atmospheric Sciences,
    !              Vol. 55, pp. 1209-1224.
    ! --------------------------------------------------------------------------------------------------
    
    DO IZ = 1, levels%NSOIL
      water%SICE(IZ)   = water%SMC(IZ) - water%SH2O(IZ)
      energy%HCPCT(IZ) = water%SH2O(IZ) * parameters%CWAT + &
                         (1.0 - parameters%SMCMAX(IZ)) * parameters%CSOIL + &
                         (parameters%SMCMAX(IZ) - water%SMC(IZ)) * parameters%CPAIR + &
                         water%SICE(IZ) * parameters%CICE
      
      SATRATIO = water%SMC(IZ) / parameters%SMCMAX(IZ)
      
      THKS = (parameters%THKQTZ ** parameters%QUARTZ)* (parameters%THKO ** (1. - parameters%QUARTZ))

      ! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
      XUNFROZ = 1.0                       ! Prevent divide by zero (suggested by D. Mocko)
      IF(water%SMC(IZ) > 0.0) XUNFROZ = water%SH2O(IZ) / water%SMC(IZ)
    
      ! SATURATED THERMAL CONDUCTIVITY
      XU = XUNFROZ * parameters%SMCMAX(IZ)
      
      ! DRY DENSITY IN KG/M3
      THKSAT = THKS ** (1. - parameters%SMCMAX(IZ))* parameters%TKICE ** (parameters%SMCMAX(IZ) - XU) * parameters%THKW ** XU
    
      ! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
      GAMMD = (1. - parameters%SMCMAX(IZ)) * 2700.
      THKDRY = (0.135 * GAMMD + 64.7)/ (2700. - 0.947 * GAMMD) ! where do these values come from? KSJ 2021-04-08
    
      IF ( (water%SH2O(IZ) + 0.0005) <  water%SMC(IZ) ) THEN
        AKE = SATRATIO
      ELSE
        IF (SATRATIO >  0.1 ) THEN
          AKE = LOG10 (SATRATIO) + 1.0
        ELSE
          AKE = 0.0
        END IF
      END IF
      
      energy%DF(IZ) = AKE * (THKSAT - THKDRY) + THKDRY
      
    END DO

  END SUBROUTINE TDFCND
  
end module ThermalPropertiesModule
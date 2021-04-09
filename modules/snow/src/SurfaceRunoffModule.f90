module SurfaceRunoffModule

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType
  use SurfaceRunoffInfiltration
 
  implicit none
  
  integer, parameter :: surface_runoff_TOPMODEL_groundwater_option    = 1
  integer, parameter :: surface_runoff_TOPMODEL_equiwatertable_option = 2
  integer, parameter :: surface_runoff_freedrainage_option            = 3
  integer, parameter :: surface_runoff_BATS_option                    = 4
  integer, parameter :: surface_runoff_MMFan07_option                 = 5
  integer, parameter :: surface_runoff_VIC_option                     = 6
  integer, parameter :: surface_runoff_XinAnJiang_option              = 7
  integer, parameter :: surface_runoff_Dynamic_VIC_option             = 8
  
contains

  subroutine SurfaceRunoff(domain, levels, options, parameters, water)
  
    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (   options_type), intent(in) :: options
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water

    select case (options%opt_run)
    
      case(surface_runoff_TOPMODEL_groundwater_option)
          call surface_runoff_TOPMODEL_groundwater(parameters, water)
    
      case(surface_runoff_TOPMODEL_equiwatertable_option)
          call surface_runoff_TOPMODEL_equiwatertable(parameters, water)
    
      case(surface_runoff_freedrainage_option)
          call surface_runoff_freedrainage(parameters, domain, levels, water)
    
      case(surface_runoff_BATS_option)
          call surface_runoff_BATS(domain, levels, parameters, water)
    
      case(surface_runoff_MMFan07_option)
          call surface_runoff_MMFan07(parameters, water)

      case(surface_runoff_VIC_option)
          call surface_runoff_VIC(parameters, domain, levels, water)

      case(surface_runoff_XinAnJiang_option)
          call surface_runoff_XinAnJiang(parameters, domain, levels, water)

      case(surface_runoff_Dynamic_VIC_option)
          call surface_runoff_Dynamic_VIC(parameters, options, domain, levels, water)

    end select
    
  end subroutine SurfaceRunoff
    
  subroutine surface_runoff_TOPMODEL_groundwater(parameters, water)
    ! TOPMODEL with groundwater (Niu et al. 2007 JGR) 
    implicit none
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
    ! local
    real :: FFF
    real :: FSAT
    
      FFF = 6.0
      FSAT   = parameters%FSATMX*EXP(-0.5*FFF*(water%ZWT-2.0))
      IF(water%QINSUR > 0.) THEN
        water%RUNSRF = water%QINSUR * ( (1.0-water%FCR(1))*FSAT + water%FCR(1) )
        water%PDDUM  = water%QINSUR - water%RUNSRF                          ! m/s 
      END IF

  end subroutine surface_runoff_TOPMODEL_groundwater

  subroutine surface_runoff_TOPMODEL_equiwatertable(parameters, water)
    !TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR)
    implicit none
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
  !local
    real :: fff
    real :: fsat
  
      FFF   = 2.0
      FSAT   = parameters%FSATMX*EXP(-0.5*FFF*water%ZWT)
      IF(water%QINSUR > 0.) THEN
        water%RUNSRF = water%QINSUR * ( (1.0-water%FCR(1))*FSAT + water%FCR(1) )
        water%PDDUM  = water%QINSUR - water%RUNSRF                          ! m/s 
      END IF

  end subroutine surface_runoff_TOPMODEL_equiwatertable

  subroutine surface_runoff_freedrainage(parameters, domain, levels, water)
    ! original free drainage scheme
    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water

    call INFIL (parameters, domain, levels, water)

  end subroutine surface_runoff_freedrainage

  subroutine surface_runoff_BATS(domain, levels, parameters, water)

    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
    !local 
    real :: smctot
    real :: dztot
    real :: fsat
    integer :: k

    SMCTOT = 0. !2-m averaged soil moisture (m3/m3)
    DZTOT  = 0. !2-m soil depth (m)
    DO K = 1,levels%nsoil
       DZTOT   = DZTOT  + domain%dzsnso(K)
       SMCTOT  = SMCTOT + water%SMC(K)/parameters%SMCMAX(K)*domain%dzsnso(K)
       IF(DZTOT >= 2.0) EXIT
    END DO
    SMCTOT = SMCTOT/DZTOT
    FSAT   = MAX(0.01,SMCTOT) ** 4.        !BATS

    IF(water%QINSUR > 0.) THEN
       water%RUNSRF = water%QINSUR * ((1.0-water%FCR(1))*FSAT+water%FCR(1))
       water%PDDUM  = water%QINSUR - water%RUNSRF                       ! m/s
    END IF

  end subroutine surface_runoff_BATS

  subroutine surface_runoff_MMFan07(parameters, water)

    implicit none
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water
    !local
    real :: fff
    real :: fsat

    FFF = 6.0
    FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-water%ZWT,0.))
    IF(water%QINSUR > 0.) THEN
       water%RUNSRF = water%QINSUR * ( (1.0-water%FCR(1))*FSAT + water%FCR(1) )
       water%PDDUM  = water%QINSUR - water%RUNSRF                          ! m/s
    END IF

  end subroutine surface_runoff_MMFan07

  subroutine surface_runoff_VIC(parameters, domain, levels, water)

    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water

    call COMPUTE_VIC_SURFRUNOFF(parameters, domain, levels, water)

  end subroutine surface_runoff_VIC

  subroutine surface_runoff_XinAnJiang(parameters, domain, levels, water)
    
    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (     water_type)             :: water

    call COMPUTE_XAJ_SURFRUNOFF(parameters, domain, levels, water)

  end subroutine surface_runoff_XinAnJiang

  subroutine surface_runoff_Dynamic_VIC(parameters, options, domain, levels, water)

    implicit none
    type (    domain_type), intent(in) :: domain
    type (    levels_type), intent(in) :: levels
    type (parameters_type), intent(in) :: parameters
    type (   options_type), intent(in) :: options
    type (     water_type)             :: water

    call DYNAMIC_VIC(parameters, options, domain, levels, water)

  end subroutine surface_runoff_Dynamic_VIC

end module SurfaceRunoffModule

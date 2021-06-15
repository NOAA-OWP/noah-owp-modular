module SoilWaterRetentionCoeff

  use LevelsType
  use DomainType
  use OptionsType
  use ParametersType
  use WaterType

contains

!== begin wdfcnd1 ==================================================================================

  SUBROUTINE WDFCND1 (parameters,WDF,WCND,SMC,FCR,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input 
    type (parameters_type), intent(in) :: parameters

    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1


!== begin wdfcnd2 ==================================================================================

  SUBROUTINE WDFCND2 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
    type (parameters_type), intent(in) :: parameters

    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1,FACTR2
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = 0.05/parameters%SMCMAX(ISOIL)
    FACTR2 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(FACTR1,FACTR2)
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR2 ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2


end module SoilWaterRetentionCoeff

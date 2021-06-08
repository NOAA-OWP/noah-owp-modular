! This module includes calculations required by NOAH-MP that are NOT part
! of the traditional column model (i.e., the old monolith module_sf_noahmplsm.F)

! Output variables include:
! COSZ = cosine of the solar zenith angle
! JULIAN = day of year from 1 to 365 (or 366 for leap year)

module UtilitiesModule

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use OptionsType
  implicit none

contains

  SUBROUTINE UtilitiesMain
    
    IMPLICIT NONE
    
  END SUBROUTINE UtilitiesMain

  SUBROUTINE calc_declin ( nowdate, latitude, longitude, cosz, yearlen, julian)
    use kwm_date_utilities
  !---------------------------------------------------------------------
    IMPLICIT NONE
  !---------------------------------------------------------------------

    REAL, PARAMETER :: DEGRAD = 3.14159265/180.
    REAL, PARAMETER :: DPD    = 360./365.
  ! !ARGUMENTS:
    character(len=19), intent(in)  :: nowdate    ! YYYY-MM-DD_HH:mm:ss
    real,              intent(in)  :: latitude
    real,              intent(in)  :: longitude
    real,              intent(out) :: cosz
    integer,           intent(out) :: yearlen
    real,              intent(out) :: JULIAN

    REAL                           :: hrang
    real                           :: DECLIN
    real                           :: tloctim
    REAL                           :: OBECL
    REAL                           :: SINOB
    REAL                           :: SXLONG
    REAL                           :: ARG
    integer                        :: iyear
    integer                        :: iday
    integer                        :: ihour
    integer                        :: iminute
    integer                        :: isecond

    !
    ! Determine the number of days in the year
    !

    read(nowdate(1:4), '(I4)') iyear
    yearlen = 365
    if (mod(iyear,4) == 0) then
       yearlen = 366
       if (mod(iyear,100) == 0) then
          yearlen = 365
          if (mod(iyear,400) == 0) then
             yearlen = 366
             if (mod(iyear,3600) == 0) then
                yearlen = 365
             endif
          endif
       endif
    endif

    !
    ! Determine the Julian time (floating-point day of year).
    !

    call geth_idts(nowdate(1:10), nowdate(1:4)//"-01-01", iday)
    read(nowdate(12:13), *) ihour
    read(nowdate(15:16), *) iminute
    read(nowdate(18:19), *) isecond
    julian = real(iday) + real(ihour)/24.

  !
  ! for short wave radiation

    DECLIN=0.

  !-----OBECL : OBLIQUITY = 23.5 DEGREE.

    OBECL=23.5*DEGRAD
    SINOB=SIN(OBECL)

  !-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

    IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)*DEGRAD
    IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)*DEGRAD
    ARG=SINOB*SIN(SXLONG)
    DECLIN=ASIN(ARG)

    TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! Local time in hours
    tloctim = AMOD(tloctim+24.0, 24.0)
    HRANG=15.*(TLOCTIM-12.)*DEGRAD
    COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

  !KWM   write(wrf_err_message,10)DECDEG/DEGRAD
  !KWM10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',' ***')
  !KWM   CALL wrf_debug (50, wrf_err_message)

  END SUBROUTINE calc_declin
  
end module UtilitiesModule
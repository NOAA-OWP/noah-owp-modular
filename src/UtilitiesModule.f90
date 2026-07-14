! This module includes required calculations that are not part
! of the traditional column model

! Output variables include:
! COSZ = cosine of the solar zenith angle
! JULIAN = day of year from 1 to 365 (or 366 for leap year)

module UtilitiesModule

  use DomainType
  use EnergyType
  use ForcingType
  implicit none

contains

  SUBROUTINE UtilitiesMain (itime, domain, forcing, energy)
    
    IMPLICIT NONE
    
    integer, intent(in)                   :: ITIME ! current integer time step
    type (    domain_type)                :: domain
    type (   forcing_type)                :: forcing
    type (    energy_type)                :: energy
    
    ! local variables
    integer  :: idt ! change in time since beginning of run (in minutes)
    integer  :: now_year, now_month, now_day, now_hour, now_minute ! current date/time components
    idt = itime * (domain%dt / 60)

    ! calculate current date components from the start date components
    ! (parsed once at init) plus the integer length of run to current time
    call advance_datetime(domain%start_year, domain%start_month, domain%start_day, & ! in
                          domain%start_hour, domain%start_minute, idt,             & ! in
                          now_year, now_month, now_day, now_hour, now_minute)        ! out

    ! calculate current declination of direct solar radiation input
    call calc_declin_components(now_year, day_of_year(now_year, now_month, now_day), & ! in
                                now_hour, now_minute, 0,                             & ! in
                                domain%lat, domain%lon, domain%terrain_slope, domain%azimuth, & ! in
                                energy%cosz, energy%cosz_horiz, forcing%yearlen, forcing%julian)  ! out

  END SUBROUTINE UtilitiesMain


  integer function nfeb(year)
!
! Compute the number of days in February for the given year.
!
    implicit none
    integer, intent(in) :: year ! Four-digit year

    nfeb = 28 ! By default, February has 28 days ...
    if (mod(year,4) == 0) then  
       nfeb = 29  ! But every four years, it has 29 days ...
       if (mod(year,100) == 0) then
          nfeb = 28  ! Except every 100 years, when it has 28 days ...
          if (mod(year,400) == 0) then
             nfeb = 29  ! Except every 400 years, when it has 29 days ...
             if (mod(year,3600) == 0) then
                nfeb = 28  ! Except every 3600 years, when it has 28 days.
             endif
          endif
       endif
    endif
  end function nfeb


  ! ---------------------------------------------------------------------
  ! Date/time routines carrying dates as integer components
  ! (year, month, day, hour, minute).
  ! ---------------------------------------------------------------------

  ! The routines below implement the same calendar nfeb() defines: the
  ! proleptic Gregorian leap-year rules (div-4 yes, div-100 no, div-400
  ! yes) with the additional exception that years divisible by 3600 are
  ! NOT leap years. Day numbers count from 0001-01-01 = day 0.

  ! Day number of Jan 1 of the given year, from the count of leap years
  ! before it.
  integer function year_start_day(yr)
    implicit none
    integer, intent(in) :: yr

    year_start_day = 365*(yr - 1) + (yr - 1)/4 - (yr - 1)/100 + (yr - 1)/400 - (yr - 1)/3600
  end function year_start_day

  ! Day number of a civil date (valid for years >= 1).
  integer function days_from_civil(yr, mo, dy)
    implicit none
    integer, intent(in) :: yr, mo, dy

    days_from_civil = year_start_day(yr) + day_of_year(yr, mo, dy)
  end function days_from_civil

  ! Signed difference newdate - olddate in minutes, for 12-char
  ! YYYYMMDDHHmm date strings: each string is parsed once into integer
  ! components, then differenced via day-number arithmetic.
  integer function minutes_between(newdate, olddate)
    implicit none
    character(len=12), intent(in) :: newdate, olddate

    integer :: yr_n, mo_n, dy_n, hr_n, mi_n
    integer :: yr_o, mo_o, dy_o, hr_o, mi_o

    read(newdate, '(I4,4I2)') yr_n, mo_n, dy_n, hr_n, mi_n
    read(olddate, '(I4,4I2)') yr_o, mo_o, dy_o, hr_o, mi_o
    minutes_between = (days_from_civil(yr_n, mo_n, dy_n) - days_from_civil(yr_o, mo_o, dy_o))*1440 &
                      + (hr_n - hr_o)*60 + (mi_n - mi_o)
  end function minutes_between

  ! Inverse of days_from_civil.
  subroutine civil_from_days(days, yr, mo, dy)
    implicit none
    integer, intent(in)  :: days
    integer, intent(out) :: yr, mo, dy

    integer, parameter :: mday(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
    integer :: doy, mlen

    ! estimate the year from the mean Gregorian year length, then step to
    ! the year whose [start, start + length) interval contains the day
    yr = int((real(days, kind=8) + 0.5d0) / 365.2425d0) + 1
    do while (year_start_day(yr + 1) <= days)
       yr = yr + 1
    end do
    do while (year_start_day(yr) > days)
       yr = yr - 1
    end do

    doy = days - year_start_day(yr)
    mo = 1
    do
       mlen = mday(mo)
       if (mo == 2) mlen = nfeb(yr)
       if (doy < mlen) exit
       doy = doy - mlen
       mo = mo + 1
    end do
    dy = doy + 1
  end subroutine civil_from_days

  ! Advance a date/time by a signed number of minutes, in O(1) day-count
  ! arithmetic.
  subroutine advance_datetime(yr, mo, dy, hr, mi, dminutes, & ! in
                              yr2, mo2, dy2, hr2, mi2)        ! out
    implicit none
    integer, intent(in)  :: yr, mo, dy, hr, mi ! date/time components
    integer, intent(in)  :: dminutes           ! change in time (in minutes)
    integer, intent(out) :: yr2, mo2, dy2, hr2, mi2 ! resulting components

    integer :: total, minute_of_day, dday

    total = hr*60 + mi + dminutes
    minute_of_day = modulo(total, 1440)
    dday = (total - minute_of_day) / 1440
    call civil_from_days(days_from_civil(yr, mo, dy) + dday, yr2, mo2, dy2)
    hr2 = minute_of_day / 60
    mi2 = mod(minute_of_day, 60)
  end subroutine advance_datetime

  ! Day-of-year offset: days since Jan 1 of the same year (Jan 1 -> 0).
  integer function day_of_year(yr, mo, dy)
    implicit none
    integer, intent(in) :: yr, mo, dy

    integer, parameter :: cum(12) = (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)

    day_of_year = cum(mo) + dy - 1
    if (mo > 2) day_of_year = day_of_year + (nfeb(yr) - 28)
  end function day_of_year

  ! Solar geometry (cosine of the solar zenith angle for the terrain and
  ! for flat ground, year length, julian day) from integer date/time
  ! components. iday is the day-of-year offset as computed by day_of_year().
  SUBROUTINE calc_declin_components (iyear, iday, ihour, iminute, isecond, & ! in
                                     latitude, longitude, slope, azimuth,  & ! in
                                     cosz, cosz_horiz, yearlen, julian)      ! out
    IMPLICIT NONE

    integer, intent(in)  :: iyear      ! year of timestep
    integer, intent(in)  :: iday       ! day-of-year offset of timestep (Jan 1 = 0)
    integer, intent(in)  :: ihour      ! hour of timestep
    integer, intent(in)  :: iminute    ! minute of timestep
    integer, intent(in)  :: isecond    ! second of timestep
    real,    intent(in)  :: latitude   ! latitude (degrees)
    real,    intent(in)  :: longitude  ! longitude (degrees)
    real,    intent(in)  :: slope      ! slope (degrees)
    real,    intent(in)  :: azimuth    ! azimuth (degrees)
    real,    intent(out) :: cosz       ! cosine of solar zenith angle
    real,    intent(out) :: cosz_horiz ! cosine of solar zenith angle for flat ground
    integer, intent(out) :: yearlen    ! year length
    real,    intent(out) :: JULIAN     ! julian day

    ! ------------------------ local variables ---------------------------
    REAL, PARAMETER :: DEGRAD = 3.14159265/180. ! convert degrees to radians
    REAL, PARAMETER :: DPD    = 360./365.

    REAL            :: hrang    ! hour angle (radians)
    real            :: DECLIN   ! solar declination (radians)
    real            :: tloctim  ! local time in hours
    REAL            :: OBECL    ! obliquity (radians)
    REAL            :: SINOB    ! sine of obliquity
    REAL            :: SXLONG   ! longitude of sun from vernal equinox (radians)
    REAL            :: ARG      ! temporary var for computing declination

    REAL            :: nvx     ! x value of normal vector
    REAL            :: nvy     ! y value of normal vector
    REAL            :: nvz     ! z value of normal vector
    REAL            :: svx     ! x value of solar vector
    REAL            :: svy     ! y value of solar vector
    REAL            :: svz     ! z value of solar vector
    ! ------------------------ end local variables ---------------------------

    ! Determine the number of days in the year
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

    ! Determine the Julian time (floating-point day of year)
    julian = real(iday) + real(ihour)/24.

  !
  ! for short wave radiation
    DECLIN = 0.

  !-----OBECL : OBLIQUITY = 23.5 DEGREE.

    OBECL = 23.5*DEGRAD
    SINOB = SIN(OBECL)

  !-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

    IF(JULIAN >= 80.) SXLONG = DPD * (JULIAN-80.) * DEGRAD
    IF(JULIAN < 80.)  SXLONG = DPD * (JULIAN+285.)* DEGRAD
    ARG = SINOB * SIN(SXLONG)
    DECLIN = ASIN(ARG)

    TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! Local time in hours
    TLOCTIM = MOD(TLOCTIM+24.0, 24.0)
    HRANG=15. * (TLOCTIM-12.) * DEGRAD

    ! The below code is new to adjust COSZ for slope and aspect
    ! It uses the approach of Corripio (2003), "Vectorial algebra algorithms for calculating terrain parameters from
    ! DEMs and solar radiation modelling in mountainous terrain", Int. J. Geographical Information Science
    ! Also implemented in Corripio's 'insol' R package

    ! First compute the normal vector for the slope and azimuth
    nvx = sin(azimuth*DEGRAD) * sin(slope*DEGRAD)
    nvy = -cos(azimuth*DEGRAD) * sin(slope*DEGRAD)
    nvz = cos(slope*DEGRAD)

    ! Next compute the unit vector for the sun
    svx = -sin(HRANG) * cos(DECLIN)
    svy = (sin(latitude*DEGRAD) * cos(HRANG) * cos(DECLIN)) - (cos(latitude*DEGRAD) * sin(DECLIN))
    svz = (cos(latitude*DEGRAD) * cos(HRANG) * cos(DECLIN)) + (sin(latitude*DEGRAD) * sin(DECLIN))

    ! Compute COSZ using the dot product of the two vectors
    ! Simplified here algebraically
    COSZ = (nvx * svx) + (nvy * svy) + (nvz * svz)

    ! We also need to know the flat ground COSZ to correct incoming solar radiation
    ! which is typically assumed to be measured/modeled for a flat surface
    ! for a horizontal plane, nvx = 0, nvy = 0, and nvz = 1 (svx, svy, svz are as calculated previously)
    nvx = 0.0
    nvy = 0.0
    nvz = 1.0
    COSZ_HORIZ = (nvx * svx) + (nvy * svy) + (nvz * svz)

  END SUBROUTINE calc_declin_components

end module UtilitiesModule

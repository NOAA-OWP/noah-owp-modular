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

    ! calculate current date components from start date components (parsed
    ! once at init) + integer length of run to current time; no string
    ! parsing on this path (issue #131)
    call advance_datetime(domain%start_year, domain%start_month, domain%start_day, & ! in
                          domain%start_hour, domain%start_minute, idt,             & ! in
                          now_year, now_month, now_day, now_hour, now_minute)        ! out

    ! keep the string 'nowdate' current for output and other consumers
    ! (formatted once; never re-parsed here)
    write(domain%nowdate, '(I4.4,4I2.2)') now_year, now_month, now_day, now_hour, now_minute

    ! calculate current declination of direct solar radiation input
    call calc_declin_components(now_year, day_of_year(now_year, now_month, now_day), & ! in
                                now_hour, now_minute, 0,                             & ! in
                                domain%lat, domain%lon, domain%terrain_slope, domain%azimuth, & ! in
                                energy%cosz, energy%cosz_horiz, forcing%yearlen, forcing%julian)  ! out

  END SUBROUTINE UtilitiesMain

  ! calculate integer day of year for current time
  ! AW: not sure this is ever called in the code at present
  subroutine geth_idts (newdate, olddate, idt)
    
    implicit none

    character (len=*) , intent(in)  :: newdate ! current date of run
    character (len=*) , intent(in)  :: olddate ! first day of current year
    integer           , intent(out) :: idt     ! integer day of year

    ! ------------------------ local variables ---------------------------
    character(len=24)  :: ndate
    character(len=24)  :: odate
    character (len=24) :: tdate
    integer :: olen  ! length of olddate
    integer :: nlen  ! length of newdate
    integer :: yrnew ! year associated with "ndate"
    integer :: monew ! month associated with "ndate"
    integer :: dynew ! day associated with "ndate"
    integer :: hrnew ! hour associated with "ndate"
    integer :: minew ! minute associated with "ndate"
    integer :: scnew ! second associated with "ndate"
    integer :: frnew
    integer :: yrold ! year associated with "odate"
    integer :: moold ! month associated with "odate"
    integer :: dyold ! day associated with "odate"
    integer :: hrold ! hour associated with "odate"
    integer :: miold ! minute associated with "odate"
    integer :: scold ! second associated with "odate"
    integer :: frold
    integer :: mday(12) ! list assigning the number of days in each month
    integer :: i
    integer :: newdys
    integer :: olddys
    logical :: npass
    logical :: opass
    integer :: isign
    integer :: ifrc
    logical :: punctuated

    olen = len(olddate)
    nlen = len(newdate)
    if (nlen /= olen) then
       write(*,'("GETH_IDTS: NLEN /= OLEN: ", A, 3x, A)') newdate(1:nlen), olddate(1:olen)
       call abort
    endif

    if (olddate > newdate) then
       isign = -1

       ifrc = olen
       olen = nlen
       nlen = ifrc

       ndate = olddate
       odate = newdate
    else
       isign = 1
       ndate = newdate
       odate = olddate
    end if

    ! Assign the number of days in a months
    mday( 1) = 31
    mday( 2) = 28
    mday( 3) = 31
    mday( 4) = 30
    mday( 5) = 31
    mday( 6) = 30
    mday( 7) = 31
    mday( 8) = 31
    mday( 9) = 30
    mday(10) = 31
    mday(11) = 30
    mday(12) = 31

    !  Determine if the date is "punctuated" or just a string of numbers.
    if ( odate(5:5) == "-") then
       punctuated = .TRUE.
    else
       punctuated = .FALSE.
    endif


    !  Break down old and new hdates into parts
    hrold = 0
    miold = 0
    scold = 0
    frold = 0

    hrnew = 0
    minew = 0
    scnew = 0
    frnew = 0

    read(odate(1:4),  '(i4)') yrold
    read(ndate(1:4),  '(i4)') yrnew

    if (punctuated) then

       ! Break down old hdate into parts
       read(odate(6:7),  '(i2)') moold
       read(odate(9:10), '(i2)') dyold
       if (olen >= 13) then
          read(odate(12:13),'(i2)') hrold
          if (olen >= 16) then
             read(odate(15:16),'(i2)') miold
             if (olen >= 19) then
                read(odate(18:19),'(i2)') scold
                if (olen > 20) then
                   if (olen == 21) then
                      read(odate(21:21),'(i1)') frold
                   else if (olen == 22) then
                      read(odate(21:22),'(i2)') frold
                   else if (olen == 23) then
                      read(odate(21:23),'(i3)') frold
                   else if (olen == 24) then
                      read(odate(21:24),'(i4)') frold
                   endif
                end if
             end if
          end if
       end if
       
       !  Break down new hdate into parts

       read(ndate(6:7),  '(i2)') monew
       read(ndate(9:10), '(i2)') dynew
       if (nlen >= 13) then
          read(ndate(12:13),'(i2)') hrnew
          if (nlen >= 16) then
             read(ndate(15:16),'(i2)') minew
             if (nlen >= 19) then
                read(ndate(18:19),'(i2)') scnew
                if (nlen > 20) then
                   read(ndate(21:nlen),*) frnew
                end if
             end if
          end if
       end if
    else

      !  Break down old hdate into parts

       read(odate(5:6),  '(i2)') moold
       read(odate(7:8), '(i2)') dyold
       if (olen >= 10) then
          read(odate(9:10),'(i2)') hrold
          if (olen >= 12) then
             read(odate(11:12),'(i2)') miold
             if (olen >= 14) then
                read(odate(13:14),'(i2)') scold
                if (olen >= 15) then
                   read(odate(15:olen),*) frold
                end if
             end if
          end if
       end if
       
       !  Break down new hdate into parts

       read(ndate(5:6),  '(i2)') monew
       read(ndate(7:8), '(i2)') dynew
       if (nlen >= 10) then
          read(ndate(9:10),'(i2)') hrnew
          if (nlen >= 12) then
             read(ndate(11:12),'(i2)') minew
             if (nlen >= 14) then
                read(ndate(13:14),'(i2)') scnew
                if (nlen >= 15) then
                   read(ndate(15:nlen),*) frnew
                end if
             end if
          end if
       end if
    endif

    ! Check that the dates make sense.
    npass = .true.
    opass = .true.

    ! Check that the month of NDATE makes sense.
    if ((monew > 12).or.(monew < 1)) then
       print*, 'GETH_IDTS:  Month of NDATE = ', monew
       npass = .false.
    end if

    ! Check that the month of ODATE makes sense.
    if ((moold > 12).or.(moold < 1)) then
       print*, 'GETH_IDTS:  Month of ODATE = ', moold
       opass = .false.
    end if

    ! Check that the day of NDATE makes sense.
    if (monew /= 2) then
       ! ...... For all months but February
       if ((dynew > mday(monew)).or.(dynew < 1)) then
          print*, 'GETH_IDTS:  Day of NDATE = ', dynew
          npass = .false.
       end if
    else if (monew == 2) then
       ! ...... For February
       if ((dynew > nfeb(yrnew)).or.(dynew < 1)) then
          print*, 'GETH_IDTS:  Day of NDATE = ', dynew
          npass = .false.
       end if
    endif

    !  Check that the day of ODATE makes sense.
    if (moold /= 2) then
       ! ...... For all months but February
       if ((dyold > mday(moold)).or.(dyold < 1)) then
          print*, 'GETH_IDTS:  Day of ODATE = ', dyold
          opass = .false.
       end if
    else if (moold == 2) then
       ! ....... For February
       if ((dyold > nfeb(yrold)).or.(dyold < 1)) then
          print*, 'GETH_IDTS:  Day of ODATE = ', dyold
          opass = .false.
       end if
    end if

    ! Check that the hour of NDATE makes sense.
    if ((hrnew > 23).or.(hrnew < 0)) then
       print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
       npass = .false.
    end if

    ! Check that the hour of ODATE makes sense.
    if ((hrold > 23).or.(hrold < 0)) then
       print*, 'GETH_IDTS:  Hour of ODATE = ', hrold
       opass = .false.
    end if

    ! Check that the minute of NDATE makes sense.
    if ((minew > 59).or.(minew < 0)) then
       print*, 'GETH_IDTS:  Minute of NDATE = ', minew
       npass = .false.
    end if

    ! Check that the minute of ODATE makes sense.
    if ((miold > 59).or.(miold < 0)) then
       print*, 'GETH_IDTS:  Minute of ODATE = ', miold
       opass = .false.
    end if

    ! Check that the second of NDATE makes sense.
    if ((scnew > 59).or.(scnew < 0)) then
       print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
       npass = .false.
    end if

    ! Check that the second of ODATE makes sense.
    if ((scold > 59).or.(scold < 0)) then
       print*, 'GETH_IDTS:  Second of ODATE = ', scold
       opass = .false.
    end if

    if (.not. npass) then
       print*, 'Screwy NDATE: ', ndate(1:nlen)
       call abort()
    end if

    if (.not. opass) then
       print*, 'Screwy ODATE: ', odate(1:olen)
       call abort()
    end if

    ! Date Checks are completed.  Continue.

    ! Compute number of days from 1 January ODATE, 00:00:00 until ndate
    ! Compute number of hours from 1 January ODATE, 00:00:00 until ndate
    ! Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

    newdys = 0
    do i = yrold, yrnew - 1
       newdys = newdys + 337 + nfeb(i)
    end do

    if (monew  >  1) then
       mday(2) = nfeb(yrnew)
       do i = 1, monew - 1
          newdys = newdys + mday(i)
       end do
       mday(2) = 28
    end if

    newdys = newdys + dynew - 1

    ! Compute number of hours from 1 January ODATE, 00:00:00 until odate
    ! Compute number of minutes from 1 January ODATE, 00:00:00 until odate

    olddys = 0

    if (moold  >  1) then
       mday(2) = nfeb(yrold)
       do i = 1, moold - 1
          olddys = olddys + mday(i)
       end do
       mday(2) = 28
    end if

    olddys = olddys + dyold -1

    !  Determine the time difference

    idt = (newdys - olddys)
    if (punctuated) then
       if (olen > 10) then
          idt = idt*24 + (hrnew - hrold)
          if (olen > 13) then
             idt = idt*60 + (minew - miold)
             if (olen > 16) then
                idt = idt*60 + (scnew - scold)
                if (olen > 20) then
                   ifrc = olen-20
                   ifrc = 10**ifrc
                   idt = idt * ifrc + (frnew-frold)
                endif
             endif
          endif
       endif
    else
       if (olen > 8) then
          idt = idt*24 + (hrnew - hrold)
          if (olen > 10) then
             idt = idt*60 + (minew - miold)
             if (olen > 12) then
                idt = idt*60 + (scnew - scold)
                if (olen > 14) then
                   ifrc = olen-14
                   ifrc = 10**ifrc
                   idt = idt * ifrc + (frnew-frold)
                endif
             endif
          endif
       endif
    endif

    if (isign  ==  -1) then
       idt = idt * isign
    end if

  end subroutine geth_idts


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

  integer function nmdays(hdate)
!
! Compute the number of days in the month of given date hdate.
!
    implicit none
    character(len=*), intent(in) :: hdate

    integer :: year, month
    integer, dimension(12), parameter :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

    if (hdate(5:5) == "-") then
       read(hdate(1:7), '(I4,1x,I2)') year, month
    else
       read(hdate(1:6), '(I4,I2)') year, month
    endif

    if (month == 2) then
       nmdays = nfeb(year)
    else
       nmdays = ndays(month)
    endif
  end function nmdays

  ! ---------------------------------------------------------------------
  ! Integer-component date/time routines (issue #131). These carry dates
  ! as integer components (year, month, day, hour, minute) so the model
  ! main loop can avoid per-timestep string parsing and formatting.
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

  ! Advance a date/time by a signed number of minutes. O(1) day-count
  ! arithmetic, unlike geth_newdate's day-at-a-time loop.
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

  ! Solar geometry from integer date/time components: the body of
  ! calc_declin minus all string parsing. iday is the day-of-year offset
  ! as computed by day_of_year().
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
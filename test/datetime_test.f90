! Table-driven unit tests for the date handling used in the main model loop
! (issue #131). Tests are expressed as tables of inputs, expected outputs,
! and a descriptive label; a driver loop per table runs each case through a
! thin adapter and reports PASS/FAIL.
!
! The adapters (advance_date, day_of_year_offset, declin_at) are the contract:
! today they wrap the string-based routines in UtilitiesModule; a later
! integer-component reimplementation only changes the adapter bodies, and the
! same tables must still pass.
!
! Expected values were derived independently of the implementation:
! calendar arithmetic with Python datetime (proleptic Gregorian), solar
! geometry from the documented formulas evaluated in double precision.

program datetime_test

  use UtilitiesModule
  implicit none

  ! ---- case record types: inputs, expected outputs, label -----------------

  ! advance a date/time by a number of minutes
  type :: advance_case
     character(len=64) :: label
     logical :: epass                     ! .true. = expected to pass; .false. = known failure of existing code
     integer :: yr, mo, dy, hr, mi        ! input date/time components
     integer :: dminutes                  ! input: minutes to advance (may be negative)
     integer :: eyr, emo, edy, ehr, emi   ! expected date/time components
  end type advance_case

  ! days since Jan 1 of the same year (Jan 1 -> 0)
  type :: doy_case
     character(len=64) :: label
     logical :: epass                     ! .true. = expected to pass; .false. = known failure of existing code
     integer :: yr, mo, dy                ! input date components
     integer :: eoffset                   ! expected day-of-year offset
  end type doy_case

  ! solar geometry for a date/time given as components
  type :: declin_case
     character(len=64) :: label
     logical :: epass                     ! .true. = expected to pass; .false. = known failure of existing code
     integer :: yr, mo, dy, hr, mi, sc    ! input date/time components
     real    :: lat, lon, slope, azimuth  ! input location/terrain (degrees)
     real    :: ecosz, ecosz_horiz        ! expected cosine of solar zenith angle
     integer :: eyearlen                  ! expected days in year
     real    :: ejulian                   ! expected floating-point day of year
     real    :: tol                       ! tolerance for real comparisons
  end type declin_case

  ! composed main-loop behavior: advance a start date/time by dminutes, then
  ! solar geometry at the result (seconds = 0, as in the model main loop)
  type :: composed_case
     character(len=64) :: label
     logical :: epass                     ! .true. = expected to pass; .false. = known failure of existing code
     integer :: yr, mo, dy, hr, mi        ! input start date/time components
     integer :: dminutes                  ! input: minutes to advance
     real    :: lat, lon, slope, azimuth  ! input location/terrain (degrees)
     real    :: ecosz, ecosz_horiz        ! expected cosine of solar zenith angle
     integer :: eyearlen                  ! expected days in year
     real    :: ejulian                   ! expected floating-point day of year
     real    :: tol                       ! tolerance for real comparisons
  end type composed_case

  ! ---- implementations under test -------------------------------------------
  ! Every table runs against each implementation listed for it. A string
  ! adapter is removed only when the string routine it wraps is removed
  ! from the codebase; while both implementations exist, both are covered.

  integer, parameter :: IMPL_STRING = 1, IMPL_COMPONENT = 2
  character(len=11), parameter :: impl_name(2) = (/ "[string]   ", "[component]" /)

  integer, parameter :: advance_impls(*)  = (/ IMPL_STRING, IMPL_COMPONENT /)
  integer, parameter :: doy_impls(*)      = (/ IMPL_STRING, IMPL_COMPONENT /)
  integer, parameter :: declin_impls(*)   = (/ IMPL_STRING, IMPL_COMPONENT /)
  integer, parameter :: composed_impls(*) = (/ IMPL_STRING, IMPL_COMPONENT /)

  ! ---- test tables ---------------------------------------------------------

  ! one case per source line (build uses -ffree-line-length-none)
  ! epass: .true. = expected to pass; .false. = known failure of the existing code
  ! all expected values were derived independently of the implementation
  ! (Python datetime for calendar arithmetic; the documented solar geometry
  ! formulas evaluated in double precision for cosz/julian), except the
  ! year-3600 rows: the model's calendar intentionally deviates from the
  ! proleptic Gregorian in treating years divisible by 3600 as NOT leap
  ! (nfeb), and those rows pin that behavior
  !                 label                                              ,   epass,    yr,  mo,  dy,  hr,  mi,  dminutes,   eyr,  emo,  edy,  ehr,  emi
  type(advance_case), parameter :: advance_cases(*) = [ &
       advance_case("identity zero minutes"                            ,  .true.,  2023,   6,  15,  10,  30,         0,  2023,    6,   15,   10,   30) &
     , advance_case("23:59 plus 1 min rolls to next day"               ,  .true.,  2023,   6,  15,  23,  59,         1,  2023,    6,   16,    0,    0) &
     , advance_case("Jan 31 23:59 plus 1 min -> Feb 1"                 ,  .true.,  2023,   1,  31,  23,  59,         1,  2023,    2,    1,    0,    0) &
     , advance_case("Apr 30 23:59 plus 1 min -> May 1"                 ,  .true.,  2023,   4,  30,  23,  59,         1,  2023,    5,    1,    0,    0) &
     , advance_case("Feb 28 23:59 plus 1 min non-leap -> Mar 1"        ,  .true.,  2023,   2,  28,  23,  59,         1,  2023,    3,    1,    0,    0) &
     , advance_case("Feb 28 23:59 plus 1 min leap -> Feb 29"           ,  .true.,  2024,   2,  28,  23,  59,         1,  2024,    2,   29,    0,    0) &
     , advance_case("Feb 29 23:59 plus 1 min leap -> Mar 1"            ,  .true.,  2024,   2,  29,  23,  59,         1,  2024,    3,    1,    0,    0) &
     , advance_case("Dec 31 23:59 plus 1 min -> new year"              ,  .true.,  2023,  12,  31,  23,  59,         1,  2024,    1,    1,    0,    0) &
     , advance_case("Feb 28 2000 plus 1 day, div-400 leap -> Feb 29"   ,  .true.,  2000,   2,  28,  12,   0,      1440,  2000,    2,   29,   12,    0) &
     , advance_case("Feb 28 1900 plus 1 day, div-100 not leap -> Mar 1",  .true.,  1900,   2,  28,  12,   0,      1440,  1900,    3,    1,   12,    0) &
     , advance_case("Feb 28 2100 plus 1 day, div-100 not leap -> Mar 1",  .true.,  2100,   2,  28,  12,   0,      1440,  2100,    3,    1,   12,    0) &
     , advance_case("Feb 28 2024 plus 1 day, div-4 leap -> Feb 29"     ,  .true.,  2024,   2,  28,  12,   0,      1440,  2024,    2,   29,   12,    0) &
     , advance_case("Feb 28 3600 +1 day, mod-3600 not leap -> Mar 1"   ,  .true.,  3600,   2,  28,  12,   0,      1440,  3600,    3,    1,   12,    0) &
     , advance_case("Mar 1 3600 -1 day, mod-3600 not leap -> Feb 28"   ,  .true.,  3600,   3,   1,   0,   0,     -1440,  3600,    2,   28,    0,    0) &
     , advance_case("plus 1440 min is exactly 1 day"                   ,  .true.,  2023,   3,  10,   6,  45,      1440,  2023,    3,   11,    6,   45) &
     , advance_case("plus 43200 min is 30 days crossing month"         ,  .true.,  2023,   1,  15,   0,   0,     43200,  2023,    2,   14,    0,    0) &
     , advance_case("plus 527040 min full leap year crossing Feb 29"   ,  .true.,  2024,   1,   1,   0,   0,    527040,  2025,    1,    1,    0,    0) &
     , advance_case("00:00 minus 1 min -> previous day"                ,  .true.,  2023,   6,  15,   0,   0,        -1,  2023,    6,   14,   23,   59) &
     , advance_case("Mar 1 minus 1440 min leap -> Feb 29"              ,  .true.,  2024,   3,   1,   0,   0,     -1440,  2024,    2,   29,    0,    0) &
     , advance_case("Mar 1 minus 1440 min non-leap -> Feb 28"          ,  .true.,  2023,   3,   1,   0,   0,     -1440,  2023,    2,   28,    0,    0) &
     , advance_case("Jan 1 00:00 minus 1 min -> Dec 31 prev year"      ,  .true.,  2023,   1,   1,   0,   0,        -1,  2022,   12,   31,   23,   59) &
     , advance_case("1998-01-01 00:00 plus 1560 min (26 h)"            ,  .true.,  1998,   1,   1,   0,   0,      1560,  1998,    1,    2,    2,    0) &
     ]

  !             label                        ,   epass,    yr,  mo,  dy,  eoffset
  type(doy_case), parameter :: doy_cases(*) = [ &
       doy_case("Jan 1 leap year -> 0"       ,  .true.,  2024,   1,   1,        0) &
     , doy_case("Feb 28 leap 2024"           ,  .true.,  2024,   2,  28,       58) &
     , doy_case("Feb 29 leap 2024"           ,  .true.,  2024,   2,  29,       59) &
     , doy_case("Mar 1 leap 2024"            ,  .true.,  2024,   3,   1,       60) &
     , doy_case("Feb 28 non-leap 2023"       ,  .true.,  2023,   2,  28,       58) &
     , doy_case("Mar 1 non-leap 2023"        ,  .true.,  2023,   3,   1,       59) &
     , doy_case("Dec 31 leap 2024 -> 365"    ,  .true.,  2024,  12,  31,      365) &
     , doy_case("Dec 31 non-leap 2023 -> 364",  .true.,  2023,  12,  31,      364) &
     , doy_case("Feb 28 1900 div-100 not leap",  .true.,  1900,   2,  28,       58) &
     , doy_case("Mar 1 1900 div-100 not leap" ,  .true.,  1900,   3,   1,       59) &
     , doy_case("Dec 31 1900 -> 364"          ,  .true.,  1900,  12,  31,      364) &
     , doy_case("Feb 28 2000 div-400 leap"    ,  .true.,  2000,   2,  28,       58) &
     , doy_case("Feb 29 2000 div-400 leap"    ,  .true.,  2000,   2,  29,       59) &
     , doy_case("Mar 1 2000 div-400 leap"     ,  .true.,  2000,   3,   1,       60) &
     , doy_case("Dec 31 2000 -> 365"          ,  .true.,  2000,  12,  31,      365) &
     , doy_case("Feb 28 2100 div-100 not leap",  .true.,  2100,   2,  28,       58) &
     , doy_case("Mar 1 2100 div-100 not leap" ,  .true.,  2100,   3,   1,       59) &
     , doy_case("Dec 31 2100 -> 364"          ,  .true.,  2100,  12,  31,      364) &
     , doy_case("Feb 28 3600 mod-3600 nonleap",  .true.,  3600,   2,  28,       58) &
     , doy_case("Mar 1 3600 mod-3600 nonleap" ,  .true.,  3600,   3,   1,       59) &
     , doy_case("Dec 31 3600 -> 364"          ,  .true.,  3600,  12,  31,      364) &
     , doy_case("15th of month 01 2023"      ,  .true.,  2023,   1,  15,       14) &
     , doy_case("15th of month 02 2023"      ,  .true.,  2023,   2,  15,       45) &
     , doy_case("15th of month 03 2023"      ,  .true.,  2023,   3,  15,       73) &
     , doy_case("15th of month 04 2023"      ,  .true.,  2023,   4,  15,      104) &
     , doy_case("15th of month 05 2023"      ,  .true.,  2023,   5,  15,      134) &
     , doy_case("15th of month 06 2023"      ,  .true.,  2023,   6,  15,      165) &
     , doy_case("15th of month 07 2023"      ,  .true.,  2023,   7,  15,      195) &
     , doy_case("15th of month 08 2023"      ,  .true.,  2023,   8,  15,      226) &
     , doy_case("15th of month 09 2023"      ,  .true.,  2023,   9,  15,      257) &
     , doy_case("15th of month 10 2023"      ,  .true.,  2023,  10,  15,      287) &
     , doy_case("15th of month 11 2023"      ,  .true.,  2023,  11,  15,      318) &
     , doy_case("15th of month 12 2023"      ,  .true.,  2023,  12,  15,      348) &
     ]

  !                label                               ,    epass,    yr,  mo,  dy,  hr,  mi,  sc,   lat,     lon,  slope,  azimuth,      ecosz,  ecosz_horiz,  eyearlen,     ejulian,     tol
  type(declin_case), parameter :: declin_cases(*) = [ &
       declin_case("yearlen 1900 jun15 noon utc"       ,   .true.,  1900,   6,  15,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.073066,     0.073066,       365,  165.500000,  1.0e-4) &
     , declin_case("yearlen 2000 jun15 noon utc"       ,   .true.,  2000,   6,  15,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.073517,     0.073517,       366,  166.500000,  1.0e-4) &
     , declin_case("yearlen 2024 jun15 noon utc"       ,   .true.,  2024,   6,  15,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.073517,     0.073517,       366,  166.500000,  1.0e-4) &
     , declin_case("yearlen 2100 jun15 noon utc"       ,   .true.,  2100,   6,  15,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.073066,     0.073066,       365,  165.500000,  1.0e-4) &
     , declin_case("yearlen 3600 mod-3600 not leap"    ,   .true.,  3600,   6,  15,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.073066,     0.073066,       365,  165.500000,  1.0e-4) &
     , declin_case("julian 2024-03-05 00:00 utc"       ,   .true.,  2024,   3,   5,   0,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.127392,     0.127392,       366,   64.000000,  1.0e-4) &
     , declin_case("julian 2024-03-05 06:00 utc"       ,   .true.,  2024,   3,   5,   6,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.804355,    -0.804355,       366,   64.250000,  1.0e-4) &
     , declin_case("julian 2024-03-05 12:00 utc"       ,   .true.,  2024,   3,   5,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.264748,    -0.264748,       366,   64.500000,  1.0e-4) &
     , declin_case("julian 2024-03-05 18:00 utc"       ,   .true.,  2024,   3,   5,  18,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.669454,     0.669454,       366,   64.750000,  1.0e-4) &
     , declin_case("equinox mar20 2024 local noon"     ,   .true.,  2024,   3,  20,  19,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.765124,     0.765124,       366,   79.791667,  1.0e-4) &
     , declin_case("equinox mar20 2024 local midnight" ,   .true.,  2024,   3,  20,   7,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.769161,    -0.769161,       366,   79.291667,  1.0e-4) &
     , declin_case("solstice jun21 2024 local noon"    ,   .true.,  2024,   6,  21,  19,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.958776,     0.958776,       366,  172.791667,  1.0e-4) &
     , declin_case("solstice jun21 2024 local midnight",   .true.,  2024,   6,  21,   7,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.446260,    -0.446260,       366,  172.291667,  1.0e-4) &
     , declin_case("solstice dec21 2024 local noon"    ,   .true.,  2024,  12,  21,  19,   0,   0,  40.0,  -105.0,    0.0,      0.0,   0.446438,     0.446438,       366,  355.791667,  1.0e-4) &
     , declin_case("solstice dec21 2024 local midnight",   .true.,  2024,  12,  21,   7,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.958776,    -0.958776,       366,  355.291667,  1.0e-4) &
     , declin_case("slope30 az180 jun21 noon"          ,   .true.,  2024,   6,  21,  19,   0,   0,  40.0,  -105.0,   30.0,    180.0,   0.972406,     0.958776,       366,  172.791667,  1.0e-4) &
     , declin_case("sxlong below 80 mar19"             ,   .true.,  2024,   3,  19,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.204874,    -0.204874,       366,   78.500000,  1.0e-4) &
     , declin_case("sxlong at-above 80 mar21"          ,   .true.,  2024,   3,  21,  12,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.196060,    -0.196060,       366,   80.500000,  1.0e-4) &
     , declin_case("tloc wrap lon170 jun15 20utc"      ,   .true.,  2024,   6,  15,  20,   0,   0,  40.0,   170.0,    0.0,      0.0,   0.495974,     0.495974,       366,  166.833333,  1.0e-4) &
     , declin_case("julian should be 1-based per docs" ,  .false.,  2024,   1,   1,   0,   0,   0,  40.0,  -105.0,    0.0,      0.0,  -0.068059,    -0.068059,       366,    1.000000,  1.0e-4) &
     , declin_case("julian should include minutes"     ,  .false.,  2024,   3,   5,   6,  30,   0,  40.0,  -105.0,    0.0,      0.0,  -0.823714,    -0.823714,       366,   64.270833,  1.0e-4) &
     ]

  !                  label                          ,   epass,    yr,  mo,  dy,  hr,  mi,  dminutes,   lat,     lon,  slope,  azimuth,     ecosz,  ecosz_horiz,  eyearlen,     ejulian,     tol
  type(composed_case), parameter :: composed_cases(*) = [ &
       composed_case("advance 1998-01-01 +27000min" ,  .true.,  1998,   1,   1,   0,   0,     27000,  40.0,  -105.0,    0.0,      0.0,  0.471150,     0.471150,       365,   18.750000,  1.0e-4) &
     , composed_case("advance 1998-01-01 +782940min",  .true.,  1998,   1,   1,   0,   0,    782940,  40.0,  -105.0,    0.0,      0.0,  0.863534,     0.863534,       365,  178.708333,  1.0e-4) &
     ]

  ! ---- driver ---------------------------------------------------------------

  integer :: nfail

  nfail = 0
  call run_advance_cases(nfail)
  call run_doy_cases(nfail)
  call run_declin_cases(nfail)
  call run_composed_cases(nfail)
  call run_drift_check(nfail)

  write(*,'(A)') repeat("-", 60)
  if (nfail > 0) then
     write(*,'(I0,A)') nfail, " test case(s) FAILED"
     error stop 1
  else
     write(*,'(A)') "All test cases passed"
  end if

contains

  ! ==== driver loops =========================================================

  subroutine run_advance_cases(nfail)
    integer, intent(inout) :: nfail
    type(advance_case) :: c
    integer :: i, j, impl, yr2, mo2, dy2, hr2, mi2
    logical :: ok

    write(*,'(A)') "advance_date cases:"
    do j = 1, size(advance_impls)
       impl = advance_impls(j)
       do i = 1, size(advance_cases)
          c = advance_cases(i)
          call advance_impl(impl, c%yr, c%mo, c%dy, c%hr, c%mi, c%dminutes, &
                            yr2, mo2, dy2, hr2, mi2)
          ok = (yr2 == c%eyr) .and. (mo2 == c%emo) .and. (dy2 == c%edy) &
               .and. (hr2 == c%ehr) .and. (mi2 == c%emi)
          call report(ok, c%epass, trim(c%label)//" "//impl_name(impl), nfail)
          if (.not. ok) then
             write(*,'(A,5I5)') "        expected:", c%eyr, c%emo, c%edy, c%ehr, c%emi
             write(*,'(A,5I5)') "        got:     ", yr2, mo2, dy2, hr2, mi2
          end if
       end do
    end do
  end subroutine run_advance_cases

  subroutine run_doy_cases(nfail)
    integer, intent(inout) :: nfail
    type(doy_case) :: c
    integer :: i, j, impl, offset
    logical :: ok

    write(*,'(A)') "day_of_year_offset cases:"
    do j = 1, size(doy_impls)
       impl = doy_impls(j)
       do i = 1, size(doy_cases)
          c = doy_cases(i)
          offset = doy_impl(impl, c%yr, c%mo, c%dy)
          ok = (offset == c%eoffset)
          call report(ok, c%epass, trim(c%label)//" "//impl_name(impl), nfail)
          if (.not. ok) then
             write(*,'(A,I5,A,I5)') "        expected:", c%eoffset, "  got:", offset
          end if
       end do
    end do
  end subroutine run_doy_cases

  subroutine run_declin_cases(nfail)
    integer, intent(inout) :: nfail
    type(declin_case) :: c
    integer :: i, j, impl, yearlen
    real    :: cosz, cosz_horiz, julian
    logical :: ok

    write(*,'(A)') "declin_at cases:"
    do j = 1, size(declin_impls)
       impl = declin_impls(j)
       do i = 1, size(declin_cases)
          c = declin_cases(i)
          call declin_impl(impl, c%yr, c%mo, c%dy, c%hr, c%mi, c%sc, &
                           c%lat, c%lon, c%slope, c%azimuth, &
                           cosz, cosz_horiz, yearlen, julian)
          ok = (abs(cosz - c%ecosz) <= c%tol) &
               .and. (abs(cosz_horiz - c%ecosz_horiz) <= c%tol) &
               .and. (yearlen == c%eyearlen) &
               .and. (abs(julian - c%ejulian) <= c%tol)
          call report(ok, c%epass, trim(c%label)//" "//impl_name(impl), nfail)
          if (.not. ok) then
             write(*,'(A,3F12.6,I5)') "        expected:", c%ecosz, c%ecosz_horiz, c%ejulian, c%eyearlen
             write(*,'(A,3F12.6,I5)') "        got:     ", cosz, cosz_horiz, julian, yearlen
          end if
       end do
    end do
  end subroutine run_declin_cases

  subroutine run_composed_cases(nfail)
    integer, intent(inout) :: nfail
    type(composed_case) :: c
    integer :: i, j, impl, yr2, mo2, dy2, hr2, mi2, yearlen
    real    :: cosz, cosz_horiz, julian
    logical :: ok

    write(*,'(A)') "composed advance_date + declin_at cases:"
    do j = 1, size(composed_impls)
       impl = composed_impls(j)
       do i = 1, size(composed_cases)
          c = composed_cases(i)
          call advance_impl(impl, c%yr, c%mo, c%dy, c%hr, c%mi, c%dminutes, &
                            yr2, mo2, dy2, hr2, mi2)
          call declin_impl(impl, yr2, mo2, dy2, hr2, mi2, 0, &
                           c%lat, c%lon, c%slope, c%azimuth, &
                           cosz, cosz_horiz, yearlen, julian)
          ok = (abs(cosz - c%ecosz) <= c%tol) &
               .and. (abs(cosz_horiz - c%ecosz_horiz) <= c%tol) &
               .and. (yearlen == c%eyearlen) &
               .and. (abs(julian - c%ejulian) <= c%tol)
          call report(ok, c%epass, trim(c%label)//" "//impl_name(impl), nfail)
          if (.not. ok) then
             write(*,'(A,3F12.6,I5)') "        expected:", c%ecosz, c%ecosz_horiz, c%ejulian, c%eyearlen
             write(*,'(A,3F12.6,I5)') "        got:     ", cosz, cosz_horiz, julian, yearlen
          end if
       end do
    end do
  end subroutine run_composed_cases

  ! Self-consistency: advancing step-by-step (as the model main loop does,
  ! one timestep at a time from the start date) must land on the same date
  ! as one advance of the total. No table needed: the two paths check each
  ! other.
  subroutine run_drift_check(nfail)
    integer, intent(inout) :: nfail
    integer, parameter :: nsteps = 26, step_minutes = 60
    integer :: j, impl, k, yr_s, mo_s, dy_s, hr_s, mi_s, yr1, mo1, dy1, hr1, mi1
    logical :: ok

    write(*,'(A)') "step-vs-total drift check:"
    do j = 1, size(advance_impls)
       impl = advance_impls(j)
       yr_s = 1998; mo_s = 1; dy_s = 1; hr_s = 0; mi_s = 0
       do k = 1, nsteps
          call advance_impl(impl, yr_s, mo_s, dy_s, hr_s, mi_s, step_minutes, &
                            yr1, mo1, dy1, hr1, mi1)
          yr_s = yr1; mo_s = mo1; dy_s = dy1; hr_s = hr1; mi_s = mi1
       end do
       call advance_impl(impl, 1998, 1, 1, 0, 0, nsteps*step_minutes, &
                         yr1, mo1, dy1, hr1, mi1)
       ok = (yr_s == yr1) .and. (mo_s == mo1) .and. (dy_s == dy1) &
            .and. (hr_s == hr1) .and. (mi_s == mi1)
       call report(ok, .true., "26 steps of 60 min == one +1560 min advance "//impl_name(impl), nfail)
       if (.not. ok) then
          write(*,'(A,5I5)') "        stepwise:", yr_s, mo_s, dy_s, hr_s, mi_s
          write(*,'(A,5I5)') "        single:  ", yr1, mo1, dy1, hr1, mi1
       end if
    end do
  end subroutine run_drift_check

  ! ==== implementation dispatch ==============================================
  ! One wrapper per adapter interface; the impl argument selects which
  ! implementation handles the case.

  subroutine advance_impl(impl, yr, mo, dy, hr, mi, dminutes, &
                          yr2, mo2, dy2, hr2, mi2)
    integer, intent(in)  :: impl, yr, mo, dy, hr, mi, dminutes
    integer, intent(out) :: yr2, mo2, dy2, hr2, mi2

    select case (impl)
    case (IMPL_STRING)
       call advance_date(yr, mo, dy, hr, mi, dminutes, yr2, mo2, dy2, hr2, mi2)
    case (IMPL_COMPONENT)
       call advance_datetime(yr, mo, dy, hr, mi, dminutes, yr2, mo2, dy2, hr2, mi2)
    end select
  end subroutine advance_impl

  integer function doy_impl(impl, yr, mo, dy)
    integer, intent(in) :: impl, yr, mo, dy

    select case (impl)
    case (IMPL_STRING)
       doy_impl = day_of_year_offset(yr, mo, dy)
    case (IMPL_COMPONENT)
       doy_impl = day_of_year(yr, mo, dy)
    end select
  end function doy_impl

  subroutine declin_impl(impl, yr, mo, dy, hr, mi, sc, lat, lon, slope, azimuth, &
                         cosz, cosz_horiz, yearlen, julian)
    integer, intent(in)  :: impl, yr, mo, dy, hr, mi, sc
    real,    intent(in)  :: lat, lon, slope, azimuth
    real,    intent(out) :: cosz, cosz_horiz, julian
    integer, intent(out) :: yearlen

    select case (impl)
    case (IMPL_STRING)
       call declin_at(yr, mo, dy, hr, mi, sc, lat, lon, slope, azimuth, &
                      cosz, cosz_horiz, yearlen, julian)
    case (IMPL_COMPONENT)
       call calc_declin_components(yr, day_of_year(yr, mo, dy), hr, mi, sc, &
                                   lat, lon, slope, azimuth, &
                                   cosz, cosz_horiz, yearlen, julian)
    end select
  end subroutine declin_impl

  ! A case counts as a test failure when its outcome differs from epass:
  ! an expected-pass case that fails (FAIL), or a known-failure case that
  ! passes (XPASS - its epass marker is stale and should be flipped).
  ! A known-failure case that fails reports XFAIL and does not count.
  subroutine report(ok, epass, label, nfail)
    logical,          intent(in)    :: ok
    logical,          intent(in)    :: epass
    character(len=*), intent(in)    :: label
    integer,          intent(inout) :: nfail

    if (ok .and. epass) then
       write(*,'(A)') "  PASS  " // trim(label)
    else if (.not. ok .and. .not. epass) then
       write(*,'(A)') "  XFAIL " // trim(label) // " (known failure)"
    else if (ok) then
       write(*,'(A)') "  XPASS " // trim(label) // " (marked known failure but passed)"
       nfail = nfail + 1
    else
       write(*,'(A)') "  FAIL  " // trim(label)
       nfail = nfail + 1
    end if
  end subroutine report

  ! ==== adapters =============================================================
  ! Version 1: wrap the current string-based routines. The reimplementation
  ! for issue #131 replaces only these bodies with calls to the new
  ! integer-component routines; the tables above stay unchanged.

  ! Advance a date by a number of minutes.
  ! Uses a 12-char unpunctuated date (YYYYMMDDHHmm), matching what the main
  ! loop passes to geth_newdate (domain%startdate/nowdate), which makes
  ! geth_newdate interpret idt as minutes.
  subroutine advance_date(yr, mo, dy, hr, mi, dminutes, &
                          yr2, mo2, dy2, hr2, mi2)
    integer, intent(in)  :: yr, mo, dy, hr, mi, dminutes
    integer, intent(out) :: yr2, mo2, dy2, hr2, mi2

    character(len=12) :: odate, ndate

    write(odate,'(I4.4,4I2.2)') yr, mo, dy, hr, mi
    call geth_newdate(odate, dminutes, ndate)
    read(ndate,'(I4,4I2)') yr2, mo2, dy2, hr2, mi2
  end subroutine advance_date

  ! Day-of-year offset (days since Jan 1 of the same year; Jan 1 -> 0),
  ! matching what calc_declin currently derives via geth_idts.
  integer function day_of_year_offset(yr, mo, dy)
    integer, intent(in) :: yr, mo, dy

    character(len=10) :: date, jan1

    write(date,'(I4.4,"-",I2.2,"-",I2.2)') yr, mo, dy
    write(jan1,'(I4.4,"-01-01")') yr
    call geth_idts(date, jan1, day_of_year_offset)
  end function day_of_year_offset

  ! Solar geometry for a date/time given as components.
  subroutine declin_at(yr, mo, dy, hr, mi, sc, lat, lon, slope, azimuth, &
                       cosz, cosz_horiz, yearlen, julian)
    integer, intent(in)  :: yr, mo, dy, hr, mi, sc
    real,    intent(in)  :: lat, lon, slope, azimuth
    real,    intent(out) :: cosz, cosz_horiz, julian
    integer, intent(out) :: yearlen

    character(len=19) :: nowdate

    write(nowdate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
         yr, mo, dy, hr, mi, sc
    call calc_declin(nowdate, lat, lon, slope, azimuth, &
                     cosz, cosz_horiz, yearlen, julian)
  end subroutine declin_at

end program datetime_test

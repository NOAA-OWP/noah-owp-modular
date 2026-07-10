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
! NOTE: the tables below hold single smoke-test rows only. The full
! characterization cases (phase 2 of docs/plan-issue-131-date-handling.md)
! are added as rows here, with expected values computed independently.

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

  ! ---- test tables ---------------------------------------------------------

  ! one case per source line (build uses -ffree-line-length-none)
  ! epass: .true. = expected to pass; .false. = known failure of the existing code
  !                                                              label                                   epass    yr  mo  dy  hr  mi  dminutes   eyr emo edy ehr emi
  type(advance_case), parameter :: advance_cases(*) = [ &
       advance_case("smoke: identity, +0 minutes",                                                    .true.,  2024,  1, 15, 12, 30,        0, 2024,  1, 15, 12, 30) &
     ]

  !                                                              label                                   epass    yr  mo  dy   eoffset
  type(doy_case), parameter :: doy_cases(*) = [ &
       doy_case("smoke: Jan 1 offset is 0",                                                           .true.,  2024,  1,  1,        0) &
     ]

  !                                                              label                                   epass    yr  mo  dy  hr  mi  sc    lat    lon  slope azimuth      ecosz ecosz_horiz eyearlen ejulian     tol
  type(declin_case), parameter :: declin_cases(*) = [ &
       declin_case("smoke: 2024-01-01 00Z equator, flat",                                             .true.,  2024,  1,  1,  0,  0,  0,   0.0,   0.0,   0.0,   0.0,  -0.92027,   -0.92027,     366,    0.0, 1.0e-4) &
     ]

  ! ---- driver ---------------------------------------------------------------

  integer :: nfail

  nfail = 0
  call run_advance_cases(nfail)
  call run_doy_cases(nfail)
  call run_declin_cases(nfail)

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
    integer :: i, yr2, mo2, dy2, hr2, mi2
    logical :: ok

    write(*,'(A)') "advance_date cases:"
    do i = 1, size(advance_cases)
       c = advance_cases(i)
       call advance_date(c%yr, c%mo, c%dy, c%hr, c%mi, c%dminutes, &
                         yr2, mo2, dy2, hr2, mi2)
       ok = (yr2 == c%eyr) .and. (mo2 == c%emo) .and. (dy2 == c%edy) &
            .and. (hr2 == c%ehr) .and. (mi2 == c%emi)
       call report(ok, c%epass, c%label, nfail)
       if (.not. ok) then
          write(*,'(A,5I5)') "        expected:", c%eyr, c%emo, c%edy, c%ehr, c%emi
          write(*,'(A,5I5)') "        got:     ", yr2, mo2, dy2, hr2, mi2
       end if
    end do
  end subroutine run_advance_cases

  subroutine run_doy_cases(nfail)
    integer, intent(inout) :: nfail
    type(doy_case) :: c
    integer :: i, offset
    logical :: ok

    write(*,'(A)') "day_of_year_offset cases:"
    do i = 1, size(doy_cases)
       c = doy_cases(i)
       offset = day_of_year_offset(c%yr, c%mo, c%dy)
       ok = (offset == c%eoffset)
       call report(ok, c%epass, c%label, nfail)
       if (.not. ok) then
          write(*,'(A,I5,A,I5)') "        expected:", c%eoffset, "  got:", offset
       end if
    end do
  end subroutine run_doy_cases

  subroutine run_declin_cases(nfail)
    integer, intent(inout) :: nfail
    type(declin_case) :: c
    integer :: i, yearlen
    real    :: cosz, cosz_horiz, julian
    logical :: ok

    write(*,'(A)') "declin_at cases:"
    do i = 1, size(declin_cases)
       c = declin_cases(i)
       call declin_at(c%yr, c%mo, c%dy, c%hr, c%mi, c%sc, &
                      c%lat, c%lon, c%slope, c%azimuth, &
                      cosz, cosz_horiz, yearlen, julian)
       ok = (abs(cosz - c%ecosz) <= c%tol) &
            .and. (abs(cosz_horiz - c%ecosz_horiz) <= c%tol) &
            .and. (yearlen == c%eyearlen) &
            .and. (abs(julian - c%ejulian) <= c%tol)
       call report(ok, c%epass, c%label, nfail)
       if (.not. ok) then
          write(*,'(A,3F12.6,I5)') "        expected:", c%ecosz, c%ecosz_horiz, c%ejulian, c%eyearlen
          write(*,'(A,3F12.6,I5)') "        got:     ", cosz, cosz_horiz, julian, yearlen
       end if
    end do
  end subroutine run_declin_cases

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

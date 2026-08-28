! Exercises the serialization protocol through the iso_c_fortran_bmi entry
! points rather than the Fortran type directly.
!
! Those entry points do something the Fortran interface does not: they size every
! transfer themselves, as get_var_nbytes / get_var_itemsize, and hand the model a
! slice of that length. A model whose nbytes and itemsize disagree with the
! buffer it actually reads or writes fails only on this path, which is the one a
! host uses. Everything else about the protocol is covered by serialization_test.
!
! Run from the run/ directory; the model is initialized from a relative path.

program serialization_iso_c_test

  use iso_c_bmif_2_0
  use iso_c_binding

  implicit none

  ! Reached by its C symbol, the way a host reaches it -- it is not public in the
  ! module that defines it
  interface
     function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
       import :: c_ptr, c_int
       type(c_ptr) :: this
       integer(kind=c_int) :: bmi_status
     end function register_bmi
  end interface

  type(c_ptr) :: handle
  integer(kind=c_int) :: st, nbytes, itemsize, reported(2), announce(2), trigger(1)
  integer(kind=c_int), allocatable :: snapshot(:)
  integer(kind=c_int64_t) :: reported_bytes
  ! Distinct bytes in every position a reportable size can occupy
  integer(kind=c_int64_t), parameter :: wide_probe = int(z'7ABCDEF0', c_int64_t)
  ! Upper half set over a lower half that would be accepted on its own, so this
  ! is only refused if the upper half arrives
  integer(kind=c_int64_t), parameter :: too_wide = 4294967300_c_int64_t
  character(kind=c_char, len=1) :: units(2048)
  integer :: nfail, i

  nfail = 0
  trigger = 1

  st = register_bmi(handle)
  call expect_true(st == BMI_SUCCESS, "register_bmi", nfail)

  st = initialize(handle, as_c_string("namelist.input"))
  call expect_true(st == BMI_SUCCESS, "initialize", nfail)

  ! The support probe, across the C boundary
  st = get_var_units(handle, as_c_string("ngen::serialization_state"), units)
  call expect_true(st == BMI_SUCCESS .and. c_string_is(units, "ngen::opaque"), &
       "state units read back as ngen::opaque", nfail)

  ! Sizing has to resolve even with no snapshot in hand: a failure here returns
  ! before the model is ever reached.
  st = get_var_nbytes(handle, as_c_string("ngen::serialization_state"), nbytes)
  call expect_true(st == BMI_SUCCESS, "state nbytes resolves before create", nfail)
  st = get_var_itemsize(handle, as_c_string("ngen::serialization_state"), itemsize)
  call expect_true(st == BMI_SUCCESS .and. itemsize > 0, "state itemsize", nfail)

  ! A trigger's nbytes must also resolve, or the shim returns before the model
  ! ever sees the call
  st = get_var_nbytes(handle, as_c_string("ngen::serialization_create"), nbytes)
  call expect_true(st == BMI_SUCCESS .and. nbytes > 0, "create nbytes", nfail)

  do i = 1, 10
     st = update(handle)
  end do

  st = set_value_int(handle, as_c_string("ngen::serialization_create"), trigger)
  call expect_true(st == BMI_SUCCESS, "create through the C entry point", nfail)

  ! An int64 delivered as two c_ints, so this also proves the halves go back
  ! together in the right order
  st = get_value_int(handle, as_c_string("ngen::serialization_size"), reported)
  reported_bytes = transfer(reported, 0_c_int64_t)
  call expect_true(st == BMI_SUCCESS .and. reported_bytes > 0, &
       "size through the C entry point", nfail)

  st = get_var_nbytes(handle, as_c_string("ngen::serialization_state"), nbytes)
  st = get_var_itemsize(handle, as_c_string("ngen::serialization_state"), itemsize)
  allocate(snapshot(nbytes / itemsize))
  snapshot = 0

  ! The shim slices this to nbytes/itemsize before the model writes into it
  st = get_value_int(handle, as_c_string("ngen::serialization_state"), snapshot)
  call expect_true(st == BMI_SUCCESS, "state read through the C entry point", nfail)
  call expect_true(snapshot(1) > 0, "payload length header survived the transfer", nfail)

  st = set_value_int(handle, as_c_string("ngen::serialization_free"), trigger)
  call expect_true(st == BMI_SUCCESS, "free through the C entry point", nfail)

  ! Restore announces the byte count first. The model reports that back from
  ! get_var_nbytes, and the shim divides it to size the payload it marshals, so
  ! an announcement that does not stick would silently truncate the restore.
  do i = 1, 5
     st = update(handle)
  end do

  announce = transfer(reported_bytes, 0_c_int, 2)
  st = set_value_int(handle, as_c_string("ngen::serialization_size"), announce)
  call expect_true(st == BMI_SUCCESS, "announced size through the C entry point", nfail)

  st = get_var_nbytes(handle, as_c_string("ngen::serialization_state"), nbytes)
  call expect_true(st == BMI_SUCCESS .and. nbytes == reported_bytes, &
       "state nbytes follows the announced size", nfail)

  st = set_value_int(handle, as_c_string("ngen::serialization_state"), snapshot)
  call expect_true(st == BMI_SUCCESS, "restore through the C entry point", nfail)

  ! Both directions, or the halves are being dropped or swapped in transit
  announce = transfer(wide_probe, 0_c_int, 2)
  st = set_value_int(handle, as_c_string("ngen::serialization_size"), announce)
  st = get_value_int(handle, as_c_string("ngen::serialization_size"), reported)
  call expect_true(transfer(reported, 0_c_int64_t) == wide_probe, &
       "a size round-trips through both halves", nfail)

  ! A size past what get_var_nbytes can report is refused. Taking the low half
  ! alone this is 4, which would be accepted, so it also proves the high half
  ! crosses the boundary.
  announce = transfer(too_wide, 0_c_int, 2)
  st = set_value_int(handle, as_c_string("ngen::serialization_size"), announce)
  call expect_true(st /= BMI_SUCCESS, &
       "a size past the transfer width is refused", nfail)

  st = finalize(handle)

  write(*,'(A)') repeat("-", 60)
  if (nfail > 0) then
     write(*,'(I0,A)') nfail, " test case(s) FAILED"
     error stop 1
  else
     write(*,'(A)') "All test cases passed"
  end if

contains

  function as_c_string(s) result(c)
    character(len=*), intent(in) :: s
    character(kind=c_char, len=1) :: c(2048)
    integer :: k

    c = c_null_char
    do k = 1, len_trim(s)
       c(k) = s(k:k)
    end do
    c(len_trim(s) + 1) = c_null_char
  end function as_c_string

  logical function c_string_is(c, s)
    character(kind=c_char, len=1), intent(in) :: c(*)
    character(len=*), intent(in) :: s
    integer :: k

    c_string_is = .true.
    do k = 1, len_trim(s)
       if (c(k) /= s(k:k)) c_string_is = .false.
    end do
    if (c(len_trim(s) + 1) /= c_null_char) c_string_is = .false.
  end function c_string_is

  subroutine expect_true(condition, description, nfail)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: description
    integer, intent(inout) :: nfail

    if (condition) then
       write(*,'(A,A)') "PASS  ", description
    else
       write(*,'(A,A)') "FAIL  ", description
       nfail = nfail + 1
    end if
  end subroutine expect_true

end program serialization_iso_c_test

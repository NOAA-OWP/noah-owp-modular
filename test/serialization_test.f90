! Tests for state serialization and the ngen BMI serialization protocol.
!
! Run from the run/ directory: the model is initialized from namelist.input via
! a relative path.
!
! Only one model in the process may read forcing. AsciiReadModule's read routine
! keeps a saved first-call flag that is never reset, so a second model reading
! forcing would land on the file's header. The restore-semantics tests therefore
! perturb state directly instead of advancing, which also makes what they assert
! about a restore more direct.

program serialization_test

  use bminoahowp
  use bmif_2_0
  use RunModule
  use StateSerialization
  use DomainType
  use messagepack
  use iso_fortran_env, only: int64

  implicit none

  ! The reserved variables, and the units a host compares against exactly
  integer, parameter :: n_reserved = 4
  character(len=32), parameter :: reserved(n_reserved) = [character(len=32) :: &
       'ngen::serialization_create', &
       'ngen::serialization_free',   &
       'ngen::serialization_size',   &
       'ngen::serialization_state']
  character(len=16), parameter :: reserved_units(n_reserved) = [character(len=16) :: &
       'ngen::trigger', 'ngen::trigger', 'bytes', 'ngen::opaque']

  integer :: nfail

  nfail = 0

  call test_payload_round_trip(nfail)
  call test_restore_semantics(nfail)
  call test_bmi_protocol(nfail)

  write(*,'(A)') repeat("-", 60)
  if (nfail > 0) then
     write(*,'(I0,A)') nfail, " test case(s) FAILED"
     error stop 1
  else
     write(*,'(A)') "All test cases passed"
  end if

contains

  ! MessagePack sizes integers by magnitude. Probe either side of its width
  ! boundaries: every magnitude has to survive the encoding, and the payload
  ! is expected to grow.
  subroutine test_payload_round_trip(nfail)
    integer, intent(inout) :: nfail
    type(domain_type) :: domain
    class(mp_arr_type), allocatable :: mp_arr
    integer(kind=int64) :: packed_size, smallest
    integer, parameter :: probes(5) = [0, 127, 128, 32768, 1000000]
    integer :: i

    allocate(domain%DZSNSO(-2:4))
    allocate(domain%ZSNSO(-2:4))
    domain%DZSNSO = 0.1
    domain%ZSNSO = -0.5
    domain%curr_datetime = 1.7d9
    domain%time_dbl = 3600.d0

    do i = 1, size(probes)
       domain%ITIME = probes(i)
       call domain_serialization(domain, mp_arr)
       call mp_arr%getsize(packed_size)
       if (i == 1) smallest = packed_size

       block
         type(domain_type) :: restored
         allocate(restored%DZSNSO(-2:4))
         allocate(restored%ZSNSO(-2:4))
         call domain_deserialization(mp_arr, restored, NOAHOWP_RESTORE_RESUME)
         call expect_true(restored%ITIME == domain%ITIME, "integer round-trips", nfail)
         call expect_true(abs(restored%curr_datetime - domain%curr_datetime) < 1.d-6, &
              "double round-trips", nfail)
       end block
    end do

    call expect_true(packed_size > smallest, "payload grows with integer magnitude", nfail)
  end subroutine test_payload_round_trip

  ! What a restore applies, and what it refuses. State is perturbed directly
  ! rather than by advancing, so this model never reads forcing.
  subroutine test_restore_semantics(nfail)
    integer, intent(inout) :: nfail
    type(noahowp_type) :: model
    integer, allocatable :: snapshot(:), truncated(:)
    integer :: st, itime_at_save, perturbed_itime, foreign(20)
    real :: tg_at_save

    call initialize_from_file(model, "namelist.input")

    ! solve_noahowp is what normally sets this, and this model never advances
    model%domain%curr_datetime = model%domain%start_datetime

    tg_at_save    = model%energy%TG
    itime_at_save = model%domain%itime

    call create_serialization(model, st)
    call expect_true(st == 0, "snapshot created", nfail)
    snapshot = model%serialization_buffer
    call free_serialization(model)

    ! Put the model somewhere the snapshot plainly does not describe
    model%energy%TG    = tg_at_save + 25.0
    model%domain%itime = itime_at_save + 17
    perturbed_itime    = model%domain%itime

    ! Hotstart: physical state applies, the clock does not. itime indexes
    ! sim_datetimes, which belongs to this run and is not in the snapshot.
    model%restore_mode = NOAHOWP_RESTORE_HOTSTART
    call restore_serialization(model, snapshot, st)
    call expect_true(st == 0, "hotstart restore succeeds", nfail)
    call expect_true(abs(model%energy%TG - tg_at_save) < 1.0e-5, &
         "hotstart applies physical state", nfail)
    call expect_true(model%domain%itime == perturbed_itime, &
         "hotstart leaves the clock alone", nfail)

    ! Resume: the whole snapshot applies, clock included. Only hotstart is driven
    ! today, so without this the resume branch would be unreachable and could rot.
    model%energy%TG    = tg_at_save + 25.0
    model%domain%itime = perturbed_itime
    model%restore_mode = NOAHOWP_RESTORE_RESUME
    call restore_serialization(model, snapshot, st)
    call expect_true(st == 0, "resume restore succeeds", nfail)
    call expect_true(abs(model%energy%TG - tg_at_save) < 1.0e-5, &
         "resume applies physical state", nfail)
    call expect_true(model%domain%itime == itime_at_save, &
         "resume takes the clock from the snapshot", nfail)

    model%restore_mode = NOAHOWP_RESTORE_HOTSTART

    ! A payload that is not ours, or not intact, is refused rather than applied
    ! to live state -- and must not abort the process on the way.
    foreign = 7
    foreign(1) = 40
    call restore_serialization(model, foreign, st)
    call expect_true(st /= 0, "a foreign payload is refused", nfail)

    truncated = snapshot(1:size(snapshot)/2)
    truncated(1) = (size(truncated) - 1) * 4
    call restore_serialization(model, truncated, st)
    call expect_true(st /= 0, "a truncated payload is refused", nfail)

    block
      integer :: too_short(1)
      too_short = 0
      call restore_serialization(model, too_short, st)
      call expect_true(st /= 0, "an undersized payload is refused", nfail)
    end block

    ! and a good payload still works afterwards
    call restore_serialization(model, snapshot, st)
    call expect_true(st == 0, "a good payload is still accepted after refusals", nfail)

    call cleanup(model)
  end subroutine test_restore_semantics

  ! The protocol as a host sees it: the support probe, the metadata its bindings
  ! need, and the save and restore sequences. This is the model that advances.
  subroutine test_bmi_protocol(nfail)
    integer, intent(inout) :: nfail
    type(bmi_noahowp) :: m
    character(len=BMI_MAX_UNITS_NAME) :: units
    character(len=BMI_MAX_TYPE_NAME) :: vtype
    character(len=BMI_MAX_VAR_NAME) :: location
    character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
    integer, allocatable :: snapshot(:)
    integer :: i, st, nbytes, itemsize, grid, reported(2), announce(2), trigger(1)
    integer(kind=int64) :: reported_bytes
    real :: tg_saved(1), tg_drifted(1), tg_restored(1)
    real :: sneqv_saved(1), sneqv_restored(1)
    logical :: found

    trigger = 1
    st = m%initialize("namelist.input")

    do i = 1, n_reserved
       ! The support probe reads units and compares them exactly; this is the
       ! whole conformance signal.
       st = m%get_var_units(trim(reserved(i)), units)
       call expect_true(st == BMI_SUCCESS .and. trim(units) == trim(reserved_units(i)), &
            "units of "//trim(reserved(i)), nfail)

       ! The Fortran bindings carry no byte type, so the opaque state travels as
       ! int. A host validates units, not type.
       st = m%get_var_type(trim(reserved(i)), vtype)
       call expect_true(st == BMI_SUCCESS .and. trim(vtype) == "integer", &
            "type of "//trim(reserved(i)), nfail)

       ! Both must succeed: the ISO-C binding sizes transfers as nbytes/itemsize
       ! before dispatching, so a failure here short-circuits the call.
       st = m%get_var_itemsize(trim(reserved(i)), itemsize)
       call expect_true(st == BMI_SUCCESS .and. itemsize > 0, &
            "itemsize of "//trim(reserved(i)), nfail)
       ! The state reports zero until there is a snapshot or an announced
       ! payload, so resolving at all is what matters here.
       st = m%get_var_nbytes(trim(reserved(i)), nbytes)
       call expect_true(st == BMI_SUCCESS .and. nbytes >= 0, &
            "nbytes of "//trim(reserved(i)), nfail)
       call expect_true(mod(nbytes, itemsize) == 0, &
            "nbytes divides by itemsize for "//trim(reserved(i)), nfail)

       ! No spatial meaning; a clean failure is preferred to an invented answer
       call expect_true(m%get_var_grid(trim(reserved(i)), grid) == BMI_FAILURE, &
            "grid refused for "//trim(reserved(i)), nfail)
       call expect_true(m%get_var_location(trim(reserved(i)), location) == BMI_FAILURE, &
            "location refused for "//trim(reserved(i)), nfail)
    end do

    ! The size variable is an int64 the bindings can only carry as two c_ints
    st = m%get_var_itemsize('ngen::serialization_size', itemsize)
    st = m%get_var_nbytes('ngen::serialization_size', nbytes)
    call expect_true(nbytes == 2 * itemsize, "size spans two items", nfail)

    ! Hosts discover the reserved names by name; the protocol requires they not
    ! be reachable by enumeration.
    found = .false.
    st = m%get_input_var_names(names)
    do i = 1, size(names)
       if (index(names(i), 'serialization') > 0) found = .true.
    end do
    call expect_true(.not. found, "reserved names absent from input var names", nfail)

    found = .false.
    st = m%get_output_var_names(names)
    do i = 1, size(names)
       if (index(names(i), 'serialization') > 0) found = .true.
    end do
    call expect_true(.not. found, "reserved names absent from output var names", nfail)

    do i = 1, 10
       st = m%update()
    end do
    st = m%get_value('TG', tg_saved)
    st = m%get_value('SNEQV', sneqv_saved)

    ! Save: create -> size -> state -> free
    call expect_true(m%set_value('ngen::serialization_create', trigger) == BMI_SUCCESS, &
         "create succeeds", nfail)
    st = m%get_value('ngen::serialization_size', reported)
    reported_bytes = transfer(reported, 0_int64)
    call expect_true(st == BMI_SUCCESS .and. reported_bytes > 0, "size reports bytes", nfail)

    st = m%get_var_nbytes('ngen::serialization_state', nbytes)
    st = m%get_var_itemsize('ngen::serialization_state', itemsize)
    call expect_true(nbytes == reported_bytes, "reported size agrees with nbytes", nfail)

    allocate(snapshot(nbytes / itemsize))
    call expect_true(m%get_value('ngen::serialization_state', snapshot) == BMI_SUCCESS, &
         "state reads back", nfail)
    call expect_true(m%set_value('ngen::serialization_free', trigger) == BMI_SUCCESS, &
         "free succeeds", nfail)
    ! The caller pairs create with free, including on error paths, so free has to
    ! stay safe whenever it is issued.
    call expect_true(m%set_value('ngen::serialization_free', trigger) == BMI_SUCCESS, &
         "free is safe to repeat", nfail)
    ! Freeing clears the metadata too, or a host reading size to decide whether a
    ! snapshot is available is told one exists after it is gone
    st = m%get_value('ngen::serialization_size', reported)
    call expect_true(st == BMI_SUCCESS .and. transfer(reported, 0_int64) == 0, &
         "size reports 0 after free", nfail)

    ! Restore announces the byte count, then delivers the bytes
    do i = 1, 10
       st = m%update()
    end do
    st = m%get_value('TG', tg_drifted)
    call expect_true(abs(tg_drifted(1) - tg_saved(1)) > 1.0e-6, &
         "model moved after the snapshot (else the restore proves nothing)", nfail)

    ! The announced size sizes a transfer, so it is bounded where it arrives
    ! rather than at the metadata call that reads it back
    announce = transfer(-8_int64, 0, 2)
    call expect_true(m%set_value('ngen::serialization_size', announce) /= BMI_SUCCESS, &
         "a negative announced size is refused", nfail)
    announce = transfer(4294967296_int64, 0, 2)
    call expect_true(m%set_value('ngen::serialization_size', announce) /= BMI_SUCCESS, &
         "an announced size past the transfer width is refused", nfail)
    announce = transfer(reported_bytes + 1, 0, 2)
    call expect_true(m%set_value('ngen::serialization_size', announce) /= BMI_SUCCESS, &
         "an announced size that is not whole items is refused", nfail)
    st = m%get_var_nbytes('ngen::serialization_state', nbytes)
    call expect_true(nbytes == 0, "a refused announcement is not stored", nfail)

    announce = transfer(reported_bytes, 0, 2)
    call expect_true(m%set_value('ngen::serialization_size', announce) == BMI_SUCCESS, &
         "announced size accepted", nfail)
    st = m%get_var_nbytes('ngen::serialization_state', nbytes)
    call expect_true(nbytes == reported_bytes, "state nbytes follows the announced size", nfail)

    call expect_true(m%set_value('ngen::serialization_state', snapshot) == BMI_SUCCESS, &
         "restore succeeds", nfail)
    st = m%get_value('TG', tg_restored)
    st = m%get_value('SNEQV', sneqv_restored)
    call expect_true(abs(tg_restored(1) - tg_saved(1)) < 1.0e-5, "TG restored", nfail)
    call expect_true(abs(sneqv_restored(1) - sneqv_saved(1)) < 1.0e-5, "SNEQV restored", nfail)

    st = m%finalize()
  end subroutine test_bmi_protocol


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

end program serialization_test

module messagepack

    ! implement buffer or c++ vector
    ! implement static buffer?
    ! implement packing
    ! implement unpacking
    ! implement file io
    use iso_fortran_env
    use messagepack_value
    use messagepack_user
    use byte_utilities

    implicit none
contains

    subroutine print_bytes_as_hex(bytes, addhexmark)
        ! prints a buffer of bytes as the unsigned hex version
        ! @param[in] bytes - byte buffer to print
        ! @param[in] addhexmark - If true, print with 0x prepended
        ! @returns none
        byte, dimension(:), allocatable, intent(in) :: bytes
        logical, intent(in) :: addhexmark

        integer :: i
        integer :: val
        write(*, "(A2)", advance="no") "[ "
        if (addhexmark) then
            do i = 1,size(bytes)
                val = int8_as_unsigned(bytes(i))
                write(*, '("0x", Z2.2, " ")', advance="no") val
            end do
        else
            do i = 1,size(bytes)
                val = int8_as_unsigned(bytes(i))
                write(*, '(Z2.2, " ")', advance="no") val
            end do
        end if
        write(*,*) "]"
    end subroutine

    subroutine unpack_array_int_1d(obj, om, errored)
        ! Attempts to unpack a 1d messagepack array of integers
        ! Note: does not check `is_unsigned`.
        ! @param[in] obj - messagepack object
        ! @param[out] om - dynamically allocated matrix
        ! @param[out] errored - .true. if an error occurred
        class(mp_value_type), allocatable, intent(in) :: obj
        integer(kind=int64), dimension(:), allocatable, intent(out) :: om
        logical, intent(out) :: errored

        ! variables
        logical :: stat
        integer(kind=int64) :: i, val, l
        class(mp_arr_type), allocatable :: arr

        errored = .true.
        call get_arr_ref(obj, arr, stat)
        if (.not.(stat)) then
            return
        end if
        ! initialize output
        l = arr%numelements()
        allocate(om(l))
        do i = 1,l
            call get_int(arr%values(i)%obj, val, stat)
            if (.not.(stat)) then
                return
            end if
            om(i) = val
        end do
        errored = .false.
    end subroutine

    subroutine unpack_array_real_1d(obj, om, errored)
        ! Attempts to unpack a 1d messagepack array of reals
        ! @param[in] obj - messagepack object
        ! @param[out] om - dynamically allocated matrix
        ! @param[out] errored - .true. if an error occurred
        class(mp_value_type), allocatable, intent(in) :: obj
        real(kind=real64), dimension(:), allocatable, intent(out) :: om
        logical, intent(out) :: errored

        ! variables
        logical :: stat
        integer(kind=int64) :: i, l
        real(kind=real64) :: val
        class(mp_arr_type), allocatable :: arr

        errored = .true.
        call get_arr_ref(obj, arr, stat)
        if (.not.(stat)) then
            return
        end if
        ! initialize output
        l = arr%numelements()
        allocate(om(l))
        do i = 1,l
            call get_real(arr%values(i)%obj, val, stat)
            if (.not.(stat)) then
                return
            end if
            om(i) = val
        end do
        errored = .false.
    end subroutine

end module

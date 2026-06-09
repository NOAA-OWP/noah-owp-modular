! defines a class that stores callbacks for handling
! user extensions
module messagepack_user
    use iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use messagepack_value
    use byte_utilities

    implicit none

    private

    public :: msgpack, unpack_func, unpack_callback
    public :: mp_timestamp_type, is_timestamp, get_timestamp_ref, register_extension

    integer, parameter, public :: MP_TS_EXT = -1
    
    abstract interface
        subroutine unpack_func(buffer, byteadvance, is_little_endian, mpv, successful)
            import int64, mp_value_type
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful
        end subroutine
    end interface

    type :: unpack_callback
        procedure(unpack_func), pointer, nopass :: cb => null()
    end type

    ! top level class where user is expected to interact with
    ! messagepack utilities
    type :: msgpack
        class(unpack_callback), allocatable, dimension(:) :: f1
        class(unpack_callback), allocatable, dimension(:) :: f2
        class(unpack_callback), allocatable, dimension(:) :: f4
        class(unpack_callback), allocatable, dimension(:) :: f8
        class(unpack_callback), allocatable, dimension(:) :: f16
        class(unpack_callback), allocatable, dimension(:) :: e8
        class(unpack_callback), allocatable, dimension(:) :: e16
        class(unpack_callback), allocatable, dimension(:) :: e32
        logical, dimension(256) :: f1_allocated
        logical, dimension(256) :: f2_allocated
        logical, dimension(256) :: f4_allocated
        logical, dimension(256) :: f8_allocated
        logical, dimension(256) :: f16_allocated
        logical, dimension(256) :: e8_allocated
        logical, dimension(256) :: e16_allocated
        logical, dimension(256) :: e32_allocated

        logical :: is_little_endian
        logical :: fail_flag
        character(:), allocatable :: error_message
        logical :: extra_bytes
    contains
        procedure :: register_extension
        procedure :: register_extension_super
        procedure :: print_value
        procedure :: print_value_with_args
        procedure :: print_version
        procedure :: failed
        procedure :: pack_alloc
        procedure :: pack_prealloc
        procedure :: unpack
        procedure :: unpack_buf
        procedure :: is_available
        procedure :: unpack_value
        procedure :: unpack_map
        procedure :: unpack_ext
        procedure :: unpack_array
        procedure :: extra_bytes_is_error
        procedure :: check_size
    end type
    interface msgpack
        procedure :: new_mp
    end interface
    
    ! #region messagepack defined extensions go here
    type, extends(mp_value_type) :: mp_timestamp_type
        integer(kind=int64) :: seconds
        integer(kind=int64) :: nanoseconds ! this must be positive
    contains
        procedure :: getsize => get_size_timestamp
        procedure :: pack => pack_timestamp
    end type
    interface mp_timestamp_type
        procedure :: new_timestamp
    end interface
    ! #endregion

    contains
        type(msgpack) function new_mp()
            logical :: err
            procedure(unpack_func), pointer :: p
            integer :: i
            allocate(new_mp%f1(256))
            allocate(new_mp%f2(256))
            allocate(new_mp%f4(256))
            allocate(new_mp%f8(256))
            allocate(new_mp%f16(256))
            allocate(new_mp%e8(256))
            allocate(new_mp%e16(256))
            allocate(new_mp%e32(256))
            do i = 1,256
                new_mp%f1_allocated  = .false.
                new_mp%f2_allocated  = .false.
                new_mp%f4_allocated  = .false.
                new_mp%f8_allocated  = .false.
                new_mp%f16_allocated = .false.
                new_mp%e8_allocated  = .false.
                new_mp%e16_allocated = .false.
                new_mp%e32_allocated = .false.
            end do

            ! AFAIK there is no stdlib equivalent of C++20 std::endian
            new_mp%is_little_endian = detect_little_endian()
            new_mp%fail_flag = .false.
            new_mp%error_message = ''
            new_mp%extra_bytes = .true.

            ! add timestamp here
            p => unpack_timestamp_32
            call new_mp%register_extension_super(MP_FE4, -1_int8, p, err)
            p => unpack_timestamp_64
            call new_mp%register_extension_super(MP_FE8, -1_int8, p, err)
            p => unpack_timestamp_96
            call new_mp%register_extension_super(MP_E8, -1_int8, p, err)
        end function

        subroutine extra_bytes_is_error(this, val)
            ! manipulate this flag
            class(msgpack) :: this
            logical, intent(in) :: val
            this%extra_bytes = val
        end subroutine

        subroutine print_version(this)
            class(msgpack) :: this
            print *, "0.3.1"
        end subroutine

        logical function failed(this)
            class(msgpack) :: this
            failed = this%fail_flag
        end function

        type(mp_timestamp_type) function new_timestamp(sec, ns)
            integer(kind=int64) :: sec
            integer(kind=int64) :: ns
            new_timestamp%seconds     = sec
            new_timestamp%nanoseconds = abs(ns)
        end function

        subroutine register_extension(this, ext, typeid, cb, error)
            ! Registers callbacks for handling extensions
            ! Only allows registering ids [0 127]
            class(msgpack) :: this
            integer, intent(in) :: ext
            integer(kind=int8), intent(in) :: typeid
            procedure(unpack_func), pointer, intent(in) :: cb
            logical, intent(out) :: error

            if (typeid < 0) then
                error = .true.
                return
            end if
            call this%register_extension_super(ext, typeid, cb, error)
        end subroutine

        subroutine register_extension_super(this, ext, typeid, cb, error)
            ! Registers callbacks for handling extensions
            ! allows ids [-128 127]
            class(msgpack) :: this
            integer, intent(in) :: ext
            integer(kind=int8), intent(in) :: typeid
            procedure(unpack_func), pointer, intent(in) :: cb
            logical, intent(out) :: error

            integer :: arr_index

            arr_index = typeid + 129 ! [-128, 127] -> [1, 256]

            select case(ext)
            case (MP_FE1)
                this%f1(arr_index)%cb => cb
                this%f1_allocated(arr_index) = .true.
            case (MP_FE2)
                this%f2(arr_index)%cb => cb
                this%f2_allocated(arr_index) = .true.
            case (MP_FE4)
                this%f4(arr_index)%cb => cb
                this%f4_allocated(arr_index) = .true.
            case (MP_FE8)
                this%f8(arr_index)%cb => cb
                this%f8_allocated(arr_index) = .true.
            case (MP_FE16)
                this%f16(arr_index)%cb => cb
                this%f16_allocated(arr_index) = .true.
            case (MP_E8)
                this%e8(arr_index)%cb => cb
                this%e8_allocated(arr_index) = .true.
            case (MP_E16)
                this%e16(arr_index)%cb => cb
                this%e16_allocated(arr_index) = .true.
            case (MP_E32)
                this%e32(arr_index)%cb => cb
                this%e16_allocated(arr_index) = .true.
            end select

            error = .false.
        end subroutine

        ! PACKING
        subroutine pack_alloc(this, mpv, buffer)
            ! Packs a messagepack object into a dynamically
            ! allocated buffer, returned to the user. The user
            ! must handle deallocation.
            ! @param[in] this - self
            ! @param[in] mpv - messagepack value to pack
            ! @param[out] buffer - will contain serialized data
            class(msgpack) :: this
            class(mp_value_type) :: mpv
            byte, allocatable, dimension(:), intent(out) :: buffer
            integer(kind=int64) :: dblen
            integer(kind=int64) :: numused

            call mpv%getsize(dblen) ! get buffer size required
            allocate(buffer(dblen)) ! allocate buffer

            call mpv%pack(buffer, numused, this%fail_flag)
            if (.not.(this%fail_flag)) then
                if (dblen /= numused) then
                    this%fail_flag = .true.
                    this%error_message = 'Internal Error: packing failed'
                end if
            end if
        end subroutine

        subroutine pack_prealloc(this, mpv, bytes_used, buffer)
            ! Packs a messagepack object into a pre-allocated buffer,
            ! returned to the user. This function does not check beforehand
            ! for the array being the correct size, and will return an error
            ! if the buffer is too small.
            class(msgpack) :: this
            class(mp_value_type) :: mpv
            integer(kind=int64), intent(out) :: bytes_used
            byte, allocatable, dimension(:), intent(inout) :: buffer

            call mpv%pack(buffer, bytes_used, this%fail_flag)
        end subroutine

        subroutine unpack(this, buffer, mpv)
            ! Unpack a MsgPack value from a buffer.
            ! - nominally contains a single value
            ! @param[in] this - self
            ! @param[in] buffer - serialized messagepack data
            ! @param[out] mpv - Deserialized value
            class(msgpack) :: this
            byte, dimension(:), intent(in) :: buffer
            class(mp_value_type), allocatable, intent(out) :: mpv

            integer(kind=int64) :: numbytes

            call this%unpack_buf(buffer, mpv, numbytes)
            if (numbytes < size(buffer) .and. this%extra_bytes) then
                ! configurable error
                this%fail_flag = .true.
                write(this%error_message, '(i0) (A)') size(buffer) - numbytes, ' extra bytes unused'
            else if (numbytes > size(buffer)) then
                this%fail_flag = .true. ! bug within reporting byte mechanism
                write(this%error_message, '(A) (i0)') "internal error. number of bytes exceeds buffer size by: ", &
                numbytes - size(buffer)
            end if
        end subroutine

        subroutine unpack_buf(this, buffer, mpv, numbytes)
            ! Unpack a single value from a buffer. Additionally returns
            !   the number of bytes used, in case the buffer has multiple
            !   MessagePack values within it or is a rolling buffer, etc.
            ! @param[in] this - self
            ! @param[in] buffer - serialized messagepack data
            ! @param[out] mpv - Deserialized value
            ! @param[out] numbytes - Number of bytes used in the buffer
            class(msgpack) :: this
            byte, dimension(:), intent(in) :: buffer
            class(mp_value_type), allocatable, intent(out) :: mpv
            integer(kind=int64), intent(out) :: numbytes

            logical :: successful

            this%fail_flag = .false.
            call this%unpack_value(buffer, numbytes, mpv, successful)
            this%fail_flag = .not.(successful)

            if (numbytes > size(buffer)) then
                this%fail_flag = .true. ! bug within reporting byte mechanism
                write(this%error_message, '(A) (i0)') "internal error. number of bytes exceeds buffer size by: ", &
                numbytes - size(buffer)
            end if
        end subroutine

        logical function is_available(this, buffer)
            ! Returns true if the buffer contains at least 1 complete
            ! messagepack value
            ! @param[in] this - instance
            ! @param[in] buffer - serialized data
            ! @returns - .true. if a complete messagepack value exists
            class(msgpack) :: this
            byte, dimension(:) :: buffer

            logical :: error
            integer(kind=int64) :: numbytes

            call this%check_size(buffer, .true., numbytes, error)
            is_available = .not.(error)
        end function

        subroutine print_value(this, obj)
            ! Prints MessagePack object with default options
            ! @param[in] this - instance
            ! @param[in] obj - MessagePack object to print
            class(msgpack) :: this
            class(mp_value_type), intent(in) :: obj
            call this%print_value_with_args(obj, 0, .false., -1)
        end subroutine

        recursive subroutine print_value_with_args(this, obj, indentation, &
                sameline, maxelems)
            ! Prints MessagePack object with a variety of configurability
            ! @param[in] this - instance
            ! @param[in] obj - MessagePack object to print in a pretty fashion
            ! @param[in] indentation - number of levels of indentation to print with
            ! @param[in] sameline - if true, compacts the output
            ! @param[in] maxelems - if non-negative, limits number of elements printed
            ! @returns None
            class(msgpack), intent(in) :: this
            class(mp_value_type), intent(in) :: obj
            integer, intent(in) :: indentation
            logical, intent(in) :: sameline
            integer, intent(in) :: maxelems
            integer(kind=int64) :: i, j, ind

            if (.not. sameline) then
                do i = 1,indentation
                    write(*, "(A2)", advance="no") "  "
                end do
            end if
    
            select type(obj)
            class is (mp_nil_type)
                write(*, "(A)", advance="no") "nil"
            class is (mp_bool_type)
                if (obj%value) then
                    write(*, "(A)", advance="no") "true"
                else
                    write(*, "(A)", advance="no") "false"
                end if
            class is (mp_int_type)
                if (obj%unsigned_64) then
                    write(*, "(I0, A)", advance="no") obj%value, "[OUT-OF-RANGE]"
                else
                    write(*, "(I0)", advance="no") obj%value
                end if
                
            class is (mp_float_type)
                if (obj%is_64) then
                    write(*, "(F0.0)", advance="no") obj%f64value
                else
                    write(*, "(F0.0)", advance="no") obj%f32value
                end if
            class is (mp_str_type)
                write(*, "(A, A, A)", advance="no") char(34), obj%value, char(34)
            class is (mp_arr_type)
                write(*, "(A)", advance="no") "["
                printarr : do j = 1,obj%numelements()
                    call this%print_value_with_args(obj%values(j)%obj, 0, .true., maxelems)
                    write(*, "(A)", advance="no") ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A3)") "..."
                        exit printarr
                    end if
                end do printarr
                write(*, "(A)", advance="no") "]" 
            class is (mp_map_type)
                write(*, "(A)") "{"
                printmap : do j = 1, obj%numelements()
                    do i = 1,indentation+1
                        write(*, "(A2)", advance="no") "  "
                    end do
                    call this%print_value_with_args(obj%keys(j)%obj, indentation + 1, &
                        .true., maxelems)
                    write(*, "(A)", advance="no") " => "
                    call this%print_value_with_args(obj%values(j)%obj, indentation + 1, &
                        .true., maxelems)
                    print *, ","
                    if (maxelems > 0 .and. i > maxelems) then
                        write(*, "(A3)") "..."
                        exit printmap
                    end if
                end do printmap
                if (.not. sameline) then
                    do i = 1,indentation
                        write(*, "(A2)", advance="no") "  "
                    end do
                end if
                write(*, "(A)") "},"
            class is (mp_bin_type)
                write(*, "(A)", advance="no") "BIN["
                printbin : do j = 1,obj%numelements()
                    write(*, "(I0, A)", advance="no") obj%values(j), ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A)") "..."
                        exit printbin
                    end if
                end do printbin
                write(*, "(A)", advance="no") "]"
            class is (mp_ext_type)
                ind = obj%exttype + 129 ! TODO
                write(*, "(A)", advance="no") "EXT["
                printext : do j = 1,obj%numelements()
                    write(*, "(I0, A)", advance="no") obj%values(j), ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A)") "..."
                        exit printext
                    end if
                end do printext
                write(*, "(A)", advance="no") "]"
            end select
            if (.not. sameline) then
                print *, ""
            end if
        end subroutine

        subroutine get_size_timestamp(this, osize)
            class(mp_timestamp_type) :: this
            integer(kind=int64), intent(out) :: osize
            if (this%nanoseconds == 0 .and. this%seconds <= 4294967296_int64 .and. &
                    this%seconds >= 0) then
                osize = 6  ! timestamp32
            else if (this%nanoseconds  <=  1073741824_int64 &
                    .and. this%seconds <= 17179869184_int64 &
                    .and. this%seconds >= 0) then
                ! nanoseconds fit into uint30, seconds fit into uint34
                osize = 10 ! timestamp32
            else
                osize = 15 ! timestamp96
            end if
        end subroutine

        subroutine pack_timestamp(this, buf, num, error)
            class(mp_timestamp_type) :: this
            byte, dimension(:) :: buf
            integer(kind=int64), intent(out) :: num
            logical, intent(out) :: error

            integer(kind=int64) :: temp
            call this%getsize(num)
            if (num > size(buf)) then
                error = .true.
                return
            end if

            select case (num)
            case (6)  ! timestamp32
                buf(1) = MP_FE4
                buf(2) = MP_TS_EXT
                call int_to_bytes_be_4(buf(3:6), int(this%seconds, kind=int32))
            case (10) ! timestamp64
                buf(1) = MP_FE8
                buf(2) = MP_TS_EXT
                temp = this%seconds
                call mvbits(this%seconds, 0, 34, temp, 0)
                call mvbits(this%nanoseconds, 0, 30, temp, 34)
                call int_to_bytes_be_8(buf(3:10), temp)
            case (15) ! timestamp96
                buf(1) = MP_E8
                buf(2) = 12
                buf(3) = MP_TS_EXT
                call int_to_bytes_be_4(buf(4:7), int(this%nanoseconds, kind=int32))
                call int_to_bytes_be_8(buf(8:15), this%seconds)
            end select

            error = .false.
        end subroutine

        subroutine unpack_timestamp_32(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int32) :: temp

            if (size(buffer(byteadvance+1:)) < 4) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_4(buffer(byteadvance+1:byteadvance+4), is_little_endian)
            mpv = mp_timestamp_type(temp, 0)
            byteadvance = byteadvance + 4

            successful = .true.
        end subroutine

        subroutine unpack_timestamp_64(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: temp, temp1, temp2

            if (size(buffer(byteadvance+1:)) < 8) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_8(buffer(byteadvance+1:byteadvance+8), is_little_endian)
            temp1 = 0
            temp2 = 0
            call mvbits(temp, 0, 34, temp1, 0)
            call mvbits(temp, 34, 30, temp2, 0)
            mpv = mp_timestamp_type(temp1, temp2)
            byteadvance = byteadvance + 8

            successful = .true.
        end subroutine

        subroutine unpack_timestamp_96(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int32) :: temp
            integer(kind=int64) :: temp2

            if (size(buffer(byteadvance+1:)) < 12) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_4(buffer(byteadvance+1:byteadvance+4), is_little_endian)
            byteadvance = byteadvance + 4
            temp2 = bytes_be_to_int_8(buffer(byteadvance+1:byteadvance+8), is_little_endian)
            mpv = mp_timestamp_type(temp2, temp)
            byteadvance = byteadvance + 8

            successful = .true.
        end subroutine

        function is_timestamp(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type(obj)
            class is (mp_timestamp_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_timestamp

        subroutine get_timestamp_ref(obj, val, stat)
            class(mp_value_type), intent(in) :: obj
            class(mp_timestamp_type), allocatable, intent(out) :: val
            logical, intent(out) :: stat

            select type(obj)
            class is (mp_timestamp_type)
                val = obj
                stat = .true.
            class default
                stat = .false.
            end select
        end subroutine

        ! unpacking shenanigans
        recursive subroutine check_size(this, buffer, recurse, &
                byteadvance, error)
            class(msgpack) :: this
            byte, dimension(:), intent(in) :: buffer
            logical, intent(in) :: recurse
            integer(kind=int64), intent(out) :: byteadvance
            logical, intent(out) :: error

            ! temp variables
            integer(kind=int64) :: length, i64_temp, i
            byte :: i8_temp
            integer(kind=int16) :: i16_temp
            integer(kind=int32) :: i32_temp

            ! set default output values
            error = .false.
            byteadvance = 1

            ! need to have data available to read
            length = size(buffer)
            if (length == 0) then
                error = .true.
                this%error_message = 'buffer is empty'
                return
            end if

            select case(buffer(1))
            case (MP_PFI_L:MP_PFI_H, MP_NIL, MP_T, MP_F)
                ! only a single byte is needed, all good
            case (MP_U8, MP_I8)
                byteadvance = 2
            case (MP_U16, MP_I16, MP_FE1)
                byteadvance = 3
            case (MP_FE2)
                byteadvance = 4
            case (MP_U32, MP_I32, MP_F32)
                byteadvance = 5
            case (MP_FE4)
                byteadvance = 6
            case (MP_U64, MP_I64, MP_F64)
                byteadvance = 9
            case (MP_FE8)
                byteadvance = 10
            case (MP_FE16)
                byteadvance = 18
            ! dynamic length values
            case (MP_FS_L:MP_FS_H)
                ! length in first 5 bits
                i8_temp = 0
                call mvbits(buffer(1), 0, 5, i8_temp, 0) ! get fixstr length
                byteadvance = 1_int64 + i8_temp
            case (MP_S8, MP_B8)
                ! length with 1 byte
                i32_temp = int8_as_unsigned(buffer(2))
                byteadvance = 1 + i32_temp
            case (MP_S16, MP_B16)
                ! length with 2 byte
                i16_temp = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                byteadvance = 1 + i16_temp
                if (length < 1 + byteadvance) then
                    error = .true.
                end if
            case (MP_S32, MP_B32)
                ! length with 4 byte
                i32_temp = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                byteadvance = 1 + i32_temp
                if (length < 1 + i32_temp) then
                    error = .true.
                end if
            ! containers
            case (MP_FA_L:MP_FA_H, MP_FM_L:MP_FM_H)
                ! length with first 4 bits
                i8_temp = 0
                call mvbits(buffer(1), 0, 4, i8_temp, 0) ! get fixarr, fixmap length
                ! recurse
                if (recurse) then
                    do i = 1,i8_temp
                        call this%check_size(buffer(byteadvance+1:), recurse, &
                            i64_temp, error)
                        if (error) then
                            return
                        end if
                        byteadvance = byteadvance + i64_temp
                    end do
                end if
            case (MP_A16, MP_M16)
                ! length with 2 byte
                byteadvance = 3
                if (length < byteadvance) then
                    error = .true.
                    return
                end if
                i16_temp = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                ! recurse
                if (recurse) then
                    do i = 1,i16_temp
                        call this%check_size(buffer(byteadvance+1:), recurse, &
                            i64_temp, error)
                        if (error) then
                            return
                        end if
                        byteadvance = byteadvance + i64_temp
                    end do
                end if
            case (MP_A32, MP_M32)
                ! length with 4 byte
                byteadvance = 5
                if (length < byteadvance) then
                    error = .true.
                    return
                end if
                i32_temp = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                ! recurse
                if (recurse) then
                    do i = 1,i32_temp
                        call this%check_size(buffer(byteadvance+1:), recurse, &
                            i64_temp, error)
                        if (error) then
                            return
                        end if
                        byteadvance = byteadvance + i64_temp
                    end do
                end if
            end select
            if (length < byteadvance) then
                error = .false.
            end if
            if (error) then
                this%error_message = 'not enough bytes'
            end if
        end subroutine
            
        recursive subroutine unpack_value(this, buffer, byteadvance, &
                mpv, successful)
            class(msgpack) :: this
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(out) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            ! other variables to use
            integer(kind=int64) :: length
            integer :: i
            integer(kind=int64) :: i_64
            byte :: btemp1 ! byte temp value
            integer(kind=int16) :: val_int16
            integer(kind=int32) :: val_int32
            integer(kind=int64) :: val_int64

            integer(kind=int64) :: i64_temp
            character(:), allocatable :: val_char

            logical :: error

            length = size(buffer)

            ! set default output values
            successful = .true.

            ! need to have data available to read
            if (length == 0) then
                successful = .false.
                this%error_message = 'buffer is empty'
                return
            end if

            ! check that the size for the entire header exists
            call this%check_size(buffer, .true., i64_temp, error)
            if (error) then
                successful = .false.
                this%error_message = 'insufficient size'
                return
            end if

            byteadvance = 1 ! default output value
            select case (buffer(1))
            case (MP_PFI_L:MP_PFI_H)
                ! the byte itself is the value
                mpv = mp_int_type(buffer(1))
            case (MP_FM_L:MP_FM_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 4, btemp1, 0) ! get fixmap length
                val_int64 = btemp1
                byteadvance = 1
                call this%unpack_map(val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_FA_L:MP_FA_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 4, btemp1, 0) ! get fixarray length
                byteadvance = 1
                call this%unpack_array(btemp1 + 0_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_FS_L:MP_FS_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 5, btemp1, 0) ! get fixstr length
                allocate(character(btemp1) :: val_char)
                do i = 1,btemp1
                    val_char(i:i) = transfer(buffer(1 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + btemp1
            case (MP_NIL)
                ! default is already nil
                mpv = mp_nil_type()
            case (MP_NU)
                successful = .false.
                this%error_message = 'Never Used detected. Invalid MsgPack'
            case (MP_F)
                mpv = mp_bool_type(.false.)
            case (MP_T)
                mpv = mp_bool_type(.true.)
            ! binary format family
            case (MP_B8)
                val_int32 = int8_as_unsigned(buffer(2))
                val_int64 = val_int32
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                class is (mp_bin_type)
                    mpv%values(:) = buffer(3:2+val_int64)
                class default
                    successful = .false.
                    this%error_message = 'internal error - bin8 cast'
                end select
                byteadvance = 2 + val_int64
            case (MP_B16)
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                val_int64 = int16_as_unsigned(val_int16)
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                class is (mp_bin_type)
                    mpv%values(:) = buffer(4:3+val_int64)
                class default
                    successful = .false.
                    this%error_message = 'internal error - bin16 bad cast'
                end select
                byteadvance = 3 + val_int64
            case (MP_B32)
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                class is (mp_bin_type)
                    mpv%values(:) = buffer(6:5+val_int64)
                class default
                    successful = .false.
                    this%error_message = 'internal error - bin32 bad cast'
                end select
                byteadvance = 5 + val_int64
            case (MP_E8)
                ! check for first 3 bytes
                i = buffer(3)
                byteadvance = 3
                call this%unpack_ext(int8_as_unsigned(buffer(2)) + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_E16)
                ! check for first 4 bytes
                i = buffer(4)
                byteadvance = 4
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                call this%unpack_ext(val_int16 + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_E32)
                ! check for first 6 bytes
                i = buffer(6)
                byteadvance = 6
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                call this%unpack_ext(val_int32 + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_F32)
                ! 4 bytes following
                mpv = new_real32(bytes_be_to_real_4(buffer(2:5), &
                    this%is_little_endian))
                byteadvance = 5
            case (MP_F64)
                ! 8 bytes following
                mpv = new_real64(bytes_be_to_real_8(buffer(2:9), this%is_little_endian))
                byteadvance = 9
            ! Unsigned integers >>>
            ! need to watch when grabbed values are negative
            case (MP_U8)
                ! 1 byte following
                mpv = mp_int_type(int8_as_unsigned(buffer(2)))
                byteadvance = 2
            case (MP_U16)
                ! 2 bytes following
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                mpv = mp_int_type(int16_as_unsigned(val_int16))
                byteadvance = 3
            case (MP_U32)
                ! 4 bytes following
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                mpv = mp_int_type(int32_as_unsigned(val_int32))
                byteadvance = 5
            case (MP_U64)
                ! 8 bytes following
                val_int64 = bytes_be_to_int_8(buffer(2:9), this%is_little_endian)
                if (val_int64 >= 0) then
                    mpv = mp_int_type(val_int64)
                else
                    mpv = mp_int_type(val_int64)
                    call set_unsigned(mpv)
                end if
                byteadvance = 9
            ! Signed integers >>>
            case (MP_I8)
                ! 1 byte following
                mpv = mp_int_type(buffer(2))
                byteadvance = 2
            case (MP_I16)
                ! 2 bytes following
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                mpv = mp_int_type(val_int32)
                byteadvance = 3
            case (MP_I32)
                ! 4 bytes following
                mpv = mp_int_type(bytes_be_to_int_4(buffer(2:5), this%is_little_endian))
                byteadvance = 5
            case (MP_I64)
                ! 8 bytes following
                mpv = mp_int_type(bytes_be_to_int_8(buffer(2:9), this%is_little_endian))
                byteadvance = 9
            ! ext format family
            case (MP_FE1)
                ! 3 bytes following
                i = buffer(2)
                byteadvance = 2
                call this%unpack_ext(1_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE2)
                ! 4 bytes following
                i = buffer(2)
                byteadvance = 2
                call this%unpack_ext(2_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE4)
                ! 6 bytes following
                i = buffer(2)
                byteadvance = 2
                call this%unpack_ext(4_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE8)
                ! 8 bytes following
                i = buffer(2)
                byteadvance = 2
                call this%unpack_ext(8_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE16)
                ! 18 bytes following
                i = buffer(2)
                byteadvance = 2
                call this%unpack_ext(16_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_S8)
                val_int16 = int8_as_unsigned(buffer(2))
                ! create string
                allocate(character(val_int16) :: val_char)
                do i = 1,val_int16
                    val_char(i:i) = transfer(buffer(2 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + val_int16
            case (MP_S16)
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                ! create string
                allocate(character(val_int32) :: val_char)
                do i = 1,val_int32
                    val_char(i:i) = transfer(buffer(3 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + val_int32
            case (MP_S32)
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                ! create string
                allocate(character(val_int64) :: val_char)
                do i_64 = 1_int64,val_int64
                    val_char(i_64:i_64) = transfer(buffer(3 + i_64), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1_int64 + val_int64
            case (MP_A16)
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                byteadvance = 3
                call this%unpack_array(int(val_int32, kind=int64), &
                    buffer, byteadvance, mpv, successful)
            case (MP_A32)
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                byteadvance = 5
                call this%unpack_array(val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_M16)
                val_int16 = bytes_be_to_int_2(buffer(2:3), this%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                byteadvance = 3
                call this%unpack_map(0_int64 + val_int32, buffer, byteadvance, &
                    mpv, successful)
            case (MP_M32)
                val_int32 = bytes_be_to_int_4(buffer(2:5), this%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                byteadvance = 5
                call this%unpack_map(val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_NFI_L:MP_NFI_H)
                ! it's the straight bit pattern there
                mpv = mp_int_type(buffer(1))
            end select
        end subroutine

        recursive subroutine unpack_array(this, length, buffer, &
                byteadvance, mpv, successful)
            class(msgpack) :: this
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any
            mpv = mp_arr_type(length)
            do i = 1,length
                call this%unpack_value(buffer(byteadvance+1:), tmp, &
                    val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if

                ! store the newly unpacked object into the array
                select type (mpv)
                class is (mp_arr_type)
                    mpv%values(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    this%error_message = 'internal error - unpack_array bad cast'
                end select
            end do
        end subroutine

        recursive subroutine unpack_map(this, length, buffer, byteadvance, &
                mpv, successful)
            class(msgpack) :: this
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any

            successful = .true.
            mpv = mp_map_type(length)
            do i = 1,length
                ! get key
                call this%unpack_value(buffer(byteadvance+1:), &
                    tmp, val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if
                select type (mpv)
                class is (mp_map_type)
                    mpv%keys(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    this%error_message = 'internal error - unpack_map bad cast'
                end select

                ! get value
                call this%unpack_value(buffer(byteadvance+1:), tmp, &
                    val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if
                select type (mpv)
                class is (mp_map_type)
                    mpv%values(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    print *, "[Error: something went terribly wrong"
                end select
            end do
        end subroutine

        subroutine unpack_ext(this, length, etype, buffer, byteadvance, &
                mpv, successful)
            class(msgpack) :: this
            integer(kind=int64), intent(in) :: length
            integer, intent(in) :: etype
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer :: ind
            if (length > size(buffer)) then
                successful = .false.
                return
            end if

            ! Custom extension handling
            ind = etype + 129
            if (ind < 1 .or. ind > 256) then
                successful = .false.
                return
            end if
            if (length == 1) then
                if (this%f1_allocated(ind)) then
                    call this%f1(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 2) then
                if (this%f2_allocated(ind)) then
                    call this%f2(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 4) then
                if (this%f4_allocated(ind)) then
                    call this%f4(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 8) then
                if (this%f8_allocated(ind)) then
                    call this%f8(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 16) then
                if (this%f16_allocated(ind)) then
                    call this%f16(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 256) then
                if (this%e8_allocated(ind)) then
                    call this%e8(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 65536) then
                if (this%e16_allocated(ind)) then
                    call this%e16(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 4294967296_int64) then
                if (this%e32_allocated(ind)) then
                    call this%e32(ind)%cb(buffer, byteadvance, &
                        this%is_little_endian, mpv, successful)
                        return
                end if
            end if

            ! regular extension
            mpv = mp_ext_type(etype, length)
            successful = .true.
            select type(mpv)
            class is (mp_ext_type)
                mpv%values = buffer(byteadvance+1:byteadvance+length)
                byteadvance = byteadvance + length
            class default
                successful = .false.
                deallocate(mpv)
                this%error_message = 'internal error - unpack_ext bad cast'
            end select
        end subroutine
end module

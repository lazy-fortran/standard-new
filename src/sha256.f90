module sha256
    !! Small streaming SHA-256 implementation for provenance writers.

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    implicit none
    private

    integer(int64), parameter :: two32 = 4294967296_int64
    integer(int64), parameter :: two31 = 2147483648_int64
    integer(int32), parameter :: round_constant(64) = [ &
        int(z'428A2F98', int32), int(z'71374491', int32), int(z'B5C0FBCF', int32), &
        int(z'E9B5DBA5', int32), int(z'3956C25B', int32), int(z'59F111F1', int32), &
        int(z'923F82A4', int32), int(z'AB1C5ED5', int32), int(z'D807AA98', int32), &
        int(z'12835B01', int32), int(z'243185BE', int32), int(z'550C7DC3', int32), &
        int(z'72BE5D74', int32), int(z'80DEB1FE', int32), int(z'9BDC06A7', int32), &
        int(z'C19BF174', int32), int(z'E49B69C1', int32), int(z'EFBE4786', int32), &
        int(z'0FC19DC6', int32), int(z'240CA1CC', int32), int(z'2DE92C6F', int32), &
        int(z'4A7484AA', int32), int(z'5CB0A9DC', int32), int(z'76F988DA', int32), &
        int(z'983E5152', int32), int(z'A831C66D', int32), int(z'B00327C8', int32), &
        int(z'BF597FC7', int32), int(z'C6E00BF3', int32), int(z'D5A79147', int32), &
        int(z'06CA6351', int32), int(z'14292967', int32), int(z'27B70A85', int32), &
        int(z'2E1B2138', int32), int(z'4D2C6DFC', int32), int(z'53380D13', int32), &
        int(z'650A7354', int32), int(z'766A0ABB', int32), int(z'81C2C92E', int32), &
        int(z'92722C85', int32), int(z'A2BFE8A1', int32), int(z'A81A664B', int32), &
        int(z'C24B8B70', int32), int(z'C76C51A3', int32), int(z'D192E819', int32), &
        int(z'D6990624', int32), int(z'F40E3585', int32), int(z'106AA070', int32), &
        int(z'19A4C116', int32), int(z'1E376C08', int32), int(z'2748774C', int32), &
        int(z'34B0BCB5', int32), int(z'391C0CB3', int32), int(z'4ED8AA4A', int32), &
        int(z'5B9CCA4F', int32), int(z'682E6FF3', int32), int(z'748F82EE', int32), &
        int(z'78A5636F', int32), int(z'84C87814', int32), int(z'8CC70208', int32), &
        int(z'90BEFFFA', int32), int(z'A4506CEB', int32), int(z'BEF9A3F7', int32), &
        int(z'C67178F2', int32)]

    type, public :: sha256_context_t
        private
        integer(int32) :: state(8) = 0_int32
        integer(int8) :: buffer(64) = 0_int8
        integer :: buffer_length = 0
        integer(int64) :: total_bytes = 0_int64
    end type sha256_context_t

    public :: sha256_final
    public :: sha256_init
    public :: sha256_update

contains

    subroutine sha256_init(context)
        type(sha256_context_t), intent(out) :: context

        context%state = [ &
            int(z'6A09E667', int32), int(z'BB67AE85', int32), &
            int(z'3C6EF372', int32), int(z'A54FF53A', int32), &
            int(z'510E527F', int32), int(z'9B05688C', int32), &
            int(z'1F83D9AB', int32), int(z'5BE0CD19', int32)]
        context%buffer = 0_int8
        context%buffer_length = 0
        context%total_bytes = 0_int64
    end subroutine sha256_init

    subroutine sha256_update(context, bytes)
        type(sha256_context_t), intent(inout) :: context
        integer(int8), intent(in) :: bytes(:)
        integer :: first, available, take

        first = 1
        do while (first <= size(bytes))
            available = 64 - context%buffer_length
            take = min(available, size(bytes) - first + 1)
            context%buffer(context%buffer_length + 1:context%buffer_length + take) = &
                bytes(first:first + take - 1)
            context%buffer_length = context%buffer_length + take
            context%total_bytes = context%total_bytes + int(take, int64)
            first = first + take
            if (context%buffer_length == 64) then
                call compress(context, context%buffer)
                context%buffer_length = 0
            end if
        end do
    end subroutine sha256_update

    subroutine sha256_final(context, digest)
        type(sha256_context_t), intent(in) :: context
        integer(int8), intent(out) :: digest(32)
        type(sha256_context_t) :: work
        integer(int64) :: bit_count
        integer :: i, n

        work = context
        bit_count = 8_int64 * context%total_bytes
        n = work%buffer_length
        n = n + 1
        work%buffer(n) = int(z'80', int8)
        if (n > 56) then
            if (n < 64) work%buffer(n + 1:64) = 0_int8
            call compress(work, work%buffer)
            n = 0
        end if
        if (n < 56) work%buffer(n + 1:56) = 0_int8
        do i = 0, 7
            work%buffer(57 + i) = int(iand(shiftr(bit_count, 8 * (7 - i)), 255_int64), int8)
        end do
        call compress(work, work%buffer)

        do i = 1, 8
            digest(4 * i - 3) = int(iand(shiftr(int(work%state(i), int64), 24), 255_int64), int8)
            digest(4 * i - 2) = int(iand(shiftr(int(work%state(i), int64), 16), 255_int64), int8)
            digest(4 * i - 1) = int(iand(shiftr(int(work%state(i), int64), 8), 255_int64), int8)
            digest(4 * i) = int(iand(int(work%state(i), int64), 255_int64), int8)
        end do
    end subroutine sha256_final

    subroutine compress(context, block)
        type(sha256_context_t), intent(inout) :: context
        integer(int8), intent(in) :: block(64)
        integer(int32) :: schedule(64), working(8), t1, t2
        integer :: i

        do i = 1, 16
            schedule(i) = iand(ishft(byte_value(block(4 * i - 3)), 24), int(z'FF000000', int32))
            schedule(i) = ior(schedule(i), iand(ishft(byte_value(block(4 * i - 2)), 16), &
                int(z'00FF0000', int32)))
            schedule(i) = ior(schedule(i), iand(ishft(byte_value(block(4 * i - 1)), 8), &
                int(z'0000FF00', int32)))
            schedule(i) = ior(schedule(i), byte_value(block(4 * i)))
        end do
        do i = 17, 64
            schedule(i) = add4(small_sigma1(schedule(i - 2)), schedule(i - 7), &
                small_sigma0(schedule(i - 15)), schedule(i - 16))
        end do

        working = context%state
        do i = 1, 64
            t1 = add5(working(8), big_sigma1(working(5)), choose(working(5), working(6), working(7)), &
                round_constant(i), schedule(i))
            t2 = add2(big_sigma0(working(1)), majority(working(1), working(2), working(3)))
            working(8) = working(7)
            working(7) = working(6)
            working(6) = working(5)
            working(5) = add2(working(4), t1)
            working(4) = working(3)
            working(3) = working(2)
            working(2) = working(1)
            working(1) = add2(t1, t2)
        end do
        do i = 1, 8
            context%state(i) = add2(context%state(i), working(i))
        end do
    end subroutine compress

    integer(int32) function byte_value(value) result(unsigned)
        integer(int8), intent(in) :: value

        unsigned = int(value, int32)
        if (unsigned < 0) unsigned = unsigned + 256_int32
    end function byte_value

    integer(int32) function add2(left, right) result(sum)
        integer(int32), intent(in) :: left, right
        integer(int64) :: value

        value = unsigned_value(left) + unsigned_value(right)
        sum = signed_value(value)
    end function add2

    integer(int32) function add4(a, b, c, d) result(sum)
        integer(int32), intent(in) :: a, b, c, d

        sum = add2(add2(a, b), add2(c, d))
    end function add4

    integer(int32) function add5(a, b, c, d, e) result(sum)
        integer(int32), intent(in) :: a, b, c, d, e

        sum = add2(add4(a, b, c, d), e)
    end function add5

    integer(int64) function unsigned_value(value) result(unsigned)
        integer(int32), intent(in) :: value

        unsigned = int(value, int64)
        if (unsigned < 0) unsigned = unsigned + two32
    end function unsigned_value

    integer(int32) function signed_value(value) result(signed)
        integer(int64), intent(in) :: value
        integer(int64) :: reduced

        reduced = modulo(value, two32)
        if (reduced >= two31) then
            signed = int(reduced - two32, int32)
        else
            signed = int(reduced, int32)
        end if
    end function signed_value

    integer(int32) function rotate_right(value, amount) result(rotated)
        integer(int32), intent(in) :: value
        integer, intent(in) :: amount

        rotated = ior(shiftr(value, amount), ishft(value, 32 - amount))
    end function rotate_right

    integer(int32) function choose(x, y, z) result(value)
        integer(int32), intent(in) :: x, y, z

        value = ieor(iand(x, y), iand(not(x), z))
    end function choose

    integer(int32) function majority(x, y, z) result(value)
        integer(int32), intent(in) :: x, y, z

        value = ieor(ieor(iand(x, y), iand(x, z)), iand(y, z))
    end function majority

    integer(int32) function big_sigma0(value) result(result_value)
        integer(int32), intent(in) :: value

        result_value = ieor(ieor(rotate_right(value, 2), rotate_right(value, 13)), &
            rotate_right(value, 22))
    end function big_sigma0

    integer(int32) function big_sigma1(value) result(result_value)
        integer(int32), intent(in) :: value

        result_value = ieor(ieor(rotate_right(value, 6), rotate_right(value, 11)), &
            rotate_right(value, 25))
    end function big_sigma1

    integer(int32) function small_sigma0(value) result(result_value)
        integer(int32), intent(in) :: value

        result_value = ieor(ieor(rotate_right(value, 7), rotate_right(value, 18)), shiftr(value, 3))
    end function small_sigma0

    integer(int32) function small_sigma1(value) result(result_value)
        integer(int32), intent(in) :: value

        result_value = ieor(ieor(rotate_right(value, 17), rotate_right(value, 19)), shiftr(value, 10))
    end function small_sigma1

end module sha256

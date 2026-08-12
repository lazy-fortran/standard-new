program test_byte_text
    !! The oracle is a fixed byte sequence, independent of buffer internals.

    use, intrinsic :: iso_fortran_env, only: int8
    use byte_buffer
    use byte_span, only: byte_span_equal, byte_span_from_array, byte_span_get, &
        byte_span_slice, byte_span_t
    implicit none

    integer(int8), parameter :: input(5) = [ &
        int(-128, int8), int(-1, int8), int(0, int8), int(1, int8), int(127, int8)]
    integer(int8), parameter :: expected(5) = [ &
        int(-128, int8), int(-1, int8), int(0, int8), int(1, int8), int(127, int8)]
    integer(int8), parameter :: suffix(2) = [int(1, int8), int(127, int8)]
    integer(int8) :: value
    type(byte_buffer_t) :: buffer, copy
    type(byte_span_t) :: whole, part, expected_span
    logical :: ok
    character(len=256) :: message
    integer :: i

    call byte_buffer_init(buffer, 1, ok, message)
    call require(ok, message)
    call byte_buffer_append_byte(buffer, input(1), ok, message)
    call require(ok, message)
    call byte_buffer_append(buffer, input(2:5), ok, message)
    call require(ok, message)
    call require(byte_buffer_size(buffer) == 5, 'buffer size is not five')
    call require(byte_buffer_capacity(buffer) >= 5, 'buffer did not grow')

    call byte_buffer_span(buffer, whole, ok, message)
    call require(ok, message)
    call require(.not. byte_span_equal(whole, part), &
        'uninitialized part unexpectedly compared equal')
    do i = 1, 5
        call byte_span_get(whole, i, value, ok, message)
        call require(ok, message)
        call require(value == expected(i), 'buffer byte differs from oracle')
    end do

    call byte_span_slice(whole, 4, 2, part, ok, message)
    call require(ok, message)
    call byte_span_from_array(suffix, 1, size(suffix), expected_span, ok, message)
    call require(ok, message)
    call require(byte_span_equal(part, expected_span), 'slice differs from oracle')

    copy = buffer
    call byte_buffer_append(copy, input, ok, message)
    call require(ok, message)
    call require(byte_buffer_size(buffer) == 5, 'buffer copy changed source size')
    call byte_span_get(whole, 1, value, ok, message)
    call require(ok, message)
    call require(value == expected(1), 'buffer copy changed source storage')

    call byte_span_get(part, 3, value, ok, message)
    call require(.not. ok, 'out-of-range span access was accepted')
    call byte_span_slice(whole, 5, 2, part, ok, message)
    call require(.not. ok, 'out-of-range span slice was accepted')

    print '(a)', 'byte buffer/span test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_byte_text

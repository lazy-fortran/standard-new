program test_byte_builder
    !! The oracle is a separately declared fixed byte sequence.

    use, intrinsic :: iso_fortran_env, only: int8
    use byte_builder
    use byte_span, only: byte_span_from_array, byte_span_get, byte_span_t
    implicit none

    integer(int8), parameter :: source_bytes(3) = [ &
        int(10, int8), int(-1, int8), int(127, int8)]
    integer(int8), parameter :: expected(6) = [ &
        int(65, int8), int(0, int8), int(10, int8), int(-1, int8), &
        int(127, int8), int(10, int8)]
    integer(int8) :: value
    type(byte_builder_t) :: builder
    type(byte_span_t) :: source, actual
    logical :: ok
    character(len=256) :: message
    integer :: i

    call byte_builder_init(builder, 1, ok, message)
    call require(ok, message)
    call byte_span_from_array(source_bytes, 1, size(source_bytes), source, ok, message)
    call require(ok, message)
    call byte_builder_append_ascii(builder, 'A', ok, message)
    call require(ok, message)
    call byte_builder_append_byte(builder, int(0, int8), ok, message)
    call require(ok, message)
    call byte_builder_append_span(builder, source, ok, message)
    call require(ok, message)
    call byte_builder_append_newline(builder, ok, message)
    call require(ok, message)
    call require(byte_builder_size(builder) == size(expected), &
        'builder size differs from oracle')

    call byte_builder_span(builder, actual, ok, message)
    call require(ok, message)
    do i = 1, size(expected)
        call byte_span_get(actual, i, value, ok, message)
        call require(ok, message)
        call require(value == expected(i), 'builder byte differs from oracle')
    end do

    call byte_builder_clear(builder)
    call require(byte_builder_size(builder) == 0, 'clear did not reset size')
    print '(a)', 'byte builder test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_byte_builder

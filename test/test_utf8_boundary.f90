program test_utf8_boundary
    !! Fixed UTF-8 byte sequences establish decoding and rejection behavior.

    use, intrinsic :: iso_fortran_env, only: int8, int32
    use byte_span, only: byte_span_from_array, byte_span_t
    use utf8_boundary
    implicit none

    integer(int8), target :: valid(10) = [ &
        int(65, int8), int(-62, int8), int(-94, int8), int(-30, int8), &
        int(-126, int8), int(-84, int8), int(-16, int8), int(-97, int8), &
        int(-104, int8), int(-128, int8)]
    integer(int8), target :: invalid_overlong(2) = [int(-64, int8), int(-128, int8)]
    integer(int8), target :: invalid_surrogate(3) = [int(-19, int8), int(-96, int8), int(-128, int8)]
    integer(int8), target :: invalid_range(4) = [int(-12, int8), int(-112, int8), &
        int(-128, int8), int(-128, int8)]
    integer(int8), target :: invalid_truncated(2) = [int(-30, int8), int(-126, int8)]
    type(byte_span_t) :: source
    type(utf8_codepoint_t) :: codepoint
    logical :: ok
    character(len=256) :: message
    integer :: bad_offset

    call byte_span_from_array(valid, 1, size(valid), source, ok, message)
    call require(ok, message)
    call utf8_validate(source, ok, bad_offset, message)
    call require(ok .and. bad_offset == 0, 'valid UTF-8 was rejected')
    call utf8_decode_next(source, 1, codepoint, ok, message)
    call require(ok .and. codepoint%value == 65_int32 .and. codepoint%width == 1, &
        'ASCII codepoint differs from oracle')
    call utf8_decode_next(source, 2, codepoint, ok, message)
    call require(ok .and. codepoint%value == 162_int32 .and. codepoint%width == 2, &
        'two-byte codepoint differs from oracle')
    call utf8_decode_next(source, 4, codepoint, ok, message)
    call require(ok .and. codepoint%value == 8364_int32 .and. codepoint%width == 3, &
        'three-byte codepoint differs from oracle')
    call utf8_decode_next(source, 7, codepoint, ok, message)
    call require(ok .and. codepoint%value == 128512_int32 .and. codepoint%width == 4, &
        'four-byte codepoint differs from oracle')
    call require(utf8_is_boundary(source, 7), 'codepoint start was not a boundary')
    call require(.not. utf8_is_boundary(source, 8), 'continuation was a boundary')
    call require(utf8_is_boundary(source, 11), 'end was not a boundary')

    call check_invalid(invalid_overlong, 'overlong sequence')
    call check_invalid(invalid_surrogate, 'surrogate sequence')
    call check_invalid(invalid_range, 'out-of-range sequence')
    call check_invalid(invalid_truncated, 'truncated sequence')

    print '(a)', 'UTF-8 boundary test passed'

contains

    subroutine check_invalid(bytes, name)
        integer(int8), target, intent(in) :: bytes(:)
        character(len=*), intent(in) :: name

        call byte_span_from_array(bytes, 1, size(bytes), source, ok, message)
        call require(ok, message)
        call utf8_validate(source, ok, bad_offset, message)
        call require(.not. ok .and. bad_offset == 1, name//' was accepted')
    end subroutine check_invalid

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_utf8_boundary

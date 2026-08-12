module utf8_boundary
    !! UTF-8 boundary validation and scalar decoding for byte spans.

    use, intrinsic :: iso_fortran_env, only: int8, int32
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    implicit none
    private

    type, public :: utf8_codepoint_t
        integer(int32) :: value = 0_int32
        integer :: width = 0
    end type utf8_codepoint_t

    public :: utf8_decode_next
    public :: utf8_is_boundary
    public :: utf8_validate

contains

    subroutine utf8_decode_next(source, offset, codepoint, ok, message)
        type(byte_span_t), intent(in) :: source
        integer, intent(in) :: offset
        type(utf8_codepoint_t), intent(out) :: codepoint
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: first, second, third, fourth, remaining

        codepoint%value = 0_int32
        codepoint%width = 0
        ok = .false.
        message = ''
        if (offset < 1 .or. offset > byte_span_length(source)) then
            message = 'UTF-8 offset is outside the span'
            return
        end if
        call get_byte(source, offset, first, ok, message)
        if (.not. ok) return
        if (first <= 127) then
            codepoint%value = int(first, int32)
            codepoint%width = 1
            ok = .true.
            return
        end if

        if (first >= 194 .and. first <= 223) then
            codepoint%width = 2
        else if (first >= 224 .and. first <= 239) then
            codepoint%width = 3
        else if (first >= 240 .and. first <= 244) then
            codepoint%width = 4
        else
            codepoint%width = 0
            message = 'invalid UTF-8 leading byte'
            return
        end if
        remaining = codepoint%width - 1
        if (offset + remaining > byte_span_length(source)) then
            codepoint%width = 0
            message = 'truncated UTF-8 sequence'
            return
        end if

        call get_byte(source, offset + 1, second, ok, message)
        if (.not. ok) return
        if (.not. is_continuation(second)) then
            codepoint%width = 0
            message = 'UTF-8 continuation byte is invalid'
            return
        end if
        if (codepoint%width == 2) then
            codepoint%value = int(first - 192, int32) * 64_int32 + int(second - 128, int32)
            ok = .true.
            return
        end if

        call get_byte(source, offset + 2, third, ok, message)
        if (.not. ok) return
        if (.not. is_continuation(third)) then
            codepoint%width = 0
            message = 'UTF-8 continuation byte is invalid'
            return
        end if
        if (first == 224 .and. second < 160) then
            codepoint%width = 0
            message = 'UTF-8 sequence is overlong'
            return
        end if
        if (first == 237 .and. second > 159) then
            codepoint%width = 0
            message = 'UTF-8 sequence encodes a surrogate'
            return
        end if
        if (codepoint%width == 3) then
            codepoint%value = int(first - 224, int32) * 4096_int32 + &
                int(second - 128, int32) * 64_int32 + int(third - 128, int32)
            ok = .true.
            return
        end if

        call get_byte(source, offset + 3, fourth, ok, message)
        if (.not. ok) return
        if (.not. is_continuation(fourth)) then
            codepoint%width = 0
            message = 'UTF-8 continuation byte is invalid'
            return
        end if
        if (first == 240 .and. second < 144) then
            codepoint%width = 0
            message = 'UTF-8 sequence is overlong'
            return
        end if
        if (first == 244 .and. second > 143) then
            codepoint%width = 0
            message = 'UTF-8 codepoint exceeds the Unicode range'
            return
        end if
        codepoint%value = int(first - 240, int32) * 262144_int32 + &
            int(second - 128, int32) * 4096_int32 + &
            int(third - 128, int32) * 64_int32 + int(fourth - 128, int32)
        ok = .true.
    end subroutine utf8_decode_next

    subroutine utf8_validate(source, ok, bad_offset, message)
        type(byte_span_t), intent(in) :: source
        logical, intent(out) :: ok
        integer, intent(out) :: bad_offset
        character(len=*), intent(out) :: message
        type(utf8_codepoint_t) :: codepoint
        integer :: offset

        ok = .true.
        bad_offset = 0
        message = ''
        offset = 1
        do while (offset <= byte_span_length(source))
            call utf8_decode_next(source, offset, codepoint, ok, message)
            if (.not. ok) then
                bad_offset = offset
                return
            end if
            if (codepoint%width <= 0) then
                ok = .false.
                bad_offset = offset
                message = 'UTF-8 decoder returned no progress'
                return
            end if
            offset = offset + codepoint%width
        end do
    end subroutine utf8_validate

    logical function utf8_is_boundary(source, offset) result(is_boundary)
        type(byte_span_t), intent(in) :: source
        integer, intent(in) :: offset
        type(utf8_codepoint_t) :: codepoint
        logical :: ok
        character(len=256) :: message
        integer :: position

        is_boundary = .false.
        if (offset < 1 .or. offset > byte_span_length(source) + 1) return
        if (offset == 1 .or. offset == byte_span_length(source) + 1) then
            is_boundary = .true.
            return
        end if
        position = 1
        do while (position < offset)
            call utf8_decode_next(source, position, codepoint, ok, message)
            if (.not. ok) return
            if (codepoint%width <= 0) return
            position = position + codepoint%width
        end do
        is_boundary = position == offset
    end function utf8_is_boundary

    subroutine get_byte(source, offset, value, ok, message)
        type(byte_span_t), intent(in) :: source
        integer, intent(in) :: offset
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8) :: raw

        call byte_span_get(source, offset, raw, ok, message)
        if (.not. ok) then
            value = 0
            return
        end if
        value = int(raw)
        if (value < 0) value = value + 256
    end subroutine get_byte

    logical function is_continuation(value) result(valid)
        integer, intent(in) :: value

        valid = value >= 128 .and. value <= 191
    end function is_continuation

end module utf8_boundary

program test_text_properties
    !! Deterministic properties over byte storage and text boundaries.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use byte_buffer
    use byte_builder
    use byte_span, only: byte_span_from_array, byte_span_get, byte_span_length, &
        byte_span_slice, byte_span_t
    use interner
    use utf8_boundary
    use writer
    implicit none

    integer(int8), target :: expected(96)
    integer(int8), parameter :: oracle_first = int(48, int8)
    integer(int8) :: value
    type(byte_buffer_t) :: buffer
    type(byte_builder_t) :: builder
    type(byte_span_t) :: whole, part
    type(writer_t) :: memory, counting
    type(interner_t) :: names
    type(utf8_codepoint_t) :: codepoint
    logical :: ok, is_new
    character(len=256) :: message
    integer :: i, first, length, position, chunk
    integer :: first_id, same_id
    integer(int8), target :: name_upper(4) = [int(84, int8), int(69, int8), &
        int(83, int8), int(84, int8)]
    integer(int8), target :: name_lower(4) = [int(116, int8), int(101, int8), &
        int(115, int8), int(116, int8)]
    integer(int8), target :: utf8(4) = [int(-16, int8), int(-97, int8), &
        int(-104, int8), int(-128, int8)]

    do i = 1, size(expected)
        expected(i) = reference_byte(i)
    end do

    call byte_buffer_init(buffer, 1, ok, message)
    call require(ok, message)
    position = 1
    chunk = 1
    do while (position <= size(expected))
        call byte_buffer_append(buffer, expected(position:min(position + chunk - 1, size(expected))), &
            ok, message)
        call require(ok, message)
        position = position + chunk
        chunk = 1 + modulo(chunk, 11)
    end do
    call byte_buffer_span(buffer, whole, ok, message)
    call require(ok, message)
    call require(byte_span_length(whole) == size(expected), 'buffer property length differs')
    call check_bytes(whole, expected, 'buffer property')
    call byte_span_get(whole, 1, value, ok, message)
    call require(ok .and. value == oracle_first, 'independent property oracle differs')

    do first = 1, size(expected)
        do length = 0, min(7, size(expected) - first + 1)
            call byte_span_slice(whole, first, length, part, ok, message)
            call require(ok, message)
            call require(byte_span_length(part) == length, 'span property length differs')
            do i = 1, length
                call byte_span_get(part, i, value, ok, message)
                call require(ok, message)
                call require(value == expected(first + i - 1), 'span property byte differs')
            end do
        end do
    end do

    call byte_builder_init(builder, 1, ok, message)
    call require(ok, message)
    call byte_builder_append(builder, expected, ok, message)
    call require(ok, message)
    call byte_builder_span(builder, part, ok, message)
    call require(ok, message)
    call check_bytes(part, expected, 'builder property')

    call writer_init_memory(memory, 1, ok, message)
    call require(ok, message)
    call writer_write_bytes(memory, expected(1:31), ok, message)
    call require(ok, message)
    call writer_write_bytes(memory, expected(32:64), ok, message)
    call require(ok, message)
    call writer_write_bytes(memory, expected(65:96), ok, message)
    call require(ok, message)
    call writer_memory_span(memory, part, ok, message)
    call require(ok, message)
    call check_bytes(part, expected, 'memory writer property')

    call writer_init_counting(counting, ok, message)
    call require(ok, message)
    call writer_write_bytes(counting, expected(1:13), ok, message)
    call require(ok, message)
    call writer_write_bytes(counting, expected(14:96), ok, message)
    call require(ok, message)
    call require(writer_size(counting) == int(size(expected), int64), &
        'counting writer property differs')

    call interner_init(names, 4, ok, message)
    call require(ok, message)
    call intern_name(name_upper, first_id, is_new)
    call require(is_new, 'property name was not new')
    call intern_name(name_lower, same_id, is_new)
    call require(.not. is_new .and. same_id == first_id, 'property case identity differs')

    call byte_span_from_array(utf8, 1, size(utf8), whole, ok, message)
    call require(ok, message)
    call utf8_validate(whole, ok, i, message)
    call require(ok, message)
    call utf8_decode_next(whole, 1, codepoint, ok, message)
    call require(ok .and. codepoint%value == 128512, 'property UTF-8 scalar differs')

    print '(a)', 'text property test passed'

contains

    integer(int8) function reference_byte(index) result(value)
        integer, intent(in) :: index
        integer :: unsigned

        unsigned = modulo(37 * index + 11, 256)
        if (unsigned > 127) unsigned = unsigned - 256
        value = int(unsigned, int8)
    end function reference_byte

    subroutine check_bytes(span, expected_bytes, context)
        type(byte_span_t), intent(in) :: span
        integer(int8), intent(in) :: expected_bytes(:)
        character(len=*), intent(in) :: context
        integer :: index

        call require(byte_span_length(span) == size(expected_bytes), context//' length differs')
        do index = 1, size(expected_bytes)
            call byte_span_get(span, index, value, ok, message)
            call require(ok, message)
            call require(value == expected_bytes(index), context//' byte differs')
        end do
    end subroutine check_bytes

    subroutine intern_name(bytes, id, new_name)
        integer(int8), target, intent(in) :: bytes(:)
        integer, intent(out) :: id
        logical, intent(out) :: new_name

        call byte_span_from_array(bytes, 1, size(bytes), whole, ok, message)
        call require(ok, message)
        call interner_intern(names, whole, id, new_name, ok, message)
        call require(ok, message)
    end subroutine intern_name

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_text_properties

program test_fortsx_arena
    !! The recursive seed and flat arena reader must emit identical canonical bytes.

    use, intrinsic :: iso_fortran_env, only: int8
    use fortsx, only: sx_node_t, sx_parse, sx_write_writer
    use fortsx_arena
    use writer, only: writer_memory_span, writer_init_memory, writer_t
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    implicit none

    character(len=*), parameter :: input = '(list   "a b" "a\"b"    )'
    character(len=*), parameter :: expected = '(list "a b" "a\"b")'//achar(10)
    character(len=256) :: message
    integer(int8) :: value, recursive_value
    integer :: i, root
    logical :: ok
    type(sx_arena_t) :: arena
    type(sx_node_t) :: recursive_node
    type(writer_t) :: arena_output, recursive_output
    type(byte_span_t) :: actual, recursive_bytes

    call sx_arena_parse(input, arena, root, ok, message)
    call require(ok, message)
    call writer_init_memory(arena_output, 1, ok, message)
    call require(ok, message)
    call sx_arena_write(arena_output, arena, root, ok, message)
    call require(ok, message)
    call writer_memory_span(arena_output, actual, ok, message)
    call require(ok, message)
    call check_bytes(actual, expected, 'arena output')
    call require(sx_arena_node_count(arena) == 4, 'arena node count differs')
    call require(sx_arena_node_kind(arena, root) == sx_arena_list, 'arena root kind differs')

    call sx_parse(input, recursive_node, ok, message)
    call require(ok, message)
    call writer_init_memory(recursive_output, 1, ok, message)
    call require(ok, message)
    call sx_write_writer(recursive_output, recursive_node, ok, message)
    call require(ok, message)
    call writer_memory_span(recursive_output, recursive_bytes, ok, message)
    call require(ok, message)
    call require(byte_span_length(actual) == byte_span_length(recursive_bytes), &
        'arena and recursive byte counts differ')
    do i = 1, byte_span_length(actual)
        call byte_span_get(actual, i, value, ok, message)
        call require(ok, message)
        call byte_span_get(recursive_bytes, i, recursive_value, ok, message)
        call require(ok, message)
        call require(value == recursive_value, 'arena and recursive bytes differ')
    end do

    print '(a)', 'fortsx arena test passed'

contains

    subroutine check_bytes(span, text, context)
        type(byte_span_t), intent(in) :: span
        character(len=*), intent(in) :: text, context
        integer(int8) :: byte
        integer :: index

        call require(byte_span_length(span) == len(text), context//' length differs')
        do index = 1, len(text)
            call byte_span_get(span, index, byte, ok, message)
            call require(ok, message)
            call require(byte == int(iachar(text(index:index)), int8), context//' byte differs')
        end do
    end subroutine check_bytes

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_fortsx_arena

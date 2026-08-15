program test_fortsx_arena_corpus
    !! The flat arena reader agrees with the recursive seed over a generated corpus.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    use fortsx, only: sx_clear, sx_node_t, sx_parse, sx_write_writer
    use fortsx_arena
    use writer, only: writer_memory_span, writer_init_memory, writer_t
    implicit none

    integer, parameter :: case_count = 64
    integer, parameter :: max_depth = 4
    character(len=*), parameter :: alphabet = 'abC 012()"'
    character(len=256), parameter :: malformed(10) = [character(len=256) :: &
        '', ')', '(a', '"abc', '"a\q"', '(a) (b)', '(a (b)', '(', '("a\q")', '"a\']
    character(len=256), parameter :: malformed_message(10) = [character(len=256) :: &
        'unexpected end of SX form', 'unexpected closing parenthesis', 'unclosed SX list', &
        'unclosed SX quoted atom', 'unsupported SX escape', 'trailing bytes after SX form', &
        'unclosed SX list', 'unclosed SX list', 'unsupported SX escape', 'unterminated SX escape']
    type(sx_node_t) :: original, recursive_node
    type(sx_arena_t) :: arena
    type(writer_t) :: source_output, recursive_output, arena_output
    type(byte_span_t) :: source_span, recursive_span, arena_span
    character(len=4096) :: line, message
    integer(int64) :: state
    integer(int8) :: byte
    integer :: i, root, source_length
    logical :: ok

    state = 1729_int64
    do i = 1, case_count
        call make_tree(original, 0, state)
        call writer_init_memory(source_output, 1, ok, message)
        call require(ok, message)
        call sx_write_writer(source_output, original, ok, message)
        call require(ok, message)
        call writer_memory_span(source_output, source_span, ok, message)
        call require(ok, message)
        call require(byte_span_length(source_span) > 1, 'generated SX source is empty')
        call byte_span_get(source_span, byte_span_length(source_span), byte, ok, message)
        call require(ok, message)
        call require(byte == int(10, int8), 'generated SX source lacks newline')
        source_length = byte_span_length(source_span) - 1
        call span_to_line(source_span, source_length, line)

        call sx_parse(line(1:source_length), recursive_node, ok, message)
        call require(ok, message)
        call sx_arena_parse(line(1:source_length), arena, root, ok, message)
        call require(ok, message)
        call writer_init_memory(recursive_output, 1, ok, message)
        call require(ok, message)
        call sx_write_writer(recursive_output, recursive_node, ok, message)
        call require(ok, message)
        call writer_memory_span(recursive_output, recursive_span, ok, message)
        call require(ok, message)
        call writer_init_memory(arena_output, 1, ok, message)
        call require(ok, message)
        call sx_arena_write(arena_output, arena, root, ok, message)
        call require(ok, message)
        call writer_memory_span(arena_output, arena_span, ok, message)
        call require(ok, message)
        call compare_spans(recursive_span, arena_span, 'generated arena output')
        call sx_clear(recursive_node)
        call sx_arena_reset(arena)
    end do

    do i = 1, size(malformed)
        call sx_parse(trim(malformed(i)), recursive_node, ok, message)
        call require(.not. ok, 'recursive seed accepted malformed corpus member')
        call require(trim(message) == trim(malformed_message(i)), &
            'recursive seed malformed message differs')
        call sx_arena_parse(trim(malformed(i)), arena, root, ok, message)
        call require(.not. ok, 'arena seed accepted malformed corpus member')
        call require(trim(message) == trim(malformed_message(i)), &
            'arena seed malformed message differs')
        call sx_clear(recursive_node)
        call sx_arena_reset(arena)
    end do

    print '(a)', 'fortsx arena corpus test passed'

contains

    recursive subroutine make_tree(node, depth, random_state)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: depth
        integer(int64), intent(inout) :: random_state
        integer :: child, length, random_index

        call sx_clear(node)
        if (depth >= max_depth .or. modulo(next_value(random_state), 4) /= 0) then
            node%kind = 1
            length = 1 + modulo(next_value(random_state), 9)
            node%atom = alphabet(:length)
            do child = 1, length
                random_index = 1 + modulo(next_value(random_state), len(alphabet))
                node%atom(child:child) = alphabet(random_index:random_index)
            end do
            return
        end if
        node%kind = 2
        node%child_count = 1 + modulo(next_value(random_state), 4)
        allocate (node%children(128))
        do child = 1, node%child_count
            call make_tree(node%children(child), depth + 1, random_state)
        end do
    end subroutine make_tree

    integer function next_value(random_state) result(value)
        integer(int64), intent(inout) :: random_state

        random_state = modulo(1103515245_int64 * random_state + 12345_int64, 2147483647_int64)
        value = int(random_state)
    end function next_value

    subroutine span_to_line(span, length, output)
        type(byte_span_t), intent(in) :: span
        integer, intent(in) :: length
        character(len=*), intent(out) :: output
        integer(int8) :: value
        integer :: i
        logical :: span_ok
        character(len=256) :: span_message

        output = ''
        do i = 1, length
            call byte_span_get(span, i, value, span_ok, span_message)
            call require(span_ok, span_message)
            output(i:i) = achar(byte_as_integer(value))
        end do
    end subroutine span_to_line

    subroutine compare_spans(left, right, context)
        type(byte_span_t), intent(in) :: left, right
        character(len=*), intent(in) :: context
        integer(int8) :: left_byte, right_byte
        integer :: i
        logical :: span_ok
        character(len=256) :: span_message

        call require(byte_span_length(left) == byte_span_length(right), context//' length differs')
        do i = 1, byte_span_length(left)
            call byte_span_get(left, i, left_byte, span_ok, span_message)
            call require(span_ok, span_message)
            call byte_span_get(right, i, right_byte, span_ok, span_message)
            call require(span_ok, span_message)
            call require(left_byte == right_byte, context//' bytes differ')
        end do
    end subroutine compare_spans

    integer function byte_as_integer(value) result(unsigned)
        integer(int8), intent(in) :: value

        unsigned = int(value)
        if (unsigned < 0) unsigned = unsigned + 256
    end function byte_as_integer

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_fortsx_arena_corpus

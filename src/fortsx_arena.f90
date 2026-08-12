module fortsx_arena
    !! Bootstrap-Core-compatible flat SX reader over byte offsets.

    use, intrinsic :: iso_fortran_env, only: int8
    use writer, only: writer_t, writer_write_ascii, writer_write_byte, writer_write_newline
    implicit none
    private

    integer, parameter, public :: sx_arena_atom = 1
    integer, parameter, public :: sx_arena_list = 2
    integer, parameter, public :: sx_arena_max_nodes = 4096
    integer, parameter, public :: sx_arena_max_children = 128

    type, public :: sx_arena_node_t
        integer :: kind = 0
        integer :: first_child = 0
        integer :: child_count = 0
        integer :: text_start = 0
        integer :: text_length = 0
    end type sx_arena_node_t

    type, public :: sx_arena_t
        private
        type(sx_arena_node_t) :: nodes(sx_arena_max_nodes)
        integer :: node_count = 0
        integer :: children(sx_arena_max_nodes * sx_arena_max_children) = 0
        integer :: child_count = 0
        integer(int8), allocatable :: text(:)
        integer :: text_length = 0
    end type sx_arena_t

    public :: sx_arena_parse
    public :: sx_arena_reset
    public :: sx_arena_write
    public :: sx_arena_node_count
    public :: sx_arena_node_kind

contains

    subroutine sx_arena_reset(arena)
        type(sx_arena_t), intent(out) :: arena

        arena%node_count = 0
        arena%child_count = 0
        arena%text_length = 0
        if (allocated(arena%text)) deallocate (arena%text)
    end subroutine sx_arena_reset

    subroutine sx_arena_parse(input, arena, root, ok, message)
        character(len=*), intent(in) :: input
        type(sx_arena_t), intent(out) :: arena
        integer, intent(out) :: root
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: position

        call sx_arena_reset(arena)
        allocate (arena%text(max(1, len(input))))
        if (len(input) > 0) then
            do position = 1, len(input)
                arena%text(position) = int(iachar(input(position:position)), int8)
            end do
        end if
        arena%text_length = len(input)
        position = 1
        call parse_form(arena, position, root, ok, message)
        if (.not. ok) return
        call skip_space(arena, position)
        if (position <= arena%text_length) then
            ok = .false.
            message = 'trailing bytes after SX form'
        end if
    end subroutine sx_arena_parse

    integer function sx_arena_node_count(arena) result(count)
        type(sx_arena_t), intent(in) :: arena

        count = arena%node_count
    end function sx_arena_node_count

    integer function sx_arena_node_kind(arena, node) result(kind)
        type(sx_arena_t), intent(in) :: arena
        integer, intent(in) :: node

        kind = 0
        if (node >= 1 .and. node <= arena%node_count) kind = arena%nodes(node)%kind
    end function sx_arena_node_kind

    recursive subroutine parse_form(arena, position, node, ok, message)
        type(sx_arena_t), intent(inout) :: arena
        integer, intent(inout) :: position
        integer, intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: start

        ok = .false.
        message = ''
        call skip_space(arena, position)
        if (position > arena%text_length) then
            message = 'unexpected end of SX form'
            return
        end if
        if (text_at(arena, position) == '(') then
            call parse_list(arena, position, node, ok, message)
            return
        end if
        if (text_at(arena, position) == '"') then
            call parse_quoted_atom(arena, position, node, ok, message)
            return
        end if
        if (text_at(arena, position) == ')') then
            message = 'unexpected closing parenthesis'
            return
        end if
        start = position
        do while (position <= arena%text_length)
            if (text_at(arena, position) == ' ' .or. text_at(arena, position) == achar(9) .or. &
                text_at(arena, position) == ')') exit
            position = position + 1
        end do
        call add_node(arena, sx_arena_atom, 0, 0, start, position - start, node, ok, message)
    end subroutine parse_form

    subroutine parse_quoted_atom(arena, position, node, ok, message)
        type(sx_arena_t), intent(inout) :: arena
        integer, intent(inout) :: position
        integer, intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: start, write_position

        position = position + 1
        start = position
        write_position = position
        do while (position <= arena%text_length)
            if (text_at(arena, position) == '"') then
                call add_node(arena, sx_arena_atom, 0, 0, start, write_position - start, &
                    node, ok, message)
                position = position + 1
                return
            end if
            if (text_at(arena, position) == achar(92)) then
                if (position == arena%text_length) then
                    ok = .false.
                    message = 'unterminated SX escape'
                    return
                end if
                if (text_at(arena, position + 1) /= '"' .and. &
                    text_at(arena, position + 1) /= achar(92)) then
                    ok = .false.
                    message = 'unsupported SX escape'
                    return
                end if
                position = position + 1
            end if
            arena%text(write_position) = int(iachar(text_at(arena, position)), int8)
            write_position = write_position + 1
            position = position + 1
        end do
        ok = .false.
        message = 'unclosed SX quoted atom'
    end subroutine parse_quoted_atom

    recursive subroutine parse_list(arena, position, node, ok, message)
        type(sx_arena_t), intent(inout) :: arena
        integer, intent(inout) :: position
        integer, intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: first_child, child, count

        position = position + 1
        first_child = arena%child_count + 1
        count = 0
        do
            call skip_space(arena, position)
            if (position > arena%text_length) then
                ok = .false.
                message = 'unclosed SX list'
                return
            end if
            if (text_at(arena, position) == ')') then
                position = position + 1
                call add_node(arena, sx_arena_list, first_child, count, 0, 0, &
                    node, ok, message)
                return
            end if
            if (count >= sx_arena_max_children) then
                ok = .false.
                message = 'SX list exceeds seed limit'
                return
            end if
            call parse_form(arena, position, child, ok, message)
            if (.not. ok) return
            arena%child_count = arena%child_count + 1
            arena%children(arena%child_count) = child
            count = count + 1
        end do
    end subroutine parse_list

    subroutine add_node(arena, kind, first_child, child_count, text_start, text_length, &
            node, ok, message)
        type(sx_arena_t), intent(inout) :: arena
        integer, intent(in) :: kind, first_child, child_count, text_start, text_length
        integer, intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (arena%node_count >= sx_arena_max_nodes) then
            ok = .false.
            message = 'SX arena node limit exceeded'
            return
        end if
        arena%node_count = arena%node_count + 1
        node = arena%node_count
        arena%nodes(node)%kind = kind
        arena%nodes(node)%first_child = first_child
        arena%nodes(node)%child_count = child_count
        arena%nodes(node)%text_start = text_start
        arena%nodes(node)%text_length = text_length
        ok = .true.
        message = ''
    end subroutine add_node

    subroutine skip_space(arena, position)
        type(sx_arena_t), intent(in) :: arena
        integer, intent(inout) :: position

        do while (position <= arena%text_length)
            if (text_at(arena, position) /= ' ' .and. text_at(arena, position) /= achar(9)) exit
            position = position + 1
        end do
    end subroutine skip_space

    character(len=1) function text_at(arena, position) result(value)
        type(sx_arena_t), intent(in) :: arena
        integer, intent(in) :: position

        value = achar(byte_as_integer(arena%text(position)))
    end function text_at

    subroutine sx_arena_write(output, arena, root, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_arena_t), intent(in) :: arena
        integer, intent(in) :: root
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (root < 1 .or. root > arena%node_count) then
            ok = .false.
            message = 'SX arena root is invalid'
            return
        end if
        call write_node(output, arena, root, ok, message)
        if (.not. ok) return
        call writer_write_newline(output, ok, message)
    end subroutine sx_arena_write

    recursive subroutine write_node(output, arena, node, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_arena_t), intent(in) :: arena
        integer, intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: child, child_node

        if (arena%nodes(node)%kind == sx_arena_atom) then
            call write_atom(output, arena, node, ok, message)
            return
        end if
        if (arena%nodes(node)%kind /= sx_arena_list) then
            ok = .false.
            message = 'invalid SX arena node kind'
            return
        end if
        call writer_write_ascii(output, '(', ok, message)
        if (.not. ok) return
        do child = 1, arena%nodes(node)%child_count
            if (child > 1) then
                call writer_write_ascii(output, ' ', ok, message)
                if (.not. ok) return
            end if
            child_node = arena%children(arena%nodes(node)%first_child + child - 1)
            call write_node(output, arena, child_node, ok, message)
            if (.not. ok) return
        end do
        call writer_write_ascii(output, ')', ok, message)
    end subroutine write_node

    subroutine write_atom(output, arena, node, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_arena_t), intent(in) :: arena
        integer, intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=1) :: value
        integer :: i, start, length
        logical :: quoted

        start = arena%nodes(node)%text_start
        length = arena%nodes(node)%text_length
        quoted = length == 0
        do i = 0, length - 1
            if (text_at(arena, start + i) == ' ' .or. text_at(arena, start + i) == achar(9) .or. &
                text_at(arena, start + i) == '(' .or. text_at(arena, start + i) == ')' .or. &
                text_at(arena, start + i) == '"' .or. text_at(arena, start + i) == achar(92)) quoted = .true.
        end do
        if (quoted) then
            call writer_write_ascii(output, '"', ok, message)
            if (.not. ok) return
        end if
        do i = 0, length - 1
            value = text_at(arena, start + i)
            if (quoted .and. (value == '"' .or. value == achar(92))) then
                call writer_write_ascii(output, achar(92), ok, message)
                if (.not. ok) return
            end if
            call writer_write_byte(output, arena%text(start + i), ok, message)
            if (.not. ok) return
        end do
        if (quoted) call writer_write_ascii(output, '"', ok, message)
    end subroutine write_atom

    integer function byte_as_integer(value) result(unsigned)
        integer(int8), intent(in) :: value

        unsigned = int(value)
        if (unsigned < 0) unsigned = unsigned + 256
    end function byte_as_integer

end module fortsx_arena

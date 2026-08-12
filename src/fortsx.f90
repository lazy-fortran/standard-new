module fortsx
    !! Small canonical S-expression reader and writer for StandardIR and ImplIR.
    !!
    !! This first seed supports atoms and lists. Storage is an explicit tree;
    !! the serializer has one spelling for every tree and never concatenates a
    !! growing character buffer.

    use writer, only: writer_t, writer_write_ascii, writer_write_newline
    implicit none
    private

    integer, parameter, public :: sx_atom = 1
    integer, parameter, public :: sx_list = 2
    integer, parameter :: sx_max_children = 128

    type, public :: sx_node_t
        integer :: kind = 0
        character(len=256) :: atom = ''
        integer :: child_count = 0
        type(sx_node_t), allocatable :: children(:)
    end type sx_node_t

    public :: sx_clear, sx_parse, sx_validate, sx_write, sx_write_writer

contains

    subroutine sx_clear(node)
        type(sx_node_t), intent(inout) :: node
        if (allocated(node%children)) deallocate (node%children)
        node%kind = 0
        node%atom = ''
        node%child_count = 0
    end subroutine sx_clear

    subroutine sx_parse(text, node, ok, message)
        character(len=*), intent(in) :: text
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: position

        call sx_clear(node)
        position = 1
        call parse_form(text, position, node, ok, message)
        if (.not. ok) return
        call skip_space(text, position)
        if (position <= len_trim(text)) then
            ok = .false.
            message = 'trailing bytes after SX form'
        end if
    end subroutine sx_parse

    recursive subroutine parse_form(text, position, node, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: start

        call sx_clear(node)
        call skip_space(text, position)
        if (position > len_trim(text)) then
            ok = .false.
            message = 'unexpected end of SX form'
            return
        end if
        if (text(position:position) == '(') then
            call parse_list(text, position, node, ok, message)
            return
        end if
        if (text(position:position) == '"') then
            call parse_quoted_atom(text, position, node, ok, message)
            return
        end if
        if (text(position:position) == ')') then
            ok = .false.
            message = 'unexpected closing parenthesis'
            return
        end if

        start = position
        do while (position <= len_trim(text))
            if (text(position:position) == ' ') exit
            if (text(position:position) == achar(9)) exit
            if (text(position:position) == ')') exit
            position = position + 1
        end do
        if (position == start) then
            ok = .false.
            message = 'empty SX atom'
            return
        end if
        if (position - start > len(node%atom)) then
            ok = .false.
            message = 'SX atom exceeds seed limit'
            return
        end if
        node%kind = sx_atom
        node%atom(1:position - start) = text(start:position - 1)
        ok = .true.
        message = ''
    end subroutine parse_form

    subroutine parse_quoted_atom(text, position, node, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: count, n
        character(len=1) :: ch

        call sx_clear(node)
        n = len_trim(text)
        position = position + 1
        count = 0
        do while (position <= n)
            if (text(position:position) == '"') then
                position = position + 1
                node%kind = sx_atom
                node%child_count = 0
                ok = .true.
                message = ''
                return
            end if
            if (text(position:position) == achar(92)) then
                position = position + 1
                if (position > n) then
                    ok = .false.
                    message = 'unterminated SX escape'
                    return
                end if
                if (text(position:position) == '"' .or. &
                    text(position:position) == achar(92)) then
                    ch = text(position:position)
                else
                    ok = .false.
                    message = 'unsupported SX escape'
                    return
                end if
            else
                ch = text(position:position)
            end if
            if (count >= len(node%atom)) then
                ok = .false.
                message = 'SX atom exceeds seed limit'
                return
            end if
            count = count + 1
            node%atom(count:count) = ch
            position = position + 1
        end do
        ok = .false.
        message = 'unclosed SX quoted atom'
    end subroutine parse_quoted_atom

    recursive subroutine parse_list(text, position, node, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: child_count
        type(sx_node_t) :: child

        node%kind = sx_list
        allocate (node%children(sx_max_children))
        child_count = 0
        position = position + 1
        do
            call skip_space(text, position)
            if (position > len_trim(text)) then
                ok = .false.
                message = 'unclosed SX list'
                return
            end if
            if (text(position:position) == ')') then
                position = position + 1
                node%child_count = child_count
                ok = .true.
                message = ''
                return
            end if
            if (child_count >= sx_max_children) then
                ok = .false.
                message = 'SX list exceeds seed limit'
                return
            end if
            call parse_form(text, position, child, ok, message)
            if (.not. ok) return
            child_count = child_count + 1
            call move_alloc(child%children, node%children(child_count)%children)
            node%children(child_count)%kind = child%kind
            node%children(child_count)%atom = child%atom
            node%children(child_count)%child_count = child%child_count
        end do
    end subroutine parse_list

    subroutine skip_space(text, position)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        do while (position <= len_trim(text))
            if (text(position:position) /= ' ' .and. &
                text(position:position) /= achar(9)) exit
            position = position + 1
        end do
    end subroutine skip_space

    subroutine sx_write(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .true.
        message = ''
        call write_node(unit, node, ok, message)
        if (.not. ok) return
        call finish_line(unit, ok, message)
    end subroutine sx_write

    subroutine sx_validate(node, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call validate_node(node, ok, message)
    end subroutine sx_validate

    recursive subroutine validate_node(node, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = .false.
        message = ''
        select case (node%kind)
        case (sx_atom)
            if (node%child_count /= 0) then
                message = 'SX atom has children'
                return
            end if
            if (allocated(node%children)) then
                message = 'SX atom has child storage'
                return
            end if
        case (sx_list)
            if (node%child_count < 0 .or. node%child_count > sx_max_children) then
                message = 'SX list child count is invalid'
                return
            end if
            if (node%child_count > 0) then
                if (.not. allocated(node%children)) then
                    message = 'SX list has no child storage'
                    return
                end if
                if (size(node%children) < node%child_count) then
                    message = 'SX list child storage is too small'
                    return
                end if
            end if
            do i = 1, node%child_count
                call validate_node(node%children(i), ok, message)
                if (.not. ok) return
            end do
        case default
            message = 'invalid SX node kind'
            return
        end select
        ok = .true.
    end subroutine validate_node

    subroutine sx_write_writer(output, node, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call sx_validate(node, ok, message)
        if (.not. ok) return
        call write_node_writer(output, node, ok, message)
        if (.not. ok) return
        call writer_write_newline(output, ok, message)
    end subroutine sx_write_writer

    recursive subroutine write_node_writer(output, node, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = .true.
        message = ''
        select case (node%kind)
        case (sx_atom)
            call write_atom_writer(output, node%atom, ok, message)
        case (sx_list)
            call writer_write_ascii(output, '(', ok, message)
            if (.not. ok) return
            do i = 1, node%child_count
                if (i > 1) then
                    call writer_write_ascii(output, ' ', ok, message)
                    if (.not. ok) return
                end if
                call write_node_writer(output, node%children(i), ok, message)
                if (.not. ok) return
            end do
            call writer_write_ascii(output, ')', ok, message)
        case default
            ok = .false.
            message = 'invalid SX node kind'
        end select
    end subroutine write_node_writer

    subroutine write_atom_writer(output, atom, ok, message)
        type(writer_t), intent(inout) :: output
        character(len=*), intent(in) :: atom
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=1) :: one
        integer :: i, n
        logical :: quoted

        n = len_trim(atom)
        quoted = n == 0
        do i = 1, n
            if (atom(i:i) == ' ' .or. atom(i:i) == achar(9) .or. &
                atom(i:i) == '(' .or. atom(i:i) == ')' .or. &
                atom(i:i) == '"' .or. atom(i:i) == achar(92)) quoted = .true.
        end do
        if (.not. quoted) then
            call writer_write_ascii(output, trim(atom), ok, message)
            return
        end if
        call writer_write_ascii(output, '"', ok, message)
        if (.not. ok) return
        do i = 1, n
            if (atom(i:i) == '"' .or. atom(i:i) == achar(92)) then
                call writer_write_ascii(output, achar(92), ok, message)
                if (.not. ok) return
            end if
            one = atom(i:i)
            call writer_write_ascii(output, one, ok, message)
            if (.not. ok) return
        end do
        call writer_write_ascii(output, '"', ok, message)
    end subroutine write_atom_writer

    recursive subroutine write_node(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: i

        select case (node%kind)
        case (sx_atom)
            call write_atom(unit, node%atom, ok, message)
        case (sx_list)
            call piece(unit, '(', ok, message)
            do i = 1, node%child_count
                if (i > 1) call piece(unit, ' ', ok, message)
                call write_node(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
            call piece(unit, ')', ok, message)
        case default
            ok = .false.
            message = 'invalid SX node kind'
        end select
    end subroutine write_node

    subroutine write_atom(unit, atom, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: atom
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: i, n
        logical :: quoted

        n = len_trim(atom)
        quoted = n == 0
        do i = 1, n
            if (atom(i:i) == ' ' .or. atom(i:i) == achar(9) .or. &
                atom(i:i) == '(' .or. atom(i:i) == ')' .or. &
                atom(i:i) == '"' .or. atom(i:i) == achar(92)) then
                quoted = .true.
            end if
        end do
        if (.not. quoted) then
            call piece(unit, trim(atom), ok, message)
            return
        end if
        call piece(unit, '"', ok, message)
        do i = 1, n
            if (atom(i:i) == '"' .or. atom(i:i) == achar(92)) then
                call piece(unit, achar(92), ok, message)
            end if
            call piece(unit, atom(i:i), ok, message)
            if (.not. ok) return
        end do
        call piece(unit, '"', ok, message)
    end subroutine write_atom

    subroutine piece(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: ios

        write (unit, '(a)', advance='no', iostat=ios) text
        if (ios /= 0) then
            ok = .false.
            message = 'cannot write SX'
        end if
    end subroutine piece

    subroutine finish_line(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: ios

        write (unit, '(a)', iostat=ios) ''
        if (ios /= 0) then
            ok = .false.
            message = 'cannot finish SX record'
        end if
    end subroutine finish_line

end module fortsx

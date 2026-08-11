module standardir_normalize
    !! Normalize a StandardIR syntax object back to grammar notation.
    !!
    !! This is deliberately a structural projection. It does not consult the
    !! PDF or the production JSONL, so the production-to-StandardIR-to-text
    !! check exercises a separate consumer of the SX representation.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    public :: standardir_normalize_syntax

contains

    subroutine standardir_normalize_syntax(node, rule, lhs, notation, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: rule, lhs, notation
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32768) :: buffer
        integer :: position

        rule = ''
        lhs = ''
        notation = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'syntax object is not an SX list'
            return
        end if
        if (node%child_count /= 5) then
            message = 'syntax object has the wrong field count'
            return
        end if
        if (.not. atom_equals(node%children(1), 'syntax')) then
            message = 'syntax object has no syntax label'
            return
        end if
        call read_atom(node%children(2), rule, ok, message)
        if (.not. ok) return
        call read_lhs(node%children(3), lhs, ok, message)
        if (.not. ok) return

        buffer = ''
        position = 0
        call emit_expression(node%children(4), buffer, position, ok, message)
        if (.not. ok) return
        if (position == 0) then
            message = 'syntax RHS normalized to empty text'
            return
        end if
        notation = buffer(1:position)
        ok = .true.
    end subroutine standardir_normalize_syntax

    subroutine read_lhs(node, lhs, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        lhs = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'lhs field is not an SX list'
            return
        end if
        if (node%child_count /= 2) then
            message = 'lhs field has the wrong field count'
            return
        end if
        if (.not. atom_equals(node%children(1), 'lhs')) then
            message = 'lhs field has no lhs label'
            return
        end if
        call read_atom(node%children(2), lhs, ok, message)
    end subroutine read_lhs

    recursive subroutine emit_expression(node, buffer, position, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: position
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, minimum, unbounded
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'RHS node is not an SX list'
            return
        end if
        if (node%child_count < 1) then
            message = 'RHS node has no label'
            return
        end if
        call read_atom(node%children(1), label, ok, message)
        if (.not. ok) return

        select case (trim(label))
        case ('rhs')
            if (node%child_count /= 2) then
                message = 'rhs field has the wrong field count'
                return
            end if
            call emit_expression(node%children(2), buffer, position, ok, message)
        case ('ref', 'token')
            if (node%child_count /= 2) then
                message = 'leaf node has the wrong field count'
                return
            end if
            call append_atom(buffer, position, node%children(2), ok, message)
        case ('seq')
            if (node%child_count < 2) then
                message = 'sequence node is empty'
                return
            end if
            do i = 2, node%child_count
                if (i > 2) call append_literal(buffer, position, ' ', ok, message)
                if (.not. ok) return
                call emit_expression(node%children(i), buffer, position, ok, message)
                if (.not. ok) return
            end do
        case ('alt')
            if (node%child_count < 2) then
                message = 'alternative node is empty'
                return
            end if
            do i = 2, node%child_count
                if (i > 2) call append_literal(buffer, position, ' or ', ok, message)
                if (.not. ok) return
                call emit_expression(node%children(i), buffer, position, ok, message)
                if (.not. ok) return
            end do
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional node has the wrong field count'
                return
            end if
            call append_literal(buffer, position, '[ ', ok, message)
            if (.not. ok) return
            call emit_expression(node%children(2), buffer, position, ok, message)
            if (.not. ok) return
            call append_literal(buffer, position, ' ]', ok, message)
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat node has the wrong field count'
                return
            end if
            call read_atom(node%children(3), minimum, ok, message)
            if (.not. ok) return
            call read_atom(node%children(4), unbounded, ok, message)
            if (.not. ok) return
            if (trim(unbounded) /= 'unbounded') then
                message = 'repeat node is not unbounded'
                return
            end if
            if (trim(minimum) == '0') then
                call append_literal(buffer, position, '[ ', ok, message)
                if (.not. ok) return
                call emit_expression(node%children(2), buffer, position, ok, message)
                if (.not. ok) return
                call append_literal(buffer, position, ' ] ...', ok, message)
            else if (trim(minimum) == '1') then
                call emit_expression(node%children(2), buffer, position, ok, message)
                if (.not. ok) return
                call append_literal(buffer, position, ' ...', ok, message)
            else
                message = 'repeat node has an unsupported minimum'
                return
            end if
        case default
            message = 'unknown StandardIR expression node '//trim(label)
            return
        end select
        if (len_trim(message) == 0) ok = .true.
    end subroutine emit_expression

    subroutine read_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'expected an SX atom'
            return
        end if
        if (len_trim(node%atom) == 0) then
            message = 'SX atom is empty'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = 'SX atom exceeds normalizer buffer'
            return
        end if
        value = trim(node%atom)
        ok = .true.
    end subroutine read_atom

    logical function atom_equals(node, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: expected

        atom_equals = .false.
        if (node%kind /= sx_atom) return
        if (trim(node%atom) == expected) atom_equals = .true.
    end function atom_equals

    subroutine append_atom(buffer, position, node, ok, message)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: position
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: value

        call read_atom(node, value, ok, message)
        if (.not. ok) return
        call append_text(buffer, position, trim(value), ok, message)
    end subroutine append_atom

    subroutine append_literal(buffer, position, value, ok, message)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: position
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call append_text(buffer, position, value, ok, message)
    end subroutine append_literal

    subroutine append_text(buffer, position, value, ok, message)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: position
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: length

        ok = .false.
        message = ''
        length = len(value)
        if (position + length > len(buffer)) then
            message = 'normalized production exceeds output buffer'
            return
        end if
        if (length > 0) buffer(position + 1:position + length) = value
        position = position + length
        ok = .true.
    end subroutine append_text

end module standardir_normalize

module standardir_grammar
    !! Emit a canonical EBNF projection from StandardIR syntax objects.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    public :: standardir_emit_antlr
    public :: standardir_emit_ebnf

contains

    subroutine standardir_emit_ebnf(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule, lhs, document, clause, page, source_hash

        ok = .false.
        message = ''
        call read_syntax_header(node, rule, lhs, document, clause, page, source_hash, &
            ok, message)
        if (.not. ok) return

        write (unit, '(a)', advance='no') '(* rule='
        write (unit, '(a)', advance='no') trim(rule)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(clause)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(page)
        write (unit, '(a)', advance='no') ' source-sha256='
        write (unit, '(a)', advance='no') trim(source_hash)
        write (unit, '(a)') ' *)'
        write (unit, '(a)', advance='no') trim(lhs)
        write (unit, '(a)', advance='no') ' ::= '
        call emit_expression(unit, node%children(4), ok, message)
        if (.not. ok) return
        write (unit, '(a)') ' ;'
    end subroutine standardir_emit_ebnf

    subroutine standardir_emit_antlr(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule, lhs, document, clause, page, source_hash

        ok = .false.
        message = ''
        call read_syntax_header(node, rule, lhs, document, clause, page, source_hash, &
            ok, message)
        if (.not. ok) return

        write (unit, '(a)', advance='no') '// rule='
        write (unit, '(a)', advance='no') trim(rule)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(clause)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(page)
        write (unit, '(a)', advance='no') ' source-sha256='
        write (unit, '(a)', advance='no') trim(source_hash)
        write (unit, '(a)')
        write (unit, '(a)') trim(antlr_name(lhs))
        write (unit, '(a)', advance='no') '    : '
        call emit_antlr_expression(unit, node%children(4), ok, message)
        if (.not. ok) return
        write (unit, '(a)')
        write (unit, '(a)') '    ;'
    end subroutine standardir_emit_antlr

    subroutine read_syntax_header(node, rule, lhs, document, clause, page, source_hash, &
            ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: rule, lhs, document, clause, page, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        rule = ''
        lhs = ''
        document = ''
        clause = ''
        page = ''
        source_hash = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 5) then
            message = 'syntax object has the wrong shape'
            return
        end if
        if (.not. atom_equals(node%children(1), 'syntax')) then
            message = 'syntax object has no syntax label'
            return
        end if
        call read_atom(node%children(2), rule, ok, message)
        if (.not. ok) return
        call read_pair(node%children(3), 'lhs', lhs, ok, message)
        if (.not. ok) return
        call read_source(node%children(5), document, clause, page, source_hash, ok, message)
    end subroutine read_syntax_header

    subroutine read_source(node, document, clause, page, source_hash, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: document, clause, page, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        document = ''
        clause = ''
        page = ''
        source_hash = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'source field has the wrong shape'
            return
        end if
        if (.not. atom_equals(node%children(1), 'source')) then
            message = 'syntax object has no source field'
            return
        end if
        do i = 2, node%child_count
            if (node%children(i)%kind /= sx_list) then
                message = 'source child is not an SX list'
                return
            end if
            if (node%children(i)%child_count < 1) then
                message = 'source child is empty'
                return
            end if
            if (atom_equals(node%children(i)%children(1), 'document')) then
                call read_pair(node%children(i), 'document', document, ok, message)
            else if (atom_equals(node%children(i)%children(1), 'clause')) then
                call read_pair(node%children(i), 'clause', clause, ok, message)
            else if (atom_equals(node%children(i)%children(1), 'page')) then
                call read_pair(node%children(i), 'page', page, ok, message)
            else if (atom_equals(node%children(i)%children(1), 'source-sha256')) then
                call read_pair(node%children(i), 'source-sha256', source_hash, ok, message)
            end if
            if (.not. ok .and. len_trim(message) /= 0) return
        end do
        if (len_trim(document) == 0 .or. len_trim(clause) == 0 .or. &
            len_trim(page) == 0 .or. len_trim(source_hash) == 0) then
            message = 'source field lacks document, clause, page or source hash'
            return
        end if
        ok = .true.
    end subroutine read_source

    recursive subroutine emit_expression(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, minimum, maximum
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'grammar expression has the wrong shape'
            return
        end if
        call read_atom(node%children(1), label, ok, message)
        if (.not. ok) return

        select case (trim(label))
        case ('rhs')
            if (node%child_count /= 2) then
                message = 'rhs expression has the wrong field count'
                return
            end if
            call emit_expression(unit, node%children(2), ok, message)
        case ('ref')
            call emit_leaf(unit, node, .false., ok, message)
        case ('token')
            call emit_leaf(unit, node, .true., ok, message)
        case ('seq')
            if (node%child_count < 2) then
                message = 'sequence expression is empty'
                return
            end if
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ' '
                call emit_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
        case ('alt')
            if (node%child_count < 2) then
                message = 'alternative expression is empty'
                return
            end if
            write (unit, '(a)', advance='no') '( '
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ' | '
                call emit_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
            write (unit, '(a)', advance='no') ' )'
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional expression has the wrong field count'
                return
            end if
            write (unit, '(a)', advance='no') '[ '
            call emit_expression(unit, node%children(2), ok, message)
            if (.not. ok) return
            write (unit, '(a)', advance='no') ' ]'
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat expression has the wrong field count'
                return
            end if
            call read_atom(node%children(3), minimum, ok, message)
            if (.not. ok) return
            call read_atom(node%children(4), maximum, ok, message)
            if (.not. ok) return
            if (trim(maximum) /= 'unbounded') then
                message = 'repeat expression is not unbounded'
                return
            end if
            if (trim(minimum) == '0') then
                write (unit, '(a)', advance='no') '{ '
                call emit_expression(unit, node%children(2), ok, message)
                if (.not. ok) return
                write (unit, '(a)', advance='no') ' }'
            else if (trim(minimum) == '1') then
                call emit_expression(unit, node%children(2), ok, message)
                if (.not. ok) return
                write (unit, '(a)', advance='no') ' { '
                call emit_expression(unit, node%children(2), ok, message)
                if (.not. ok) return
                write (unit, '(a)', advance='no') ' }'
            else
                message = 'repeat expression has an unsupported minimum'
                return
            end if
        case default
            message = 'unknown grammar expression '//trim(label)
            return
        end select
        if (len_trim(message) == 0) ok = .true.
    end subroutine emit_expression

    recursive subroutine emit_antlr_expression(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, minimum, maximum, value
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'grammar expression has the wrong shape'
            return
        end if
        call read_atom(node%children(1), label, ok, message)
        if (.not. ok) return

        select case (trim(label))
        case ('rhs')
            if (node%child_count /= 2) then
                message = 'rhs expression has the wrong field count'
                return
            end if
            call emit_antlr_expression(unit, node%children(2), ok, message)
        case ('ref')
            if (node%child_count /= 2) then
                message = 'reference expression has the wrong field count'
                return
            end if
            call read_atom(node%children(2), value, ok, message)
            if (ok) write (unit, '(a)', advance='no') trim(antlr_name(value))
        case ('token')
            if (node%child_count /= 2) then
                message = 'token expression has the wrong field count'
                return
            end if
            call read_atom(node%children(2), value, ok, message)
            if (ok) call emit_antlr_literal(unit, value)
        case ('seq')
            if (node%child_count < 2) then
                message = 'sequence expression is empty'
                return
            end if
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ' '
                call emit_antlr_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
        case ('alt')
            if (node%child_count < 2) then
                message = 'alternative expression is empty'
                return
            end if
            write (unit, '(a)', advance='no') '( '
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ' | '
                call emit_antlr_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
            write (unit, '(a)', advance='no') ' )'
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional expression has the wrong field count'
                return
            end if
            write (unit, '(a)', advance='no') '( '
            call emit_antlr_expression(unit, node%children(2), ok, message)
            if (.not. ok) return
            write (unit, '(a)', advance='no') ' )?'
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat expression has the wrong field count'
                return
            end if
            call read_atom(node%children(3), minimum, ok, message)
            if (.not. ok) return
            call read_atom(node%children(4), maximum, ok, message)
            if (.not. ok) return
            if (trim(maximum) /= 'unbounded') then
                message = 'repeat expression is not unbounded'
                return
            end if
            if (trim(minimum) == '0') then
                write (unit, '(a)', advance='no') '( '
                call emit_antlr_expression(unit, node%children(2), ok, message)
                if (.not. ok) return
                write (unit, '(a)', advance='no') ' )*'
            else if (trim(minimum) == '1') then
                write (unit, '(a)', advance='no') '( '
                call emit_antlr_expression(unit, node%children(2), ok, message)
                if (.not. ok) return
                write (unit, '(a)', advance='no') ' )+'
            else
                message = 'repeat expression has an unsupported minimum'
                return
            end if
        case default
            message = 'unknown grammar expression '//trim(label)
            return
        end select
        if (len_trim(message) == 0) ok = .true.
    end subroutine emit_antlr_expression

    subroutine emit_antlr_literal(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value

        integer :: i

        write (unit, '(a)', advance='no') achar(39)
        do i = 1, len_trim(value)
            if (value(i:i) == achar(39) .or. value(i:i) == achar(92)) &
                write (unit, '(a)', advance='no') achar(92)
            write (unit, '(a)', advance='no') value(i:i)
        end do
        write (unit, '(a)', advance='no') achar(39)
    end subroutine emit_antlr_literal

    character(len=1024) function antlr_name(value)
        character(len=*), intent(in) :: value

        character(len=16) :: encoded
        integer :: code, i, position

        antlr_name = 'r_'
        position = 3
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                (code >= iachar('0') .and. code <= iachar('9')) .or. code == iachar('_')) then
                if (position <= len(antlr_name)) then
                    antlr_name(position:position) = value(i:i)
                    position = position + 1
                end if
            else
                write (encoded, '("_x",z2.2,"_")') code
                if (position + len_trim(encoded) - 1 <= len(antlr_name)) then
                    antlr_name(position:position + len_trim(encoded) - 1) = trim(encoded)
                    position = position + len_trim(encoded)
                end if
            end if
        end do
    end function antlr_name

    subroutine emit_leaf(unit, node, quoted, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(in) :: quoted
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: value

        ok = .false.
        message = ''
        if (node%child_count /= 2) then
            message = 'grammar leaf has the wrong field count'
            return
        end if
        call read_atom(node%children(2), value, ok, message)
        if (.not. ok) return
        if (quoted) write (unit, '(a)', advance='no') '"'
        write (unit, '(a)', advance='no') trim(value)
        if (quoted) write (unit, '(a)', advance='no') '"'
    end subroutine emit_leaf

    subroutine read_pair(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 2) then
            message = 'source pair has the wrong shape'
            return
        end if
        if (.not. atom_equals(node%children(1), label)) then
            message = 'source pair label differs'
            return
        end if
        call read_atom(node%children(2), value, ok, message)
    end subroutine read_pair

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
        if (len_trim(node%atom) > len(value)) then
            message = 'SX atom exceeds grammar emitter buffer'
            return
        end if
        value = trim(node%atom)
        ok = len_trim(value) > 0
        if (.not. ok) message = 'SX atom is empty'
    end subroutine read_atom

    logical function atom_equals(node, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: expected

        atom_equals = node%kind == sx_atom .and. trim(node%atom) == expected
    end function atom_equals

end module standardir_grammar

module standardir_treesitter
    !! Emit a tree-sitter grammar.js projection from StandardIR syntax objects.

    use fortsx, only: sx_max_atom_length, sx_node_t
    use standardir_grouping, only: standardir_group_t
    use standardir_syntax_fields, only: standardir_read_atom, &
        standardir_read_syntax_header
    implicit none
    private

    public :: standardir_emit_treesitter
    public :: standardir_emit_treesitter_group

contains

    subroutine standardir_emit_treesitter(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule, lhs, document, clause, page, source_hash
        character(len=sx_max_atom_length) :: source_lineage, source_expression_hash, target_expression_hash
        character(len=64) :: source_byte_start, source_byte_length

        ok = .false.
        message = ''
        call standardir_read_syntax_header(node, rule, lhs, document, clause, page, &
            source_hash, ok, message, source_lineage, source_byte_start, source_byte_length, &
            source_expression_hash, target_expression_hash)
        if (.not. ok) return

        write (unit, '(a)', advance='no') '// rule='
        write (unit, '(a)', advance='no') trim(rule)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(clause)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(page)
        write (unit, '(a)', advance='no') ' source-canonical-text-sha256='
        write (unit, '(a)', advance='no') trim(source_hash)
        if (len_trim(source_byte_start) > 0) then
            write (unit, '(a)', advance='no') ' source-byte-start='
            write (unit, '(a)', advance='no') trim(source_byte_start)
            write (unit, '(a)', advance='no') ' source-byte-length='
            write (unit, '(a)', advance='no') trim(source_byte_length)
        end if
        if (len_trim(source_lineage) > 0) then
            write (unit, '(a)', advance='no') ' source-lineage='
            write (unit, '(a)', advance='no') trim(source_lineage)
        end if
        if (len_trim(source_expression_hash) > 0) then
            write (unit, '(a)', advance='no') ' source-expression-sha256='
            write (unit, '(a)', advance='no') trim(source_expression_hash)
        end if
        if (len_trim(target_expression_hash) > 0) then
            write (unit, '(a)', advance='no') ' target-expression-sha256='
            write (unit, '(a)', advance='no') trim(target_expression_hash)
        end if
        write (unit, '(a)')
        write (unit, '(a)', advance='no') trim(treesitter_name(lhs))
        write (unit, '(a)', advance='no') ': $ => '
        call emit_expression(unit, node%children(4), ok, message)
        if (.not. ok) return
        write (unit, '(a)') ','
    end subroutine standardir_emit_treesitter

    subroutine standardir_emit_treesitter_group(unit, nodes, group, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_group_t), intent(in) :: group
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule, lhs, document, clause, page, source_hash
        character(len=sx_max_atom_length) :: source_lineage, source_expression_hash, target_expression_hash
        character(len=64) :: source_byte_start, source_byte_length
        integer :: i, index

        ok = .false.
        message = ''
        if (group%count < 1) then
            message = 'cannot emit an empty tree-sitter group'
            return
        end if
        write (unit, '(a)', advance='no') trim(treesitter_name(group%lhs))//': $ => '
        if (group%count > 1) write (unit, '(a)', advance='no') 'choice('
        do i = 1, group%count
            index = group%indices(i)
            call standardir_read_syntax_header(nodes(index), rule, lhs, document, clause, page, &
                source_hash, ok, message, source_lineage, source_byte_start, source_byte_length, &
                source_expression_hash, target_expression_hash)
            if (.not. ok) return
            if (i > 1) write (unit, '(a)', advance='no') ', '
            call emit_provenance(unit, rule, document, clause, page, source_hash, source_lineage, &
                source_byte_start, source_byte_length, source_expression_hash, target_expression_hash)
            call emit_expression(unit, nodes(index)%children(4), ok, message)
            if (.not. ok) return
        end do
        if (group%count > 1) write (unit, '(a)', advance='no') ')'
        write (unit, '(a)') ','
        ok = .true.
        message = ''
    end subroutine standardir_emit_treesitter_group

    subroutine emit_provenance(unit, rule, document, clause, page, source_hash, source_lineage, &
            source_byte_start, source_byte_length, source_expression_hash, target_expression_hash)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: rule, document, clause, page, source_hash
        character(len=*), intent(in) :: source_lineage, source_byte_start, source_byte_length, &
            source_expression_hash, target_expression_hash

        write (unit, '(a)', advance='no') '// rule='
        write (unit, '(a)', advance='no') trim(rule)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(clause)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(page)
        write (unit, '(a)', advance='no') ' source-canonical-text-sha256='
        write (unit, '(a)', advance='no') trim(source_hash)
        if (len_trim(source_byte_start) > 0) then
            write (unit, '(a)', advance='no') ' source-byte-start='
            write (unit, '(a)', advance='no') trim(source_byte_start)
            write (unit, '(a)', advance='no') ' source-byte-length='
            write (unit, '(a)', advance='no') trim(source_byte_length)
        end if
        if (len_trim(source_lineage) > 0) then
            write (unit, '(a)', advance='no') ' source-lineage='
            write (unit, '(a)', advance='no') trim(source_lineage)
        end if
        if (len_trim(source_expression_hash) > 0) then
            write (unit, '(a)', advance='no') ' source-expression-sha256='
            write (unit, '(a)', advance='no') trim(source_expression_hash)
        end if
        if (len_trim(target_expression_hash) > 0) then
            write (unit, '(a)', advance='no') ' target-expression-sha256='
            write (unit, '(a)', advance='no') trim(target_expression_hash)
        end if
        write (unit, '(a)')
    end subroutine emit_provenance

    recursive subroutine emit_expression(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, minimum, maximum, value
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= 2 .or. node%child_count < 1) then
            message = 'grammar expression has the wrong shape'
            return
        end if
        call standardir_read_atom(node%children(1), label, ok, message)
        if (.not. ok) return

        select case (trim(label))
        case ('rhs')
            if (node%child_count /= 2) then
                message = 'rhs expression has the wrong field count'
                return
            end if
            call emit_expression(unit, node%children(2), ok, message)
        case ('ref')
            call read_leaf(node, 'ref', value, ok, message)
            if (ok) then
                write (unit, '(a)', advance='no') '$.'//trim(treesitter_name(value))
            end if
        case ('token')
            call read_leaf(node, 'token', value, ok, message)
            if (ok) call emit_literal(unit, value)
        case ('seq')
            if (node%child_count < 2) then
                message = 'sequence expression is empty'
                return
            end if
            write (unit, '(a)', advance='no') 'seq('
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ', '
                call emit_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
            write (unit, '(a)', advance='no') ')'
        case ('alt')
            if (node%child_count < 2) then
                message = 'alternative expression is empty'
                return
            end if
            write (unit, '(a)', advance='no') 'choice('
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ', '
                call emit_expression(unit, node%children(i), ok, message)
                if (.not. ok) return
            end do
            write (unit, '(a)', advance='no') ')'
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional expression has the wrong field count'
                return
            end if
            write (unit, '(a)', advance='no') 'optional('
            call emit_expression(unit, node%children(2), ok, message)
            if (.not. ok) return
            write (unit, '(a)', advance='no') ')'
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat expression has the wrong field count'
                return
            end if
            call standardir_read_atom(node%children(3), minimum, ok, message)
            if (.not. ok) return
            call standardir_read_atom(node%children(4), maximum, ok, message)
            if (.not. ok) return
            if (trim(maximum) /= 'unbounded') then
                message = 'repeat expression is not unbounded'
                return
            end if
            if (trim(minimum) == '0') then
                write (unit, '(a)', advance='no') 'repeat('
            else if (trim(minimum) == '1') then
                write (unit, '(a)', advance='no') 'repeat1('
            else
                message = 'repeat expression has an unsupported minimum'
                return
            end if
            call emit_expression(unit, node%children(2), ok, message)
            if (.not. ok) return
            write (unit, '(a)', advance='no') ')'
        case default
            message = 'unknown tree-sitter expression '//trim(label)
            return
        end select
        if (len_trim(message) == 0) ok = .true.
    end subroutine emit_expression

    subroutine read_leaf(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%child_count /= 2) then
            message = trim(label)//' expression has the wrong field count'
            return
        end if
        if (node%children(1)%kind /= 1 .or. trim(node%children(1)%atom) /= trim(label)) then
            message = trim(label)//' expression has the wrong label'
            return
        end if
        call standardir_read_atom(node%children(2), value, ok, message)
    end subroutine read_leaf

    subroutine emit_literal(unit, value)
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
    end subroutine emit_literal

    character(len=1024) function treesitter_name(value)
        character(len=*), intent(in) :: value
        character(len=16) :: encoded
        integer :: code, i, position

        treesitter_name = 'r_'
        position = 3
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                (code >= iachar('0') .and. code <= iachar('9')) .or. code == iachar('_')) then
                treesitter_name(position:position) = value(i:i)
                position = position + 1
            else
                if (code <= 255) then
                    write (encoded, '("_x",z2.2,"_")') code
                else
                    write (encoded, '("_x",z4.4,"_")') code
                end if
                treesitter_name(position:position + len_trim(encoded) - 1) = trim(encoded)
                position = position + len_trim(encoded)
            end if
        end do
    end function treesitter_name

end module standardir_treesitter

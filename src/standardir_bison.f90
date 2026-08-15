module standardir_bison
    !! Emit a Bison grammar projection from StandardIR syntax objects.

    use fortsx, only: sx_atom, sx_list, sx_max_atom_length, sx_node_t
    use standardir_grouping, only: standardir_group_t, standardir_max_group_members
    use standardir_syntax_fields, only: standardir_read_atom, standardir_read_pair, &
        standardir_read_syntax_header
    implicit none
    private

    integer, parameter :: max_helpers = 256

    type :: bison_helper_t
        character(len=256) :: name = ''
        character(len=32) :: kind = ''
        type(sx_node_t) :: node
    end type bison_helper_t

    public :: standardir_emit_bison
    public :: standardir_emit_bison_group
    public :: standardir_emit_bison_start

contains

    subroutine standardir_emit_bison_start(unit, names, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: names(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (size(names) < 1) then
            message = 'Bison start set is empty'
            return
        end if
        write (unit, '(a)') 'standardir_start:'
        do i = 1, size(names)
            if (i == 1) then
                write (unit, '(a)', advance='no') '    '
            else
                write (unit, '(a)', advance='no') '  | '
            end if
            write (unit, '(a)') trim(bison_name(names(i)))
        end do
        write (unit, '(a)') '  ;'
        ok = .true.
    end subroutine standardir_emit_bison_start

    subroutine standardir_emit_bison(unit, node, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(bison_helper_t) :: helpers(max_helpers)
        character(len=256) :: rule, lhs, document, clause, page, source_hash
        character(len=sx_max_atom_length) :: source_lineage, source_expression_hash, target_expression_hash
        character(len=64) :: source_byte_start, source_byte_length
        character(len=256) :: top_symbol
        integer :: helper_count, i

        ok = .false.
        message = ''
        call standardir_read_syntax_header(node, rule, lhs, document, clause, page, source_hash, &
            ok, message, source_lineage, source_byte_start, source_byte_length, source_expression_hash, &
            target_expression_hash)
        if (.not. ok) return
        helper_count = 0
        call prepare_root(node%children(4), rule, helpers, helper_count, top_symbol, ok, message)
        if (.not. ok) return

        write (unit, '(a)', advance='no') '/* rule='
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
        write (unit, '(a)') ' */'
        write (unit, '(a)', advance='no') trim(bison_name(lhs))
        write (unit, '(a)') ':'
        write (unit, '(a)', advance='no') '    '
        if (len_trim(top_symbol) > 0) then
            write (unit, '(a)') trim(top_symbol)
        else
            call emit_inline(unit, node%children(4), helpers, helper_count, ok, message)
            if (.not. ok) return
            write (unit, '(a)')
        end if
        write (unit, '(a)') '  ;'
        do i = 1, helper_count
            call emit_helper(unit, helpers(i), helpers, helper_count, ok, message)
            if (.not. ok) return
        end do
    end subroutine standardir_emit_bison

    subroutine standardir_emit_bison_group(unit, nodes, group, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_group_t), intent(in) :: group
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(bison_helper_t) :: helpers(max_helpers)
        character(len=256) :: rule, lhs, document, clause, page, source_hash
        character(len=sx_max_atom_length) :: source_lineage, source_expression_hash, target_expression_hash
        character(len=64) :: source_byte_start, source_byte_length
        character(len=256) :: top_symbols(standardir_max_group_members)
        integer :: helper_count, i, index

        ok = .false.
        message = ''
        helper_count = 0
        if (group%count < 1) then
            message = 'cannot emit an empty Bison group'
            return
        end if
        do i = 1, group%count
            index = group%indices(i)
            call standardir_read_syntax_header(nodes(index), rule, lhs, document, clause, page, &
                source_hash, ok, message, source_lineage, source_byte_start, source_byte_length, &
                source_expression_hash, target_expression_hash)
            if (.not. ok) return
            call prepare_root(nodes(index)%children(4), rule, helpers, helper_count, &
                top_symbols(i), ok, message)
            if (.not. ok) return
        end do
        do i = 1, group%count
            index = group%indices(i)
            call standardir_read_syntax_header(nodes(index), rule, lhs, document, clause, page, &
                source_hash, ok, message, source_lineage, source_byte_start, source_byte_length, &
                source_expression_hash, target_expression_hash)
            if (.not. ok) return
            if (i == 1) then
                write (unit, '(a)', advance='no') trim(bison_name(group%lhs))//':'
            else
                write (unit, '(a)', advance='no') '  |'
            end if
            write (unit, '(a)', advance='no') new_line('a')//'    '
            call emit_bison_provenance(unit, rule, document, clause, page, source_hash, source_lineage, &
                source_byte_start, source_byte_length, source_expression_hash, target_expression_hash)
            if (len_trim(top_symbols(i)) > 0) then
                write (unit, '(a)', advance='no') trim(top_symbols(i))
            else
                call emit_inline(unit, nodes(index)%children(4), helpers, helper_count, ok, message)
                if (.not. ok) return
            end if
            write (unit, '(a)')
        end do
        write (unit, '(a)') '  ;'
        do i = 1, helper_count
            call emit_helper(unit, helpers(i), helpers, helper_count, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine standardir_emit_bison_group

    subroutine emit_bison_provenance(unit, rule, document, clause, page, source_hash, source_lineage, &
            source_byte_start, source_byte_length, source_expression_hash, target_expression_hash)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: rule, document, clause, page, source_hash
        character(len=*), intent(in) :: source_lineage, source_byte_start, source_byte_length, &
            source_expression_hash, target_expression_hash

        write (unit, '(a)', advance='no') '/* rule='
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
        write (unit, '(a)') ' */'
    end subroutine emit_bison_provenance

    recursive subroutine prepare_root(node, rule, helpers, helper_count, top_symbol, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: rule
        type(bison_helper_t), intent(inout) :: helpers(:)
        integer, intent(inout) :: helper_count
        character(len=*), intent(out) :: top_symbol
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label

        top_symbol = ''
        call read_label(node, label, ok, message)
        if (.not. ok) return
        if (trim(label) == 'rhs') then
            if (node%child_count /= 2) then
                ok = .false.
                message = 'rhs expression has the wrong field count'
                return
            end if
            call prepare_root(node%children(2), rule, helpers, helper_count, top_symbol, ok, message)
        else if (trim(label) == 'seq') then
            call prepare_sequence(node, rule, helpers, helper_count, ok, message)
        else
            call ensure_symbol(node, rule, helpers, helper_count, top_symbol, ok, message)
        end if
    end subroutine prepare_root

    recursive subroutine ensure_symbol(node, rule, helpers, helper_count, symbol, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: rule
        type(bison_helper_t), intent(inout) :: helpers(:)
        integer, intent(inout) :: helper_count
        character(len=*), intent(out) :: symbol
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label
        integer :: existing

        symbol = ''
        call read_label(node, label, ok, message)
        if (.not. ok) return
        select case (trim(label))
        case ('ref')
            call read_value(node, 'ref', symbol, ok, message)
            if (ok) symbol = trim(bison_name(symbol))
            return
        case ('token')
            call read_value(node, 'token', symbol, ok, message)
            if (ok) symbol = bison_literal_text(symbol)
            return
        case ('rhs')
            if (node%child_count /= 2) then
                ok = .false.
                message = 'rhs expression has the wrong field count'
                return
            end if
            call ensure_symbol(node%children(2), rule, helpers, helper_count, symbol, ok, message)
            return
        end select

        existing = find_helper(node, helpers, helper_count)
        if (existing > 0) then
            symbol = helpers(existing)%name
            ok = .true.
            message = ''
            return
        end if
        if (helper_count >= size(helpers)) then
            ok = .false.
            message = 'Bison helper limit exceeded'
            return
        end if
        helper_count = helper_count + 1
        helpers(helper_count)%name = 'h_'//trim(bison_name(rule))//'_'//integer_text(helper_count)
        helpers(helper_count)%kind = trim(label)
        helpers(helper_count)%node = node
        symbol = helpers(helper_count)%name
        ok = .true.
        message = ''
        call prepare_helper(node, rule, helpers, helper_count, ok, message)
    end subroutine ensure_symbol

    recursive subroutine prepare_helper(node, rule, helpers, helper_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: rule
        type(bison_helper_t), intent(inout) :: helpers(:)
        integer, intent(inout) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, ignored
        integer :: i

        call read_label(node, label, ok, message)
        if (.not. ok) return
        select case (trim(label))
        case ('seq')
            call prepare_sequence(node, rule, helpers, helper_count, ok, message)
        case ('alt')
            if (node%child_count < 2) then
                ok = .false.
                message = 'alternative expression is empty'
                return
            end if
            do i = 2, node%child_count
                call prepare_branch(node%children(i), rule, helpers, helper_count, ok, message)
                if (.not. ok) return
            end do
        case ('optional', 'repeat')
            if (node%child_count < 2) then
                ok = .false.
                message = 'Bison helper expression is incomplete'
                return
            end if
            call ensure_symbol(node%children(2), rule, helpers, helper_count, ignored, ok, message)
        case default
            ok = .false.
            message = 'unknown Bison helper expression '//trim(label)
        end select
    end subroutine prepare_helper

    recursive subroutine prepare_sequence(node, rule, helpers, helper_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: rule
        type(bison_helper_t), intent(inout) :: helpers(:)
        integer, intent(inout) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: ignored
        integer :: i

        ok = .true.
        message = ''
        if (node%child_count < 2) then
            ok = .false.
            message = 'sequence expression is empty'
            return
        end if
        do i = 2, node%child_count
            call ensure_symbol(node%children(i), rule, helpers, helper_count, ignored, ok, message)
            if (.not. ok) return
        end do
    end subroutine prepare_sequence

    recursive subroutine prepare_branch(node, rule, helpers, helper_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: rule
        type(bison_helper_t), intent(inout) :: helpers(:)
        integer, intent(inout) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, ignored

        call read_label(node, label, ok, message)
        if (.not. ok) return
        if (trim(label) == 'seq') then
            call prepare_sequence(node, rule, helpers, helper_count, ok, message)
        else
            call ensure_symbol(node, rule, helpers, helper_count, ignored, ok, message)
        end if
    end subroutine prepare_branch

    recursive subroutine emit_inline(unit, node, helpers, helper_count, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, symbol
        integer :: i

        call read_label(node, label, ok, message)
        if (.not. ok) return
        select case (trim(label))
        case ('rhs')
            call emit_inline(unit, node%children(2), helpers, helper_count, ok, message)
        case ('seq')
            do i = 2, node%child_count
                if (i > 2) write (unit, '(a)', advance='no') ' '
                call emit_symbol(unit, node%children(i), helpers, helper_count, ok, message)
                if (.not. ok) return
            end do
        case default
            symbol = helper_for(node, helpers, helper_count)
            if (len_trim(symbol) == 0) then
                ok = .false.
                message = 'Bison helper was not prepared'
                return
            end if
            write (unit, '(a)', advance='no') trim(symbol)
        end select
        if (len_trim(message) == 0) ok = .true.
    end subroutine emit_inline

    subroutine emit_symbol(unit, node, helpers, helper_count, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label, value, symbol

        call read_label(node, label, ok, message)
        if (.not. ok) return
        if (trim(label) == 'ref') then
            call read_value(node, 'ref', value, ok, message)
            if (ok) write (unit, '(a)', advance='no') trim(bison_name(value))
        else if (trim(label) == 'token') then
            call read_value(node, 'token', value, ok, message)
            if (ok) call emit_bison_literal(unit, value)
        else
            symbol = helper_for(node, helpers, helper_count)
            if (len_trim(symbol) == 0) then
                ok = .false.
                message = 'Bison expression was not prepared'
                return
            end if
            write (unit, '(a)', advance='no') trim(symbol)
        end if
    end subroutine emit_symbol

    subroutine emit_helper(unit, helper, helpers, helper_count, ok, message)
        integer, intent(in) :: unit
        type(bison_helper_t), intent(in) :: helper
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .true.
        message = ''
        write (unit, '(a)', advance='no') trim(helper%name)
        write (unit, '(a)') ':'
        select case (trim(helper%kind))
        case ('seq')
            write (unit, '(a)', advance='no') '    '
            call emit_inline(unit, helper%node, helpers, helper_count, ok, message)
            write (unit, '(a)')
        case ('alt')
            do i = 2, helper%node%child_count
                if (i == 2) then
                    write (unit, '(a)', advance='no') '    '
                else
                    write (unit, '(a)', advance='no') '  | '
                end if
                call emit_branch(unit, helper%node%children(i), helpers, helper_count, ok, message)
                write (unit, '(a)')
                if (.not. ok) return
            end do
        case ('optional')
            write (unit, '(a)') '    %empty'
            write (unit, '(a)', advance='no') '  | '
            call emit_symbol(unit, helper%node%children(2), helpers, helper_count, ok, message)
            write (unit, '(a)')
        case ('repeat')
            write (unit, '(a)') '    %empty'
            write (unit, '(a)', advance='no') '  | '
            call emit_symbol(unit, helper%node%children(2), helpers, helper_count, ok, message)
            write (unit, '(a)', advance='no') ' '//trim(helper%name)
            write (unit, '(a)')
        case default
            ok = .false.
            message = 'unknown Bison helper kind '//trim(helper%kind)
            return
        end select
        if (.not. ok) return
        write (unit, '(a)') '  ;'
    end subroutine emit_helper

    subroutine emit_branch(unit, node, helpers, helper_count, ok, message)
        integer, intent(in) :: unit
        type(sx_node_t), intent(in) :: node
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label

        call read_label(node, label, ok, message)
        if (.not. ok) return
        if (trim(label) == 'seq') then
            call emit_inline(unit, node, helpers, helper_count, ok, message)
        else
            call emit_symbol(unit, node, helpers, helper_count, ok, message)
        end if
    end subroutine emit_branch

    subroutine emit_bison_literal(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value

        integer :: i

        write (unit, '(a)', advance='no') achar(34)
        do i = 1, len_trim(value)
            if (value(i:i) == achar(34) .or. value(i:i) == achar(92)) &
                write (unit, '(a)', advance='no') achar(92)
            write (unit, '(a)', advance='no') value(i:i)
        end do
        write (unit, '(a)', advance='no') achar(34)
    end subroutine emit_bison_literal

    character(len=1024) function bison_literal_text(value)
        character(len=*), intent(in) :: value
        integer :: i, position

        bison_literal_text = achar(34)
        position = 2
        do i = 1, len_trim(value)
            if (value(i:i) == achar(34) .or. value(i:i) == achar(92)) then
                bison_literal_text(position:position) = achar(92)
                position = position + 1
            end if
            bison_literal_text(position:position) = value(i:i)
            position = position + 1
        end do
        bison_literal_text(position:position) = achar(34)
    end function bison_literal_text

    character(len=1024) function bison_name(value)
        character(len=*), intent(in) :: value

        character(len=16) :: encoded
        integer :: code, i, position

        bison_name = 'r_'
        position = 3
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                (code >= iachar('0') .and. code <= iachar('9')) .or. code == iachar('_')) then
                bison_name(position:position) = value(i:i)
                position = position + 1
            else
                write (encoded, '("_x",z2.2,"_")') code
                bison_name(position:position + len_trim(encoded) - 1) = trim(encoded)
                position = position + len_trim(encoded)
            end if
        end do
    end function bison_name

    integer function find_helper(node, helpers, helper_count)
        type(sx_node_t), intent(in) :: node
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        integer :: i

        find_helper = 0
        do i = 1, helper_count
            if (same_node(node, helpers(i)%node)) then
                find_helper = i
                return
            end if
        end do
    end function find_helper

    recursive logical function same_node(left, right) result(equal)
        type(sx_node_t), intent(in) :: left, right
        integer :: i

        equal = left%kind == right%kind .and. left%child_count == right%child_count
        if (.not. equal) return
        if (left%kind == sx_atom) then
            equal = trim(left%atom) == trim(right%atom)
            return
        end if
        do i = 1, left%child_count
            if (.not. same_node(left%children(i), right%children(i))) then
                equal = .false.
                return
            end if
        end do
    end function same_node

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

    subroutine read_value(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_read_pair(node, label, value, ok, message)
    end subroutine read_value

    subroutine read_label(node, label, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        label = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'grammar expression has the wrong shape'
            return
        end if
        call standardir_read_atom(node%children(1), label, ok, message)
    end subroutine read_label

    character(len=256) function helper_for(node, helpers, helper_count)
        type(sx_node_t), intent(in) :: node
        type(bison_helper_t), intent(in) :: helpers(:)
        integer, intent(in) :: helper_count
        integer :: found

        helper_for = ''
        found = find_helper(node, helpers, helper_count)
        if (found > 0) helper_for = helpers(found)%name
    end function helper_for

end module standardir_bison

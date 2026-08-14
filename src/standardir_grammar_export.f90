module standardir_grammar_export
    !! Batch export for normalized, source-backed StandardIR grammar rules.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_bison, only: standardir_emit_bison_group
    use standardir_grammar, only: standardir_emit_antlr_group, standardir_emit_ebnf_group
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_node_t, &
        standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_groups, standardir_max_syntax_records
    use standardir_treesitter, only: standardir_emit_treesitter_group
    implicit none
    private

    integer, parameter, public :: standardir_grammar_format_ebnf = 1
    integer, parameter, public :: standardir_grammar_format_antlr4 = 2
    integer, parameter, public :: standardir_grammar_format_bison = 3
    integer, parameter, public :: standardir_grammar_format_tree_sitter = 4

    public :: standardir_grammar_export_batch

contains

    subroutine standardir_grammar_export_batch(unit, rules, format, ok, message)
        integer, intent(in) :: unit, format
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t), allocatable :: nodes(:)
        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i, j, ios, scratch

        ok = .false.
        message = ''
        if (format < standardir_grammar_format_ebnf .or. &
            format > standardir_grammar_format_tree_sitter) then
            message = 'grammar export format is unsupported'
            return
        end if
        if (size(rules) < 1) then
            message = 'grammar export batch is empty'
            return
        end if
        if (size(rules) > standardir_max_syntax_records) then
            message = 'grammar export batch exceeds the syntax record limit'
            return
        end if
        do i = 2, size(rules)
            if (trim(rules(i)%lhs) /= trim(rules(i - 1)%lhs)) then
                do j = 1, i - 1
                    if (trim(rules(j)%lhs) == trim(rules(i)%lhs)) then
                        message = 'grammar export batch interleaves LHS groups'
                        return
                    end if
                end do
            end if
        end do

        allocate (nodes(size(rules)))
        do i = 1, size(rules)
            call rule_to_syntax(rules(i), nodes(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_group_syntax(nodes, size(nodes), groups, group_count, ok, message)
        if (.not. ok) return

        open (newunit=scratch, status='scratch', action='readwrite', iostat=ios)
        if (ios /= 0) then
            message = 'could not open grammar export scratch output'
            return
        end if
        call emit_groups(scratch, nodes, groups, group_count, format, ok, message)
        if (.not. ok) then
            close (scratch)
            return
        end if
        rewind (scratch)
        call copy_output(scratch, unit, ok, message)
        close (scratch)
    end subroutine standardir_grammar_export_batch

    subroutine emit_groups(unit, nodes, groups, group_count, format, ok, message)
        integer, intent(in) :: unit, group_count, format
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_group_t), intent(in) :: groups(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, index

        ok = .false.
        message = ''
        do i = 1, group_count
            do j = 1, groups(i)%count
                index = groups(i)%indices(j)
                call emit_source_rule_annotation(unit, nodes(index), format, ok, message)
                if (.not. ok) return
            end do
            select case (format)
            case (standardir_grammar_format_ebnf)
                call standardir_emit_ebnf_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_antlr4)
                call standardir_emit_antlr_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_bison)
                call standardir_emit_bison_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_tree_sitter)
                call standardir_emit_treesitter_group(unit, nodes, groups(i), ok, message)
            end select
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine emit_groups

    subroutine emit_source_rule_annotation(unit, node, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: source_rule

        ok = .false.
        message = ''
        if (node%child_count /= 5 .or. node%children(5)%kind /= sx_list .or. &
            node%children(5)%child_count /= 6) then
            message = 'canonical grammar source is malformed'
            return
        end if
        if (node%children(5)%children(4)%kind /= sx_list .or. &
            node%children(5)%children(4)%child_count /= 2) then
            message = 'canonical grammar source rule is malformed'
            return
        end if
        if (node%children(5)%children(4)%children(2)%kind /= sx_atom) then
            message = 'canonical grammar source rule is not an atom'
            return
        end if
        source_rule = trim(node%children(5)%children(4)%children(2)%atom)
        if (len_trim(source_rule) == 0) then
            message = 'canonical grammar source rule is empty'
            return
        end if
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)') '(* source-rule='//trim(source_rule)//' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)') '// source-rule='//trim(source_rule)
        case (standardir_grammar_format_bison)
            write (unit, '(a)') '/* source-rule='//trim(source_rule)//' */'
        end select
        ok = .true.
        message = ''
    end subroutine emit_source_rule_annotation

    subroutine copy_output(source_unit, target_unit, ok, message)
        integer, intent(in) :: source_unit, target_unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=65536) :: line
        integer :: ios

        ok = .false.
        message = ''
        do
            read (source_unit, '(a)', iostat=ios) line
            if (ios < 0) exit
            if (ios > 0) then
                message = 'could not read staged grammar export'
                return
            end if
            write (target_unit, '(a)', iostat=ios) trim(line)
            if (ios /= 0) then
                message = 'could not write grammar export'
                return
            end if
        end do
        ok = .true.
        message = ''
    end subroutine copy_output

    subroutine rule_to_syntax(rule, syntax, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        type(sx_node_t), intent(out) :: syntax
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call clear_node(syntax)
        call standardir_grammar_validate(rule, ok, message)
        if (.not. ok) return
        if (rule%resolution /= standardir_grammar_resolution_resolved) then
            ok = .false.
            message = 'grammar export requires resolved rules'
            return
        end if

        call validate_export_tree(rule, rule%root, 0, ok, message)
        if (.not. ok) return

        call make_list(syntax, 5)
        call make_atom(syntax%children(1), 'syntax')
        call make_atom(syntax%children(2), trim(rule%id))
        call make_pair(syntax%children(3), 'lhs', trim(rule%lhs), ok, message)
        if (.not. ok) return
        call make_list(syntax%children(4), 2)
        call make_atom(syntax%children(4)%children(1), 'rhs')
        call build_expression(rule, rule%root, syntax%children(4)%children(2), ok, message)
        if (.not. ok) return
        call make_source(syntax%children(5), rule, ok, message)
    end subroutine rule_to_syntax

    recursive subroutine validate_export_tree(rule, index, depth, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index, depth
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child, last

        ok = .false.
        message = ''
        if (index < 1 .or. index > size(rule%nodes%values)) then
            message = 'grammar export child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'grammar export node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        if (node%child_count == 0) then
            ok = .true.
            return
        end if
        child = node%first_child
        do i = 1, node%child_count
            call validate_export_tree(rule, child, depth + 1, ok, message)
            if (.not. ok) return
            call subtree_end(rule, child, 0, last, ok, message)
            if (.not. ok) return
            child = last + 1
        end do
        ok = .true.
        message = ''
    end subroutine validate_export_tree

    recursive subroutine build_expression(rule, index, expression, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index
        type(sx_node_t), intent(out) :: expression
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child, last

        call clear_node(expression)
        ok = .false.
        message = ''
        node = rule%nodes%values(index)
        select case (node%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            call make_list(expression, 2)
            if (node%kind == standardir_grammar_reference) then
                call make_atom(expression%children(1), 'ref')
            else
                call make_atom(expression%children(1), 'token')
            end if
            call make_atom(expression%children(2), trim(node%name))
        case (standardir_grammar_sequence, standardir_grammar_choice)
            call make_list(expression, node%child_count + 1)
            if (node%kind == standardir_grammar_sequence) then
                call make_atom(expression%children(1), 'seq')
            else
                call make_atom(expression%children(1), 'alt')
            end if
            child = node%first_child
            do i = 1, node%child_count
                call build_expression(rule, child, expression%children(i + 1), ok, message)
                if (.not. ok) return
                call subtree_end(rule, child, 0, last, ok, message)
                if (.not. ok) return
                child = last + 1
            end do
        case (standardir_grammar_optional)
            call make_list(expression, 2)
            call make_atom(expression%children(1), 'optional')
            call build_expression(rule, node%first_child, expression%children(2), ok, message)
            if (.not. ok) return
        case (standardir_grammar_repeat)
            call make_list(expression, 4)
            call make_atom(expression%children(1), 'repeat')
            call build_expression(rule, node%first_child, expression%children(2), ok, message)
            if (.not. ok) return
            call make_atom(expression%children(3), integer_text(node%minimum))
            call make_atom(expression%children(4), 'unbounded')
        case default
            message = 'normalized grammar node kind is unsupported'
            return
        end select
        ok = .true.
        message = ''
    end subroutine build_expression

    recursive subroutine subtree_end(rule, index, depth, last, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index, depth
        integer, intent(out) :: last
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child

        ok = .false.
        message = ''
        if (index < 1 .or. index > size(rule%nodes%values)) then
            message = 'normalized grammar child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'normalized grammar node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        last = index
        if (node%child_count > 0) then
            child = node%first_child
            do i = 1, node%child_count
                call subtree_end(rule, child, depth + 1, last, ok, message)
                if (.not. ok) return
                child = last + 1
            end do
        end if
        ok = .true.
        message = ''
    end subroutine subtree_end

    subroutine make_source(node, rule, ok, message)
        type(sx_node_t), intent(out) :: node
        type(standardir_grammar_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(node, 6)
        call make_atom(node%children(1), 'source')
        call make_pair(node%children(2), 'document', trim(rule%source%document), ok, message)
        if (.not. ok) return
        call make_pair(node%children(3), 'clause', trim(rule%source%clause), ok, message)
        if (.not. ok) return
        call make_pair(node%children(4), 'rule', trim(rule%source%rule), ok, message)
        if (.not. ok) return
        call make_pair(node%children(5), 'page', integer_text(rule%source%page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(6), 'source-sha256', trim(rule%source%source_hash), ok, &
            message)
    end subroutine make_source

    subroutine make_pair(node, label, value, ok, message)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(node, 2)
        call make_atom(node%children(1), label)
        call make_atom(node%children(2), value)
        ok = len_trim(label) > 0 .and. len_trim(value) > 0
        if (ok) then
            message = ''
        else
            message = 'canonical grammar field is empty'
        end if
    end subroutine make_pair

    subroutine make_list(node, count)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: count

        call clear_node(node)
        node%kind = sx_list
        node%child_count = count
        allocate (node%children(count))
    end subroutine make_list

    subroutine make_atom(node, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: value

        call clear_node(node)
        node%kind = sx_atom
        node%atom = trim(value)
    end subroutine make_atom

    subroutine clear_node(node)
        type(sx_node_t), intent(inout) :: node

        if (allocated(node%children)) deallocate (node%children)
        node%kind = 0
        node%atom = ''
        node%child_count = 0
    end subroutine clear_node

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

end module standardir_grammar_export

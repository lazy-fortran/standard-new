module standardir_grammar_export
    !! Batch export for normalized, source-backed StandardIR grammar rules.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_bison, only: standardir_emit_bison_group
    use standardir_grammar, only: standardir_emit_antlr_group, standardir_emit_ebnf_group
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_rule_t, &
        standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_targetnorm, only: standardir_grammar_normalize, &
        standardir_target_expression_t, standardir_target_rule_t
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
    public :: standardir_grammar_normalize
    public :: standardir_target_expression_t
    public :: standardir_target_rule_t

contains


    subroutine standardir_grammar_export_batch(unit, rules, format, ok, message)
        integer, intent(in) :: unit, format
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
        type(sx_node_t), allocatable :: nodes(:), suppressed_nodes(:)
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

        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message)
        if (.not. ok) return
        allocate (nodes(size(normalized)))
        do i = 1, size(normalized)
            call target_rule_to_syntax(normalized(i), nodes(i), ok, message)
            if (.not. ok) return
        end do
        allocate (suppressed_nodes(size(suppressed)))
        do i = 1, size(suppressed)
            call target_rule_to_syntax(suppressed(i), suppressed_nodes(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_group_syntax(nodes, size(nodes), groups, group_count, ok, message)
        if (.not. ok) return

        open (newunit=scratch, status='scratch', action='readwrite', iostat=ios)
        if (ios /= 0) then
            message = 'could not open grammar export scratch output'
            return
        end if
        call emit_groups(scratch, nodes, suppressed_nodes, groups, group_count, format, ok, message)
        if (.not. ok) then
            close (scratch)
            return
        end if
        rewind (scratch)
        call copy_output(scratch, unit, ok, message)
        close (scratch)
    end subroutine standardir_grammar_export_batch

    subroutine emit_groups(unit, nodes, suppressed, groups, group_count, format, ok, message)
        integer, intent(in) :: unit, group_count, format
        type(sx_node_t), intent(in) :: nodes(:), suppressed(:)
        type(standardir_group_t), intent(in) :: groups(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, index

        ok = .false.
        message = ''
        do i = 1, group_count
            do j = 1, size(suppressed)
                call suppressed_provenance(unit, suppressed(j), groups(i)%lhs, format, ok, message)
                if (.not. ok) return
            end do
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

    subroutine target_rule_to_syntax(rule, syntax, ok, message)
        type(standardir_target_rule_t), intent(in) :: rule
        type(sx_node_t), intent(out) :: syntax
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(syntax, 5)
        call make_atom(syntax%children(1), 'syntax')
        call make_atom(syntax%children(2), trim(rule%id))
        call make_pair(syntax%children(3), 'lhs', trim(rule%lhs), ok, message)
        if (.not. ok) return
        call make_list(syntax%children(4), 2)
        call make_atom(syntax%children(4)%children(1), 'rhs')
        call target_expression_to_syntax(rule%expression, syntax%children(4)%children(2), ok, message)
        if (.not. ok) return
        call make_target_source(syntax%children(5), rule, ok, message)
    end subroutine target_rule_to_syntax

    recursive subroutine target_expression_to_syntax(expression, node, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        character(len=32) :: minimum

        call clear_node(node)
        ok = .false.
        message = ''
        select case (expression%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            call make_list(node, 2)
            if (expression%kind == standardir_grammar_reference) then
                call make_atom(node%children(1), 'ref')
            else
                call make_atom(node%children(1), 'token')
            end if
            call make_atom(node%children(2), trim(expression%name))
        case (standardir_grammar_sequence, standardir_grammar_choice)
            if (.not. allocated(expression%children) .or. size(expression%children) < 1) then
                message = 'normalized target expression is empty'
                return
            end if
            call make_list(node, size(expression%children) + 1)
            if (expression%kind == standardir_grammar_sequence) then
                call make_atom(node%children(1), 'seq')
            else
                call make_atom(node%children(1), 'alt')
            end if
            do i = 1, size(expression%children)
                call target_expression_to_syntax(expression%children(i), node%children(i + 1), ok, message)
                if (.not. ok) return
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            call make_list(node, merge(4, 2, expression%kind == standardir_grammar_repeat))
            if (expression%kind == standardir_grammar_optional) then
                call make_atom(node%children(1), 'optional')
            else
                call make_atom(node%children(1), 'repeat')
                write (minimum, '(i0)') expression%minimum
                call make_atom(node%children(3), trim(minimum))
                call make_atom(node%children(4), 'unbounded')
            end if
            call target_expression_to_syntax(expression%children(1), node%children(2), ok, message)
            if (.not. ok) return
        case default
            message = 'normalized target expression has an unsupported kind'
            return
        end select
        ok = .true.
        message = ''
    end subroutine target_expression_to_syntax

    subroutine suppressed_provenance(unit, node, lhs, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule_id, node_lhs, document, clause, page, source_hash
        integer :: i

        ok = .false.
        message = ''
        if (node%child_count /= 5 .or. node%children(3)%kind /= sx_list .or. &
            node%children(3)%child_count /= 2 .or. node%children(3)%children(2)%kind /= sx_atom) then
            message = 'suppressed grammar provenance is malformed'
            return
        end if
        node_lhs = trim(node%children(3)%children(2)%atom)
        if (node_lhs /= trim(lhs)) then
            ok = .true.
            return
        end if
        if (node%children(2)%kind /= sx_atom .or. node%children(5)%kind /= sx_list) then
            message = 'suppressed grammar provenance header is malformed'
            return
        end if
        rule_id = trim(node%children(2)%atom)
        document = ''; clause = ''; page = ''; source_hash = ''
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'suppressed grammar provenance field is malformed'
                return
            end if
            select case (trim(node%children(5)%children(i)%children(1)%atom))
            case ('document')
                document = trim(node%children(5)%children(i)%children(2)%atom)
            case ('clause')
                clause = trim(node%children(5)%children(i)%children(2)%atom)
            case ('page')
                page = trim(node%children(5)%children(i)%children(2)%atom)
            case ('source-sha256')
                source_hash = trim(node%children(5)%children(i)%children(2)%atom)
            end select
        end do
        call emit_source_rule_annotation(unit, node, format, ok, message)
        if (.not. ok) return
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(* rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)//' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)') '// rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)
        case (standardir_grammar_format_bison)
            write (unit, '(a)') '/* rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)//' */'
        end select
        ok = .true.
        message = ''
    end subroutine suppressed_provenance

    subroutine make_target_source(node, rule, ok, message)
        type(sx_node_t), intent(out) :: node
        type(standardir_target_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: alternative

        call make_list(node, 7)
        call make_atom(node%children(1), 'source')
        call make_pair(node%children(2), 'document', trim(rule%source%document), ok, message)
        if (.not. ok) return
        call make_pair(node%children(3), 'clause', trim(rule%source%clause), ok, message)
        if (.not. ok) return
        call make_pair(node%children(4), 'rule', trim(rule%source%rule), ok, message)
        if (.not. ok) return
        write (alternative, '(i0)') rule%alternative
        call make_pair(node%children(5), 'alternative', trim(alternative), ok, message)
        if (.not. ok) return
        call make_pair(node%children(6), 'page', integer_text(rule%source%page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(7), 'source-sha256', trim(rule%source%source_hash), ok, message)
    end subroutine make_target_source

    subroutine emit_source_rule_annotation(unit, node, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: source_rule, source_alternative
        integer :: i

        ok = .false.
        message = ''
        source_rule = ''
        source_alternative = ''
        if (node%child_count /= 5 .or. node%children(5)%kind /= sx_list .or. &
            node%children(5)%child_count < 6) then
            message = 'canonical grammar source is malformed'
            return
        end if
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'canonical grammar source child is malformed'
                return
            end if
            if (trim(node%children(5)%children(i)%children(1)%atom) == 'rule') then
                source_rule = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'alternative') then
                source_alternative = trim(node%children(5)%children(i)%children(2)%atom)
            end if
        end do
        if (len_trim(source_rule) == 0) then
            message = 'canonical grammar source rule is empty'
            return
        end if
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)') ' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)', advance='no') '// source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)')
        case (standardir_grammar_format_bison)
            write (unit, '(a)', advance='no') '/* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)') ' */'
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

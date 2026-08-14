module standardir_grammar_closure
    !! Close source-backed syntax records and adapt them for target export.
    !!
    !! The closure graph and the grammar representation remain separate.  Raw
    !! source records are selected by occurrence identity, adapted without
    !! rewriting, and only the small R401/R402/R403-style facts supplied by the
    !! caller are materialized as derived typed rules.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_grammar_producer, only: standardir_grammar_node_t, &
        standardir_grammar_origin_mechanical, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
    use standardir_grammar_sx_adapter_support, only: read_syntax
    use standardir_lexical, only: standardir_lexical_facts_t, standardir_lexical_validate
    use standardir_export, only: standardir_source_ref_t
    use standardir_reference_closure, only: closure_add_reference, closure_compute, &
        closure_kind_alias, closure_kind_erratum, closure_kind_lexical, closure_kind_list, &
        closure_kind_scalar, closure_kind_semantic_only, closure_kind_unresolved, &
        closure_classification_t, closure_input_record_t, closure_record_t, closure_result_t
    implicit none
    private

    public :: standardir_grammar_close_sx

contains

    subroutine standardir_grammar_close_sx(nodes, node_count, classifications, &
            classification_count, roots, root_count, lexical, rules, semantic_skipped, &
            lexical_closed, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: node_count
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        type(standardir_lexical_facts_t), intent(in) :: lexical
        type(standardir_grammar_rule_t), allocatable, intent(inout) :: rules(:)
        integer, intent(out) :: semantic_skipped, lexical_closed
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_input_record_t), allocatable :: input(:)
        type(closure_result_t) :: result
        type(standardir_grammar_rule_t), allocatable :: staged(:), one(:)
        type(standardir_source_ref_t) :: source
        type(sx_node_t) :: expression
        character(len=128) :: rule, lhs, id
        integer :: i, j, input_index, count
        logical :: local_ok, skip

        if (allocated(rules)) deallocate (rules)
        semantic_skipped = 0
        lexical_closed = 0
        ok = .false.
        message = ''
        if (node_count < 1 .or. node_count > size(nodes)) then
            message = 'grammar closure has no source records'
            return
        end if
        call standardir_lexical_validate(lexical, ok, message)
        if (.not. ok) return

        allocate (input(node_count))
        do i = 1, node_count
            call read_syntax(nodes(i), rule, lhs, expression, source, ok, message)
            if (.not. ok) then
                message = 'source record '//integer_text(i)//': '//trim(message)
                return
            end if
            write (id, '("occurrence-",i0)') i
            input(i) = closure_input_record_t()
            input(i)%id = trim(id)
            input(i)%lhs = trim(lhs)
            input(i)%source = source
            call collect_references(expression, input(i), source, &
                ok, message)
            if (.not. ok) then
                message = 'source record '//integer_text(i)//': '//trim(message)
                return
            end if
        end do

        call closure_compute(input, node_count, classifications, classification_count, roots, &
            root_count, result, ok, message)
        if (.not. ok) return

        ok = .false.
        allocate (staged(0))
        do i = 1, result%record_count
            if (.not. result%records(i)%derived) then
                input_index = find_input(input, node_count, result%records(i)%id)
                if (input_index == 0) then
                    message = 'closure result lost normative occurrence identity'
                    return
                end if
                skip = contains_semantic_reference(nodes(input_index), classifications, &
                    classification_count)
                if (skip) then
                    semantic_skipped = semantic_skipped + 1
                    cycle
                end if
                call standardir_grammar_adapt_sx(nodes(input_index), &
                    standardir_grammar_origin_mechanical, &
                    standardir_grammar_resolution_resolved, one, local_ok, message)
                if (.not. local_ok) return
                do j = 1, size(one)
                    call append_rule(staged, one(j))
                end do
                if (allocated(one)) deallocate (one)
            else
                select case (result%records(i)%kind)
                case (closure_kind_semantic_only)
                    semantic_skipped = semantic_skipped + 1
                case (closure_kind_lexical)
                    if (.not. lexical_contains(lexical, result%records(i)%lhs)) then
                        message = 'closure lexical fact has no lexical export: '// &
                            trim(result%records(i)%lhs)
                        return
                    end if
                    lexical_closed = lexical_closed + 1
                case (closure_kind_unresolved)
                    message = 'closure contains unresolved reference: '// &
                        trim(result%records(i)%lhs)
                    return
                case (closure_kind_alias, closure_kind_list, closure_kind_scalar, &
                        closure_kind_erratum)
                    call make_derived_rule(result%records(i), one, local_ok, message)
                    if (.not. local_ok) return
                    call append_rule(staged, one(1))
                    if (allocated(one)) deallocate (one)
                case default
                    message = 'closure produced an unsupported derived kind'
                    return
                end select
            end if
        end do
        call sort_rules(staged)
        allocate (rules(size(staged)))
        if (size(staged) > 0) rules = staged
        if (.not. allocated(rules)) then
            message = 'grammar closure lost its staged rule array'
            return
        end if
        ok = .true.
        message = ''
    end subroutine standardir_grammar_close_sx

    recursive subroutine collect_references(node, record, source, ok, message)
        type(sx_node_t), intent(in) :: node
        type(closure_input_record_t), intent(inout) :: record
        type(standardir_source_ref_t), intent(in) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i
        character(len=128) :: label, name

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'grammar expression is not a list'
            return
        end if
        if (node%child_count < 1) then
            message = 'grammar expression is empty'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'grammar expression label is malformed'
            return
        end if
        label = trim(node%children(1)%atom)
        if (label == 'ref') then
            if (node%child_count /= 2) then
                message = 'reference expression has the wrong field count'
                return
            end if
            if (node%children(2)%kind /= sx_atom) then
                message = 'reference name is malformed'
                return
            end if
            name = trim(node%children(2)%atom)
            call closure_add_reference(record, name, source, ok, message)
            return
        end if
        if (label == 'token') then
            if (node%child_count /= 2) then
                message = 'token expression has the wrong field count'
                return
            end if
            if (node%children(2)%kind /= sx_atom) then
                message = 'token value is malformed'
                return
            end if
            ok = .true.
            return
        end if
        if (label == 'optional') then
            if (node%child_count /= 2) then
                message = 'optional expression has the wrong field count'
                return
            end if
            call collect_references(node%children(2), record, source, ok, message)
            return
        end if
        if (label == 'repeat') then
            if (node%child_count /= 4) then
                message = 'repeat expression has the wrong field count'
                return
            end if
            call collect_references(node%children(2), record, source, ok, message)
            return
        end if
        if (label /= 'seq' .and. label /= 'alt') then
            message = 'unsupported grammar expression: '//trim(label)
            return
        end if
        do i = 2, node%child_count
            call collect_references(node%children(i), record, source, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine collect_references

    function find_input(input, input_count, id) result(found)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=*), intent(in) :: id
        integer :: found, i

        found = 0
        do i = 1, input_count
            if (trim(input(i)%id) == trim(id)) then
                found = i
                return
            end if
        end do
    end function find_input

    recursive logical function contains_semantic_reference(node, classifications, &
            classification_count) result(found)
        type(sx_node_t), intent(in) :: node
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        integer :: i, fact
        character(len=128) :: label, name

        found = .false.
        if (node%kind /= sx_list) return
        if (node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        label = trim(node%children(1)%atom)
        if (label == 'ref') then
            if (node%child_count /= 2) return
            if (node%children(2)%kind /= sx_atom) return
            name = trim(node%children(2)%atom)
            fact = find_classification(classifications, classification_count, name)
            if (fact > 0) then
                if (classifications(fact)%kind == closure_kind_semantic_only) then
                    found = .true.
                    return
                end if
            end if
        end if
        do i = 2, node%child_count
            if (contains_semantic_reference(node%children(i), classifications, &
                classification_count)) then
                found = .true.
                return
            end if
        end do
    end function contains_semantic_reference

    function find_classification(values, value_count, name) result(found)
        type(closure_classification_t), intent(in) :: values(:)
        integer, intent(in) :: value_count
        character(len=*), intent(in) :: name
        integer :: found, i

        found = 0
        do i = 1, value_count
            if (trim(values(i)%name) == trim(name)) then
                found = i
                return
            end if
        end do
    end function find_classification

    logical function lexical_contains(facts, source_term)
        type(standardir_lexical_facts_t), intent(in) :: facts
        character(len=*), intent(in) :: source_term
        integer :: i

        lexical_contains = .false.
        do i = 1, facts%count
            if (trim(facts%facts(i)%source_term) == trim(source_term)) then
                lexical_contains = .true.
                return
            end if
        end do
    end function lexical_contains

    subroutine make_derived_rule(record, value, ok, message)
        type(closure_record_t), intent(in) :: record
        type(standardir_grammar_rule_t), allocatable, intent(out) :: value(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_rule_t) :: rule

        allocate (value(1))
        rule = standardir_grammar_rule_t()
        rule%id = 'derived-'//trim(record%lhs)
        rule%alternative = 1
        rule%lhs = trim(record%lhs)
        rule%source = record%provenance
        rule%origin = standardir_grammar_origin_mechanical
        rule%resolution = standardir_grammar_resolution_resolved
        select case (record%kind)
        case (closure_kind_alias, closure_kind_scalar, closure_kind_erratum)
            call make_alias_nodes(rule, record%target, ok, message)
        case (closure_kind_list)
            call make_list_nodes(rule, record%target, record%separator, ok, message)
        case default
            ok = .false.
            message = 'unsupported derived rule kind'
        end select
        if (.not. ok) then
            deallocate (value)
            return
        end if
        call standardir_grammar_validate(rule, ok, message)
        if (.not. ok) then
            deallocate (value)
            return
        end if
        value(1) = rule
    end subroutine make_derived_rule

    subroutine make_alias_nodes(rule, target, ok, message)
        type(standardir_grammar_rule_t), intent(inout) :: rule
        character(len=*), intent(in) :: target
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        allocate (rule%nodes%values(2))
        rule%root = 1
        rule%nodes%values = standardir_grammar_node_t()
        rule%nodes%values(1)%kind = standardir_grammar_sequence
        rule%nodes%values(1)%name = '-'
        rule%nodes%values(1)%first_child = 2
        rule%nodes%values(1)%child_count = 1
        rule%nodes%values(2)%kind = standardir_grammar_reference
        rule%nodes%values(2)%name = trim(target)
        ok = len_trim(target) > 0
        message = ''
        if (.not. ok) message = 'derived alias has an empty target'
    end subroutine make_alias_nodes

    subroutine make_list_nodes(rule, target, separator, ok, message)
        type(standardir_grammar_rule_t), intent(inout) :: rule
        character(len=*), intent(in) :: target, separator
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        allocate (rule%nodes%values(6))
        rule%root = 1
        rule%nodes%values = standardir_grammar_node_t()
        rule%nodes%values(1)%kind = standardir_grammar_sequence
        rule%nodes%values(1)%name = '-'
        rule%nodes%values(1)%first_child = 2
        rule%nodes%values(1)%child_count = 2
        rule%nodes%values(2)%kind = standardir_grammar_reference
        rule%nodes%values(2)%name = trim(target)
        rule%nodes%values(3)%kind = standardir_grammar_repeat
        rule%nodes%values(3)%name = '-'
        rule%nodes%values(3)%first_child = 4
        rule%nodes%values(3)%child_count = 1
        rule%nodes%values(3)%minimum = 0
        rule%nodes%values(3)%unbounded = .true.
        rule%nodes%values(4)%kind = standardir_grammar_sequence
        rule%nodes%values(4)%name = '-'
        rule%nodes%values(4)%first_child = 5
        rule%nodes%values(4)%child_count = 2
        rule%nodes%values(5)%kind = standardir_grammar_token
        rule%nodes%values(5)%name = trim(separator)
        rule%nodes%values(6)%kind = standardir_grammar_reference
        rule%nodes%values(6)%name = trim(target)
        ok = len_trim(target) > 0
        if (len_trim(separator) == 0) ok = .false.
        message = ''
        if (.not. ok) message = 'derived list has an empty target or separator'
    end subroutine make_list_nodes

    subroutine append_rule(values, value)
        type(standardir_grammar_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_grammar_rule_t), intent(in) :: value
        type(standardir_grammar_rule_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_rule

    subroutine sort_rules(values)
        type(standardir_grammar_rule_t), intent(inout) :: values(:)
        type(standardir_grammar_rule_t) :: current
        integer :: i, j

        do i = 2, size(values)
            current = values(i)
            j = i - 1
            do while (j >= 1)
                if (trim(values(j)%lhs) <= trim(current%lhs)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = current
        end do
    end subroutine sort_rules

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

end module standardir_grammar_closure

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

    integer, parameter, public :: standardir_grammar_disposition_selected = 1
    integer, parameter, public :: standardir_grammar_disposition_omitted_root = 2
    integer, parameter, public :: standardir_grammar_disposition_omitted_helper = 3

    type, public :: standardir_grammar_disposition_t
        character(len=128) :: name = ''
        integer :: disposition = 0
        character(len=128) :: reason = ''
        type(standardir_source_ref_t) :: source
        integer :: origin = standardir_grammar_origin_mechanical
    end type standardir_grammar_disposition_t

    public :: standardir_grammar_close_sx
    public :: standardir_grammar_close_selected_sx

contains

    subroutine standardir_grammar_close_sx(nodes, node_count, classifications, &
            classification_count, roots, root_count, lexical, rules, semantic_skipped, &
            lexical_closed, ok, message, semantic_skipped_names, semantic_skipped_details)
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
        character(len=128), allocatable, intent(out), optional :: semantic_skipped_names(:)
        character(len=512), allocatable, intent(out), optional :: semantic_skipped_details(:)

        type(standardir_grammar_disposition_t), allocatable :: dispositions(:)

        call standardir_grammar_close_sx_impl(nodes, node_count, classifications, classification_count, &
            roots, root_count, roots, root_count, lexical, rules, semantic_skipped, lexical_closed, &
            dispositions, .false., ok, message, semantic_skipped_names, semantic_skipped_details)
    end subroutine standardir_grammar_close_sx

    subroutine standardir_grammar_close_selected_sx(nodes, node_count, classifications, &
            classification_count, declared_roots, declared_root_count, selected_root, lexical, &
            rules, semantic_skipped, lexical_closed, dispositions, ok, message, &
            semantic_skipped_names, semantic_skipped_details)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: node_count
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: declared_roots(:)
        integer, intent(in) :: declared_root_count
        character(len=*), intent(in) :: selected_root
        type(standardir_lexical_facts_t), intent(in) :: lexical
        type(standardir_grammar_rule_t), allocatable, intent(inout) :: rules(:)
        integer, intent(out) :: semantic_skipped, lexical_closed
        type(standardir_grammar_disposition_t), allocatable, intent(out) :: dispositions(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128), allocatable, intent(out), optional :: semantic_skipped_names(:)
        character(len=512), allocatable, intent(out), optional :: semantic_skipped_details(:)

        character(len=128) :: selection_roots(1)

        selection_roots(1) = trim(selected_root)
        call standardir_grammar_close_sx_impl(nodes, node_count, classifications, classification_count, &
            declared_roots, declared_root_count, selection_roots, 1, lexical, rules, &
            semantic_skipped, lexical_closed, dispositions, .true., ok, message, &
            semantic_skipped_names, semantic_skipped_details)
    end subroutine standardir_grammar_close_selected_sx

    subroutine standardir_grammar_close_sx_impl(nodes, node_count, classifications, &
            classification_count, declared_roots, declared_root_count, selection_roots, &
            selection_root_count, lexical, rules, semantic_skipped, lexical_closed, dispositions, &
            selected_mode, ok, message, semantic_skipped_names, semantic_skipped_details)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: node_count
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: declared_roots(:)
        integer, intent(in) :: declared_root_count
        character(len=*), intent(in) :: selection_roots(:)
        integer, intent(in) :: selection_root_count
        type(standardir_lexical_facts_t), intent(in) :: lexical
        type(standardir_grammar_rule_t), allocatable, intent(inout) :: rules(:)
        integer, intent(out) :: semantic_skipped, lexical_closed
        type(standardir_grammar_disposition_t), allocatable, intent(out) :: dispositions(:)
        logical, intent(in) :: selected_mode
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128), allocatable, intent(out), optional :: semantic_skipped_names(:)
        character(len=512), allocatable, intent(out), optional :: semantic_skipped_details(:)

        type(closure_input_record_t), allocatable :: input(:)
        type(closure_classification_t), allocatable :: effective_classifications(:)
        type(closure_result_t) :: result
        type(standardir_grammar_rule_t), allocatable :: staged(:), one(:)
        type(standardir_source_ref_t) :: source
        type(sx_node_t), allocatable :: lexicalized_nodes(:)
        type(sx_node_t) :: expression
        character(len=128) :: rule, lhs, id
        character(len=128) :: semantic_name
        integer :: i, j, input_index, count, effective_count
        logical :: local_ok, skip, semantic_found

        if (allocated(rules)) deallocate (rules)
        semantic_skipped = 0
        lexical_closed = 0
        if (present(semantic_skipped_names)) then
            allocate (semantic_skipped_names(0))
        end if
        if (present(semantic_skipped_details)) then
            allocate (semantic_skipped_details(0))
        end if
        allocate (dispositions(0))
        ok = .false.
        message = ''
        if (node_count < 1 .or. node_count > size(nodes)) then
            message = 'grammar closure has no source records'
            return
        end if
        if (declared_root_count < 1 .or. declared_root_count > size(declared_roots)) then
            message = 'selected grammar export has no declared roots'
            return
        end if
        call standardir_lexical_validate(lexical, ok, message)
        if (.not. ok) return

        call make_effective_classifications(classifications, classification_count, lexical, &
            effective_classifications, effective_count, ok, message)
        if (.not. ok) return
        allocate (input(node_count))
        allocate (lexicalized_nodes(node_count))
        do i = 1, node_count
            call lexicalize_tokens(nodes(i), lexical, lexicalized_nodes(i), ok, message)
            if (.not. ok) then
                message = 'source record '//integer_text(i)//': '//trim(message)
                return
            end if
            call read_syntax(lexicalized_nodes(i), rule, lhs, expression, source, ok, message)
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

        call closure_compute(input, node_count, effective_classifications, effective_count, &
            selection_roots, selection_root_count, result, ok, message)
        if (.not. ok) return

        if (selected_mode) then
            call make_selected_dispositions(input, node_count, declared_roots, declared_root_count, &
                selection_roots(1), effective_classifications, effective_count, result, dispositions)
        end if

        ok = .false.
        allocate (staged(0))
        do i = 1, result%record_count
            if (.not. result%records(i)%derived) then
                input_index = find_input(input, node_count, result%records(i)%id)
                if (input_index == 0) then
                    message = 'closure result lost normative occurrence identity'
                    return
                end if
                skip = contains_semantic_reference(lexicalized_nodes(input_index), &
                    effective_classifications, effective_count)
                if (skip) then
                    semantic_skipped = semantic_skipped + 1
                    if (present(semantic_skipped_names)) then
                        call append_skipped_name(semantic_skipped_names, input(input_index)%lhs)
                    end if
                    if (present(semantic_skipped_details)) then
                        semantic_found = find_semantic_reference_name(lexicalized_nodes(input_index), &
                            effective_classifications, effective_count, semantic_name)
                        if (.not. semantic_found) semantic_name = ''
                        call append_skipped_detail(semantic_skipped_details, input(input_index)%lhs, &
                            semantic_name, input(input_index)%source)
                    end if
                    cycle
                end if
                call standardir_grammar_adapt_sx(lexicalized_nodes(input_index), &
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
                    if (present(semantic_skipped_names)) then
                        call append_skipped_name(semantic_skipped_names, result%records(i)%lhs)
                    end if
                    if (present(semantic_skipped_details)) then
                        call append_skipped_detail(semantic_skipped_details, result%records(i)%lhs, '', &
                            result%records(i)%source)
                    end if
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
    end subroutine standardir_grammar_close_sx_impl

    subroutine make_selected_dispositions(input, input_count, declared_roots, declared_root_count, &
            selected_root, classifications, classification_count, result, dispositions)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=*), intent(in) :: declared_roots(:)
        integer, intent(in) :: declared_root_count
        character(len=*), intent(in) :: selected_root
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        type(closure_result_t), intent(in) :: result
        type(standardir_grammar_disposition_t), allocatable, intent(out) :: dispositions(:)

        integer :: i
        logical :: declared, reachable
        type(standardir_grammar_disposition_t) :: disposition

        allocate (dispositions(0))
        do i = 1, input_count
            if (disposition_exists(dispositions, trim(input(i)%lhs))) cycle
            declared = root_exists(declared_roots, declared_root_count, input(i)%lhs)
            reachable = result_name_exists(result, input(i)%lhs)
            disposition = standardir_grammar_disposition_t()
            disposition%name = trim(input(i)%lhs)
            disposition%source = input(i)%source
            disposition%origin = standardir_grammar_origin_mechanical
            if (reachable) then
                disposition%disposition = standardir_grammar_disposition_selected
                disposition%reason = 'reachable from selected root'
            else if (declared) then
                disposition%disposition = standardir_grammar_disposition_omitted_root
                disposition%reason = 'not reachable from selected root'
            else
                disposition%disposition = standardir_grammar_disposition_omitted_helper
                disposition%reason = 'not reachable from selected root'
            end if
            call append_disposition(dispositions, disposition)
        end do
        do i = 1, result%record_count
            if (.not. result%records(i)%derived) cycle
            if (result%records(i)%kind == closure_kind_semantic_only .or. &
                result%records(i)%kind == closure_kind_lexical) cycle
            if (disposition_exists(dispositions, trim(result%records(i)%lhs))) cycle
            disposition = standardir_grammar_disposition_t()
            disposition%name = trim(result%records(i)%lhs)
            disposition%disposition = standardir_grammar_disposition_selected
            disposition%reason = 'reachable from selected root'
            disposition%source = result%records(i)%provenance
            disposition%origin = standardir_grammar_origin_mechanical
            call append_disposition(dispositions, disposition)
        end do
        do i = 1, declared_root_count
            if (disposition_exists(dispositions, trim(declared_roots(i)))) cycle
            disposition = standardir_grammar_disposition_t()
            disposition%name = trim(declared_roots(i))
            disposition%source = disposition_source(classifications, classification_count, &
                declared_roots(i))
            disposition%origin = standardir_grammar_origin_mechanical
            if (trim(declared_roots(i)) == trim(selected_root)) then
                disposition%disposition = standardir_grammar_disposition_selected
                disposition%reason = 'selected root has no source record'
            else
                disposition%disposition = standardir_grammar_disposition_omitted_root
                disposition%reason = 'not reachable from selected root'
            end if
            call append_disposition(dispositions, disposition)
        end do
    end subroutine make_selected_dispositions

    function disposition_source(classifications, classification_count, name) result(source)
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: name
        type(standardir_source_ref_t) :: source
        integer :: i

        source = standardir_source_ref_t()
        do i = 1, classification_count
            if (trim(classifications(i)%name) == trim(name)) then
                source = classifications(i)%source
                return
            end if
        end do
    end function disposition_source

    logical function root_exists(roots, root_count, name)
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        character(len=*), intent(in) :: name
        integer :: i

        root_exists = .false.
        do i = 1, root_count
            if (trim(roots(i)) == trim(name)) then
                root_exists = .true.
                return
            end if
        end do
    end function root_exists

    logical function result_name_exists(result, name)
        type(closure_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        result_name_exists = .false.
        do i = 1, result%record_count
            if (trim(result%records(i)%lhs) == trim(name)) then
                result_name_exists = .true.
                return
            end if
        end do
    end function result_name_exists

    logical function disposition_exists(dispositions, name)
        type(standardir_grammar_disposition_t), intent(in) :: dispositions(:)
        character(len=*), intent(in) :: name
        integer :: i

        disposition_exists = .false.
        do i = 1, size(dispositions)
            if (trim(dispositions(i)%name) == trim(name)) then
                disposition_exists = .true.
                return
            end if
        end do
    end function disposition_exists

    subroutine append_disposition(values, value)
        type(standardir_grammar_disposition_t), allocatable, intent(inout) :: values(:)
        type(standardir_grammar_disposition_t), intent(in) :: value
        type(standardir_grammar_disposition_t), allocatable :: expanded(:)
        integer :: count

        count = size(values)
        allocate (expanded(count + 1))
        if (count > 0) expanded(:count) = values
        expanded(count + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_disposition

    subroutine make_effective_classifications(classifications, classification_count, lexical, &
            values, value_count, ok, message)
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        type(standardir_lexical_facts_t), intent(in) :: lexical
        type(closure_classification_t), allocatable, intent(out) :: values(:)
        integer, intent(out) :: value_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_classification_t) :: candidate
        integer :: i, ios, page
        character(len=128) :: page_text

        ok = .false.
        message = ''
        value_count = classification_count
        allocate (values(max(1, classification_count + lexical%count)))
        values = closure_classification_t()
        if (classification_count > 0) values(:classification_count) = classifications(:classification_count)
        do i = 1, lexical%count
            if (find_classification(values, value_count, lexical%facts(i)%source_term) > 0) cycle
            candidate = closure_classification_t()
            candidate%name = trim(lexical%facts(i)%source_term)
            candidate%kind = closure_kind_lexical
            candidate%target = trim(lexical%facts(i)%target_name)
            candidate%source%document = trim(lexical%facts(i)%document)
            candidate%source%clause = trim(lexical%facts(i)%clause)
            candidate%source%rule = trim(lexical%facts(i)%source_rule)
            candidate%source%source_hash = trim(lexical%facts(i)%source_hash)
            page_text = trim(lexical%facts(i)%source_page)
            read (page_text, *, iostat=ios) page
            if (ios /= 0) then
                message = 'lexical fact source page is not an integer: '// &
                    trim(lexical%facts(i)%source_page)
                return
            end if
            if (page <= 0) then
                message = 'lexical fact source page is not positive'
                return
            end if
            candidate%source%page = page
            value_count = value_count + 1
            values(value_count) = candidate
        end do
        ok = .true.
    end subroutine make_effective_classifications

    recursive subroutine lexicalize_tokens(node, lexical, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_lexical_facts_t), intent(in) :: lexical
        type(sx_node_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i
        logical :: is_token, is_atom

        value = node
        ok = .false.
        message = ''
        if (node%kind == sx_atom) then
            ok = .true.
            return
        end if
        if (node%kind /= sx_list) then
            message = 'source SX contains an invalid node kind'
            return
        end if
        do i = 1, node%child_count
            call lexicalize_tokens(node%children(i), lexical, value%children(i), ok, message)
            if (.not. ok) return
        end do
        is_token = .false.
        if (node%child_count >= 1) then
            if (node%children(1)%kind == sx_atom) then
                is_token = trim(node%children(1)%atom) == 'token'
            end if
        end if
        is_atom = .false.
        if (node%child_count >= 2) then
            is_atom = node%children(2)%kind == sx_atom
        end if
        if (is_token .and. is_atom) then
            if (lexical_contains(lexical, node%children(2)%atom)) then
                value%children(1)%atom = 'ref'
            end if
        end if
        ok = .true.
    end subroutine lexicalize_tokens

    subroutine append_skipped_name(names, name)
        character(len=128), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: name
        character(len=128), allocatable :: expanded(:)
        integer :: i, count

        do i = 1, size(names)
            if (trim(names(i)) == trim(name)) return
        end do
        count = size(names)
        allocate (expanded(count + 1))
        if (count > 0) expanded(:count) = names
        expanded(count + 1) = trim(name)
        call move_alloc(expanded, names)
    end subroutine append_skipped_name

    subroutine append_skipped_detail(details, name, dependency, source)
        character(len=512), allocatable, intent(inout) :: details(:)
        character(len=*), intent(in) :: name, dependency
        type(standardir_source_ref_t), intent(in) :: source
        character(len=512), allocatable :: expanded(:)
        character(len=512) :: value
        integer :: i, count

        write (value, '(a," source-rule=",a," page=",i0," byte-start=",i0," byte-length=",i0)') &
            trim(name), trim(source%rule), source%page, source%byte_start, source%byte_length
        if (len_trim(dependency) > 0) value = trim(value)//' dependency='//trim(dependency)
        do i = 1, size(details)
            if (trim(details(i)) == trim(value)) return
        end do
        count = size(details)
        allocate (expanded(count + 1))
        if (count > 0) expanded(:count) = details
        expanded(count + 1) = trim(value)
        call move_alloc(expanded, details)
    end subroutine append_skipped_detail

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

    recursive logical function find_semantic_reference_name(node, classifications, &
            classification_count, name) result(found)
        type(sx_node_t), intent(in) :: node
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(out) :: name
        integer :: i, fact

        found = .false.
        name = ''
        if (node%kind /= sx_list .or. node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        if (trim(node%children(1)%atom) == 'ref' .and. node%child_count == 2 .and. &
            node%children(2)%kind == sx_atom) then
            fact = find_classification(classifications, classification_count, node%children(2)%atom)
            if (fact > 0 .and. classifications(fact)%kind == closure_kind_semantic_only) then
                name = trim(node%children(2)%atom)
                found = .true.
                return
            end if
        end if
        do i = 2, node%child_count
            if (find_semantic_reference_name(node%children(i), classifications, classification_count, name)) then
                found = .true.
                return
            end if
        end do
    end function find_semantic_reference_name

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
        rule%source_expression_present = .false.
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

program test_standardir_reference_closure
    !! Independent expected-set and failure controls for source-backed closure.

    use standardir_export, only: standardir_source_ref_t
    use standardir_reference_closure
    implicit none

    type(closure_input_record_t) :: records(4)
    type(closure_classification_t) :: facts(8)
    type(closure_result_t) :: result
    character(len=32) :: roots(1)
    character(len=256) :: message
    type(standardir_source_ref_t) :: source
    logical :: ok
    integer :: i

    call make_source(source, 'fixture', 'closure', 'N1', 10)
    call make_record(records(1), 'N1', 'root', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(1), 'widget-name', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(1), 'widget-list', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(1), 'scalar-widget', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(1), 'WORD', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(1), 'semantic-hole', source, ok, message)
    call require(ok, message)

    call make_record(records(2), 'N2', 'widget', source, ok, message)
    call require(ok, message)
    call closure_add_reference(records(2), 'WORD', source, ok, message)
    call require(ok, message)
    call make_record(records(3), 'N3', 'outside', source, ok, message)
    call require(ok, message)
    call make_record(records(4), 'N4', 'unused', source, ok, message)
    call require(ok, message)

    call make_alias(facts(1), 'widget-name', '-name', 'alias-family', source)
    call make_list(facts(2), 'widget-list', '-list', ',', 'list-family', source)
    call make_scalar(facts(3), 'scalar-widget', 'scalar-', 'scalar-family', source)
    call make_lexical(facts(4), 'WORD', 'word', 'lexical-family', source)
    call make_state(facts(5), 'semantic-hole', closure_kind_semantic_only, source)
    roots(1) = 'root'

    call closure_compute(records, 4, facts, 5, roots, 1, result, ok, message)
    call require(ok, message)
    call require(result%normative_count == 2, 'closure selected the wrong normative records')
    call require(result%derived_count == 5, 'closure selected the wrong derived records')
    call require(result%record_count == 7, 'closure record count differs')
    call require(trim(result%records(1)%lhs) == 'root' .and. &
        trim(result%records(2)%lhs) == 'widget', 'normative order differs')
    call require(.not. result%records(1)%derived .and. .not. result%records(2)%derived, &
        'normative records were marked derived')
    call require(result%records(3)%derived .and. trim(result%records(3)%lhs) == 'widget-name' .and. &
        result%records(3)%kind == closure_kind_alias .and. &
        trim(result%records(3)%target) == 'widget', 'generic suffix alias closure differs')
    call require(result%records(4)%derived .and. trim(result%records(4)%lhs) == 'widget-list' .and. &
        result%records(4)%kind == closure_kind_list .and. &
        trim(result%records(4)%target) == 'widget' .and. &
        trim(result%records(4)%separator) == ',', 'generic list closure differs')
    call require(result%records(5)%derived .and. trim(result%records(5)%lhs) == 'scalar-widget' .and. &
        result%records(5)%kind == closure_kind_scalar .and. &
        trim(result%records(5)%target) == 'widget', 'generic prefix scalar closure differs')
    call require(result%records(6)%kind == closure_kind_lexical .and. &
        trim(result%records(6)%terminal) == 'word', 'lexical closure differs')
    call require(result%records(7)%kind == closure_kind_semantic_only, &
        'semantic-only state was not retained')
    call require(trim(result%records(3)%source%rule) == 'N1' .and. &
        trim(result%records(3)%provenance%rule) == 'N1', &
        'derived source and classification provenance were not preserved')
    call require(trim(result%records(1)%source%rule) == 'N1' .and. &
        trim(result%records(1)%provenance%rule) == 'N1', &
        'normative provenance was not preserved')
    call closure_validate_result(result, ok, message)
    call require(ok, message)

    call missing_provenance_control(records, facts, roots, result)
    call conflicting_classification_control(records, facts, roots, result)
    call cycle_control(records, facts, roots, result)
    call unclassified_control(records, facts, roots, result)
    call large_dynamic_capacity_control
    print '(a)', 'StandardIR reference closure test passed'

contains

    subroutine make_source(value, document, clause, rule, page)
        type(standardir_source_ref_t), intent(out) :: value
        character(len=*), intent(in) :: document, clause, rule
        integer, intent(in) :: page

        value%document = document
        value%clause = clause
        value%rule = rule
        value%page = page
        value%source_hash = 'fixture-hash'
    end subroutine make_source

    subroutine make_record(value, id, lhs, value_source, ok, text)
        type(closure_input_record_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs
        type(standardir_source_ref_t), intent(in) :: value_source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: text

        value = closure_input_record_t()
        value%id = id
        value%lhs = lhs
        value%source = value_source
        ok = .true.
        text = ''
    end subroutine make_record

    subroutine make_alias(value, name, suffix, family, value_source)
        type(closure_classification_t), intent(out) :: value
        character(len=*), intent(in) :: name, suffix, family
        type(standardir_source_ref_t), intent(in) :: value_source

        value = closure_classification_t()
        value%name = name
        value%kind = closure_kind_alias
        value%suffix = suffix
        value%family = family
        value%source = value_source
    end subroutine make_alias

    subroutine make_list(value, name, suffix, separator, family, value_source)
        type(closure_classification_t), intent(out) :: value
        character(len=*), intent(in) :: name, suffix, separator, family
        type(standardir_source_ref_t), intent(in) :: value_source

        value = closure_classification_t()
        value%name = name
        value%kind = closure_kind_list
        value%suffix = suffix
        value%separator = separator
        value%family = family
        value%source = value_source
    end subroutine make_list

    subroutine make_scalar(value, name, prefix, family, value_source)
        type(closure_classification_t), intent(out) :: value
        character(len=*), intent(in) :: name, prefix, family
        type(standardir_source_ref_t), intent(in) :: value_source

        value = closure_classification_t()
        value%name = name
        value%kind = closure_kind_scalar
        value%prefix = prefix
        value%family = family
        value%source = value_source
    end subroutine make_scalar

    subroutine make_lexical(value, name, terminal, family, value_source)
        type(closure_classification_t), intent(out) :: value
        character(len=*), intent(in) :: name, terminal, family
        type(standardir_source_ref_t), intent(in) :: value_source

        value = closure_classification_t()
        value%name = name
        value%kind = closure_kind_lexical
        value%terminal = terminal
        value%family = family
        value%source = value_source
    end subroutine make_lexical

    subroutine make_state(value, name, kind, value_source)
        type(closure_classification_t), intent(out) :: value
        character(len=*), intent(in) :: name
        integer, intent(in) :: kind
        type(standardir_source_ref_t), intent(in) :: value_source

        value = closure_classification_t()
        value%name = name
        value%kind = kind
        value%source = value_source
    end subroutine make_state

    subroutine missing_provenance_control(input, classifications, root_names, output)
        type(closure_input_record_t), intent(in) :: input(:)
        type(closure_classification_t), intent(in) :: classifications(:)
        character(len=*), intent(in) :: root_names(:)
        type(closure_result_t), intent(out) :: output
        type(closure_input_record_t) :: mutated(size(input))
        logical :: local_ok
        character(len=256) :: local_message

        mutated = input
        mutated(1)%source%page = 0
        call closure_compute(mutated, 4, classifications, 5, root_names, 1, output, local_ok, &
            local_message)
        call require(.not. local_ok .and. output%record_count == 0, &
            'missing source provenance was accepted')
    end subroutine missing_provenance_control

    subroutine conflicting_classification_control(input, classifications, root_names, output)
        type(closure_input_record_t), intent(in) :: input(:)
        type(closure_classification_t), intent(in) :: classifications(:)
        character(len=*), intent(in) :: root_names(:)
        type(closure_result_t), intent(out) :: output
        type(closure_classification_t) :: mutated(6)
        logical :: local_ok
        character(len=256) :: local_message

        mutated(1:5) = classifications
        mutated(6) = classifications(1)
        mutated(6)%target = 'other'
        call closure_compute(input, 4, mutated, 6, root_names, 1, output, local_ok, local_message)
        call require(.not. local_ok .and. output%record_count == 0, &
            'conflicting classifications were accepted: '//trim(local_message))
    end subroutine conflicting_classification_control

    subroutine cycle_control(input, classifications, root_names, output)
        type(closure_input_record_t), intent(in) :: input(:)
        type(closure_classification_t), intent(in) :: classifications(:)
        character(len=*), intent(in) :: root_names(:)
        type(closure_result_t), intent(out) :: output
        type(closure_input_record_t) :: mutated(size(input))
        type(closure_classification_t) :: mutated_facts(7)
        type(standardir_source_ref_t) :: value_source
        logical :: local_ok
        character(len=256) :: local_message

        mutated = input
        call make_source(value_source, 'fixture', 'closure', 'C1', 11)
        call closure_add_reference(mutated(2), 'cycle-a', value_source, local_ok, local_message)
        call require(local_ok, local_message)
        mutated_facts(1:5) = classifications
        mutated_facts(6) = closure_classification_t()
        mutated_facts(6)%name = 'cycle-a'
        mutated_facts(6)%kind = closure_kind_alias
        mutated_facts(6)%target = 'cycle-b'
        mutated_facts(6)%source = value_source
        mutated_facts(7) = mutated_facts(6)
        mutated_facts(7)%name = 'cycle-b'
        mutated_facts(7)%target = 'cycle-a'
        call closure_compute(mutated, 4, mutated_facts, 7, root_names, 1, output, local_ok, &
            local_message)
        call require(.not. local_ok .and. output%record_count == 0, &
            'classification cycle was accepted')
    end subroutine cycle_control

    subroutine unclassified_control(input, classifications, root_names, output)
        type(closure_input_record_t), intent(in) :: input(:)
        type(closure_classification_t), intent(in) :: classifications(:)
        character(len=*), intent(in) :: root_names(:)
        type(closure_result_t), intent(out) :: output
        type(closure_input_record_t) :: mutated(size(input))
        type(standardir_source_ref_t) :: value_source
        logical :: local_ok
        character(len=256) :: local_message

        mutated = input
        call make_source(value_source, 'fixture', 'closure', 'U1', 12)
        call closure_add_reference(mutated(2), 'not-classified', value_source, local_ok, &
            local_message)
        call require(local_ok, local_message)
        call closure_compute(mutated, 4, classifications, 5, root_names, 1, output, local_ok, &
            local_message)
        call require(.not. local_ok .and. output%record_count == 0, &
            'unclassified reference was accepted')
    end subroutine unclassified_control

    subroutine large_dynamic_capacity_control
        integer, parameter :: large_count = closure_max_records + 1
        type(closure_input_record_t), allocatable :: large(:)
        type(closure_classification_t) :: no_facts(1)
        type(closure_result_t) :: output
        type(standardir_source_ref_t) :: value_source
        character(len=32) :: current_name, next_name, last_name
        character(len=32) :: large_roots(1)
        character(len=256) :: local_message
        logical :: local_ok
        integer :: i

        allocate (large(large_count))
        no_facts = closure_classification_t()
        call make_source(value_source, 'fixture', 'large-closure', 'N-large', 13)
        do i = 1, large_count
            write (current_name, '(a,i0)') 'node-', i
            call make_record(large(i), 'record-'//trim(current_name), trim(current_name), &
                value_source, local_ok, local_message)
            call require(local_ok, local_message)
            if (i < large_count) then
                write (next_name, '(a,i0)') 'node-', i + 1
                call closure_add_reference(large(i), trim(next_name), value_source, local_ok, &
                    local_message)
                call require(local_ok, local_message)
            end if
        end do
        large_roots(1) = 'node-1'
        write (last_name, '(a,i0)') 'node-', large_count
        call closure_compute(large, large_count, no_facts, 0, large_roots, 1, output, local_ok, &
            local_message)
        call require(local_ok, local_message)
        call require(output%normative_count == large_count .and. output%derived_count == 0, &
            'dynamic closure capacity did not retain all normative records')
        call require(output%record_count == large_count .and. &
            trim(output%records(large_count)%lhs) == trim(last_name), &
            'dynamic closure output order or capacity differs')
    end subroutine large_dynamic_capacity_control

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_reference_closure

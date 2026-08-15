module standardir_reference_closure
    !! Deterministic closure of source-backed grammar references.
    !!
    !! The caller supplies the normative productions and a sidecar table of
    !! classifications.  This module does not know any standard vocabulary:
    !! suffixes, prefixes, family labels and replacement names are data.

    use standardir_export, only: standardir_source_ref_t, standardir_validate_source_ref
    use standardir_reference_closure_types, only: closure_max_references, closure_max_records, &
        closure_max_classifications, closure_max_name_length, closure_kind_production, &
        closure_kind_alias, closure_kind_list, closure_kind_scalar, closure_kind_lexical, &
        closure_kind_erratum, closure_kind_semantic_only, closure_kind_unresolved, &
        closure_reference_t, closure_input_record_t, closure_classification_t, closure_record_t, &
        closure_result_t
    use standardir_reference_closure_support, only: closure_append_derived, &
        closure_append_normative, closure_normalise_classifications, closure_reorder_derived, &
        closure_validate_inputs, closure_validate_result, find_fact, find_productions
    implicit none
    private

    public :: closure_max_references, closure_max_records, closure_max_classifications
    public :: closure_max_name_length, closure_kind_production, closure_kind_alias
    public :: closure_kind_list, closure_kind_scalar, closure_kind_lexical
    public :: closure_kind_erratum, closure_kind_semantic_only, closure_kind_unresolved
    public :: closure_reference_t, closure_input_record_t, closure_classification_t
    public :: closure_record_t, closure_result_t

    public :: closure_add_reference
    public :: closure_compute
    public :: closure_validate_result

contains

    subroutine closure_add_reference(record, name, source, ok, message)
        type(closure_input_record_t), intent(inout) :: record
        character(len=*), intent(in) :: name
        type(standardir_source_ref_t), intent(in) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i
        type(closure_reference_t), allocatable :: expanded(:)
        integer :: capacity

        ok = .false.
        message = ''
        if (len_trim(name) == 0) then
            message = 'closure reference name is empty'
            return
        end if
        if (len_trim(name) > closure_max_name_length) then
            message = 'closure reference name is too long'
            return
        end if
        call standardir_validate_source_ref(source, ok, message)
        if (.not. ok) return
        if (record%reference_count < 0) then
            ok = .false.
            message = 'closure record reference count is negative'
            return
        end if
        if (record%reference_count > 0) then
            if (.not. allocated(record%references)) then
                ok = .false.
                message = 'closure record reference storage is missing'
                return
            end if
        end if
        do i = 1, record%reference_count
            if (trim(record%references(i)%name) == trim(name)) then
                ok = .true.
                return
            end if
        end do
        if (.not. allocated(record%references)) then
            allocate (record%references(1))
        else if (record%reference_count >= size(record%references)) then
            capacity = max(1, 2 * size(record%references))
            allocate (expanded(capacity))
            if (record%reference_count > 0) then
                expanded(:record%reference_count) = record%references(:record%reference_count)
            end if
            call move_alloc(expanded, record%references)
        end if
        record%reference_count = record%reference_count + 1
        record%references(record%reference_count)%name = trim(name)
        record%references(record%reference_count)%source = source
        ok = .true.
    end subroutine closure_add_reference

    subroutine closure_compute(input, input_count, classifications, classification_count, &
            roots, root_count, result, ok, message)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        type(closure_result_t), intent(out) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_classification_t), allocatable :: facts(:)
        type(closure_result_t) :: staged
        integer :: fact_count
        logical, allocatable :: selected(:)
        integer, allocatable :: queue(:)
        integer :: head, tail, i, j, count, steps

        result = closure_result_t()
        staged = closure_result_t()
        ok = .false.
        message = ''
        call closure_validate_inputs(input, input_count, classifications, classification_count, &
            roots, root_count, ok, message)
        if (.not. ok) return
        allocate (facts(max(1, classification_count)))
        allocate (selected(input_count), queue(input_count))
        allocate (staged%records(input_count + classification_count))
        facts = closure_classification_t()
        selected = .false.
        queue = 0
        call closure_normalise_classifications(classifications, classification_count, facts, &
            fact_count, &
            ok, message)
        if (.not. ok) return

        head = 1
        tail = 0
        do i = 1, root_count
            call select_productions(input, input_count, trim(roots(i)), selected, queue, tail, &
                count, ok, message)
            if (.not. ok) return
            if (count == 0) then
                ok = .false.
                message = 'closure root is not a normative production: '//trim(roots(i))
                return
            end if
        end do

        steps = 0
        do while (head <= tail)
            steps = steps + 1
            if (steps > input_count * max(1, classification_count + 1)) then
                message = 'reference closure did not converge'
                return
            end if
            i = queue(head)
            head = head + 1
            do j = 1, input(i)%reference_count
                call visit_name(input, input_count, facts, fact_count, &
                    input(i)%references(j)%name, input(i)%references(j)%source, selected, &
                    queue, tail, staged, ok, message)
                if (.not. ok) return
            end do
        end do

        do i = 1, input_count
            if (selected(i)) then
                call closure_append_normative(staged, input(i), ok, message)
                if (.not. ok) return
            end if
        end do

        ! visit_name stages derived records in result.  Move them after the
        ! normative prefix so the output order is independent of queue order.
        call closure_reorder_derived(staged, ok, message)
        if (.not. ok) return
        call closure_validate_result(staged, ok, message)
        if (ok) result = staged
    end subroutine closure_compute

    subroutine visit_name(input, input_count, facts, fact_count, name, occurrence_source, selected, &
            queue, tail, result, ok, message)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count, fact_count
        type(closure_classification_t), intent(in) :: facts(:)
        character(len=*), intent(in) :: name
        type(standardir_source_ref_t), intent(in) :: occurrence_source
        logical, intent(inout) :: selected(:)
        integer, intent(inout) :: queue(:), tail
        type(closure_result_t), intent(inout) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: index, match_count, fact_index
        integer, allocatable :: stack(:)

        ok = .false.
        message = ''
        call find_productions(input, input_count, name, index, match_count)
        fact_index = find_fact(facts, fact_count, name)
        if (match_count > 0 .and. fact_index > 0) then
            if (facts(fact_index)%kind /= closure_kind_production) then
                message = 'reference has both production and non-production classifications: '// &
                    trim(name)
                return
            end if
        end if
        if (match_count > 0) then
            call select_productions(input, input_count, name, selected, queue, tail, match_count, &
                ok, message)
            return
        end if
        if (fact_index == 0) then
            message = 'unclassified closure reference: '//trim(name)
            return
        end if
        allocate (stack(max(1, fact_count)))
        stack = 0
        call visit_fact(input, input_count, facts, fact_count, fact_index, occurrence_source, &
            stack, 0, selected, queue, tail, result, ok, message)
    end subroutine visit_name

    recursive subroutine visit_fact(input, input_count, facts, fact_count, fact_index, &
            occurrence_source, stack, depth, selected, queue, tail, result, ok, message)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count, fact_count, fact_index, depth
        type(closure_classification_t), intent(in) :: facts(:)
        type(standardir_source_ref_t), intent(in) :: occurrence_source
        integer, intent(inout) :: stack(:)
        logical, intent(inout) :: selected(:)
        integer, intent(inout) :: queue(:), tail
        type(closure_result_t), intent(inout) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, index, match_count, next_fact
        character(len=closure_max_name_length) :: target

        ok = .false.
        message = ''
        if (depth >= size(stack)) then
            message = 'classification closure did not converge'
            return
        end if
        do i = 1, depth
            if (stack(i) == fact_index) then
                message = 'cyclic closure classification: '//trim(facts(fact_index)%name)
                return
            end if
        end do
        stack(depth + 1) = fact_index
        call closure_append_derived(result, facts(fact_index), occurrence_source, ok, message)
        if (.not. ok) return

        select case (facts(fact_index)%kind)
        case (closure_kind_alias, closure_kind_scalar, closure_kind_erratum)
            target = trim(facts(fact_index)%target)
            call find_productions(input, input_count, target, index, match_count)
            next_fact = find_fact(facts, fact_count, target)
            if (match_count > 0 .and. next_fact > 0 .and. &
                facts(next_fact)%kind /= closure_kind_production) then
                message = 'classification target has conflicting production and fact: '//trim(target)
                return
            end if
            if (match_count > 0) then
                call select_productions(input, input_count, target, selected, queue, tail, &
                    match_count, ok, message)
                return
            end if
            if (next_fact == 0) then
                message = 'unclassified closure target: '//trim(target)
                return
            end if
            call visit_fact(input, input_count, facts, fact_count, next_fact, facts(fact_index)%source, &
                stack, depth + 1, selected, queue, tail, result, ok, message)
        case (closure_kind_list)
            target = trim(facts(fact_index)%target)
            call find_productions(input, input_count, target, index, match_count)
            next_fact = find_fact(facts, fact_count, target)
            if (match_count > 0 .and. next_fact > 0 .and. &
                facts(next_fact)%kind /= closure_kind_production) then
                message = 'list element has conflicting production and fact: '//trim(target)
                return
            end if
            if (match_count > 0) then
                call select_productions(input, input_count, target, selected, queue, tail, &
                    match_count, ok, message)
                return
            end if
            if (next_fact == 0) then
                message = 'unclassified list element: '//trim(target)
                return
            end if
            call visit_fact(input, input_count, facts, fact_count, next_fact, facts(fact_index)%source, &
                stack, depth + 1, selected, queue, tail, result, ok, message)
        case default
            ok = .true.
        end select
    end subroutine visit_fact

    subroutine select_productions(input, input_count, name, selected, queue, tail, count, ok, message)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=*), intent(in) :: name
        logical, intent(inout) :: selected(:)
        integer, intent(inout) :: queue(:), tail
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        count = 0
        ok = .false.
        message = ''
        do i = 1, input_count
            if (trim(input(i)%lhs) == trim(name)) then
                count = count + 1
                if (.not. selected(i)) then
                    if (tail >= size(queue)) then
                        message = 'reference closure queue is full'
                        return
                    end if
                    tail = tail + 1
                    queue(tail) = i
                    selected(i) = .true.
                end if
            end if
        end do
        ok = .true.
    end subroutine select_productions

end module standardir_reference_closure

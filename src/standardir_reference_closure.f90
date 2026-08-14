module standardir_reference_closure
    !! Deterministic closure of source-backed grammar references.
    !!
    !! The caller supplies the normative productions and a sidecar table of
    !! classifications.  This module does not know any standard vocabulary:
    !! suffixes, prefixes, family labels and replacement names are data.

    use standardir_export, only: standardir_source_ref_t, standardir_validate_source_ref
    implicit none
    private

    integer, parameter, public :: closure_max_references = 32
    integer, parameter, public :: closure_max_records = 512
    integer, parameter, public :: closure_max_classifications = 512
    integer, parameter, public :: closure_max_name_length = 128

    integer, parameter, public :: closure_kind_production = 1
    integer, parameter, public :: closure_kind_alias = 2
    integer, parameter, public :: closure_kind_list = 3
    integer, parameter, public :: closure_kind_scalar = 4
    integer, parameter, public :: closure_kind_lexical = 5
    integer, parameter, public :: closure_kind_erratum = 6
    integer, parameter, public :: closure_kind_semantic_only = 7
    integer, parameter, public :: closure_kind_unresolved = 8

    type, public :: closure_reference_t
        character(len=closure_max_name_length) :: name = ''
        type(standardir_source_ref_t) :: source
    end type closure_reference_t

    type, public :: closure_input_record_t
        character(len=closure_max_name_length) :: id = ''
        character(len=closure_max_name_length) :: lhs = ''
        integer :: reference_count = 0
        type(closure_reference_t) :: references(closure_max_references)
        type(standardir_source_ref_t) :: source
    end type closure_input_record_t

    type, public :: closure_classification_t
        character(len=closure_max_name_length) :: name = ''
        integer :: kind = 0
        character(len=closure_max_name_length) :: target = ''
        character(len=closure_max_name_length) :: separator = ''
        character(len=closure_max_name_length) :: terminal = ''
        character(len=closure_max_name_length) :: family = ''
        character(len=closure_max_name_length) :: suffix = ''
        character(len=closure_max_name_length) :: prefix = ''
        type(standardir_source_ref_t) :: source
    end type closure_classification_t

    type, public :: closure_record_t
        character(len=closure_max_name_length) :: id = ''
        character(len=closure_max_name_length) :: lhs = ''
        integer :: reference_count = 0
        type(closure_reference_t), allocatable :: references(:)
        type(standardir_source_ref_t) :: source
        type(standardir_source_ref_t) :: provenance
        integer :: kind = 0
        logical :: derived = .false.
        character(len=closure_max_name_length) :: derived_from = ''
        character(len=closure_max_name_length) :: target = ''
        character(len=closure_max_name_length) :: separator = ''
        character(len=closure_max_name_length) :: terminal = ''
    end type closure_record_t

    type, public :: closure_result_t
        integer :: record_count = 0
        integer :: normative_count = 0
        integer :: derived_count = 0
        type(closure_record_t), allocatable :: records(:)
    end type closure_result_t

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
        if (record%reference_count < 0 .or. record%reference_count >= &
            closure_max_references) then
            ok = .false.
            message = 'closure record reference table is full'
            return
        end if
        do i = 1, record%reference_count
            if (trim(record%references(i)%name) == trim(name)) then
                ok = .true.
                return
            end if
        end do
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

        type(closure_classification_t) :: facts(closure_max_classifications)
        type(closure_result_t) :: staged
        integer :: fact_count
        logical :: selected(closure_max_records)
        integer :: queue(closure_max_records)
        integer :: head, tail, i, j, count, steps

        result = closure_result_t()
        staged = closure_result_t()
        allocate (staged%records(closure_max_records))
        facts = closure_classification_t()
        selected = .false.
        queue = 0
        ok = .false.
        message = ''

        call validate_inputs(input, input_count, classifications, classification_count, &
            roots, root_count, ok, message)
        if (.not. ok) return
        call normalise_classifications(classifications, classification_count, facts, fact_count, &
            ok, message)
        if (.not. ok) return

        head = 1
        tail = 0
        do i = 1, root_count
            call select_productions(input, input_count, trim(roots(i)), selected, queue, tail, &
                count, ok, message)
            if (.not. ok) return
            if (count == 0) then
                message = 'closure root is not a normative production: '//trim(roots(i))
                return
            end if
        end do

        steps = 0
        do while (head <= tail)
            steps = steps + 1
            if (steps > closure_max_records * closure_max_references) then
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
                call append_normative(staged, input(i), ok, message)
                if (.not. ok) return
            end if
        end do

        ! visit_name stages derived records in result.  Move them after the
        ! normative prefix so the output order is independent of queue order.
        call reorder_derived(staged, ok, message)
        if (.not. ok) return
        call closure_validate_result(staged, ok, message)
        if (ok) result = staged
    end subroutine closure_compute

    subroutine validate_inputs(input, input_count, classifications, classification_count, &
            roots, root_count, ok, message)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(closure_classification_t), intent(in) :: classifications(:)
        integer, intent(in) :: classification_count
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j

        ok = .false.
        message = ''
        if (input_count < 1 .or. input_count > size(input) .or. &
            input_count > closure_max_records) then
            message = 'closure input count is outside storage'
            return
        end if
        if (classification_count < 0 .or. classification_count > size(classifications) .or. &
            classification_count > closure_max_classifications) then
            message = 'closure classification count is outside storage'
            return
        end if
        if (root_count < 1 .or. root_count > size(roots)) then
            message = 'closure has no roots'
            return
        end if
        do i = 1, root_count
            if (len_trim(roots(i)) == 0) then
                message = 'closure root is empty'
                return
            end if
        end do
        do i = 1, input_count
            if (len_trim(input(i)%id) == 0 .or. len_trim(input(i)%lhs) == 0) then
                message = 'normative closure record has an empty identity'
                return
            end if
            if (input(i)%reference_count < 0 .or. input(i)%reference_count > &
                closure_max_references) then
                message = 'normative closure record has an invalid reference count'
                return
            end if
            call standardir_validate_source_ref(input(i)%source, ok, message)
            if (.not. ok) then
                message = 'normative closure record lacks source provenance'
                return
            end if
            do j = 1, input(i)%reference_count
                if (len_trim(input(i)%references(j)%name) == 0) then
                    message = 'normative closure record has an empty reference'
                    return
                end if
                call standardir_validate_source_ref(input(i)%references(j)%source, ok, message)
                if (.not. ok) then
                    message = 'closure reference lacks source provenance'
                    return
                end if
            end do
        end do
        do i = 1, classification_count
            call validate_classification(classifications(i), ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine validate_inputs

    subroutine validate_classification(fact, ok, message)
        type(closure_classification_t), intent(in) :: fact
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (len_trim(fact%name) == 0) then
            message = 'closure classification has an empty name'
            return
        end if
        if (fact%kind < closure_kind_production .or. fact%kind > closure_kind_unresolved) then
            message = 'closure classification kind is invalid'
            return
        end if
        call standardir_validate_source_ref(fact%source, ok, message)
        if (.not. ok) then
            message = 'closure classification lacks source provenance'
            return
        end if
        ok = .true.
    end subroutine validate_classification

    subroutine normalise_classifications(input, input_count, output, output_count, ok, message)
        type(closure_classification_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        type(closure_classification_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_classification_t) :: fact
        integer :: i, previous

        output = closure_classification_t()
        output_count = 0
        ok = .false.
        message = ''
        do i = 1, input_count
            fact = input(i)
            call derive_target(fact, ok, message)
            if (.not. ok) return
            previous = find_fact(output, output_count, trim(fact%name))
            if (previous > 0) then
                if (.not. same_classification(output(previous), fact)) then
                    ok = .false.
                    message = 'conflicting classifications for reference: '//trim(fact%name)
                    return
                end if
                cycle
            end if
            if (output_count >= size(output)) then
                ok = .false.
                message = 'closure classification table is full'
                return
            end if
            output_count = output_count + 1
            output(output_count) = fact
        end do
        ok = .true.
    end subroutine normalise_classifications

    subroutine derive_target(fact, ok, message)
        type(closure_classification_t), intent(inout) :: fact
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=closure_max_name_length) :: candidate

        ok = .false.
        message = ''
        select case (fact%kind)
        case (closure_kind_production)
            if (len_trim(fact%target) == 0) fact%target = fact%name
        case (closure_kind_alias, closure_kind_list)
            if (len_trim(fact%target) == 0) then
                if (len_trim(fact%suffix) == 0) then
                    message = 'classification needs a target or generic suffix: '//trim(fact%name)
                    return
                end if
                call strip_suffix(fact%name, fact%suffix, candidate, ok)
                if (.not. ok) then
                    message = 'classification name does not have its declared suffix: '// &
                        trim(fact%name)
                    return
                end if
                fact%target = candidate
            end if
        case (closure_kind_scalar)
            if (len_trim(fact%target) == 0) then
                if (len_trim(fact%prefix) == 0) then
                    message = 'scalar classification needs a target or generic prefix: '// &
                        trim(fact%name)
                    return
                end if
                call strip_prefix(fact%name, fact%prefix, candidate, ok)
                if (.not. ok) then
                    message = 'classification name does not have its declared prefix: '// &
                        trim(fact%name)
                    return
                end if
                fact%target = candidate
            end if
        case (closure_kind_erratum)
            if (len_trim(fact%target) == 0) then
                message = 'fixed erratum has no replacement target: '//trim(fact%name)
                return
            end if
        case (closure_kind_lexical)
            if (len_trim(fact%terminal) == 0) fact%terminal = fact%name
        case (closure_kind_semantic_only, closure_kind_unresolved)
            continue
        end select
        ok = .true.
    end subroutine derive_target

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
        integer :: stack(closure_max_records)

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
        call append_derived(result, facts(fact_index), occurrence_source, ok, message)
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

    subroutine append_normative(result, input, ok, message)
        type(closure_result_t), intent(inout) :: result
        type(closure_input_record_t), intent(in) :: input
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_record_t) :: record

        record = closure_record_t()
        allocate (record%references(closure_max_references))
        record%id = input%id
        record%lhs = input%lhs
        record%reference_count = input%reference_count
        record%references = input%references
        record%source = input%source
        record%provenance = input%source
        record%kind = closure_kind_production
        record%derived = .false.
        call append_record(result, record, ok, message)
        if (ok) result%normative_count = result%normative_count + 1
    end subroutine append_normative

    subroutine append_derived(result, fact, occurrence_source, ok, message)
        type(closure_result_t), intent(inout) :: result
        type(closure_classification_t), intent(in) :: fact
        type(standardir_source_ref_t), intent(in) :: occurrence_source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_record_t) :: record
        integer :: i

        do i = 1, result%record_count
            if (result%records(i)%derived .and. &
                trim(result%records(i)%lhs) == trim(fact%name) .and. &
                result%records(i)%kind == fact%kind) then
                ok = .true.
                message = ''
                return
            end if
        end do
        record = closure_record_t()
        allocate (record%references(closure_max_references))
        record%id = fact%name
        record%lhs = fact%name
        record%source = occurrence_source
        record%provenance = fact%source
        record%kind = fact%kind
        record%derived = .true.
        record%derived_from = fact%name
        record%target = fact%target
        record%separator = fact%separator
        record%terminal = fact%terminal
        call append_record(result, record, ok, message)
        if (ok) result%derived_count = result%derived_count + 1
    end subroutine append_derived

    subroutine append_record(result, record, ok, message)
        type(closure_result_t), intent(inout) :: result
        type(closure_record_t), intent(in) :: record
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (result%record_count >= size(result%records)) then
            message = 'reference closure result is full'
            return
        end if
        result%record_count = result%record_count + 1
        result%records(result%record_count) = record
        ok = .true.
    end subroutine append_record

    subroutine reorder_derived(result, ok, message)
        type(closure_result_t), intent(inout) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_record_t), allocatable :: normative(:), derived(:)
        integer :: i, normative_count, derived_count, cursor

        allocate (normative(closure_max_records), derived(closure_max_records))
        normative = closure_record_t()
        derived = closure_record_t()
        normative_count = 0
        derived_count = 0
        do i = 1, result%record_count
            if (result%records(i)%derived) then
                derived_count = derived_count + 1
                derived(derived_count) = result%records(i)
            else
                normative_count = normative_count + 1
                normative(normative_count) = result%records(i)
            end if
        end do
        if (normative_count + derived_count > size(result%records)) then
            ok = .false.
            message = 'reference closure result is full after ordering'
            return
        end if
        cursor = 0
        do i = 1, normative_count
            cursor = cursor + 1
            result%records(cursor) = normative(i)
        end do
        do i = 1, derived_count
            cursor = cursor + 1
            result%records(cursor) = derived(i)
        end do
        result%record_count = cursor
        result%derived_count = derived_count
        deallocate (normative, derived)
        ok = .true.
        message = ''
    end subroutine reorder_derived

    subroutine closure_validate_result(result, ok, message)
        type(closure_result_t), intent(in) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, normative, derived

        ok = .false.
        message = ''
        if (.not. allocated(result%records)) then
            message = 'closure result records are not allocated'
            return
        end if
        if (result%record_count < 0 .or. result%record_count > size(result%records)) then
            message = 'closure result count is invalid'
            return
        end if
        normative = 0
        derived = 0
        do i = 1, result%record_count
            if (result%records(i)%derived) then
                derived = derived + 1
                if (len_trim(result%records(i)%derived_from) == 0) then
                    message = 'derived closure record lacks derivation provenance'
                    return
                end if
            else
                normative = normative + 1
            end if
            call standardir_validate_source_ref(result%records(i)%source, ok, message)
            if (.not. ok) then
                message = 'closure result record lacks source provenance'
                return
            end if
            call standardir_validate_source_ref(result%records(i)%provenance, ok, message)
            if (.not. ok) then
                message = 'closure result record lacks derivation provenance'
                return
            end if
            if (result%records(i)%reference_count < 0 .or. &
                result%records(i)%reference_count > closure_max_references) then
                message = 'closure result record has an invalid reference count'
                return
            end if
            if (result%records(i)%reference_count > 0) then
                if (.not. allocated(result%records(i)%references)) then
                    message = 'closure result references are not allocated'
                    return
                end if
            end if
            do j = 1, result%records(i)%reference_count
                call standardir_validate_source_ref(result%records(i)%references(j)%source, ok, &
                    message)
                if (.not. ok) then
                    message = 'closure result reference lacks source provenance'
                    return
                end if
            end do
        end do
        if (normative /= result%normative_count .or. derived /= result%derived_count) then
            message = 'closure result provenance counts are inconsistent'
            return
        end if
        ok = .true.
    end subroutine closure_validate_result

    subroutine find_productions(input, input_count, name, index, match_count)
        type(closure_input_record_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=*), intent(in) :: name
        integer, intent(out) :: index, match_count
        integer :: i

        index = 0
        match_count = 0
        do i = 1, input_count
            if (trim(input(i)%lhs) == trim(name)) then
                match_count = match_count + 1
                if (index == 0) index = i
            end if
        end do
    end subroutine find_productions

    integer function find_fact(facts, fact_count, name)
        type(closure_classification_t), intent(in) :: facts(:)
        integer, intent(in) :: fact_count
        character(len=*), intent(in) :: name
        integer :: i

        find_fact = 0
        do i = 1, fact_count
            if (trim(facts(i)%name) == trim(name)) then
                find_fact = i
                return
            end if
        end do
    end function find_fact

    logical function same_classification(left, right)
        type(closure_classification_t), intent(in) :: left, right

        same_classification = .false.
        if (trim(left%name) /= trim(right%name)) return
        if (left%kind /= right%kind) return
        if (trim(left%target) /= trim(right%target)) return
        if (trim(left%separator) /= trim(right%separator)) return
        if (trim(left%terminal) /= trim(right%terminal)) return
        if (trim(left%family) /= trim(right%family)) return
        if (trim(left%suffix) /= trim(right%suffix)) return
        if (trim(left%prefix) /= trim(right%prefix)) return
        same_classification = .true.
    end function same_classification

    subroutine strip_suffix(name, suffix, result, ok)
        character(len=*), intent(in) :: name, suffix
        character(len=*), intent(out) :: result
        logical, intent(out) :: ok
        integer :: name_length, suffix_length

        result = ''
        name_length = len_trim(name)
        suffix_length = len_trim(suffix)
        ok = .false.
        if (suffix_length <= 0 .or. name_length < suffix_length) return
        if (name(name_length - suffix_length + 1:name_length) /= trim(suffix)) return
        if (name_length == suffix_length) return
        result = name(:name_length - suffix_length)
        ok = len_trim(result) > 0
    end subroutine strip_suffix

    subroutine strip_prefix(name, prefix, result, ok)
        character(len=*), intent(in) :: name, prefix
        character(len=*), intent(out) :: result
        logical, intent(out) :: ok
        integer :: name_length, prefix_length

        result = ''
        name_length = len_trim(name)
        prefix_length = len_trim(prefix)
        ok = .false.
        if (prefix_length <= 0 .or. name_length < prefix_length) return
        if (name(:prefix_length) /= trim(prefix)) return
        if (name_length == prefix_length) return
        result = name(prefix_length + 1:name_length)
        ok = len_trim(result) > 0
    end subroutine strip_prefix

end module standardir_reference_closure

module standardir_reference_closure_support
    !! Source validation, normalization, and result staging for closure.

    use standardir_export, only: standardir_source_ref_t, standardir_validate_source_ref
    use standardir_reference_closure_types
    implicit none
    private

    public :: closure_append_derived, closure_append_normative, closure_normalise_classifications
    public :: closure_reorder_derived, closure_validate_inputs, closure_validate_result
    public :: find_fact, find_productions

contains

    subroutine closure_validate_inputs(input, input_count, classifications, classification_count, &
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
        if (input_count < 1 .or. input_count > size(input)) then
            message = 'closure input count is outside storage'
            return
        end if
        if (classification_count < 0 .or. classification_count > size(classifications)) then
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
    end subroutine closure_validate_inputs

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

    subroutine closure_normalise_classifications(input, input_count, output, output_count, ok, message)
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
    end subroutine closure_normalise_classifications

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

    subroutine closure_append_normative(result, input, ok, message)
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
    end subroutine closure_append_normative

    subroutine closure_append_derived(result, fact, occurrence_source, ok, message)
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
    end subroutine closure_append_derived

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

    subroutine closure_reorder_derived(result, ok, message)
        type(closure_result_t), intent(inout) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_record_t), allocatable :: normative(:), derived(:)
        integer :: i, normative_count, derived_count, cursor

        allocate (normative(max(1, result%record_count)))
        allocate (derived(max(1, result%record_count)))
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
    end subroutine closure_reorder_derived

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

end module standardir_reference_closure_support

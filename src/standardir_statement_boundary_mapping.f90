module standardir_statement_boundary_mapping
    !! Resolve statement-boundary paths against source grammar trees.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_grammar_producer, only: standardir_grammar_choice, standardir_grammar_node_t, &
        standardir_grammar_optional, standardir_grammar_reference, standardir_grammar_repeat, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_statement_boundary, only: standardir_statement_boundary_plan_t
    use standardir_statement_sequence, only: standardir_statement_sequence_candidate_t
    implicit none
    private

    character(len=6), parameter, public :: standardir_boundary_mapped = 'mapped'
    character(len=9), parameter, public :: standardir_boundary_ambiguous = 'ambiguous'
    character(len=11), parameter, public :: standardir_boundary_unsupported = 'unsupported'
    character(len=10), parameter, public :: standardir_boundary_suppressed = 'suppressed'

    type, public :: standardir_statement_boundary_mapping_t
        type(standardir_statement_sequence_candidate_t) :: candidate
        character(len=16) :: disposition = ''
        character(len=256) :: reason = ''
        integer :: source_node_index = 0
        integer :: source_node_kind = 0
        character(len=128) :: source_node_name = ''
        integer :: alternative = 0
        integer, allocatable :: alternatives(:)
    end type standardir_statement_boundary_mapping_t

    public :: standardir_statement_boundary_map

contains

    subroutine standardir_statement_boundary_map(plan, rules, mappings, ok, message)
        type(standardir_statement_boundary_plan_t), intent(in) :: plan
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        type(standardir_statement_boundary_mapping_t), allocatable, intent(out) :: mappings(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (.not. allocated(plan%sites)) then
            message = 'statement boundary mapping requires an allocated plan'
            return
        end if
        allocate (mappings(size(plan%sites)))
        do i = 1, size(plan%sites)
            mappings(i) = standardir_statement_boundary_mapping_t()
            mappings(i)%candidate = plan%sites(i)%candidate
            call map_site(mappings(i), rules)
        end do
        ok = .true.
        message = ''
    end subroutine standardir_statement_boundary_map

    subroutine map_site(value, rules)
        type(standardir_statement_boundary_mapping_t), intent(inout) :: value
        type(standardir_grammar_rule_t), intent(in) :: rules(:)

        integer(int64) :: page, byte_start
        integer, allocatable :: path(:), match_rules(:), match_nodes(:), alternatives(:)
        integer :: i, match_count, identity_count, node_index
        logical :: valid, path_ok, occurrence_match, identity_match
        character(len=256) :: reason, path_reason

        if (trim(value%candidate%status) == 'suppressed') then
            call set_disposition(value, standardir_boundary_suppressed, 'plan site is suppressed')
            return
        end if
        if (trim(value%candidate%status) /= 'candidate') then
            call set_disposition(value, standardir_boundary_unsupported, &
                'plan site has an unsupported status')
            return
        end if

        call validate_candidate(value%candidate, page, byte_start, valid, reason)
        if (.not. valid) then
            call set_disposition(value, standardir_boundary_unsupported, trim(reason))
            return
        end if
        call parse_path(value%candidate%expression_path, path, path_ok, path_reason)
        if (.not. path_ok) then
            call set_disposition(value, standardir_boundary_unsupported, trim(path_reason))
            return
        end if

        allocate (match_rules(0), match_nodes(0), alternatives(0))
        match_count = 0
        identity_count = 0
        do i = 1, size(rules)
            identity_match = same_source_identity(value%candidate, rules(i))
            if (identity_match) identity_count = identity_count + 1
            occurrence_match = same_source_occurrence(value%candidate, rules(i), page, byte_start)
            if (.not. occurrence_match) cycle
            call source_path_index(rules(i), path, node_index, valid, reason)
            if (.not. valid) then
                call set_disposition(value, standardir_boundary_unsupported, trim(reason))
                deallocate (match_rules, match_nodes, alternatives)
                return
            end if
            call append_match(match_rules, match_nodes, alternatives, i, node_index, rules(i)%alternative)
            match_count = match_count + 1
        end do

        if (match_count == 0) then
            if (identity_count == 0) then
                reason = 'source grammar rule and source occurrence were not found'
            else
                reason = 'source occurrence lineage did not match exactly'
            end if
            call set_disposition(value, standardir_boundary_unsupported, trim(reason))
        else if (match_count > 1) then
            value%disposition = standardir_boundary_ambiguous
            value%reason = 'multiple source alternatives match the complete occurrence and path'
            value%alternatives = alternatives
        else
            value%disposition = standardir_boundary_mapped
            value%reason = 'source occurrence and canonical path resolved structurally'
            value%source_node_index = match_nodes(1)
            value%source_node_kind = rules(match_rules(1))%nodes%values(match_nodes(1))%kind
            value%source_node_name = trim(rules(match_rules(1))%nodes%values(match_nodes(1))%name)
            value%alternative = alternatives(1)
            value%alternatives = alternatives
        end if
        deallocate (match_rules, match_nodes, alternatives)
    end subroutine map_site

    subroutine validate_candidate(candidate, page, byte_start, ok, reason)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        integer(int64), intent(out) :: page, byte_start
        logical, intent(out) :: ok
        character(len=*), intent(out) :: reason

        page = 0_int64
        byte_start = 0_int64
        ok = .false.
        reason = ''
        if (len_trim(candidate%source_rule) == 0 .or. len_trim(candidate%source_lhs) == 0) then
            reason = 'candidate lacks source rule or lhs'
            return
        end if
        if (len_trim(candidate%source_document) == 0 .or. len_trim(candidate%source_clause) == 0 .or. &
            len_trim(candidate%source_hash) == 0 .or. len_trim(candidate%source_page) == 0 .or. &
            len_trim(candidate%source_byte_start) == 0) then
            reason = 'candidate source lineage is incomplete'
            return
        end if
        if (len_trim(candidate%expression_path) == 0) then
            reason = 'candidate canonical expression path is missing'
            return
        end if
        call parse_decimal(candidate%source_page, page, ok)
        if (.not. ok .or. page < 1_int64) then
            ok = .false.
            reason = 'candidate source page is not a positive decimal'
            return
        end if
        call parse_decimal(candidate%source_byte_start, byte_start, ok)
        if (.not. ok) then
            reason = 'candidate source byte start is not a nonnegative decimal'
            return
        end if
        ok = .true.
        reason = ''
    end subroutine validate_candidate

    logical function same_source_identity(candidate, rule)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        type(standardir_grammar_rule_t), intent(in) :: rule

        same_source_identity = trim(candidate%source_rule) == trim(rule%source%rule) .and. &
            trim(candidate%source_lhs) == trim(rule%lhs)
    end function same_source_identity

    logical function same_source_occurrence(candidate, rule, page, byte_start)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer(int64), intent(in) :: page, byte_start

        same_source_occurrence = same_source_identity(candidate, rule)
        if (.not. same_source_occurrence) return
        if (trim(candidate%source_document) /= trim(rule%source%document)) then
            same_source_occurrence = .false.
            return
        end if
        if (trim(candidate%source_clause) /= trim(rule%source%clause)) then
            same_source_occurrence = .false.
            return
        end if
        if (trim(candidate%source_hash) /= trim(rule%source%source_hash)) then
            same_source_occurrence = .false.
            return
        end if
        if (page /= int(rule%source%page, int64)) then
            same_source_occurrence = .false.
            return
        end if
        same_source_occurrence = byte_start == rule%source%byte_start
    end function same_source_occurrence

    subroutine source_path_index(rule, path, index, ok, reason)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: path(:)
        integer, intent(out) :: index
        logical, intent(out) :: ok
        character(len=*), intent(out) :: reason

        type(standardir_grammar_node_t) :: node
        integer :: i, j, child, last, root_last

        index = 0
        ok = .false.
        reason = ''
        call validate_source_nodes(rule, ok, reason)
        if (.not. ok) then
            reason = 'source grammar node shape is unsupported: '//trim(reason)
            return
        end if
        call standardir_grammar_validate(rule, ok, reason)
        if (.not. ok) then
            reason = 'source grammar node shape is unsupported: '//trim(reason)
            return
        end if
        call subtree_end(rule, rule%root, 0, root_last, ok, reason)
        if (.not. ok) then
            reason = 'source grammar node shape is unsupported: '//trim(reason)
            return
        end if
        index = rule%root
        do i = 1, size(path)
            node = rule%nodes%values(index)
            if (path(i) > node%child_count) then
                ok = .false.
                reason = 'canonical expression path is missing from the source tree'
                return
            end if
            child = node%first_child
            do j = 1, path(i) - 1
                call subtree_end(rule, child, 0, last, ok, reason)
                if (.not. ok) then
                    reason = 'source grammar node shape is unsupported: '//trim(reason)
                    return
                end if
                child = last + 1
            end do
            index = child
        end do
        ok = .true.
        reason = ''
    end subroutine source_path_index

    subroutine validate_source_nodes(rule, ok, reason)
        type(standardir_grammar_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: reason

        type(standardir_grammar_node_t) :: node
        integer :: i, last_child

        ok = .false.
        reason = ''
        if (.not. allocated(rule%nodes%values)) then
            reason = 'grammar node list is not allocated'
            return
        end if
        if (size(rule%nodes%values) < 1) then
            reason = 'grammar node list is empty'
            return
        end if
        if (rule%root < 1) then
            reason = 'grammar root is outside the node list'
            return
        end if
        if (rule%root > size(rule%nodes%values)) then
            reason = 'grammar root is outside the node list'
            return
        end if
        do i = 1, size(rule%nodes%values)
            node = rule%nodes%values(i)
            if (node%kind < standardir_grammar_reference .or. node%kind > standardir_grammar_repeat) then
                reason = 'grammar node kind is invalid'
                return
            end if
            if (len_trim(node%name) == 0) then
                reason = 'grammar node name is empty'
                return
            end if
            if (node%minimum < 0) then
                reason = 'grammar node minimum is negative'
                return
            end if
            if (node%child_count < 0 .or. node%first_child < 0) then
                reason = 'grammar node child range is negative'
                return
            end if
            if (node%child_count == 0) then
                if (node%first_child /= 0) then
                    reason = 'grammar leaf has a nonzero first child'
                    return
                end if
            else
                if (node%first_child < 1) then
                    reason = 'grammar child range has no first child'
                    return
                end if
                last_child = node%first_child + node%child_count - 1
                if (last_child > size(rule%nodes%values)) then
                    reason = 'grammar node child range exceeds node list'
                    return
                end if
            end if
            select case (node%kind)
            case (standardir_grammar_reference)
                if (node%child_count /= 0 .or. node%minimum /= 1 .or. node%unbounded) then
                    reason = 'grammar reference metadata is invalid'
                    return
                end if
            case (standardir_grammar_token)
                if (node%child_count /= 0 .or. node%minimum /= 1 .or. node%unbounded) then
                    reason = 'grammar token metadata is invalid'
                    return
                end if
            case (standardir_grammar_sequence, standardir_grammar_choice)
                if (node%child_count < 1 .or. node%minimum /= 1 .or. node%unbounded) then
                    reason = 'grammar group metadata is invalid'
                    return
                end if
            case (standardir_grammar_optional)
                if (node%child_count /= 1 .or. node%minimum /= 0 .or. node%unbounded) then
                    reason = 'grammar optional metadata is invalid'
                    return
                end if
            case (standardir_grammar_repeat)
                if (node%child_count /= 1 .or. node%minimum > 1 .or. .not. node%unbounded) then
                    reason = 'grammar repeat metadata is invalid'
                    return
                end if
            end select
        end do
        ok = .true.
        reason = ''
    end subroutine validate_source_nodes

    recursive subroutine subtree_end(rule, index, depth, last, ok, reason)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index, depth
        integer, intent(out) :: last
        logical, intent(out) :: ok
        character(len=*), intent(out) :: reason

        type(standardir_grammar_node_t) :: node
        integer :: i, child, child_last

        last = 0
        ok = .false.
        reason = ''
        if (index < 1 .or. index > size(rule%nodes%values)) then
            reason = 'source grammar child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            reason = 'source grammar node tree is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        last = index
        child = node%first_child
        do i = 1, node%child_count
            call subtree_end(rule, child, depth + 1, child_last, ok, reason)
            if (.not. ok) return
            last = child_last
            child = last + 1
        end do
        ok = .true.
    end subroutine subtree_end

    subroutine parse_path(value, path, ok, reason)
        character(len=*), intent(in) :: value
        integer, allocatable, intent(out) :: path(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: reason

        character(len=len_trim(value)) :: text
        integer :: i, first, length, number, ios

        allocate (path(0))
        ok = .false.
        reason = ''
        if (len_trim(value) < 3) then
            reason = 'candidate canonical expression path is malformed'
            return
        end if
        text = trim(value)
        length = len(text)
        if (text(1:3) /= 'rhs') then
            reason = 'candidate canonical expression path is malformed'
            return
        end if
        if (length == 3) then
            ok = .true.
            return
        end if
        i = 4
        do while (i <= length)
            if (text(i:i) /= '/') then
                reason = 'candidate canonical expression path is malformed'
                return
            end if
            i = i + 1
            if (i > length) then
                reason = 'candidate canonical expression path is malformed'
                return
            end if
            first = i
            do while (i <= length)
                if (.not. is_digit(text(i:i))) exit
                i = i + 1
            end do
            if (i == first) then
                reason = 'candidate canonical expression path is malformed'
                return
            end if
            if (text(first:first) == '0') then
                reason = 'candidate canonical expression path is malformed'
                return
            end if
            read (text(first:i - 1), *, iostat=ios) number
            if (ios /= 0 .or. number < 1) then
                reason = 'candidate canonical expression path is malformed'
                return
            end if
            call append_integer(path, number)
        end do
        ok = .true.
        reason = ''
    end subroutine parse_path

    subroutine parse_decimal(value, result, ok)
        character(len=*), intent(in) :: value
        integer(int64), intent(out) :: result
        logical, intent(out) :: ok

        character(len=64) :: text
        integer :: i, ios

        result = 0_int64
        ok = len_trim(value) > 0
        if (.not. ok) return
        do i = 1, len_trim(value)
            if (.not. is_digit(value(i:i))) then
                ok = .false.
                return
            end if
        end do
        text = trim(value)
        read (text, *, iostat=ios) result
        ok = ios == 0
    end subroutine parse_decimal

    subroutine append_integer(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value
        integer, allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_integer

    subroutine append_match(rule_indices, node_indices, alternatives, rule_index, node_index, alternative)
        integer, allocatable, intent(inout) :: rule_indices(:), node_indices(:), alternatives(:)
        integer, intent(in) :: rule_index, node_index, alternative
        integer, allocatable :: expanded_rules(:), expanded_nodes(:), expanded_alternatives(:)
        integer :: old_size

        old_size = size(rule_indices)
        allocate (expanded_rules(old_size + 1), expanded_nodes(old_size + 1), expanded_alternatives(old_size + 1))
        if (old_size > 0) then
            expanded_rules(:old_size) = rule_indices
            expanded_nodes(:old_size) = node_indices
            expanded_alternatives(:old_size) = alternatives
        end if
        expanded_rules(old_size + 1) = rule_index
        expanded_nodes(old_size + 1) = node_index
        expanded_alternatives(old_size + 1) = alternative
        call move_alloc(expanded_rules, rule_indices)
        call move_alloc(expanded_nodes, node_indices)
        call move_alloc(expanded_alternatives, alternatives)
    end subroutine append_match

    subroutine set_disposition(value, disposition, reason)
        type(standardir_statement_boundary_mapping_t), intent(inout) :: value
        character(len=*), intent(in) :: disposition, reason

        value%disposition = trim(disposition)
        value%reason = trim(reason)
    end subroutine set_disposition

    logical function is_digit(value)
        character(len=1), intent(in) :: value

        is_digit = value >= '0' .and. value <= '9'
    end function is_digit

end module standardir_statement_boundary_mapping

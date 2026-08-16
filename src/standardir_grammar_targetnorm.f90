module standardir_grammar_targetnorm
    !! Graph-derived normalization for source-backed grammar targets.

    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_node_t, &
        standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_grammar_target_support, only: append_expression, append_target, &
        append_source_witness, collect_lhs_names, contains_expression, dependency_provenance, &
        merge_provenance, merge_roles, merge_source_witnesses, &
        refresh_source_witnesses, source_witness_exists, &
        same_expression, standardir_target_expression_t, standardir_target_provenance_t, &
        standardir_target_role_family_config_t, standardir_target_role_family_factored, &
        standardir_target_role_family_rejected, standardir_target_role_family_witness_t, &
        standardir_target_rule_t, standardir_target_source_witness_t, standardir_grammar_factor_role_family, &
        standardir_grammar_validate_role_family_witness
    use standardir_grammar_target_fingerprint, only: standardir_target_expression_sha256
    use standardir_grammar_correspondence, only: standardir_correspondence_ambiguous, &
        standardir_correspondence_mapped, standardir_correspondence_suppressed, &
        standardir_correspondence_unsupported, standardir_grammar_append_correspondence_trace, &
        standardir_grammar_correspondence_trace_t
    implicit none
    private

    public :: standardir_grammar_normalize
    public :: standardir_grammar_factor_role_family
    public :: standardir_grammar_validate_role_family_witness
    public :: refresh_source_witnesses
    public :: standardir_target_expression_t
    public :: standardir_target_provenance_t
    public :: standardir_target_source_witness_t
    public :: standardir_target_role_family_config_t
    public :: standardir_target_role_family_factored
    public :: standardir_target_role_family_rejected
    public :: standardir_target_role_family_witness_t
    public :: standardir_target_rule_t
    public :: standardir_grammar_correspondence_trace_t
    public :: standardir_correspondence_mapped
    public :: standardir_correspondence_ambiguous
    public :: standardir_correspondence_suppressed
    public :: standardir_correspondence_unsupported


contains

    subroutine standardir_grammar_normalize(rules, normalized, suppressed, ok, message, trace)
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        type(standardir_target_rule_t), allocatable, intent(out) :: normalized(:), suppressed(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(out), optional :: trace(:)

        type(standardir_target_rule_t), allocatable :: working(:)
        character(len=128), allocatable :: names(:)
        logical, allocatable :: nullable(:)
        type(standardir_target_rule_t) :: item
        type(standardir_target_expression_t) :: normalized_expression
        type(standardir_grammar_correspondence_trace_t), allocatable :: trace_values(:)
        logical :: trace_requested
        integer :: i, name_count, suppressed_before

        if (allocated(normalized)) deallocate (normalized)
        if (allocated(suppressed)) deallocate (suppressed)
        allocate (working(0), suppressed(0))
        allocate (trace_values(0))
        trace_requested = present(trace)
        if (trace_requested) then
            if (allocated(trace)) deallocate (trace)
            allocate (trace(0))
        end if
        ok = .false.
        message = ''
        if (size(rules) < 1) then
            message = 'grammar normalization input is empty'
            return
        end if
        do i = 1, size(rules)
            call rule_to_target(rules(i), item, ok, message)
            if (.not. ok) return
            call append_target(working, item)
        end do

        call collect_lhs_names(working, names, name_count)
        call compute_nullable(working, names, name_count, nullable)
        do i = 1, size(working)
            call normalize_expression(working(i)%expression, names, nullable, normalized_expression, ok, &
                message, trace_requested, working(i), source_path_for_expression(working(i)%expression, 'rhs'), &
                'rule-expression', trace_values)
            if (.not. ok) return
            working(i)%expression = normalized_expression
        end do
        call deduplicate_rules(working, suppressed, ok, message)
        if (.not. ok) return
        if (trace_requested) call mark_suppressed_trace(trace_values, suppressed, 'rule-deduplicate')
        suppressed_before = size(suppressed)
        call eliminate_left_recursion(working, suppressed, names, name_count, nullable, ok, message)
        if (.not. ok) return
        if (trace_requested) call mark_left_recursion_trace(trace_values, suppressed, suppressed_before + 1)
        call deduplicate_rules(working, suppressed, ok, message)
        if (.not. ok) return
        if (trace_requested) call mark_suppressed_trace(trace_values, suppressed, 'rule-deduplicate')
        call reject_remaining_left_recursion(working, names, name_count, nullable, ok, message)
        if (.not. ok) return
        call refresh_target_expression_hashes(working, ok, message)
        if (.not. ok) return
        call refresh_source_witnesses(working, ok, message)
        if (.not. ok) return
        if (size(working) < 1) then
            message = 'grammar normalization removed every alternative'
            return
        end if
        if (trace_requested) call finalize_trace_paths(trace_values)
        call move_alloc(working, normalized)
        if (trace_requested) call move_alloc(trace_values, trace)
        ok = .true.
        message = ''
    end subroutine standardir_grammar_normalize

    subroutine refresh_target_expression_hashes(values, ok, message)
        type(standardir_target_rule_t), intent(inout) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        do i = 1, size(values)
            call standardir_target_expression_sha256(values(i)%expression, &
                values(i)%target_expression_sha256, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine refresh_target_expression_hashes

    subroutine rule_to_target(rule, value, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        type(standardir_target_rule_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=64) :: target_expression_sha256
        type(standardir_target_source_witness_t) :: source_witness

        call standardir_grammar_validate(rule, ok, message)
        if (.not. ok) return
        if (rule%resolution /= standardir_grammar_resolution_resolved) then
            message = 'grammar normalization requires resolved rules'
            return
        end if
        value = standardir_target_rule_t()
        value%id = trim(rule%id)
        value%alternative = rule%alternative
        value%lhs = trim(rule%lhs)
        value%source = rule%source
        allocate (value%provenance(1))
        value%provenance(1)%source = rule%source
        value%provenance(1)%alternative = rule%alternative
        value%provenance(1)%source_expression_present = rule%source_expression_present
        call build_target_expression(rule, rule%root, 0, value%expression, ok, message)
        if (.not. ok) return
        call standardir_target_expression_sha256(value%expression, target_expression_sha256, ok, message)
        if (.not. ok) return
        if (rule%source_expression_present) then
            if (len_trim(rule%source_expression_sha256) > 0) then
                value%provenance(1)%source_expression_sha256 = trim(rule%source_expression_sha256)
            else
                value%provenance(1)%source_expression_sha256 = target_expression_sha256
            end if
        end if
        value%target_expression_sha256 = target_expression_sha256
        if (rule%source_expression_present) then
            allocate (value%source_witnesses(1))
            source_witness = standardir_target_source_witness_t()
            source_witness%source = value%provenance(1)
            source_witness%target_rule_id = trim(value%id)
            source_witness%target_lhs = trim(value%lhs)
            source_witness%target_alternative = value%alternative
            source_witness%reason = 'source-alternative-preservation'
            source_witness%target_expression_sha256 = trim(value%target_expression_sha256)
            value%source_witnesses(1) = source_witness
        end if
        allocate (value%source_roles(1))
        value%source_roles(1) = trim(rule%lhs)
        value%origin = rule%origin
        value%resolution = rule%resolution
        ok = .true.
        message = ''
    end subroutine rule_to_target

    recursive subroutine build_target_expression(rule, index, depth, expression, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index, depth
        type(standardir_target_expression_t), intent(out) :: expression
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child, last

        expression = standardir_target_expression_t()
        ok = .false.
        message = ''
        if (index < 1 .or. index > size(rule%nodes%values)) then
            message = 'grammar normalization child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'grammar normalization node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        expression%kind = node%kind
        expression%name = trim(node%name)
        expression%source_expression_path = trim(node%source_expression_path)
        expression%minimum = node%minimum
        expression%unbounded = node%unbounded
        if (node%child_count > 0) then
            allocate (expression%children(node%child_count))
            child = node%first_child
            do i = 1, node%child_count
                call build_target_expression(rule, child, depth + 1, expression%children(i), ok, message)
                if (.not. ok) return
                call target_subtree_end(rule, child, depth + 1, last, ok, message)
                if (.not. ok) return
                child = last + 1
            end do
        end if
        ok = .true.
        message = ''
    end subroutine build_target_expression

    recursive subroutine target_subtree_end(rule, index, depth, last, ok, message)
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
            message = 'grammar normalization child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'grammar normalization node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        last = index
        child = node%first_child
        do i = 1, node%child_count
            call target_subtree_end(rule, child, depth + 1, last, ok, message)
            if (.not. ok) return
            child = last + 1
        end do
        ok = .true.
        message = ''
    end subroutine target_subtree_end


    subroutine compute_nullable(values, names, name_count, nullable)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: name_count
        logical, allocatable, intent(out) :: nullable(:)

        logical :: changed, value
        integer :: i, j

        allocate (nullable(name_count))
        nullable = .false.
        do
            changed = .false.
            do i = 1, size(values)
                value = expression_nullable(values(i)%expression, names, nullable)
                if (value) then
                    do j = 1, name_count
                        if (trim(names(j)) == trim(values(i)%lhs) .and. .not. nullable(j)) then
                            nullable(j) = .true.
                            changed = .true.
                        end if
                    end do
                end if
            end do
            if (.not. changed) exit
        end do
    end subroutine compute_nullable

    recursive logical function expression_nullable(expression, names, nullable) result(value)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        integer :: i, j

        select case (expression%kind)
        case (standardir_grammar_reference)
            value = .false.
            do i = 1, size(names)
                if (trim(names(i)) == trim(expression%name)) value = nullable(i)
            end do
        case (standardir_grammar_token)
            value = .false.
        case (standardir_grammar_sequence)
            value = .true.
            do i = 1, size(expression%children)
                if (.not. expression_nullable(expression%children(i), names, nullable)) then
                    value = .false.
                    exit
                end if
            end do
        case (standardir_grammar_choice)
            value = .false.
            do i = 1, size(expression%children)
                if (expression_nullable(expression%children(i), names, nullable)) then
                    value = .true.
                    exit
                end if
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            value = expression%minimum == 0 .or. &
                expression_nullable(expression%children(1), names, nullable)
        case default
            value = .false.
        end select
    end function expression_nullable

    recursive subroutine normalize_expression(expression, names, nullable, result, ok, message, &
            trace_requested, context, source_path, boundary_role, trace)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        type(standardir_target_expression_t), intent(out) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        logical, intent(in) :: trace_requested
        type(standardir_target_rule_t), intent(in) :: context
        character(len=*), intent(in) :: source_path, boundary_role
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)

        type(standardir_target_expression_t), allocatable :: children(:)
        type(standardir_target_expression_t) :: child
        integer :: child_start, child_end, flatten_base, i, j, output_count
        logical :: duplicate
        character(len=64) :: operation
        character(len=16) :: root_disposition
        character(len=64) :: input_hash, output_hash

        result = standardir_target_expression_t()
        ok = .false.
        message = ''
        if (expression%kind == standardir_grammar_reference .or. &
            expression%kind == standardir_grammar_token) then
            result = expression
            if (trace_requested) then
                call append_trace_node(trace, context, expression, source_path, boundary_role, &
                    'identity', standardir_correspondence_mapped, result, ok, message)
            end if
            ok = .true.
            return
        end if
        if (.not. allocated(expression%children)) then
            message = 'grammar normalization encountered an empty expression'
            return
        end if
        if (size(expression%children) < 1) then
            message = 'grammar normalization encountered an empty expression'
            return
        end if
        allocate (children(0))
        output_count = 0
        do i = 1, size(expression%children)
            child_start = size(trace)
            call normalize_expression(expression%children(i), names, nullable, child, ok, message, &
                trace_requested, context, source_path_for_expression(expression%children(i), &
                child_path(source_path, i)), child_role(expression%kind), trace)
            if (.not. ok) return
            child_end = size(trace)
            if (expression%kind == standardir_grammar_sequence .and. &
                child%kind == standardir_grammar_sequence) then
                if (trace_requested) then
                    call rebase_flattened_trace(trace, child_start + 1, child_end, source_path, output_count, &
                        'sequence-flatten')
                end if
                do j = 1, size(child%children)
                    call append_expression(children, child%children(j))
                    output_count = output_count + 1
                end do
            else if (expression%kind == standardir_grammar_choice .and. &
                    child%kind == standardir_grammar_choice) then
                flatten_base = output_count
                if (trace_requested) then
                    call rebase_flattened_trace(trace, child_start + 1, child_end, source_path, output_count, &
                        'choice-flatten')
                end if
                do j = 1, size(child%children)
                    call contains_expression(children, child%children(j), duplicate)
                    if (.not. duplicate) then
                        call append_expression(children, child%children(j))
                        output_count = output_count + 1
                        if (trace_requested) then
                            call compact_trace_slot(trace, child_start + 1, child_end, flatten_base + j, &
                                output_count)
                        end if
                    else if (trace_requested) then
                        call mark_trace_slot(trace, child_start + 1, child_end, flatten_base + j, &
                            'choice-deduplicate')
                    end if
                end do
            else
                duplicate = .false.
                if (expression%kind == standardir_grammar_choice) then
                    call contains_expression(children, child, duplicate)
                end if
                if (.not. duplicate) then
                    call append_expression(children, child)
                    output_count = output_count + 1
                    if (trace_requested) then
                        call prefix_trace_paths(trace, child_start + 1, child_end, output_count)
                    end if
                else if (trace_requested) then
                    call mark_trace_range(trace, child_start + 1, child_end, &
                        standardir_correspondence_suppressed, 'choice-deduplicate')
                end if
            end if
        end do
        if (size(children) < 1) then
            message = 'grammar normalization produced an empty expression'
            return
        end if
        result = expression
        call move_alloc(children, result%children)
        operation = 'identity'
        root_disposition = standardir_correspondence_mapped
        if (result%kind == standardir_grammar_optional) then
            if (expression_nullable(result%children(1), names, nullable)) result = result%children(1)
            if (result%kind /= standardir_grammar_optional) then
                operation = 'optional-wrapper-removal'
                root_disposition = standardir_correspondence_suppressed
            end if
        else if (result%kind == standardir_grammar_repeat) then
            if (result%children(1)%kind == standardir_grammar_optional) then
                result%minimum = 0
                result%children(1) = result%children(1)%children(1)
                operation = 'optional-wrapper-removal'
                root_disposition = standardir_correspondence_suppressed
            end if
        end if
        if (result%kind == standardir_grammar_sequence .or. result%kind == standardir_grammar_choice) then
            if (size(result%children) == 1) then
                result = result%children(1)
                if (operation == 'identity') then
                    if (expression%kind == standardir_grammar_sequence) then
                        operation = 'sequence-collapse'
                    else
                        operation = 'choice-collapse'
                    end if
                    root_disposition = standardir_correspondence_suppressed
                end if
            end if
        end if
        if (expression%kind == standardir_grammar_sequence .and. result%kind == standardir_grammar_sequence) then
            if (trace_requested) call mark_trace_operation(trace, source_path, 'sequence-flatten')
            operation = 'sequence-flatten'
        else if (expression%kind == standardir_grammar_choice .and. result%kind == standardir_grammar_choice) then
            if (trace_requested) call mark_trace_operation(trace, source_path, 'choice-flatten')
            operation = 'choice-flatten'
        end if
        if (trace_requested) then
            call trace_expression_hash(expression, context, source_path, input_hash, ok, message)
            if (.not. ok) return
            call standardir_target_expression_sha256(result, output_hash, ok, message)
            if (.not. ok) return
            call append_trace_root(trace, context, expression, source_path, boundary_role, operation, &
                input_hash, output_hash, result, ok, message, root_disposition)
            if (.not. ok) return
            if (operation == 'optional-wrapper-removal') then
                call mark_trace_operation(trace, source_path, 'optional-wrapper-removal')
            end if
        end if
        ok = .true.
        message = ''
    end subroutine normalize_expression

    subroutine append_trace_node(trace, context, expression, source_path, boundary_role, operation, &
            disposition, result, ok, message)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        type(standardir_target_rule_t), intent(in) :: context
        type(standardir_target_expression_t), intent(in) :: expression, result
        character(len=*), intent(in) :: source_path, boundary_role, operation, disposition
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=64) :: input_hash, output_hash

        call trace_expression_hash(expression, context, source_path, input_hash, ok, message)
        if (.not. ok) return
        call standardir_target_expression_sha256(result, output_hash, ok, message)
        if (.not. ok) return
        call append_trace_root(trace, context, expression, source_path, boundary_role, operation, &
            input_hash, output_hash, result, ok, message, disposition)
    end subroutine append_trace_node

    subroutine append_trace_root(trace, context, expression, source_path, boundary_role, operation, &
            input_hash, output_hash, result, ok, message, disposition)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        type(standardir_target_rule_t), intent(in) :: context
        type(standardir_target_expression_t), intent(in) :: expression, result
        character(len=*), intent(in) :: source_path, boundary_role, operation
        character(len=*), intent(in) :: input_hash, output_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: disposition
        type(standardir_grammar_correspondence_trace_t) :: row

        row = standardir_grammar_correspondence_trace_t()
        row%source = context%source
        row%source_alternative = context%alternative
        row%raw_source_expression_path = trim(source_path)
        row%source_node_kind = expression%kind
        row%source_node_name = trim(expression%name)
        row%source_boundary_role = trim(boundary_role)
        row%target_rule_id = trim(context%id)
        row%target_lhs = trim(context%lhs)
        row%target_alternative = context%alternative
        row%target_expression_path = ''
        row%target_sequence_boundary_slot = 0
        row%transformation = trim(operation)
        row%input_expression_sha256 = trim(input_hash)
        row%output_expression_sha256 = trim(output_hash)
        row%disposition = standardir_correspondence_mapped
        if (present(disposition)) row%disposition = trim(disposition)
        if (trim(row%disposition) == standardir_correspondence_suppressed) then
            row%reason = 'source expression was removed by generic normalization'
        end if
        call standardir_grammar_append_correspondence_trace(trace, row)
        ok = .true.
        message = ''
    end subroutine append_trace_root

    subroutine trace_expression_hash(expression, context, source_path, fingerprint, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        type(standardir_target_rule_t), intent(in) :: context
        character(len=*), intent(in) :: source_path
        character(len=*), intent(out) :: fingerprint
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        fingerprint = ''
        ok = .false.
        message = ''
        if (trim(source_path) == 'rhs' .and. allocated(context%provenance)) then
            if (size(context%provenance) > 0) then
                if (context%provenance(1)%source_expression_present .and. &
                    len_trim(context%provenance(1)%source_expression_sha256) > 0) then
                    fingerprint = trim(context%provenance(1)%source_expression_sha256)
                    ok = .true.
                    return
                end if
            end if
        end if
        call standardir_target_expression_sha256(expression, fingerprint, ok, message)
    end subroutine trace_expression_hash

    function child_path(parent, index) result(value)
        character(len=*), intent(in) :: parent
        integer, intent(in) :: index
        character(len=512) :: value
        character(len=32) :: text

        write (text, '(i0)') index
        value = trim(parent)//'/'//trim(text)
    end function child_path

    function source_path_for_expression(expression, fallback) result(value)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=*), intent(in) :: fallback
        character(len=512) :: value

        if (len_trim(expression%source_expression_path) > 0) then
            value = trim(expression%source_expression_path)
        else
            value = trim(fallback)
        end if
    end function source_path_for_expression

    function child_role(kind) result(value)
        integer, intent(in) :: kind
        character(len=64) :: value

        select case (kind)
        case (standardir_grammar_sequence)
            value = 'sequence-element'
        case (standardir_grammar_choice)
            value = 'choice-alternative'
        case (standardir_grammar_optional)
            value = 'optional-content'
        case (standardir_grammar_repeat)
            value = 'repeat-content'
        case default
            value = 'expression-child'
        end select
    end function child_role

    subroutine prefix_trace_paths(trace, first, last, slot)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer, intent(in) :: first, last, slot
        character(len=32) :: text
        integer :: i

        write (text, '(i0)') slot
        do i = first, last
            if (trim(trace(i)%disposition) /= standardir_correspondence_mapped .and. &
                trim(trace(i)%disposition) /= standardir_correspondence_ambiguous) cycle
            if (len_trim(trace(i)%target_expression_path) == 0) then
                trace(i)%target_expression_path = '/'//trim(text)
            else
                trace(i)%target_expression_path = '/'//trim(text)// &
                    trim(trace(i)%target_expression_path)
            end if
            if (trace(i)%target_sequence_boundary_slot == 0) then
                trace(i)%target_sequence_boundary_slot = slot
            end if
        end do
    end subroutine prefix_trace_paths

    subroutine rebase_flattened_trace(trace, first, last, source_path, old_count, operation)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer, intent(in) :: first, last, old_count
        character(len=*), intent(in) :: source_path
        character(len=*), intent(in) :: operation
        character(len=512) :: path, remainder
        character(len=32) :: number_text, slot_text
        integer :: i, ios, nested_slot, target_slot

        do i = first, last
            path = trim(trace(i)%target_expression_path)
            if (len_trim(path) == 0) then
                trace(i)%disposition = standardir_correspondence_suppressed
                trace(i)%transformation = trim(operation)
                trace(i)%reason = 'nested expression wrapper has no target node'
                cycle
            end if
            read (path(2:), *, iostat=ios) nested_slot
            if (ios /= 0) cycle
            target_slot = old_count + nested_slot
            write (slot_text, '(i0)') target_slot
            remainder = ''
            if (len_trim(path) > 0) then
                number_text = ''
                write (number_text, '(i0)') nested_slot
                if (len_trim(path) > len_trim(number_text)) then
                    remainder = path(len_trim(number_text) + 2:)
                end if
            end if
            trace(i)%target_expression_path = '/'//trim(slot_text)//trim(remainder)
            trace(i)%target_sequence_boundary_slot = target_slot
            trace(i)%transformation = trim(operation)
        end do
    end subroutine rebase_flattened_trace

    subroutine mark_trace_range(trace, first, last, disposition, operation)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: disposition, operation
        integer :: i

        do i = first, last
            trace(i)%target_expression_path = ''
            trace(i)%target_sequence_boundary_slot = 0
            trace(i)%disposition = trim(disposition)
            trace(i)%transformation = trim(operation)
            trace(i)%reason = 'source expression was not uniquely retained'
        end do
    end subroutine mark_trace_range

    subroutine mark_trace_slot(trace, first, last, slot, operation)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer, intent(in) :: first, last, slot
        character(len=*), intent(in) :: operation
        character(len=512) :: path
        character(len=32) :: slot_text
        integer :: i

        write (slot_text, '(i0)') slot
        do i = first, last
            path = trim(trace(i)%target_expression_path)
            if (len_trim(path) == 0) cycle
            if (path == '/'//trim(slot_text) .or. &
                index(path, '/'//trim(slot_text)//'/') == 1) then
                trace(i)%target_expression_path = ''
                trace(i)%target_sequence_boundary_slot = 0
                trace(i)%disposition = standardir_correspondence_suppressed
                trace(i)%transformation = trim(operation)
                trace(i)%reason = 'duplicate choice alternative was suppressed'
            end if
        end do
    end subroutine mark_trace_slot

    subroutine compact_trace_slot(trace, first, last, old_slot, new_slot)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer, intent(in) :: first, last, old_slot, new_slot
        character(len=512) :: path
        character(len=32) :: old_text, new_text
        integer :: i

        write (old_text, '(i0)') old_slot
        write (new_text, '(i0)') new_slot
        do i = first, last
            path = trim(trace(i)%target_expression_path)
            if (path == '/'//trim(old_text)) then
                trace(i)%target_expression_path = '/'//trim(new_text)
            else if (index(path, '/'//trim(old_text)//'/') == 1) then
                trace(i)%target_expression_path = '/'//trim(new_text)//path(len_trim(old_text) + 1:)
            else
                cycle
            end if
            trace(i)%target_sequence_boundary_slot = new_slot
        end do
    end subroutine compact_trace_slot

    subroutine mark_trace_operation(trace, source_path, operation)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        character(len=*), intent(in) :: source_path, operation
        integer :: i

        do i = 1, size(trace)
            if (trim(trace(i)%raw_source_expression_path) == trim(source_path)) then
                trace(i)%transformation = trim(operation)
            end if
        end do
    end subroutine mark_trace_operation

    subroutine mark_left_recursion_trace(trace, suppressed, first_suppressed)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        type(standardir_target_rule_t), allocatable, intent(in) :: suppressed(:)
        integer, intent(in) :: first_suppressed
        integer :: i, j

        do j = first_suppressed, size(suppressed)
            do i = 1, size(trace)
                if (trim(trace(i)%source%rule) /= trim(suppressed(j)%source%rule)) cycle
                if (trace(i)%source_alternative /= suppressed(j)%alternative) cycle
                trace(i)%target_expression_path = ''
                trace(i)%target_sequence_boundary_slot = 0
                trace(i)%disposition = standardir_correspondence_unsupported
                trace(i)%transformation = 'left-recursion-elimination'
                trace(i)%reason = 'left-recursion lowering has no sound correspondence trace'
            end do
        end do
    end subroutine mark_left_recursion_trace

    subroutine mark_suppressed_trace(trace, suppressed, operation)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        type(standardir_target_rule_t), allocatable, intent(in) :: suppressed(:)
        character(len=*), intent(in) :: operation
        integer :: i, j

        do j = 1, size(suppressed)
            do i = 1, size(trace)
                if (trim(trace(i)%source%rule) /= trim(suppressed(j)%source%rule)) cycle
                if (trace(i)%source_alternative /= suppressed(j)%alternative) cycle
                if (trim(trace(i)%disposition) == standardir_correspondence_unsupported) cycle
                trace(i)%target_expression_path = ''
                trace(i)%target_sequence_boundary_slot = 0
                trace(i)%disposition = standardir_correspondence_suppressed
                trace(i)%transformation = trim(operation)
                trace(i)%reason = 'duplicate target alternative was suppressed'
            end do
        end do
    end subroutine mark_suppressed_trace

    subroutine finalize_trace_paths(trace)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: trace(:)
        integer :: i

        do i = 1, size(trace)
            if (len_trim(trace(i)%target_expression_path) > 0) then
                trace(i)%target_expression_path = 'rhs'//trim(trace(i)%target_expression_path)
            end if
        end do
    end subroutine finalize_trace_paths


    subroutine deduplicate_rules(values, suppressed, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: suppressed(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: unique(:)
        type(standardir_target_provenance_t), allocatable :: merged(:)
        type(standardir_target_source_witness_t), allocatable :: merged_witnesses(:)
        character(len=128), allocatable :: merged_roles(:)
        integer :: i, j
        logical :: duplicate

        allocate (unique(0))
        do i = 1, size(values)
            duplicate = .false.
            do j = 1, size(unique)
                if (trim(unique(j)%lhs) == trim(values(i)%lhs) .and. &
                    same_expression(unique(j)%expression, values(i)%expression)) then
                    duplicate = .true.
                    call merge_provenance(unique(j)%provenance, values(i)%provenance, merged)
                    call move_alloc(merged, unique(j)%provenance)
                    call merge_source_witnesses(unique(j)%source_witnesses, values(i)%source_witnesses, &
                        merged_witnesses)
                    call move_alloc(merged_witnesses, unique(j)%source_witnesses)
                    call merge_roles(unique(j)%source_roles, values(i)%source_roles, merged_roles)
                    call move_alloc(merged_roles, unique(j)%source_roles)
                    exit
                end if
            end do
            if (duplicate) then
                call append_target(suppressed, values(i))
            else
                call append_target(unique, values(i))
            end if
        end do
        call move_alloc(unique, values)
        ok = .true.
        message = ''
    end subroutine deduplicate_rules

    subroutine rules_for_lhs(values, lhs, selected)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        type(standardir_target_rule_t), allocatable, intent(out) :: selected(:)
        integer :: i

        allocate (selected(0))
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(lhs)) call append_target(selected, values(i))
        end do
    end subroutine rules_for_lhs

    logical function has_leading_reference(values, lhs, reference)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, reference
        type(standardir_target_expression_t) :: tail
        logical :: found
        integer :: i

        has_leading_reference = .false.
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(lhs)) cycle
            call leading_reference_tail(values(i)%expression, reference, tail, found)
            if (found) then
                has_leading_reference = .true.
                return
            end if
        end do
    end function has_leading_reference

    subroutine compute_left_corner_reachability(values, names, name_count, nullable, reachable)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: name_count
        logical, intent(in) :: nullable(:)
        logical, allocatable, intent(out) :: reachable(:,:)

        logical, allocatable :: direct(:,:), visited(:)
        integer, allocatable :: stack(:)
        integer :: i, j, current, top

        allocate (direct(name_count, name_count), reachable(name_count, name_count))
        direct = .false.
        do i = 1, name_count
            do j = 1, name_count
                direct(i, j) = has_left_corner_edge(values, names(i), names(j), names, nullable)
            end do
        end do
        reachable = .false.
        allocate (visited(name_count), stack(name_count))
        do i = 1, name_count
            visited = .false.
            top = 1
            stack(1) = i
            do while (top > 0)
                current = stack(top)
                top = top - 1
                if (visited(current)) cycle
                visited(current) = .true.
                do j = 1, name_count
                    if (direct(current, j) .and. .not. visited(j)) then
                        top = top + 1
                        stack(top) = j
                    end if
                end do
            end do
            reachable(i, :) = visited
        end do
    end subroutine compute_left_corner_reachability

    logical function has_left_corner_edge(values, lhs, reference, names, nullable)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, reference
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        logical :: found
        integer :: i

        has_left_corner_edge = .false.
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(lhs)) cycle
            call has_left_corner(values(i)%expression, reference, names, nullable, found)
            if (found) then
                has_left_corner_edge = .true.
                return
            end if
        end do
    end function has_left_corner_edge

    subroutine eliminate_left_recursion(values, suppressed, names, name_count, nullable, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: suppressed(:)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: name_count
        logical, intent(in) :: nullable(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: source(:), group(:)
        logical, allocatable :: left_corner_reachable(:,:)
        integer :: i, j

        ok = .false.
        message = ''
        call compute_left_corner_reachability(values, names, name_count, nullable, &
            left_corner_reachable)
        do i = 1, name_count
            do j = 1, i - 1
                if (.not. has_leading_reference(values, names(i), names(j))) cycle
                if (.not. left_corner_reachable(j, i)) cycle
                call rules_for_lhs(values, names(j), source)
                call substitute_leading_reference(values, names(i), names(j), source, ok, message)
                if (.not. ok) return
            end do
            call rules_for_lhs(values, names(i), group)
            call eliminate_direct_group(values, suppressed, group, names(i), names, nullable, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine eliminate_left_recursion

    subroutine substitute_leading_reference(values, target_lhs, source_lhs, source, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: target_lhs, source_lhs
        type(standardir_target_rule_t), intent(in) :: source(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: replaced(:)
        type(standardir_target_rule_t) :: candidate
        type(standardir_target_expression_t) :: tail, expression
        type(standardir_target_source_witness_t), allocatable :: merged_witnesses(:)
        integer :: i, j
        logical :: found

        allocate (replaced(0))
        ok = .false.
        message = ''
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(target_lhs)) then
                call append_target(replaced, values(i))
                cycle
            end if
            call leading_reference_tail(values(i)%expression, source_lhs, tail, found)
            if (.not. found) then
                call append_target(replaced, values(i))
                cycle
            end if
            if (size(source) < 1) then
                message = 'left-recursion substitution has no source alternatives'
                return
            end if
            do j = 1, size(source)
                candidate = values(i)
                expression = concatenate_present(source(j)%expression, tail)
                candidate%expression = expression
                candidate%source = source(j)%source
                call merge_provenance(source(j)%provenance, values(i)%provenance, &
                    candidate%provenance)
                call merge_source_witnesses(source(j)%source_witnesses, values(i)%source_witnesses, &
                    merged_witnesses)
                call move_alloc(merged_witnesses, candidate%source_witnesses)
                call merge_roles(source(j)%source_roles, values(i)%source_roles, &
                    candidate%source_roles)
                candidate%alternative = source(j)%alternative
                candidate%origin = source(j)%origin
                candidate%resolution = source(j)%resolution
                if (j > 1) candidate%id = derived_id(candidate%id, j)
                call append_target(replaced, candidate)
            end do
        end do
        call move_alloc(replaced, values)
        ok = .true.
        message = ''
    end subroutine substitute_leading_reference


    subroutine eliminate_direct_group(values, suppressed, group, lhs, names, nullable, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: suppressed(:)
        type(standardir_target_rule_t), intent(in) :: group(:)
        character(len=*), intent(in) :: lhs
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: recursive_rules(:), beta_rules(:), replacement(:)
        type(standardir_target_rule_t), allocatable :: helper_rules(:)
        type(standardir_target_expression_t), allocatable :: alphas(:)
        type(standardir_target_expression_t) :: tail, nullable_beta, nullable_alpha
        type(standardir_target_rule_t) :: item
        character(len=128) :: helper_lhs
        integer :: i
        logical :: found, left_corner

        allocate (recursive_rules(0), beta_rules(0), alphas(0), helper_rules(0))
        ok = .false.
        message = ''
        do i = 1, size(group)
            call leading_reference_tail(group(i)%expression, lhs, tail, found)
            call has_left_corner(group(i)%expression, lhs, names, nullable, left_corner)
            if (left_corner .and. .not. found) then
                call split_nullable_left_recursion(group(i)%expression, lhs, nullable_beta, &
                    nullable_alpha, found)
                if (.not. found) then
                    message = 'nullable or nested left recursion cannot preserve source mapping for '//trim(lhs)
                    return
                end if
                if (nullable_beta%kind == 0 .or. nullable_alpha%kind == 0) then
                    message = 'nullable or nested left recursion has no finite base for '//trim(lhs)
                    return
                end if
                item = group(i)
                item%expression = nullable_beta
                call append_target(beta_rules, item)
                item%expression = nullable_alpha
                call append_target(recursive_rules, item)
                call append_expression(alphas, nullable_alpha)
                cycle
            end if
            if (found) then
                if (tail%kind == 0) then
                    message = 'unit left recursion is unsupported for '//trim(lhs)
                    return
                end if
                if (expression_nullable(tail, names, nullable)) then
                    message = 'left-recursion suffix is nullable and unsupported for '//trim(lhs)
                    return
                end if
                call append_target(recursive_rules, group(i))
                call append_expression(alphas, tail)
            else
                call append_target(beta_rules, group(i))
            end if
        end do
        if (size(recursive_rules) == 0) then
            ok = .true.
            message = ''
            return
        end if
        if (size(beta_rules) == 0) then
            message = 'left-recursive grammar family has no base alternative for '//trim(lhs)
            return
        end if
        helper_lhs = make_helper_lhs(values, lhs, ok, message)
        if (.not. ok) return
        do i = 1, size(recursive_rules)
            item = recursive_rules(i)
            call preserve_source_witnesses(item, recursive_rules(i)%provenance, 'generated-helper')
            call dependency_provenance(recursive_rules(i)%provenance, item%provenance)
            item%id = derived_id(item%id, -1)
            if (i > 1) item%id = derived_id(item%id, i)
            item%lhs = helper_lhs
            item%expression = alphas(i)
            call append_target(helper_rules, item)
        end do
        do i = 1, size(recursive_rules)
            call append_target(suppressed, recursive_rules(i))
        end do
        allocate (replacement(0))
        do i = 1, size(beta_rules)
            item = beta_rules(i)
            item%expression = concatenate_expressions(item%expression, &
                make_repeat(make_reference(helper_lhs)))
            if (i > 1) item%id = derived_id(item%id, i)
            call append_target(replacement, item)
        end do
        do i = 1, size(helper_rules)
            call append_target(replacement, helper_rules(i))
        end do
        call replace_lhs(values, lhs, replacement)
        ok = .true.
        message = ''
    end subroutine eliminate_direct_group

    subroutine preserve_source_witnesses(value, provenance, reason)
        type(standardir_target_rule_t), intent(inout) :: value
        type(standardir_target_provenance_t), allocatable, intent(in) :: provenance(:)
        character(len=*), intent(in) :: reason
        type(standardir_target_source_witness_t) :: witness
        integer :: i

        if (.not. allocated(provenance)) return
        do i = 1, size(provenance)
            if (.not. provenance(i)%source_expression_present) cycle
            witness = standardir_target_source_witness_t()
            witness%source = provenance(i)
            witness%target_rule_id = trim(value%id)
            witness%target_lhs = trim(value%lhs)
            witness%target_alternative = value%alternative
            witness%reason = trim(reason)
            witness%target_expression_sha256 = trim(value%target_expression_sha256)
            if (.not. source_witness_exists(value%source_witnesses, provenance(i))) then
                call append_source_witness(value%source_witnesses, witness)
            end if
        end do
    end subroutine preserve_source_witnesses

    subroutine replace_lhs(values, lhs, replacement)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: lhs
        type(standardir_target_rule_t), intent(in) :: replacement(:)
        type(standardir_target_rule_t), allocatable :: changed(:)
        integer :: i, j
        logical :: inserted

        allocate (changed(0))
        inserted = .false.
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(lhs)) then
                if (.not. inserted) then
                    do j = 1, size(replacement)
                        call append_target(changed, replacement(j))
                    end do
                    inserted = .true.
                end if
            else
                call append_target(changed, values(i))
            end if
        end do
        call move_alloc(changed, values)
    end subroutine replace_lhs

    function make_helper_lhs(values, lhs, ok, message) result(value)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: value, candidate
        integer :: i, suffix
        logical :: collision

        if (len_trim(lhs) + len('__left_recursion') > 128) then
            ok = .false.
            message = 'generated left-recursion helper name is too long'
            return
        end if
        value = trim(lhs)//'__left_recursion'
        suffix = 0
        do
            collision = .false.
            do i = 1, size(values)
                if (trim(values(i)%lhs) == trim(value)) collision = .true.
            end do
            if (.not. collision) exit
            suffix = suffix + 1
            if (len_trim(lhs) + len('__left_recursion_') + 12 > 128) then
                ok = .false.
                message = 'generated left-recursion helper name is too long'
                return
            end if
            write (candidate, '(a,i0)') trim(lhs)//'__left_recursion_', suffix
            if (len_trim(candidate) > 128) then
                ok = .false.
                message = 'generated left-recursion helper name is too long'
                return
            end if
            value = trim(candidate)
        end do
        if (len_trim(value) > 128) then
            ok = .false.
            message = 'generated left-recursion helper name is too long'
            return
        end if
        ok = .true.
        message = ''
    end function make_helper_lhs

    function derived_id(id, number) result(value)
        character(len=*), intent(in) :: id
        integer, intent(in) :: number
        character(len=128) :: value
        character(len=32) :: suffix

        if (number < 0) then
            suffix = '__left_recursion_helper'
        else
            write (suffix, '(a,i0)') '__expanded_', number
        end if
        value = trim(id)//trim(suffix)
    end function derived_id

    function make_reference(name) result(value)
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_reference
        value%name = trim(name)
    end function make_reference

    function make_choice(values) result(value)
        type(standardir_target_expression_t), intent(in) :: values(:)
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_choice
        allocate (value%children(size(values)))
        value%children = values
    end function make_choice

    function make_repeat(child) result(value)
        type(standardir_target_expression_t), intent(in) :: child
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_repeat
        value%minimum = 0
        value%unbounded = .true.
        allocate (value%children(1))
        value%children(1) = child
    end function make_repeat

    function concatenate_expressions(left, right) result(value)
        type(standardir_target_expression_t), intent(in) :: left, right
        type(standardir_target_expression_t) :: value
        type(standardir_target_expression_t), allocatable :: parts(:)
        integer :: i

        if (left%kind == 0) then
            value = right
            return
        end if
        if (right%kind == 0) then
            value = left
            return
        end if
        allocate (parts(0))
        if (left%kind == standardir_grammar_sequence) then
            do i = 1, size(left%children)
                call append_expression(parts, left%children(i))
            end do
        else
            call append_expression(parts, left)
        end if
        if (right%kind == standardir_grammar_sequence) then
            do i = 1, size(right%children)
                call append_expression(parts, right%children(i))
            end do
        else
            call append_expression(parts, right)
        end if
        if (size(parts) == 1) then
            value = parts(1)
        else
            value = standardir_target_expression_t()
            value%kind = standardir_grammar_sequence
            call move_alloc(parts, value%children)
        end if
    end function concatenate_expressions

    subroutine leading_reference_tail(expression, name, tail, found)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t), intent(out) :: tail
        logical, intent(out) :: found
        type(standardir_target_expression_t), allocatable :: rest(:)
        integer :: i

        tail = standardir_target_expression_t()
        found = .false.
        if (expression%kind == standardir_grammar_reference) then
            if (trim(expression%name) == trim(name)) found = .true.
            return
        end if
        if (expression%kind /= standardir_grammar_sequence) return
        if (.not. allocated(expression%children)) return
        if (size(expression%children) < 1) return
        if (expression%children(1)%kind /= standardir_grammar_reference .or. &
            trim(expression%children(1)%name) /= trim(name)) return
        found = .true.
        if (size(expression%children) == 1) return
        allocate (rest(size(expression%children) - 1))
        do i = 2, size(expression%children)
            rest(i - 1) = expression%children(i)
        end do
        if (size(rest) == 1) then
            tail = rest(1)
        else
            tail%kind = standardir_grammar_sequence
            call move_alloc(rest, tail%children)
        end if
    end subroutine leading_reference_tail

    subroutine split_nullable_left_recursion(expression, name, beta, alpha, found)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t), intent(out) :: beta, alpha
        logical, intent(out) :: found

        type(standardir_target_expression_t), allocatable :: expanded(:), betas(:), alphas(:)
        type(standardir_target_expression_t) :: tail
        logical :: local_found
        integer :: i

        beta = standardir_target_expression_t()
        alpha = standardir_target_expression_t()
        found = .false.
        allocate (expanded(0), betas(0), alphas(0))
        call expand_nullable_prefix(expression, expanded)
        do i = 1, size(expanded)
            call leading_reference_tail(expanded(i), name, tail, local_found)
            if (local_found) then
                if (tail%kind == 0) return
                call append_expression(alphas, tail)
            else
                if (expanded(i)%kind == 0) return
                call append_expression(betas, expanded(i))
            end if
        end do
        if (size(alphas) < 1 .or. size(betas) < 1) return
        if (size(betas) == 1) then
            beta = betas(1)
        else
            beta = make_choice(betas)
        end if
        if (size(alphas) == 1) then
            alpha = alphas(1)
        else
            alpha = make_choice(alphas)
        end if
        found = .true.
    end subroutine split_nullable_left_recursion

    recursive subroutine expand_nullable_prefix(expression, values)
        type(standardir_target_expression_t), intent(in) :: expression
        type(standardir_target_expression_t), allocatable, intent(inout) :: values(:)

        type(standardir_target_expression_t), allocatable :: prefixes(:)
        type(standardir_target_expression_t) :: suffix, value
        integer :: i, j

        if (expression%kind == standardir_grammar_optional) then
            if (.not. allocated(expression%children)) then
                call append_expression(values, expression)
                return
            end if
            if (size(expression%children) /= 1) then
                call append_expression(values, expression)
                return
            end if
            call append_expression(values, standardir_target_expression_t())
            call expand_nullable_prefix(expression%children(1), values)
            return
        end if
        if (expression%kind /= standardir_grammar_sequence) then
            call append_expression(values, expression)
            return
        end if
        if (.not. allocated(expression%children)) then
            call append_expression(values, expression)
            return
        end if
        if (size(expression%children) < 1) then
            call append_expression(values, expression)
            return
        end if
        if (expression%children(1)%kind /= standardir_grammar_optional) then
            call append_expression(values, expression)
            return
        end if
        if (.not. allocated(expression%children(1)%children)) then
            call append_expression(values, expression)
            return
        end if
        if (size(expression%children(1)%children) /= 1) then
            call append_expression(values, expression)
            return
        end if

        allocate (prefixes(0))
        call expand_nullable_prefix(expression%children(1)%children(1), prefixes)
        if (size(expression%children) == 1) then
            do i = 1, size(prefixes)
                call append_expression(values, prefixes(i))
            end do
            call append_expression(values, standardir_target_expression_t())
            return
        end if
        suffix = expression%children(2)
        do j = 3, size(expression%children)
            suffix = concatenate_present(suffix, expression%children(j))
        end do
        call append_expression(values, suffix)
        do i = 1, size(prefixes)
            value = concatenate_present(prefixes(i), suffix)
            call append_expression(values, value)
        end do
    end subroutine expand_nullable_prefix

    function concatenate_present(left, right) result(value)
        type(standardir_target_expression_t), intent(in) :: left, right
        type(standardir_target_expression_t) :: value

        if (left%kind == 0) then
            value = right
        else if (right%kind == 0) then
            value = left
        else
            value = concatenate_expressions(left, right)
        end if
    end function concatenate_present

    recursive subroutine has_left_corner(expression, name, names, nullable, found)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=*), intent(in) :: name
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        logical, intent(out) :: found
        integer :: i

        found = .false.
        select case (expression%kind)
        case (standardir_grammar_reference)
            found = trim(expression%name) == trim(name)
        case (standardir_grammar_sequence)
            do i = 1, size(expression%children)
                call has_left_corner(expression%children(i), name, names, nullable, found)
                if (found) return
                if (.not. expression_nullable(expression%children(i), names, nullable)) return
            end do
        case (standardir_grammar_choice)
            do i = 1, size(expression%children)
                call has_left_corner(expression%children(i), name, names, nullable, found)
                if (found) return
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            call has_left_corner(expression%children(1), name, names, nullable, found)
        end select
    end subroutine has_left_corner

    subroutine reject_remaining_left_recursion(values, names, name_count, nullable, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: name_count
        logical, intent(in) :: nullable(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j
        logical :: found

        ok = .false.
        message = ''
        do i = 1, size(values)
            do j = 1, name_count
                if (trim(values(i)%lhs) == trim(names(j))) then
                    call has_left_corner(values(i)%expression, names(j), names, nullable, found)
                    if (found) then
                        message = 'left-recursion normalization left a cyclic grammar family'
                        return
                    end if
                end if
            end do
        end do
        ok = .true.
        message = ''
    end subroutine reject_remaining_left_recursion
end module standardir_grammar_targetnorm

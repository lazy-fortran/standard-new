module standardir_grammar_v0_export
    !! Canonical SX producer for one normalized StandardIR grammar rule.

    use fortsx, only: sx_atom, sx_clear, sx_list, sx_node_t
    use standardir_export, only: standardir_source_ref_t, standardir_validate_source_ref
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_node_t, standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_disputed, &
        standardir_grammar_resolution_resolved, standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_origin_differential, standardir_grammar_origin_imported, &
        standardir_grammar_origin_human, standardir_grammar_origin_llm, &
        standardir_grammar_origin_llm_repair, standardir_grammar_origin_mechanical, &
        standardir_grammar_origin_search, standardir_grammar_origin_smt, &
        standardir_grammar_rule_t, standardir_grammar_validate
    use standardir_grammar_target_records, only: standardir_target_expression_t, standardir_target_rule_t
    implicit none
    private

    integer, parameter :: max_nodes = 256

    public :: standardir_grammar_v0_produce

    interface standardir_grammar_v0_produce
        module procedure standardir_grammar_v0_produce_target
        module procedure standardir_grammar_v0_produce_flat
    end interface

contains

    subroutine standardir_grammar_v0_produce_target(rule, node, ok, message)
        type(standardir_target_rule_t), intent(in) :: rule
        type(sx_node_t), intent(inout) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: flat(max_nodes)
        integer :: count

        call sx_clear(node)
        ok = .false.
        message = ''
        call validate_rule(rule, ok, message)
        if (.not. ok) return
        count = 0
        call flatten(rule%expression, flat, count, ok, message, 1)
        if (.not. ok) return
        call validate_flat(flat, count, ok, message)
        if (.not. ok) return
        call make_record_fields(rule%id, rule%alternative, rule%lhs, flat(:count), &
            rule%provenance(1)%source, rule%origin, rule%resolution, node, ok, message)
        if (.not. ok) call sx_clear(node)
    end subroutine standardir_grammar_v0_produce_target

    subroutine standardir_grammar_v0_produce_flat(rule, node, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        type(sx_node_t), intent(inout) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call sx_clear(node)
        call standardir_grammar_validate(rule, ok, message)
        if (.not. ok) return
        if (rule%alternative < 1 .or. rule%root /= 1) then
            ok = .false.; message = 'grammar rule identity or root is invalid'; return
        end if
        call validate_flat(rule%nodes%values, size(rule%nodes%values), ok, message)
        if (.not. ok) return
        call make_record_fields(rule%id, rule%alternative, rule%lhs, rule%nodes%values, &
            rule%source, rule%origin, rule%resolution, node, ok, message)
        if (.not. ok) call sx_clear(node)
    end subroutine standardir_grammar_v0_produce_flat

    subroutine validate_rule(rule, ok, message)
        type(standardir_target_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (len_trim(rule%id) == 0 .or. len_trim(rule%lhs) == 0) then
            message = 'grammar rule identity is incomplete'
            return
        end if
        if (rule%alternative < 1) then
            message = 'grammar alternative is invalid'
            return
        end if
        if (rule%origin < standardir_grammar_origin_mechanical .or. &
                rule%origin > standardir_grammar_origin_differential) then
            message = 'grammar origin is invalid'
            return
        end if
        if (rule%resolution < standardir_grammar_resolution_resolved .or. &
                rule%resolution > standardir_grammar_resolution_disputed) then
            message = 'grammar resolution is invalid'
            return
        end if
        if (.not. allocated(rule%provenance) .or. size(rule%provenance) /= 1) then
            message = 'grammar provenance is missing or ambiguous'
            return
        end if
        if (rule%provenance(1)%alternative /= rule%alternative) then
            message = 'grammar provenance alternative is inconsistent'
            return
        end if
        call standardir_validate_source_ref(rule%provenance(1)%source, ok, message)
        if (.not. ok) return
        if (trim(rule%provenance(1)%source%rule) /= trim(rule%id)) then
            message = 'grammar provenance rule is inconsistent'
            return
        end if
        if (rule%expression%kind < standardir_grammar_reference .or. &
                rule%expression%kind > standardir_grammar_repeat) then
            message = 'grammar expression kind is invalid'
            return
        end if
        ok = .true.
    end subroutine validate_rule

    recursive subroutine flatten(expression, flat, count, ok, message, depth)
        type(standardir_target_expression_t), intent(in) :: expression
        type(standardir_grammar_node_t), intent(inout) :: flat(:)
        integer, intent(inout) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth

        integer :: first, i, child_count

        ok = .false.
        message = ''
        if (depth > max_nodes .or. count >= size(flat)) then
            message = 'grammar expression exceeds node capacity'
            return
        end if
        count = count + 1
        first = count
        flat(first) = standardir_grammar_node_t()
        flat(first)%name = '-'
        select case (expression%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            if (len_trim(expression%name) == 0 .or. allocated(expression%children)) then
                message = 'grammar leaf is malformed'
                return
            end if
            flat(first)%kind = expression%kind
            flat(first)%name = trim(expression%name)
            flat(first)%minimum = 1
        case (standardir_grammar_sequence, standardir_grammar_choice)
            if (.not. allocated(expression%children) .or. size(expression%children) < 1) then
                message = 'grammar group is empty'
                return
            end if
            flat(first)%kind = expression%kind
            flat(first)%minimum = 1
            flat(first)%child_count = size(expression%children)
            flat(first)%first_child = count + 1
            do i = 1, size(expression%children)
                call flatten(expression%children(i), flat, count, ok, message, depth + 1)
                if (.not. ok) return
            end do
        case (standardir_grammar_optional)
            if (.not. allocated(expression%children) .or. size(expression%children) /= 1) then
                message = 'grammar optional node is malformed'
                return
            end if
            flat(first)%kind = standardir_grammar_optional
            flat(first)%child_count = 1
            flat(first)%first_child = count + 1
            call flatten(expression%children(1), flat, count, ok, message, depth + 1)
            if (.not. ok) return
        case (standardir_grammar_repeat)
            if (.not. allocated(expression%children) .or. size(expression%children) /= 1 .or. &
                    expression%minimum < 0 .or. expression%minimum > 1 .or. .not. expression%unbounded) then
                message = 'grammar repeat node is malformed'
                return
            end if
            flat(first)%kind = standardir_grammar_repeat
            flat(first)%minimum = expression%minimum
            flat(first)%unbounded = .true.
            flat(first)%child_count = 1
            flat(first)%first_child = count + 1
            call flatten(expression%children(1), flat, count, ok, message, depth + 1)
            if (.not. ok) return
        case default
            message = 'grammar expression kind is invalid'
            return
        end select
        ok = .true.
    end subroutine flatten

    subroutine validate_flat(flat, count, ok, message)
        type(standardir_grammar_node_t), intent(in) :: flat(:)
        integer, intent(in) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, last

        ok = .false.
        message = ''
        if (count < 1 .or. count > size(flat)) then
            message = 'grammar node table is empty or oversized'
            return
        end if
        do i = 1, count
            if (flat(i)%first_child < 0 .or. flat(i)%child_count < 0) then
                message = 'grammar node offsets are negative'
                return
            end if
            if (flat(i)%child_count == 0 .and. flat(i)%first_child /= 0) then
                message = 'grammar leaf has a nonzero child offset'
                return
            end if
            if (flat(i)%child_count > 0) then
                if (flat(i)%first_child < 1) then
                    message = 'grammar child offset is invalid'
                    return
                end if
                last = flat(i)%first_child + flat(i)%child_count - 1
                if (last < flat(i)%first_child .or. last > count) then
                    message = 'grammar child offsets exceed node table'
                    return
                end if
            end if
        end do
        ok = .true.
    end subroutine validate_flat

    subroutine make_record_fields(id, alternative, lhs, flat, source, origin, resolution, node, ok, message)
        character(len=*), intent(in) :: id, lhs
        integer, intent(in) :: alternative, origin, resolution
        type(standardir_grammar_node_t), intent(in) :: flat(:)
        type(standardir_source_ref_t), intent(in) :: source
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call make_list(node, 9)
        call make_atom(node%children(1), 'syntax-rule')
        call make_pair(node%children(2), 'id', trim(id))
        call make_pair_int(node%children(3), 'alternative', alternative)
        call make_pair(node%children(4), 'lhs', trim(lhs))
        call make_pair_int(node%children(5), 'root', 1)
        call make_list(node%children(6), 2)
        call make_atom(node%children(6)%children(1), 'nodes')
        call make_list(node%children(6)%children(2), size(flat) + 1)
        call make_atom(node%children(6)%children(2)%children(1), 'grammar-nodes')
        do i = 1, size(flat)
            call make_node(node%children(6)%children(2)%children(i + 1), flat(i))
        end do
        call make_source(node%children(7), source)
        call make_pair_enum(node%children(8), 'origin', origin, .true.)
        call make_pair_enum(node%children(9), 'resolution', resolution, .false.)
        ok = .true.
        message = ''
    end subroutine make_record_fields

    subroutine make_node(node, value)
        type(sx_node_t), intent(out) :: node
        type(standardir_grammar_node_t), intent(in) :: value
        call make_list(node, 7)
        call make_atom(node%children(1), 'grammar-node')
        call make_atom(node%children(2), kind_name(value%kind))
        call make_atom(node%children(3), trim(value%name))
        call make_atom_int(node%children(4), value%minimum)
        call make_atom(node%children(5), merge('true ', 'false', value%unbounded))
        call make_atom_int(node%children(6), value%first_child)
        call make_atom_int(node%children(7), value%child_count)
    end subroutine make_node

    subroutine make_source(node, source)
        use standardir_export, only: standardir_source_ref_t
        type(sx_node_t), intent(out) :: node
        type(standardir_source_ref_t), intent(in) :: source
        call make_list(node, 2)
        call make_atom(node%children(1), 'source')
        call make_list(node%children(2), 6)
        call make_atom(node%children(2)%children(1), 'source-ref')
        call make_pair(node%children(2)%children(2), 'document', trim(source%document))
        call make_pair(node%children(2)%children(3), 'clause', trim(source%clause))
        call make_pair(node%children(2)%children(4), 'rule', trim(source%rule))
        call make_pair_int(node%children(2)%children(5), 'page', source%page)
        call make_pair(node%children(2)%children(6), 'source-hash', trim(source%source_hash))
    end subroutine make_source

    subroutine make_pair(node, label, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label, value
        call make_list(node, 2)
        call make_atom(node%children(1), label)
        call make_atom(node%children(2), value)
    end subroutine make_pair

    subroutine make_pair_int(node, label, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label
        integer, intent(in) :: value
        call make_list(node, 2)
        call make_atom(node%children(1), label)
        call make_atom_int(node%children(2), value)
    end subroutine make_pair_int

    subroutine make_pair_enum(node, label, value, origin)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label
        integer, intent(in) :: value
        logical, intent(in) :: origin
        if (origin) then
            call make_pair(node, label, origin_name(value))
        else
            call make_pair(node, label, resolution_name(value))
        end if
    end subroutine make_pair_enum

    subroutine make_list(node, count)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: count
        node%kind = sx_list
        node%child_count = count
        allocate (node%children(count))
    end subroutine make_list

    subroutine make_atom(node, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: value
        node%kind = sx_atom
        node%child_count = 0
        node%atom = trim(value)
    end subroutine make_atom

    subroutine make_atom_int(node, value)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
        call make_atom(node, trim(text))
    end subroutine make_atom_int

    function kind_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=16) :: name
        character(len=16), parameter :: names(6) = [ character(len=16) :: &
            'reference', 'token', 'sequence', 'choice', 'optional', 'repeat' ]
        name = names(kind)
    end function kind_name

    function origin_name(value) result(name)
        integer, intent(in) :: value
        character(len=16) :: name
        character(len=16), parameter :: names(8) = [ character(len=16) :: &
            'mechanical', 'search', 'smt', 'llm', 'llm-repair', 'human', 'imported', 'differential' ]
        name = names(value)
    end function origin_name

    function resolution_name(value) result(name)
        integer, intent(in) :: value
        character(len=16) :: name
        character(len=16), parameter :: names(3) = [ character(len=16) :: &
            'resolved', 'unresolved', 'disputed' ]
        name = names(value)
    end function resolution_name

end module standardir_grammar_v0_export

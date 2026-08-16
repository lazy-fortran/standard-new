module standardir_grammar_sx_adapter
    !! Bounded, transactional adapter from raw StandardIR SX to typed rules.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_node_t, standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_origin_differential, standardir_grammar_origin_mechanical, &
        standardir_grammar_repeat, standardir_grammar_resolution_disputed, &
        standardir_grammar_resolution_resolved, standardir_grammar_resolution_unresolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_grammar_sx_adapter_support, only: read_syntax, trim_expression, &
        validate_metadata
    use standardir_grammar_source_fingerprint, only: standardir_grammar_source_expression_sha256
    implicit none
    private

    integer, parameter, public :: standardir_grammar_adapter_max_nodes = 256

    public :: standardir_grammar_adapt_sx

contains

    subroutine standardir_grammar_adapt_sx(node, origin, resolution, values, ok, message, source_node)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: origin, resolution
        type(standardir_grammar_rule_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(sx_node_t), intent(in), optional :: source_node

        type(standardir_grammar_rule_t), allocatable :: staged(:)
        type(standardir_source_ref_t) :: source
        type(sx_node_t) :: expression
        character(len=128) :: rule, lhs
        character(len=64), allocatable :: raw_hashes(:)
        integer :: alternative_count, i, count, cursor, first

        if (allocated(values)) deallocate (values)
        ok = .false.
        message = ''
        call validate_metadata(origin, resolution, ok, message)
        if (.not. ok) return
        call read_syntax(node, rule, lhs, expression, source, ok, message)
        if (.not. ok) return
        if (present(source_node)) then
            call standardir_grammar_capture_source_identity(source_node, raw_hashes, ok, message)
        else
            call standardir_grammar_capture_source_identity(node, raw_hashes, ok, message)
        end if
        if (.not. ok) return
        if (trim(source%rule) /= trim(rule)) then
            message = 'syntax rule and source rule differ'
            return
        end if
        call alternative_count_for(expression, alternative_count, ok, message)
        if (.not. ok) return
        allocate (staged(alternative_count))
        do i = 1, alternative_count
            if (trim_expression(expression) == 'alt') then
                staged(i) = standardir_grammar_rule_t()
                call copy_alternative(expression, i, staged(i), rule, lhs, source, origin, resolution, &
                    raw_hashes, 'rhs/'//integer_text(i), ok, message)
            else
                staged(i) = standardir_grammar_rule_t()
                call copy_expression(expression, staged(i), rule, lhs, source, origin, resolution, 1, &
                    raw_hashes, 'rhs', ok, message)
            end if
            if (.not. ok) then
                deallocate (staged)
                return
            end if
        end do
        call validate_staged(staged, ok, message)
        if (.not. ok) then
            deallocate (staged)
            return
        end if
        call move_alloc(staged, values)
    end subroutine standardir_grammar_adapt_sx

    subroutine alternative_count_for(node, count, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: label
        integer :: child_count

        count = 0
        call expression_shape(node, child_count, ok, message, 1)
        if (.not. ok) return
        label = trim_expression(node)
        if (trim(label) == 'alt') then
            count = node%child_count - 1
        else
            count = 1
        end if
        if (count < 1) then
            ok = .false.
            message = 'syntax has no alternatives'
        end if
    end subroutine alternative_count_for

    recursive subroutine expression_shape(node, count, ok, message, depth)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth

        character(len=32) :: label
        integer :: i, child_count

        count = 0
        ok = .false.
        message = ''
        if (depth > standardir_grammar_adapter_max_nodes) then
            message = 'grammar expression is cyclic or exceeds depth bound'
            return
        end if
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'grammar expression is not a nonempty list'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'grammar expression label is malformed'
            return
        end if
        label = trim(node%children(1)%atom)
        select case (trim(label))
        case ('ref', 'token')
            if (node%child_count /= 2) then
                message = 'grammar leaf has the wrong field count'
                return
            end if
            if (node%children(2)%kind /= sx_atom .or. &
                len_trim(node%children(2)%atom) == 0) then
                message = 'grammar leaf name is malformed'
                return
            end if
            count = 1
        case ('seq', 'alt')
            if (node%child_count < 2) then
                message = 'grammar group is empty'
                return
            end if
            count = 1
            do i = 2, node%child_count
                call expression_shape(node%children(i), child_count, ok, message, depth + 1)
                if (.not. ok) return
                count = count + child_count
            end do
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional expression has the wrong field count'
                return
            end if
            call expression_shape(node%children(2), child_count, ok, message, depth + 1)
            if (.not. ok) return
            count = 1 + child_count
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat expression has the wrong field count'
                return
            end if
            if (node%children(3)%kind /= sx_atom .or. &
                node%children(4)%kind /= sx_atom .or. &
                trim(node%children(4)%atom) /= 'unbounded') then
                message = 'repeat expression metadata is unsupported'
                return
            end if
            if (trim(node%children(3)%atom) /= '0' .and. &
                trim(node%children(3)%atom) /= '1') then
                message = 'repeat minimum is unsupported'
                return
            end if
            call expression_shape(node%children(2), child_count, ok, message, depth + 1)
            if (.not. ok) return
            count = 1 + child_count
        case default
            message = 'unsupported grammar expression: '//trim(label)
            return
        end select
        if (count > standardir_grammar_adapter_max_nodes) then
            message = 'grammar expression exceeds node bound'
            return
        end if
        ok = .true.
    end subroutine expression_shape

    subroutine copy_alternative(expression, alternative, value, rule, lhs, source, origin, &
            resolution, raw_hashes, source_path, ok, message)
        type(sx_node_t), intent(in) :: expression
        integer, intent(in) :: alternative, origin, resolution
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: rule, lhs
        type(standardir_source_ref_t), intent(in) :: source
        character(len=64), intent(in) :: raw_hashes(:)
        character(len=*), intent(in) :: source_path
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t) :: selected

        selected = expression%children(alternative + 1)
        call copy_expression(selected, value, rule, lhs, source, origin, resolution, alternative, &
            raw_hashes, source_path, ok, message)
    end subroutine copy_alternative

    subroutine copy_expression(expression, value, rule, lhs, source, origin, resolution, &
            alternative, raw_hashes, source_path, ok, message)
        type(sx_node_t), intent(in) :: expression
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: rule, lhs
        type(standardir_source_ref_t), intent(in) :: source
        integer, intent(in) :: origin, resolution, alternative
        character(len=64), intent(in) :: raw_hashes(:)
        character(len=*), intent(in) :: source_path
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: count, cursor, first

        call expression_shape(expression, count, ok, message, 1)
        if (.not. ok) return
        allocate (value%nodes%values(count))
        cursor = 0
        call append_expression(expression, value%nodes%values, cursor, first, ok, message, 1, source_path)
        if (.not. ok) return
        if (alternative < 1 .or. alternative > size(raw_hashes)) then
            ok = .false.
            message = 'source expression identity alternative is outside its raw source table'
            return
        end if
        value%source_expression_sha256 = raw_hashes(alternative)
        value%id = trim(rule)
        value%alternative = alternative
        value%lhs = trim(lhs)
        value%root = first
        value%source = source
        value%origin = origin
        value%resolution = resolution
    end subroutine copy_expression

    subroutine standardir_grammar_capture_source_identity(node, hashes, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=64), allocatable, intent(out) :: hashes(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t) :: expression
        type(standardir_source_ref_t) :: source
        character(len=128) :: rule, lhs
        integer :: count, i

        allocate (hashes(0))
        call read_syntax(node, rule, lhs, expression, source, ok, message)
        if (.not. ok) return
        call alternative_count_for(expression, count, ok, message)
        if (.not. ok) return
        deallocate (hashes)
        allocate (hashes(count))
        do i = 1, count
            if (trim_expression(expression) == 'alt') then
                call standardir_grammar_source_expression_sha256(expression%children(i + 1), hashes(i), ok, message)
            else
                call standardir_grammar_source_expression_sha256(expression, hashes(i), ok, message)
            end if
            if (.not. ok) return
        end do
    end subroutine standardir_grammar_capture_source_identity

    recursive subroutine append_expression(node, values, cursor, first, ok, message, depth, source_path)
        type(sx_node_t), intent(in) :: node
        type(standardir_grammar_node_t), intent(inout) :: values(:)
        integer, intent(inout) :: cursor
        integer, intent(out) :: first
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, intent(in) :: depth
        character(len=*), intent(in) :: source_path

        character(len=32) :: label
        integer :: i, child_first, minimum, ios

        first = 0
        ok = .false.
        message = ''
        if (depth > standardir_grammar_adapter_max_nodes) then
            message = 'grammar expression is cyclic or exceeds depth bound'
            return
        end if
        label = trim_expression(node)
        cursor = cursor + 1
        if (cursor > size(values)) then
            message = 'grammar expression node count changed during adaptation'
            return
        end if
        first = cursor
        values(first) = standardir_grammar_node_t()
        values(first)%name = '-'
        values(first)%source_expression_path = trim(source_path)
        values(first)%minimum = 1
        select case (trim(label))
        case ('ref', 'token')
            values(first)%kind = merge(standardir_grammar_token, standardir_grammar_reference, &
                trim(label) == 'token')
            values(first)%name = trim(node%children(2)%atom)
        case ('seq', 'alt')
            values(first)%kind = merge(standardir_grammar_choice, standardir_grammar_sequence, &
                trim(label) == 'alt')
            values(first)%child_count = node%child_count - 1
            values(first)%first_child = cursor + 1
            do i = 2, node%child_count
                call append_expression(node%children(i), values, cursor, child_first, ok, message, &
                    depth + 1, trim(source_path)//'/'//integer_text(i - 1))
                if (.not. ok) return
            end do
        case ('optional', 'repeat')
            values(first)%kind = merge(standardir_grammar_repeat, standardir_grammar_optional, &
                trim(label) == 'repeat')
            values(first)%minimum = 0
            if (trim(label) == 'repeat') then
                read (node%children(3)%atom, *, iostat=ios) minimum
                if (ios /= 0) then
                    message = 'repeat minimum is not an integer'
                    return
                end if
                values(first)%minimum = minimum
                values(first)%unbounded = .true.
            end if
            values(first)%child_count = 1
            values(first)%first_child = cursor + 1
            call append_expression(node%children(2), values, cursor, child_first, ok, message, &
                depth + 1, trim(source_path)//'/1')
            if (.not. ok) return
        case default
            message = 'unsupported grammar expression: '//trim(label)
            return
        end select
        ok = .true.
    end subroutine append_expression

    subroutine validate_staged(values, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = .false.
        message = ''
        do i = 1, size(values)
            call standardir_grammar_validate(values(i), ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine validate_staged

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function integer_text

end module standardir_grammar_sx_adapter

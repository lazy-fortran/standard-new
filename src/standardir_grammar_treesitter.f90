module standardir_grammar_treesitter
    !! Tree-sitter-specific lowering for source-backed grammar targets.

    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_target_support, only: append_expression, &
        contains_expression, refresh_source_witnesses, same_expression, &
        standardir_target_expression_t, standardir_target_rule_t
    use standardir_grammar_target_fingerprint, only: standardir_target_expression_sha256
    implicit none
    private

    integer, parameter :: max_nullable_sequence_variants = 256

    public :: standardir_grammar_lower_treesitter

contains

    subroutine standardir_grammar_lower_treesitter(values, entry_root, entry_nullable, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in), optional :: entry_root
        logical, intent(out) :: entry_nullable
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: working(:)
        type(standardir_target_expression_t) :: full, positive
        character(len=128), allocatable :: names(:)
        logical, allocatable :: nullable(:), changed(:)
        logical :: full_nullable, has_positive, needs_positive
        character(len=128) :: start_lhs
        character(len=512) :: detail
        integer :: i, start_index

        entry_nullable = .false.
        ok = .false.
        message = ''
        if (size(values) < 1) then
            message = 'target-disposition=unsupported reason=empty-tree-sitter-target'
            return
        end if
        working = values
        call collect_names(working, names)
        call compute_nullable(working, names, nullable)

        if (present(entry_root)) then
            if (len_trim(entry_root) == 0) then
                message = 'target-disposition=unsupported reason=empty-tree-sitter-entry-root'
                return
            end if
            start_lhs = trim(entry_root)
            start_index = find_name(names, start_lhs)
            if (start_index == 0) then
                message = 'target-disposition=unsupported reason=missing-tree-sitter-entry-root lhs='// &
                    trim(start_lhs)
                return
            end if
            entry_nullable = nullable(start_index)
        else
            start_lhs = trim(working(1)%lhs)
            start_index = find_name(names, start_lhs)
            entry_nullable = nullable(start_index)
        end if

        allocate (changed(size(working)))
        changed = .false.
        do i = 1, size(working)
            call lower_expression(working(i)%expression, names, nullable, full, positive, &
                full_nullable, has_positive, ok, message)
            if (.not. ok) then
                detail = trim(message)
                write (message, '(a,a,a,a,i0,a,a)') trim(detail), ' source-rule=', &
                    trim(working(i)%source%rule), ' source-alternative=', working(i)%alternative, &
                    ' lhs=', trim(working(i)%lhs)
                return
            end if
            needs_positive = (present(entry_root) .or. trim(working(i)%lhs) /= trim(start_lhs)) .and. full_nullable
            if (needs_positive) then
                if (.not. has_positive) then
                    write (message, '(a,a,a,a,i0,a,a)') &
                        'target-disposition=unsupported reason=nullable-rule-has-no-nonempty-form ', &
                        'source-rule=', trim(working(i)%source%rule), ' source-alternative=', &
                        working(i)%alternative, ' lhs=', trim(working(i)%lhs)
                    return
                end if
                working(i)%expression = positive
            else
                working(i)%expression = full
            end if
            changed(i) = needs_positive .or. &
                .not. same_expression(values(i)%expression, working(i)%expression)
        end do

        do i = 1, size(working)
            call standardir_target_expression_sha256(working(i)%expression, &
                working(i)%target_expression_sha256, ok, message)
            if (.not. ok) return
        end do
        call refresh_source_witnesses(working, ok, message)
        if (.not. ok) return
        do i = 1, size(working)
            if (.not. changed(i)) cycle
            if (.not. allocated(working(i)%source_witnesses)) cycle
            call mark_transformation_witnesses(working(i))
        end do
        call move_alloc(working, values)
        ok = .true.
        message = ''
    end subroutine standardir_grammar_lower_treesitter

    recursive subroutine lower_expression(expression, names, nullable, full, positive, &
            full_nullable, has_positive, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        type(standardir_target_expression_t), intent(out) :: full, positive
        logical, intent(out) :: full_nullable, has_positive, ok
        character(len=*), intent(out) :: message

        type(standardir_target_expression_t), allocatable :: full_children(:), positive_children(:)
        type(standardir_target_expression_t) :: child_full, child_positive
        logical, allocatable :: child_nullable(:), child_has_positive(:)
        logical :: child_nullable_value
        integer :: i

        full = standardir_target_expression_t()
        positive = standardir_target_expression_t()
        full_nullable = .false.
        has_positive = .false.
        ok = .false.
        message = ''
        select case (expression%kind)
        case (standardir_grammar_reference)
            full = make_reference(expression%name)
            if (find_name(names, trim(expression%name)) > 0) then
                full_nullable = nullable(find_name(names, trim(expression%name)))
            end if
            has_positive = .true.
            positive = make_reference(expression%name)
            if (full_nullable) full = make_optional(full)
        case (standardir_grammar_token)
            full = expression
            positive = expression
            has_positive = .true.
        case (standardir_grammar_optional)
            if (.not. valid_children(expression, 1)) then
                message = 'target-disposition=unsupported reason=malformed-optional'
                return
            end if
            call lower_expression(expression%children(1), names, nullable, child_full, child_positive, &
                full_nullable, has_positive, ok, message)
            if (.not. ok) return
            full = make_optional(child_full)
            full_nullable = .true.
            positive = child_positive
        case (standardir_grammar_repeat)
            if (.not. valid_children(expression, 1)) then
                message = 'target-disposition=unsupported reason=malformed-repeat'
                return
            end if
            call lower_expression(expression%children(1), names, nullable, child_full, child_positive, &
                child_nullable_value, has_positive, ok, message)
            if (.not. ok) return
            if (child_nullable_value) then
                if (.not. has_positive) then
                    message = 'target-disposition=unsupported reason=nullable-repeat-has-no-positive-operand'
                    return
                end if
                full = make_repeat(child_positive, 0, expression%unbounded)
            else
                full = make_repeat(child_full, expression%minimum, expression%unbounded)
            end if
            full_nullable = expression%minimum == 0 .or. child_nullable_value
            if (has_positive) positive = make_repeat(child_positive, 1, expression%unbounded)
        case (standardir_grammar_sequence)
            if (.not. allocated(expression%children)) then
                message = 'target-disposition=unsupported reason=empty-sequence'
                return
            end if
            if (size(expression%children) < 1) then
                message = 'target-disposition=unsupported reason=empty-sequence'
                return
            end if
            allocate (full_children(size(expression%children)), positive_children(size(expression%children)), &
                child_nullable(size(expression%children)), child_has_positive(size(expression%children)))
            do i = 1, size(expression%children)
                call lower_expression(expression%children(i), names, nullable, full_children(i), &
                    child_positive, child_nullable(i), child_has_positive(i), ok, message)
                if (.not. ok) return
                positive_children(i) = child_positive
            end do
            full = make_sequence(full_children)
            full_nullable = all(child_nullable)
            if (.not. full_nullable) then
                positive = full
                has_positive = .true.
            else
                call positive_sequence(positive_children, child_nullable, child_has_positive, positive, &
                    has_positive, ok, message)
                if (.not. ok) return
            end if
        case (standardir_grammar_choice)
            if (.not. allocated(expression%children)) then
                message = 'target-disposition=unsupported reason=empty-choice'
                return
            end if
            if (size(expression%children) < 1) then
                message = 'target-disposition=unsupported reason=empty-choice'
                return
            end if
            allocate (full_children(size(expression%children)), child_nullable(size(expression%children)), &
                child_has_positive(size(expression%children)), positive_children(0))
            do i = 1, size(expression%children)
                call lower_expression(expression%children(i), names, nullable, full_children(i), &
                    child_positive, child_nullable(i), child_has_positive(i), ok, message)
                if (.not. ok) return
                if (child_has_positive(i)) call append_expression(positive_children, child_positive)
            end do
            full = make_choice(full_children)
            full_nullable = any(child_nullable)
            has_positive = size(positive_children) > 0
            if (has_positive) positive = make_choice(positive_children)
        case default
            message = 'target-disposition=unsupported reason=unknown-expression-kind'
            return
        end select
        ok = .true.
        message = ''
    end subroutine lower_expression

    subroutine positive_sequence(children, child_nullable, child_has_positive, positive, has_positive, &
            ok, message)
        type(standardir_target_expression_t), intent(in) :: children(:)
        logical, intent(in) :: child_nullable(:), child_has_positive(:)
        type(standardir_target_expression_t), intent(out) :: positive
        logical, intent(out) :: has_positive, ok
        character(len=*), intent(out) :: message

        type(standardir_target_expression_t), allocatable :: variants(:), next(:)
        type(standardir_target_expression_t) :: value
        integer :: i, j

        positive = standardir_target_expression_t()
        has_positive = .false.
        ok = .false.
        message = ''
        allocate (variants(1))
        variants(1) = standardir_target_expression_t()
        do i = 1, size(children)
            if (child_nullable(i)) then
                if (size(variants) > max_nullable_sequence_variants / 2) then
                    message = 'target-disposition=unsupported reason=nullable-sequence-expansion-bound'
                    return
                end if
            end if
            allocate (next(0))
            do j = 1, size(variants)
                if (child_nullable(i)) call append_unique_expression(next, variants(j))
                if (child_has_positive(i)) then
                    value = append_sequence_part(variants(j), children(i))
                    call append_unique_expression(next, value)
                end if
            end do
            call move_alloc(next, variants)
            if (size(variants) > max_nullable_sequence_variants) then
                message = 'target-disposition=unsupported reason=nullable-sequence-expansion-bound'
                return
            end if
        end do
        allocate (next(0))
        do i = 1, size(variants)
            if (variants(i)%kind /= 0) call append_unique_expression(next, variants(i))
        end do
        if (size(next) == 0) then
            ok = .true.
            return
        end if
        if (size(next) == 1) then
            positive = next(1)
        else
            positive = make_choice(next)
        end if
        has_positive = .true.
        ok = .true.
        message = ''
    end subroutine positive_sequence

    subroutine mark_transformation_witnesses(value)
        type(standardir_target_rule_t), intent(inout) :: value
        integer :: i

        do i = 1, size(value%source_witnesses)
            value%source_witnesses(i)%reason = 'tree-sitter-nullable-lowering'
        end do
    end subroutine mark_transformation_witnesses

    subroutine compute_nullable(values, names, nullable)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), intent(in) :: names(:)
        logical, allocatable, intent(out) :: nullable(:)
        logical :: changed, value
        integer :: i, j

        allocate (nullable(size(names)))
        nullable = .false.
        do
            changed = .false.
            do i = 1, size(values)
                value = expression_nullable(values(i)%expression, names, nullable)
                if (.not. value) cycle
                j = find_name(names, trim(values(i)%lhs))
                if (j > 0) then
                    if (.not. nullable(j)) then
                        nullable(j) = .true.
                        changed = .true.
                    end if
                end if
            end do
            if (.not. changed) exit
        end do
    end subroutine compute_nullable

    recursive logical function expression_nullable(expression, names, nullable) result(value)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        integer :: i, index

        select case (expression%kind)
        case (standardir_grammar_reference)
            index = find_name(names, trim(expression%name))
            value = .false.
            if (index > 0) value = nullable(index)
        case (standardir_grammar_token)
            value = .false.
        case (standardir_grammar_sequence)
            value = .true.
            if (.not. allocated(expression%children)) return
            do i = 1, size(expression%children)
                if (.not. expression_nullable(expression%children(i), names, nullable)) then
                    value = .false.
                    return
                end if
            end do
        case (standardir_grammar_choice)
            value = .false.
            if (.not. allocated(expression%children)) return
            do i = 1, size(expression%children)
                if (expression_nullable(expression%children(i), names, nullable)) then
                    value = .true.
                    return
                end if
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            value = expression%minimum == 0
            if (.not. value .and. allocated(expression%children)) then
                value = expression_nullable(expression%children(1), names, nullable)
            end if
        case default
            value = .false.
        end select
    end function expression_nullable

    subroutine collect_names(values, names)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), allocatable, intent(out) :: names(:)
        integer :: i

        allocate (names(0))
        do i = 1, size(values)
            if (find_name(names, trim(values(i)%lhs)) == 0) call append_name(names, trim(values(i)%lhs))
        end do
    end subroutine collect_names

    integer function find_name(names, value)
        character(len=128), intent(in) :: names(:)
        character(len=*), intent(in) :: value
        integer :: i

        find_name = 0
        do i = 1, size(names)
            if (trim(names(i)) == trim(value)) then
                find_name = i
                return
            end if
        end do
    end function find_name

    subroutine append_name(values, value)
        character(len=128), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: value
        character(len=128), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = trim(value)
        call move_alloc(expanded, values)
    end subroutine append_name

    subroutine append_unique_expression(values, value)
        type(standardir_target_expression_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_expression_t), intent(in) :: value
        logical :: found

        call contains_expression(values, value, found)
        if (.not. found) call append_expression(values, value)
    end subroutine append_unique_expression

    logical function valid_children(expression, expected)
        type(standardir_target_expression_t), intent(in) :: expression
        integer, intent(in) :: expected

        valid_children = .false.
        if (.not. allocated(expression%children)) return
        valid_children = size(expression%children) == expected
    end function valid_children

    function make_reference(name) result(value)
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_reference
        value%name = trim(name)
    end function make_reference

    function make_optional(child) result(value)
        type(standardir_target_expression_t), intent(in) :: child
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_optional
        allocate (value%children(1))
        value%children(1) = child
    end function make_optional

    function make_repeat(child, minimum, unbounded) result(value)
        type(standardir_target_expression_t), intent(in) :: child
        integer, intent(in) :: minimum
        logical, intent(in) :: unbounded
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_repeat
        value%minimum = minimum
        value%unbounded = unbounded
        allocate (value%children(1))
        value%children(1) = child
    end function make_repeat

    function make_sequence(children) result(value)
        type(standardir_target_expression_t), intent(in) :: children(:)
        type(standardir_target_expression_t) :: value

        if (size(children) == 1) then
            value = children(1)
            return
        end if
        value = standardir_target_expression_t()
        value%kind = standardir_grammar_sequence
        allocate (value%children(size(children)))
        value%children = children
    end function make_sequence

    function make_choice(children) result(value)
        type(standardir_target_expression_t), intent(in) :: children(:)
        type(standardir_target_expression_t) :: value

        if (size(children) == 1) then
            value = children(1)
            return
        end if
        value = standardir_target_expression_t()
        value%kind = standardir_grammar_choice
        allocate (value%children(size(children)))
        value%children = children
    end function make_choice

    function append_sequence_part(prefix, child) result(value)
        type(standardir_target_expression_t), intent(in) :: prefix, child
        type(standardir_target_expression_t) :: value
        type(standardir_target_expression_t), allocatable :: parts(:)
        integer :: i

        if (prefix%kind == 0) then
            value = child
            return
        end if
        if (child%kind == 0) then
            value = prefix
            return
        end if
        allocate (parts(0))
        if (prefix%kind == standardir_grammar_sequence) then
            do i = 1, size(prefix%children)
                call append_expression(parts, prefix%children(i))
            end do
        else
            call append_expression(parts, prefix)
        end if
        if (child%kind == standardir_grammar_sequence) then
            do i = 1, size(child%children)
                call append_expression(parts, child%children(i))
            end do
        else
            call append_expression(parts, child)
        end if
        value = make_sequence(parts)
    end function append_sequence_part

end module standardir_grammar_treesitter

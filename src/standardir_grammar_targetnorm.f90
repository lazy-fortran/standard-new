module standardir_grammar_targetnorm
    !! Graph-derived normalization for source-backed grammar targets.

    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_node_t, &
        standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    implicit none
    private

    public :: standardir_grammar_normalize
    public :: standardir_target_expression_t
    public :: standardir_target_rule_t

    type, public :: standardir_target_expression_t
        integer :: kind = 0
        character(len=128) :: name = ''
        integer :: minimum = 0
        logical :: unbounded = .false.
        type(standardir_target_expression_t), allocatable :: children(:)
    end type standardir_target_expression_t

    type, public :: standardir_target_rule_t
        character(len=128) :: id = ''
        integer :: alternative = 0
        character(len=128) :: lhs = ''
        type(standardir_target_expression_t) :: expression
        type(standardir_source_ref_t) :: source
        integer :: origin = 0
        integer :: resolution = 0
    end type standardir_target_rule_t

contains

    subroutine standardir_grammar_normalize(rules, normalized, suppressed, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        type(standardir_target_rule_t), allocatable, intent(out) :: normalized(:), suppressed(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: working(:)
        character(len=128), allocatable :: names(:)
        logical, allocatable :: nullable(:)
        type(standardir_target_rule_t) :: item
        type(standardir_target_expression_t) :: normalized_expression
        integer :: i, name_count

        if (allocated(normalized)) deallocate (normalized)
        if (allocated(suppressed)) deallocate (suppressed)
        allocate (working(0), suppressed(0))
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
            call normalize_expression(working(i)%expression, names, nullable, &
                normalized_expression, ok, message)
            if (.not. ok) return
            working(i)%expression = normalized_expression
        end do
        call deduplicate_rules(working, suppressed, ok, message)
        if (.not. ok) return
        call eliminate_left_recursion(working, suppressed, names, name_count, nullable, ok, message)
        if (.not. ok) return
        call deduplicate_rules(working, suppressed, ok, message)
        if (.not. ok) return
        call reject_remaining_left_recursion(working, names, name_count, nullable, ok, message)
        if (.not. ok) return
        if (size(working) < 1) then
            message = 'grammar normalization removed every alternative'
            return
        end if
        call move_alloc(working, normalized)
        ok = .true.
        message = ''
    end subroutine standardir_grammar_normalize

    subroutine rule_to_target(rule, value, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        type(standardir_target_rule_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

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
        value%origin = rule%origin
        value%resolution = rule%resolution
        call build_target_expression(rule, rule%root, 0, value%expression, ok, message)
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

    subroutine collect_lhs_names(values, names, name_count)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), allocatable, intent(out) :: names(:)
        integer, intent(out) :: name_count

        integer :: i, j
        logical :: found

        allocate (names(0))
        name_count = 0
        do i = 1, size(values)
            found = .false.
            do j = 1, name_count
                if (trim(names(j)) == trim(values(i)%lhs)) found = .true.
            end do
            if (.not. found) then
                name_count = name_count + 1
                call append_name(names, trim(values(i)%lhs))
            end if
        end do
    end subroutine collect_lhs_names

    subroutine append_name(names, value)
        character(len=128), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: value
        character(len=128), allocatable :: expanded(:)
        integer :: n

        n = size(names)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = names
        expanded(n + 1) = trim(value)
        call move_alloc(expanded, names)
    end subroutine append_name

    subroutine append_target(values, value)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), intent(in) :: value
        type(standardir_target_rule_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_target

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

    recursive subroutine normalize_expression(expression, names, nullable, result, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        logical, intent(in) :: nullable(:)
        type(standardir_target_expression_t), intent(out) :: result
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_expression_t), allocatable :: children(:)
        type(standardir_target_expression_t) :: child
        integer :: i, j
        logical :: duplicate

        result = standardir_target_expression_t()
        ok = .false.
        message = ''
        if (expression%kind == standardir_grammar_reference .or. &
            expression%kind == standardir_grammar_token) then
            result = expression
            ok = .true.
            return
        end if
        if (.not. allocated(expression%children) .or. size(expression%children) < 1) then
            message = 'grammar normalization encountered an empty expression'
            return
        end if
        allocate (children(0))
        do i = 1, size(expression%children)
            call normalize_expression(expression%children(i), names, nullable, child, ok, message)
            if (.not. ok) return
            if (expression%kind == standardir_grammar_sequence .and. &
                child%kind == standardir_grammar_sequence) then
                do j = 1, size(child%children)
                    call append_expression(children, child%children(j))
                end do
            else if (expression%kind == standardir_grammar_choice .and. &
                    child%kind == standardir_grammar_choice) then
                do j = 1, size(child%children)
                    duplicate = .false.
                    call contains_expression(children, child%children(j), duplicate)
                    if (.not. duplicate) call append_expression(children, child%children(j))
                end do
            else
                duplicate = .false.
                if (expression%kind == standardir_grammar_choice) &
                    call contains_expression(children, child, duplicate)
                if (.not. duplicate) call append_expression(children, child)
            end if
        end do
        if (size(children) < 1) then
            message = 'grammar normalization produced an empty expression'
            return
        end if
        result = expression
        call move_alloc(children, result%children)
        if (result%kind == standardir_grammar_optional .and. &
            expression_nullable(result%children(1), names, nullable)) then
            result = result%children(1)
        else if (result%kind == standardir_grammar_repeat .and. &
                result%children(1)%kind == standardir_grammar_optional) then
            result%minimum = 0
            result%children(1) = result%children(1)%children(1)
        end if
        if ((result%kind == standardir_grammar_sequence .or. &
            result%kind == standardir_grammar_choice) .and. size(result%children) == 1) then
            result = result%children(1)
        end if
        ok = .true.
        message = ''
    end subroutine normalize_expression

    subroutine append_expression(values, value)
        type(standardir_target_expression_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_expression_t), intent(in) :: value
        type(standardir_target_expression_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_expression

    subroutine contains_expression(values, value, found)
        type(standardir_target_expression_t), intent(in) :: values(:), value
        logical, intent(out) :: found
        integer :: i

        found = .false.
        do i = 1, size(values)
            if (same_expression(values(i), value)) then
                found = .true.
                return
            end if
        end do
    end subroutine contains_expression

    recursive logical function same_expression(left, right) result(equal)
        type(standardir_target_expression_t), intent(in) :: left, right
        integer :: i

        equal = .false.
        if (left%kind /= right%kind) return
        if (trim(left%name) /= trim(right%name)) return
        if (left%minimum /= right%minimum) return
        if (left%unbounded .neqv. right%unbounded) return
        if (allocated(left%children) .neqv. allocated(right%children)) then
            return
        end if
        if (.not. allocated(left%children)) then
            equal = .true.
            return
        end if
        if (size(left%children) /= size(right%children)) return
        do i = 1, size(left%children)
            if (.not. same_expression(left%children(i), right%children(i))) return
        end do
        equal = .true.
    end function same_expression

    subroutine deduplicate_rules(values, suppressed, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: suppressed(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: unique(:)
        integer :: i, j
        logical :: duplicate

        allocate (unique(0))
        do i = 1, size(values)
            duplicate = .false.
            do j = 1, size(unique)
                if (trim(unique(j)%lhs) == trim(values(i)%lhs) .and. &
                    same_expression(unique(j)%expression, values(i)%expression)) then
                    duplicate = .true.
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
        if (expression%kind /= standardir_grammar_sequence .or. &
            .not. allocated(expression%children) .or. size(expression%children) < 1) return
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

module standardir_grammar_export
    !! Batch export for normalized, source-backed StandardIR grammar rules.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_bison, only: standardir_emit_bison_group
    use standardir_grammar, only: standardir_emit_antlr_group, standardir_emit_ebnf_group
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_node_t, &
        standardir_grammar_sequence, standardir_grammar_token, &
        standardir_grammar_validate
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_groups, standardir_max_syntax_records
    use standardir_treesitter, only: standardir_emit_treesitter_group
    implicit none
    private

    integer, parameter, public :: standardir_grammar_format_ebnf = 1
    integer, parameter, public :: standardir_grammar_format_antlr4 = 2
    integer, parameter, public :: standardir_grammar_format_bison = 3
    integer, parameter, public :: standardir_grammar_format_tree_sitter = 4

    public :: standardir_grammar_export_batch
    public :: standardir_grammar_normalize

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
        call validate_export_tree(rule, rule%root, 0, ok, message)
        if (.not. ok) return
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

        equal = left%kind == right%kind .and. trim(left%name) == trim(right%name) .and. &
            left%minimum == right%minimum .and. left%unbounded .eqv. right%unbounded
        if (.not. equal) return
        if (allocated(left%children) .neqv. allocated(right%children)) then
            equal = .false.
            return
        end if
        if (.not. allocated(left%children)) return
        equal = size(left%children) == size(right%children)
        if (.not. equal) return
        do i = 1, size(left%children)
            if (.not. same_expression(left%children(i), right%children(i))) then
                equal = .false.
                return
            end if
        end do
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

    subroutine eliminate_left_recursion(values, suppressed, names, name_count, nullable, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: suppressed(:)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: name_count
        logical, intent(in) :: nullable(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: source(:), group(:)
        integer :: i, j

        ok = .false.
        message = ''
        do i = 1, name_count
            do j = 1, i - 1
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
                expression = concatenate_expressions(source(j)%expression, tail)
                values(i)%expression = expression
                if (j > 1) values(i)%id = derived_id(values(i)%id, j)
                call append_target(replaced, values(i))
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
        type(standardir_target_expression_t), allocatable :: alphas(:)
        type(standardir_target_expression_t) :: tail, helper_expression
        type(standardir_target_rule_t) :: helper, item
        character(len=128) :: helper_lhs
        integer :: i
        logical :: found, left_corner

        allocate (recursive_rules(0), beta_rules(0), alphas(0))
        ok = .false.
        message = ''
        do i = 1, size(group)
            call leading_reference_tail(group(i)%expression, lhs, tail, found)
            call has_left_corner(group(i)%expression, lhs, names, nullable, left_corner)
            if (left_corner .and. .not. found) then
                message = 'nullable or nested left recursion cannot preserve source mapping'
                return
            end if
            if (found) then
                if (tail%kind == 0) then
                    message = 'unit left recursion is unsupported'
                    return
                end if
                if (expression_nullable(tail, names, nullable)) then
                    message = 'left-recursion suffix is nullable and unsupported'
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
            message = 'left-recursive grammar family has no base alternative'
            return
        end if
        helper_lhs = make_helper_lhs(values, lhs, ok, message)
        if (.not. ok) return
        if (size(alphas) == 1) then
            helper_expression = alphas(1)
        else
            helper_expression = make_choice(alphas)
        end if
        helper = recursive_rules(1)
        helper%id = derived_id(recursive_rules(1)%id, -1)
        helper%lhs = helper_lhs
        helper%alternative = recursive_rules(1)%alternative
        helper%expression = make_repeat(helper_expression)
        do i = 1, size(recursive_rules)
            call append_target(suppressed, recursive_rules(i))
        end do
        allocate (replacement(0))
        do i = 1, size(beta_rules)
            item = beta_rules(i)
            item%expression = concatenate_expressions(item%expression, make_reference(helper_lhs))
            if (i > 1) item%id = derived_id(item%id, i)
            call append_target(replacement, item)
        end do
        call append_target(replacement, helper)
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

    subroutine standardir_grammar_export_batch(unit, rules, format, ok, message)
        integer, intent(in) :: unit, format
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
        type(sx_node_t), allocatable :: nodes(:), suppressed_nodes(:)
        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i, j, ios, scratch

        ok = .false.
        message = ''
        if (format < standardir_grammar_format_ebnf .or. &
            format > standardir_grammar_format_tree_sitter) then
            message = 'grammar export format is unsupported'
            return
        end if
        if (size(rules) < 1) then
            message = 'grammar export batch is empty'
            return
        end if
        if (size(rules) > standardir_max_syntax_records) then
            message = 'grammar export batch exceeds the syntax record limit'
            return
        end if
        do i = 2, size(rules)
            if (trim(rules(i)%lhs) /= trim(rules(i - 1)%lhs)) then
                do j = 1, i - 1
                    if (trim(rules(j)%lhs) == trim(rules(i)%lhs)) then
                        message = 'grammar export batch interleaves LHS groups'
                        return
                    end if
                end do
            end if
        end do

        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message)
        if (.not. ok) return
        allocate (nodes(size(normalized)))
        do i = 1, size(normalized)
            call target_rule_to_syntax(normalized(i), nodes(i), ok, message)
            if (.not. ok) return
        end do
        allocate (suppressed_nodes(size(suppressed)))
        do i = 1, size(suppressed)
            call target_rule_to_syntax(suppressed(i), suppressed_nodes(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_group_syntax(nodes, size(nodes), groups, group_count, ok, message)
        if (.not. ok) return

        open (newunit=scratch, status='scratch', action='readwrite', iostat=ios)
        if (ios /= 0) then
            message = 'could not open grammar export scratch output'
            return
        end if
        call emit_groups(scratch, nodes, suppressed_nodes, groups, group_count, format, ok, message)
        if (.not. ok) then
            close (scratch)
            return
        end if
        rewind (scratch)
        call copy_output(scratch, unit, ok, message)
        close (scratch)
    end subroutine standardir_grammar_export_batch

    subroutine emit_groups(unit, nodes, suppressed, groups, group_count, format, ok, message)
        integer, intent(in) :: unit, group_count, format
        type(sx_node_t), intent(in) :: nodes(:), suppressed(:)
        type(standardir_group_t), intent(in) :: groups(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, index

        ok = .false.
        message = ''
        do i = 1, group_count
            do j = 1, size(suppressed)
                call suppressed_provenance(unit, suppressed(j), groups(i)%lhs, format, ok, message)
                if (.not. ok) return
            end do
            do j = 1, groups(i)%count
                index = groups(i)%indices(j)
                call emit_source_rule_annotation(unit, nodes(index), format, ok, message)
                if (.not. ok) return
            end do
            select case (format)
            case (standardir_grammar_format_ebnf)
                call standardir_emit_ebnf_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_antlr4)
                call standardir_emit_antlr_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_bison)
                call standardir_emit_bison_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_tree_sitter)
                call standardir_emit_treesitter_group(unit, nodes, groups(i), ok, message)
            end select
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine emit_groups

    subroutine target_rule_to_syntax(rule, syntax, ok, message)
        type(standardir_target_rule_t), intent(in) :: rule
        type(sx_node_t), intent(out) :: syntax
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(syntax, 5)
        call make_atom(syntax%children(1), 'syntax')
        call make_atom(syntax%children(2), trim(rule%id))
        call make_pair(syntax%children(3), 'lhs', trim(rule%lhs), ok, message)
        if (.not. ok) return
        call make_list(syntax%children(4), 2)
        call make_atom(syntax%children(4)%children(1), 'rhs')
        call target_expression_to_syntax(rule%expression, syntax%children(4)%children(2), ok, message)
        if (.not. ok) return
        call make_target_source(syntax%children(5), rule, ok, message)
    end subroutine target_rule_to_syntax

    recursive subroutine target_expression_to_syntax(expression, node, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        character(len=32) :: minimum

        call clear_node(node)
        ok = .false.
        message = ''
        select case (expression%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            call make_list(node, 2)
            if (expression%kind == standardir_grammar_reference) then
                call make_atom(node%children(1), 'ref')
            else
                call make_atom(node%children(1), 'token')
            end if
            call make_atom(node%children(2), trim(expression%name))
        case (standardir_grammar_sequence, standardir_grammar_choice)
            if (.not. allocated(expression%children) .or. size(expression%children) < 1) then
                message = 'normalized target expression is empty'
                return
            end if
            call make_list(node, size(expression%children) + 1)
            if (expression%kind == standardir_grammar_sequence) then
                call make_atom(node%children(1), 'seq')
            else
                call make_atom(node%children(1), 'alt')
            end if
            do i = 1, size(expression%children)
                call target_expression_to_syntax(expression%children(i), node%children(i + 1), ok, message)
                if (.not. ok) return
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            call make_list(node, merge(4, 2, expression%kind == standardir_grammar_repeat))
            if (expression%kind == standardir_grammar_optional) then
                call make_atom(node%children(1), 'optional')
            else
                call make_atom(node%children(1), 'repeat')
                write (minimum, '(i0)') expression%minimum
                call make_atom(node%children(3), trim(minimum))
                call make_atom(node%children(4), 'unbounded')
            end if
            call target_expression_to_syntax(expression%children(1), node%children(2), ok, message)
            if (.not. ok) return
        case default
            message = 'normalized target expression has an unsupported kind'
            return
        end select
        ok = .true.
        message = ''
    end subroutine target_expression_to_syntax

    subroutine suppressed_provenance(unit, node, lhs, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule_id, node_lhs, document, clause, page, source_hash
        integer :: i

        ok = .false.
        message = ''
        if (node%child_count /= 5 .or. node%children(3)%kind /= sx_list .or. &
            node%children(3)%child_count /= 2 .or. node%children(3)%children(2)%kind /= sx_atom) then
            message = 'suppressed grammar provenance is malformed'
            return
        end if
        node_lhs = trim(node%children(3)%children(2)%atom)
        if (node_lhs /= trim(lhs)) then
            ok = .true.
            return
        end if
        if (node%children(2)%kind /= sx_atom .or. node%children(5)%kind /= sx_list) then
            message = 'suppressed grammar provenance header is malformed'
            return
        end if
        rule_id = trim(node%children(2)%atom)
        document = ''; clause = ''; page = ''; source_hash = ''
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'suppressed grammar provenance field is malformed'
                return
            end if
            select case (trim(node%children(5)%children(i)%children(1)%atom))
            case ('document')
                document = trim(node%children(5)%children(i)%children(2)%atom)
            case ('clause')
                clause = trim(node%children(5)%children(i)%children(2)%atom)
            case ('page')
                page = trim(node%children(5)%children(i)%children(2)%atom)
            case ('source-sha256')
                source_hash = trim(node%children(5)%children(i)%children(2)%atom)
            end select
        end do
        call emit_source_rule_annotation(unit, node, format, ok, message)
        if (.not. ok) return
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(* rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)//' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)') '// rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)
        case (standardir_grammar_format_bison)
            write (unit, '(a)') '/* rule='//trim(rule_id)//' document='//trim(document)// &
                ' clause='//trim(clause)//' page='//trim(page)//' source-sha256='//trim(source_hash)//' */'
        end select
        ok = .true.
        message = ''
    end subroutine suppressed_provenance

    subroutine make_target_source(node, rule, ok, message)
        type(sx_node_t), intent(out) :: node
        type(standardir_target_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: alternative

        call make_list(node, 7)
        call make_atom(node%children(1), 'source')
        call make_pair(node%children(2), 'document', trim(rule%source%document), ok, message)
        if (.not. ok) return
        call make_pair(node%children(3), 'clause', trim(rule%source%clause), ok, message)
        if (.not. ok) return
        call make_pair(node%children(4), 'rule', trim(rule%source%rule), ok, message)
        if (.not. ok) return
        write (alternative, '(i0)') rule%alternative
        call make_pair(node%children(5), 'alternative', trim(alternative), ok, message)
        if (.not. ok) return
        call make_pair(node%children(6), 'page', integer_text(rule%source%page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(7), 'source-sha256', trim(rule%source%source_hash), ok, message)
    end subroutine make_target_source

    subroutine emit_source_rule_annotation(unit, node, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: source_rule, source_alternative
        integer :: i

        ok = .false.
        message = ''
        source_rule = ''
        source_alternative = ''
        if (node%child_count /= 5 .or. node%children(5)%kind /= sx_list .or. &
            node%children(5)%child_count < 6) then
            message = 'canonical grammar source is malformed'
            return
        end if
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'canonical grammar source child is malformed'
                return
            end if
            if (trim(node%children(5)%children(i)%children(1)%atom) == 'rule') then
                source_rule = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'alternative') then
                source_alternative = trim(node%children(5)%children(i)%children(2)%atom)
            end if
        end do
        if (len_trim(source_rule) == 0) then
            message = 'canonical grammar source rule is empty'
            return
        end if
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)') ' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)', advance='no') '// source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)')
        case (standardir_grammar_format_bison)
            write (unit, '(a)', advance='no') '/* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            write (unit, '(a)') ' */'
        end select
        ok = .true.
        message = ''
    end subroutine emit_source_rule_annotation

    subroutine copy_output(source_unit, target_unit, ok, message)
        integer, intent(in) :: source_unit, target_unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=65536) :: line
        integer :: ios

        ok = .false.
        message = ''
        do
            read (source_unit, '(a)', iostat=ios) line
            if (ios < 0) exit
            if (ios > 0) then
                message = 'could not read staged grammar export'
                return
            end if
            write (target_unit, '(a)', iostat=ios) trim(line)
            if (ios /= 0) then
                message = 'could not write grammar export'
                return
            end if
        end do
        ok = .true.
        message = ''
    end subroutine copy_output

    subroutine rule_to_syntax(rule, syntax, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        type(sx_node_t), intent(out) :: syntax
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call clear_node(syntax)
        call standardir_grammar_validate(rule, ok, message)
        if (.not. ok) return
        if (rule%resolution /= standardir_grammar_resolution_resolved) then
            ok = .false.
            message = 'grammar export requires resolved rules'
            return
        end if

        call validate_export_tree(rule, rule%root, 0, ok, message)
        if (.not. ok) return

        call make_list(syntax, 5)
        call make_atom(syntax%children(1), 'syntax')
        call make_atom(syntax%children(2), trim(rule%id))
        call make_pair(syntax%children(3), 'lhs', trim(rule%lhs), ok, message)
        if (.not. ok) return
        call make_list(syntax%children(4), 2)
        call make_atom(syntax%children(4)%children(1), 'rhs')
        call build_expression(rule, rule%root, syntax%children(4)%children(2), ok, message)
        if (.not. ok) return
        call make_source(syntax%children(5), rule, ok, message)
    end subroutine rule_to_syntax

    recursive subroutine validate_export_tree(rule, index, depth, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index, depth
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child, last

        ok = .false.
        message = ''
        if (index < 1 .or. index > size(rule%nodes%values)) then
            message = 'grammar export child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'grammar export node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        if (node%child_count == 0) then
            ok = .true.
            return
        end if
        child = node%first_child
        do i = 1, node%child_count
            call validate_export_tree(rule, child, depth + 1, ok, message)
            if (.not. ok) return
            call subtree_end(rule, child, 0, last, ok, message)
            if (.not. ok) return
            child = last + 1
        end do
        ok = .true.
        message = ''
    end subroutine validate_export_tree

    recursive subroutine build_expression(rule, index, expression, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: rule
        integer, intent(in) :: index
        type(sx_node_t), intent(out) :: expression
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_grammar_node_t) :: node
        integer :: i, child, last

        call clear_node(expression)
        ok = .false.
        message = ''
        node = rule%nodes%values(index)
        select case (node%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            call make_list(expression, 2)
            if (node%kind == standardir_grammar_reference) then
                call make_atom(expression%children(1), 'ref')
            else
                call make_atom(expression%children(1), 'token')
            end if
            call make_atom(expression%children(2), trim(node%name))
        case (standardir_grammar_sequence, standardir_grammar_choice)
            call make_list(expression, node%child_count + 1)
            if (node%kind == standardir_grammar_sequence) then
                call make_atom(expression%children(1), 'seq')
            else
                call make_atom(expression%children(1), 'alt')
            end if
            child = node%first_child
            do i = 1, node%child_count
                call build_expression(rule, child, expression%children(i + 1), ok, message)
                if (.not. ok) return
                call subtree_end(rule, child, 0, last, ok, message)
                if (.not. ok) return
                child = last + 1
            end do
        case (standardir_grammar_optional)
            call make_list(expression, 2)
            call make_atom(expression%children(1), 'optional')
            call build_expression(rule, node%first_child, expression%children(2), ok, message)
            if (.not. ok) return
        case (standardir_grammar_repeat)
            call make_list(expression, 4)
            call make_atom(expression%children(1), 'repeat')
            call build_expression(rule, node%first_child, expression%children(2), ok, message)
            if (.not. ok) return
            call make_atom(expression%children(3), integer_text(node%minimum))
            call make_atom(expression%children(4), 'unbounded')
        case default
            message = 'normalized grammar node kind is unsupported'
            return
        end select
        ok = .true.
        message = ''
    end subroutine build_expression

    recursive subroutine subtree_end(rule, index, depth, last, ok, message)
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
            message = 'normalized grammar child index is outside the node table'
            return
        end if
        if (depth >= size(rule%nodes%values)) then
            message = 'normalized grammar node table is cyclic'
            return
        end if
        node = rule%nodes%values(index)
        last = index
        if (node%child_count > 0) then
            child = node%first_child
            do i = 1, node%child_count
                call subtree_end(rule, child, depth + 1, last, ok, message)
                if (.not. ok) return
                child = last + 1
            end do
        end if
        ok = .true.
        message = ''
    end subroutine subtree_end

    subroutine make_source(node, rule, ok, message)
        type(sx_node_t), intent(out) :: node
        type(standardir_grammar_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(node, 6)
        call make_atom(node%children(1), 'source')
        call make_pair(node%children(2), 'document', trim(rule%source%document), ok, message)
        if (.not. ok) return
        call make_pair(node%children(3), 'clause', trim(rule%source%clause), ok, message)
        if (.not. ok) return
        call make_pair(node%children(4), 'rule', trim(rule%source%rule), ok, message)
        if (.not. ok) return
        call make_pair(node%children(5), 'page', integer_text(rule%source%page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(6), 'source-sha256', trim(rule%source%source_hash), ok, &
            message)
    end subroutine make_source

    subroutine make_pair(node, label, value, ok, message)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(node, 2)
        call make_atom(node%children(1), label)
        call make_atom(node%children(2), value)
        ok = len_trim(label) > 0 .and. len_trim(value) > 0
        if (ok) then
            message = ''
        else
            message = 'canonical grammar field is empty'
        end if
    end subroutine make_pair

    subroutine make_list(node, count)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: count

        call clear_node(node)
        node%kind = sx_list
        node%child_count = count
        allocate (node%children(count))
    end subroutine make_list

    subroutine make_atom(node, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: value

        call clear_node(node)
        node%kind = sx_atom
        node%atom = trim(value)
    end subroutine make_atom

    subroutine clear_node(node)
        type(sx_node_t), intent(inout) :: node

        if (allocated(node%children)) deallocate (node%children)
        node%kind = 0
        node%atom = ''
        node%child_count = 0
    end subroutine clear_node

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

end module standardir_grammar_export

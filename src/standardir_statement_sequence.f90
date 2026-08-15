module standardir_statement_sequence
    !! Source-topology candidates for statement-sequence boundaries.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_lexical_layout, only: standardir_layout_t, standardir_layout_validate
    use standardir_syntax_fields, only: standardir_read_syntax_header
    implicit none
    private

    character(len=16), parameter, public :: standardir_sequence_repeat_item = 'repeat-item'
    character(len=20), parameter, public :: standardir_sequence_first_plus_repeat = 'first-plus-repeat'
    character(len=25), parameter, public :: standardir_sequence_compound_repeat_item = 'compound-repeat-item'
    character(len=17), parameter, public :: standardir_sequence_internal = 'sequence-internal'
    character(len=17), parameter, public :: standardir_sequence_compound_internal = 'sequence-internal'

    type, public :: standardir_statement_sequence_candidate_t
        character(len=128) :: source_rule = ''
        character(len=128) :: source_lhs = ''
        character(len=128) :: source_document = ''
        character(len=128) :: source_clause = ''
        character(len=128) :: source_hash = ''
        character(len=512) :: expression_path = ''
        character(len=128) :: item = ''
        character(len=32) :: kind = ''
        character(len=128) :: derivation = ''
        character(len=64) :: source_page = ''
        character(len=64) :: source_byte_start = ''
        character(len=32) :: status = ''
    end type standardir_statement_sequence_candidate_t

    public :: standardir_statement_sequence_analyze

    type :: sequence_rule_t
        character(len=128) :: id = ''
        character(len=128) :: lhs = ''
        character(len=128) :: document = ''
        character(len=128) :: clause = ''
        character(len=128) :: source_hash = ''
        character(len=64) :: page = ''
        character(len=64) :: byte_start = ''
        type(sx_node_t) :: rhs
    end type sequence_rule_t

contains

    subroutine standardir_statement_sequence_analyze(nodes, layout, values, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_layout_t), intent(in) :: layout
        type(standardir_statement_sequence_candidate_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sequence_rule_t), allocatable :: rules(:)
        character(len=128), allocatable :: reachable(:), nullable(:), statement_classes(:)
        character(len=32) :: suffix
        integer :: i, rule_count, suffix_count
        logical :: local_ok, unsupported_found

        if (allocated(values)) deallocate (values)
        ok = .false.
        message = ''

        call standardir_layout_validate(layout, local_ok, message)
        if (.not. local_ok) then
            message = 'invalid v2 lexical layout: '//trim(message)
            return
        end if
        suffix = ''
        suffix_count = 0
        do i = 1, layout%count
            if (trim(layout%records(i)%kind) == 'statement-class-suffix') then
                suffix_count = suffix_count + 1
                suffix = trim(layout%records(i)%suffix)
                if (trim(layout%records(i)%source_form) /= 'all') then
                    message = 'statement-class-suffix fact must have source-form all'
                    return
                end if
            end if
        end do
        if (suffix_count /= 1) then
            message = 'expected exactly one statement-class-suffix fact'
            return
        end if

        rule_count = 0
        do i = 1, size(nodes)
            if (is_syntax_record(nodes(i))) rule_count = rule_count + 1
        end do
        if (rule_count == 0) then
            message = 'no StandardIR syntax records were supplied'
            return
        end if
        allocate (rules(rule_count))
        rule_count = 0
        do i = 1, size(nodes)
            if (.not. is_syntax_record(nodes(i))) cycle
            rule_count = rule_count + 1
            call read_rule(nodes(i), rules(rule_count), local_ok, message)
            if (.not. local_ok) then
                deallocate (rules)
                return
            end if
        end do

        allocate (reachable(0), nullable(0), statement_classes(0))
        do i = 1, size(rules)
            if (ends_with(rules(i)%lhs, suffix)) then
                call append_name(statement_classes, rules(i)%lhs)
                call append_name(reachable, rules(i)%lhs)
            end if
        end do
        call fixed_point_reachable(rules, reachable)
        call fixed_point_nullable(rules, nullable)

        unsupported_found = .false.
        do i = 1, size(rules)
            call visit_expression(rules(i)%rhs, 'rhs', rules(i), statement_classes, reachable, &
                nullable, values, unsupported_found, local_ok, message)
            if (.not. local_ok) then
                deallocate (rules, reachable, nullable, statement_classes)
                if (allocated(values)) deallocate (values)
                return
            end if
        end do
        if (.not. allocated(values)) allocate (values(0))
        call sort_candidates(values)
        deallocate (rules, reachable, nullable, statement_classes)
        ok = .not. unsupported_found
        if (.not. ok) message = 'unsupported repeated statement-bearing shape at repeat item'
    end subroutine standardir_statement_sequence_analyze

    logical function is_syntax_record(node)
        type(sx_node_t), intent(in) :: node

        is_syntax_record = .false.
        if (node%kind /= sx_list) return
        if (node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        is_syntax_record = trim(node%children(1)%atom) == 'syntax'
    end function is_syntax_record

    subroutine read_rule(node, rule, ok, message)
        type(sx_node_t), intent(in) :: node
        type(sequence_rule_t), intent(out) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: document, clause, source_hash
        character(len=64) :: page, byte_length

        rule = sequence_rule_t()
        call validate_syntax_record(node, ok, message)
        if (.not. ok) return
        call standardir_read_syntax_header(node, rule%id, rule%lhs, document, clause, page, source_hash, &
            ok, message, source_byte_start=rule%byte_start, source_byte_length=byte_length)
        if (.not. ok) return
        rule%document = trim(document)
        rule%clause = trim(clause)
        rule%source_hash = trim(source_hash)
        rule%page = trim(page)
        rule%rhs = node%children(4)%children(2)
    end subroutine read_rule

    subroutine validate_syntax_record(node, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'syntax object has the wrong shape'
            return
        end if
        if (node%child_count /= 5) then
            message = 'syntax object has the wrong shape'
            return
        end if
        if (node%children(4)%kind /= sx_list) then
            message = 'syntax rhs field is malformed'
            return
        end if
        if (node%children(4)%child_count /= 2) then
            message = 'syntax rhs field is malformed'
            return
        end if
        if (node%children(4)%children(1)%kind /= sx_atom) then
            message = 'syntax rhs field is malformed'
            return
        end if
        if (trim(node%children(4)%children(1)%atom) /= 'rhs') then
            message = 'syntax rhs field is malformed'
            return
        end if
        if (node%children(4)%children(2)%kind /= sx_list) then
            message = 'syntax rhs expression is malformed'
            return
        end if
        call validate_expression(node%children(4)%children(2), ok, message)
    end subroutine validate_syntax_record

    recursive subroutine validate_expression(node, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: label
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'grammar expression is not an SX list'
            return
        end if
        if (node%child_count < 1) then
            message = 'grammar expression is empty'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'grammar expression label is not an atom'
            return
        end if
        label = trim(node%children(1)%atom)
        select case (label)
        case ('ref', 'token')
            if (node%child_count /= 2) then
                message = 'grammar leaf is malformed'
                return
            end if
            if (node%children(2)%kind /= sx_atom) then
                message = 'grammar leaf is malformed'
                return
            end if
        case ('seq', 'alt')
            if (node%child_count < 2) then
                message = 'grammar group is empty'
                return
            end if
            do i = 2, node%child_count
                call validate_expression(node%children(i), ok, message)
                if (.not. ok) return
            end do
        case ('optional')
            if (node%child_count /= 2) then
                message = 'optional expression is malformed'
                return
            end if
            call validate_expression(node%children(2), ok, message)
            if (.not. ok) return
        case ('repeat')
            if (node%child_count /= 4) then
                message = 'repeat expression is malformed'
                return
            end if
            if (node%children(3)%kind /= sx_atom .or. node%children(4)%kind /= sx_atom) then
                message = 'repeat expression metadata is malformed'
                return
            end if
            if (trim(node%children(3)%atom) /= '0' .and. trim(node%children(3)%atom) /= '1') then
                message = 'repeat expression has an unsupported minimum'
                return
            end if
            if (trim(node%children(4)%atom) /= 'unbounded') then
                message = 'repeat expression is not unbounded'
                return
            end if
            call validate_expression(node%children(2), ok, message)
            if (.not. ok) return
        case default
            message = 'unsupported grammar expression: '//trim(label)
            return
        end select
        ok = .true.
    end subroutine validate_expression

    subroutine fixed_point_reachable(rules, reachable)
        type(sequence_rule_t), intent(in) :: rules(:)
        character(len=128), allocatable, intent(inout) :: reachable(:)
        integer :: i
        logical :: changed

        changed = .true.
        do while (changed)
            changed = .false.
            do i = 1, size(rules)
                if (contains_any_name(rules(i)%rhs, reachable)) then
                    if (.not. name_present(reachable, rules(i)%lhs)) then
                        call append_name(reachable, rules(i)%lhs)
                        changed = .true.
                    end if
                end if
            end do
        end do
    end subroutine fixed_point_reachable

    subroutine fixed_point_nullable(rules, nullable)
        type(sequence_rule_t), intent(in) :: rules(:)
        character(len=128), allocatable, intent(inout) :: nullable(:)
        integer :: i
        logical :: changed, expression_ok

        changed = .true.
        do while (changed)
            changed = .false.
            do i = 1, size(rules)
                expression_ok = expression_nullable(rules(i)%rhs, nullable)
                if (expression_ok) then
                    if (.not. name_present(nullable, rules(i)%lhs)) then
                        call append_name(nullable, rules(i)%lhs)
                        changed = .true.
                    end if
                end if
            end do
        end do
    end subroutine fixed_point_nullable

    recursive logical function expression_nullable(node, nullable) result(value)
        type(sx_node_t), intent(in) :: node
        character(len=128), intent(in) :: nullable(:)
        character(len=32) :: label
        integer :: i

        value = .false.
        label = trim(node%children(1)%atom)
        select case (label)
        case ('optional')
            value = .true.
        case ('repeat')
            value = trim(node%children(3)%atom) == '0'
        case ('ref')
            value = name_present(nullable, trim(node%children(2)%atom))
        case ('alt')
            do i = 2, node%child_count
                if (expression_nullable(node%children(i), nullable)) value = .true.
            end do
        case ('seq')
            value = .true.
            do i = 2, node%child_count
                if (.not. expression_nullable(node%children(i), nullable)) value = .false.
            end do
        case ('token')
            value = .false.
        end select
    end function expression_nullable

    recursive logical function contains_any_name(node, names) result(found)
        type(sx_node_t), intent(in) :: node
        character(len=128), intent(in) :: names(:)
        integer :: i

        found = .false.
        if (is_label(node, 'ref')) then
            found = name_present(names, trim(node%children(2)%atom))
            if (found) return
        end if
        if (node%kind /= sx_list) return
        do i = 2, node%child_count
            if (contains_any_name(node%children(i), names)) then
                found = .true.
                return
            end if
        end do
    end function contains_any_name

    recursive subroutine visit_expression(node, path, rule, statement_classes, reachable, nullable, &
            values, unsupported_found, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: path
        type(sequence_rule_t), intent(in) :: rule
        character(len=128), intent(in) :: statement_classes(:), reachable(:), nullable(:)
        type(standardir_statement_sequence_candidate_t), allocatable, intent(inout) :: values(:)
        logical, intent(inout) :: unsupported_found
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: item_name, derivation
        integer, allocatable :: positions(:)
        character(len=128), allocatable :: position_names(:)
        integer :: i, j
        logical :: supported, statement_bearing, item_candidate, prefix_statement

        ok = .false.
        message = ''
        call append_sequence_internal_candidates(node, path, rule, statement_classes, reachable, nullable, values)
        if (is_label(node, 'repeat')) then
            item_candidate = repeat_item(node, statement_classes, reachable, nullable, item_name, derivation, &
                positions, position_names, supported, statement_bearing, ok, message)
            if (.not. ok) return
            if (.not. supported .and. statement_bearing) then
                call append_candidate(values, rule, path, 'expression', 'unsupported-repeat-item', derivation, &
                    'unsupported')
                unsupported_found = .true.
            else if (item_candidate) then
                if (trim(item_name) /= 'sequence') then
                    call append_candidate(values, rule, path, trim(item_name), standardir_sequence_repeat_item, &
                        derivation)
                else
                    call append_candidate(values, rule, path, 'sequence', &
                        standardir_sequence_compound_repeat_item, derivation)
                end if
            end if
            if (allocated(positions)) deallocate (positions)
            if (allocated(position_names)) deallocate (position_names)
        end if

        if (is_label(node, 'seq')) then
            do i = 2, node%child_count
                if (.not. is_label(node%children(i), 'repeat')) cycle
                item_candidate = repeat_item(node%children(i), statement_classes, reachable, nullable, item_name, &
                    derivation, positions, position_names, supported, statement_bearing, ok, message)
                if (.not. ok) return
                if (.not. item_candidate) then
                    if (allocated(positions)) deallocate (positions)
                    if (allocated(position_names)) deallocate (position_names)
                    cycle
                end if
                prefix_statement = .false.
                do j = 2, i - 1
                    if (contains_any_name(node%children(j), reachable)) prefix_statement = .true.
                end do
                if (prefix_statement) then
                    call append_candidate(values, rule, trim(path)//'/'//itoa(i), trim(item_name), &
                        standardir_sequence_first_plus_repeat, derivation)
                end if
                if (allocated(positions)) deallocate (positions)
                if (allocated(position_names)) deallocate (position_names)
            end do
        end if

        if (node%kind == sx_list) then
            do i = 2, node%child_count
                call visit_expression(node%children(i), trim(path)//'/'//itoa(i), rule, statement_classes, &
                    reachable, nullable, values, unsupported_found, ok, message)
                if (.not. ok) return
            end do
        end if
        ok = .true.
    end subroutine visit_expression

    subroutine append_sequence_internal_candidates(node, path, rule, statement_classes, reachable, nullable, &
            values)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: path
        type(sequence_rule_t), intent(in) :: rule
        character(len=128), intent(in) :: statement_classes(:), reachable(:), nullable(:)
        type(standardir_statement_sequence_candidate_t), allocatable, intent(inout) :: values(:)
        character(len=128) :: statement
        integer :: i
        logical :: direct

        if (.not. is_label(node, 'seq')) return
        do i = 2, node%child_count - 1
            direct = direct_reference(node%children(i), statement)
            if (.not. direct) cycle
            if (.not. name_present(statement_classes, trim(statement))) cycle
            if (suffix_reaches_statement_boundary(node, i + 1, reachable, nullable)) then
                call append_candidate(values, rule, trim(path)//'/'//itoa(i - 1), trim(statement), &
                    standardir_sequence_internal, trim(statement))
            end if
        end do
    end subroutine append_sequence_internal_candidates

    logical function suffix_reaches_statement_boundary(node, first, reachable, nullable)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: first
        character(len=128), intent(in) :: reachable(:), nullable(:)
        integer :: i

        suffix_reaches_statement_boundary = .true.
        do i = first, node%child_count
            if (contains_any_name(node%children(i), reachable)) then
                suffix_reaches_statement_boundary = .true.
                return
            end if
            if (.not. expression_nullable(node%children(i), nullable)) then
                suffix_reaches_statement_boundary = .false.
                return
            end if
        end do
    end function suffix_reaches_statement_boundary

    logical function repeat_item(node, statement_classes, reachable, nullable, item_name, derivation, positions, &
            position_names, supported, statement_bearing, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=128), intent(in) :: statement_classes(:), reachable(:), nullable(:)
        character(len=*), intent(out) :: item_name, derivation
        integer, allocatable, intent(out) :: positions(:)
        character(len=128), allocatable, intent(out) :: position_names(:)
        logical, intent(out) :: supported, statement_bearing, ok
        character(len=*), intent(out) :: message
        character(len=128) :: direct_name
        logical :: direct

        item_name = ''
        derivation = ''
        allocate (positions(0), position_names(0))
        supported = .false.
        statement_bearing = .false.
        ok = .false.
        message = ''
        direct = direct_reference(node%children(2), direct_name)
        if (direct) then
            if (name_present(reachable, trim(direct_name))) then
                item_name = trim(direct_name)
                derivation = trim(direct_name)
                supported = .true.
                statement_bearing = .true.
                repeat_item = .true.
            else
                repeat_item = .false.
            end if
            ok = .true.
            return
        end if

        if (is_label(node%children(2), 'seq')) then
            call compound_item(node%children(2), statement_classes, reachable, nullable, positions, &
                position_names, supported, statement_bearing)
            if (supported .and. statement_bearing) then
                item_name = 'sequence'
                call derivation_names(node%children(2), reachable, derivation)
                repeat_item = .true.
                ok = .true.
                return
            end if
        else
            if (.not. contains_any_name(node%children(2), reachable)) then
                repeat_item = .false.
                ok = .true.
                return
            end if
        end if
        if (contains_any_name(node%children(2), reachable)) then
            call derivation_names(node%children(2), reachable, derivation)
            supported = .false.
            statement_bearing = .true.
            ok = .true.
            repeat_item = .false.
            return
        end if
        repeat_item = .false.
        ok = .true.
    end function repeat_item

    subroutine compound_item(node, statement_classes, reachable, nullable, positions, position_names, &
            supported, statement_bearing)
        type(sx_node_t), intent(in) :: node
        character(len=128), intent(in) :: statement_classes(:), reachable(:), nullable(:)
        integer, allocatable, intent(inout) :: positions(:)
        character(len=128), allocatable, intent(inout) :: position_names(:)
        logical, intent(out) :: supported, statement_bearing
        character(len=128) :: direct_name
        integer :: i
        logical :: direct, allowed

        supported = .true.
        statement_bearing = .false.
        do i = 2, node%child_count
            direct = direct_reference(node%children(i), direct_name)
            if (direct) then
                if (name_present(statement_classes, trim(direct_name))) then
                    call append_position(positions, position_names, i - 1, trim(direct_name))
                    statement_bearing = .true.
                else
                    if (.not. name_present(reachable, trim(direct_name))) supported = .false.
                end if
            else
                allowed = expression_nullable(node%children(i), nullable)
                if (.not. allowed) allowed = contains_any_name(node%children(i), reachable)
                if (.not. allowed) supported = .false.
            end if
        end do
    end subroutine compound_item

    subroutine derivation_names(node, reachable, text)
        type(sx_node_t), intent(in) :: node
        character(len=128), intent(in) :: reachable(:)
        character(len=*), intent(out) :: text
        character(len=128), allocatable :: names(:)
        integer :: i, length, name_length

        text = ''
        allocate (names(0))
        do i = 1, size(reachable)
            if (contains_name(node, trim(reachable(i)))) call append_name(names, trim(reachable(i)))
        end do
        call sort_names(names)
        length = 0
        do i = 1, size(names)
            if (i > 1) then
                length = length + 1
                text(length:length) = ','
            end if
            name_length = len_trim(names(i))
            text(length + 1:length + name_length) = trim(names(i))
            length = length + name_length
        end do
        deallocate (names)
    end subroutine derivation_names

    recursive logical function contains_name(node, name) result(found)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (is_label(node, 'ref')) then
            found = trim(node%children(2)%atom) == trim(name)
            if (found) return
        end if
        if (node%kind /= sx_list) return
        do i = 2, node%child_count
            if (contains_name(node%children(i), name)) then
                found = .true.
                return
            end if
        end do
    end function contains_name

    logical function direct_reference(node, name)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: name

        name = ''
        direct_reference = is_label(node, 'ref')
        if (direct_reference) name = trim(node%children(2)%atom)
    end function direct_reference

    logical function is_label(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label

        is_label = .false.
        if (node%kind /= sx_list) return
        if (node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        is_label = trim(node%children(1)%atom) == trim(label)
    end function is_label

    logical function ends_with(value, suffix)
        character(len=*), intent(in) :: value, suffix
        integer :: value_length, suffix_length

        value_length = len_trim(value)
        suffix_length = len_trim(suffix)
        ends_with = .false.
        if (suffix_length == 0) return
        if (value_length < suffix_length) return
        ends_with = value(value_length - suffix_length + 1:value_length) == trim(suffix)
    end function ends_with

    logical function name_present(values, name)
        character(len=128), intent(in) :: values(:)
        character(len=*), intent(in) :: name
        integer :: i

        name_present = .false.
        do i = 1, size(values)
            if (trim(values(i)) == trim(name)) then
                name_present = .true.
                return
            end if
        end do
    end function name_present

    subroutine append_name(values, name)
        character(len=128), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: name
        character(len=128), allocatable :: extended(:)
        integer :: old_size

        if (name_present(values, name)) return
        old_size = size(values)
        allocate (extended(old_size + 1))
        if (old_size > 0) extended(:old_size) = values
        extended(old_size + 1) = trim(name)
        call move_alloc(extended, values)
    end subroutine append_name

    subroutine append_position(positions, names, position, name)
        integer, allocatable, intent(inout) :: positions(:)
        character(len=128), allocatable, intent(inout) :: names(:)
        integer, intent(in) :: position
        character(len=*), intent(in) :: name
        integer, allocatable :: extended_positions(:)
        character(len=128), allocatable :: extended_names(:)
        integer :: old_size

        old_size = size(positions)
        allocate (extended_positions(old_size + 1), extended_names(old_size + 1))
        if (old_size > 0) then
            extended_positions(:old_size) = positions
            extended_names(:old_size) = names
        end if
        extended_positions(old_size + 1) = position
        extended_names(old_size + 1) = trim(name)
        call move_alloc(extended_positions, positions)
        call move_alloc(extended_names, names)
    end subroutine append_position

    subroutine append_candidate(values, rule, path, item, kind, derivation, status)
        type(standardir_statement_sequence_candidate_t), allocatable, intent(inout) :: values(:)
        type(sequence_rule_t), intent(in) :: rule
        character(len=*), intent(in) :: path, item, kind, derivation
        character(len=*), intent(in), optional :: status
        type(standardir_statement_sequence_candidate_t), allocatable :: extended(:)
        integer :: old_size

        old_size = 0
        if (allocated(values)) old_size = size(values)
        allocate (extended(old_size + 1))
        if (old_size > 0) extended(:old_size) = values
        extended(old_size + 1)%source_rule = trim(rule%id)
        extended(old_size + 1)%source_lhs = trim(rule%lhs)
        extended(old_size + 1)%source_document = trim(rule%document)
        extended(old_size + 1)%source_clause = trim(rule%clause)
        extended(old_size + 1)%source_hash = trim(rule%source_hash)
        extended(old_size + 1)%expression_path = trim(path)
        extended(old_size + 1)%item = trim(item)
        extended(old_size + 1)%kind = trim(kind)
        extended(old_size + 1)%derivation = trim(derivation)
        extended(old_size + 1)%source_page = trim(rule%page)
        extended(old_size + 1)%source_byte_start = trim(rule%byte_start)
        extended(old_size + 1)%status = 'candidate'
        if (present(status)) extended(old_size + 1)%status = trim(status)
        call move_alloc(extended, values)
    end subroutine append_candidate

    subroutine sort_names(values)
        character(len=128), allocatable, intent(inout) :: values(:)
        character(len=128) :: temporary
        integer :: i, j

        do i = 2, size(values)
            temporary = values(i)
            j = i - 1
            do while (j >= 1)
                if (trim(values(j)) <= trim(temporary)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = temporary
        end do
    end subroutine sort_names

    subroutine sort_candidates(values)
        type(standardir_statement_sequence_candidate_t), allocatable, intent(inout) :: values(:)
        type(standardir_statement_sequence_candidate_t) :: temporary
        integer :: i, j

        if (.not. allocated(values)) return
        do i = 2, size(values)
            temporary = values(i)
            j = i - 1
            do while (j >= 1)
                if (.not. candidate_after(values(j), temporary)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = temporary
        end do
    end subroutine sort_candidates

    logical function candidate_after(left, right)
        type(standardir_statement_sequence_candidate_t), intent(in) :: left, right

        candidate_after = .false.
        if (trim(left%source_lhs) > trim(right%source_lhs)) then
            candidate_after = .true.
        else if (trim(left%source_lhs) < trim(right%source_lhs)) then
            return
        else if (trim(left%source_rule) > trim(right%source_rule)) then
            candidate_after = .true.
        else if (trim(left%source_rule) < trim(right%source_rule)) then
            return
        else if (trim(left%source_byte_start) > trim(right%source_byte_start)) then
            candidate_after = .true.
        else if (trim(left%source_byte_start) < trim(right%source_byte_start)) then
            return
        else if (trim(left%expression_path) > trim(right%expression_path)) then
            candidate_after = .true.
        else if (trim(left%expression_path) < trim(right%expression_path)) then
            return
        else
            candidate_after = trim(left%kind) > trim(right%kind)
        end if
    end function candidate_after

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function itoa

end module standardir_statement_sequence

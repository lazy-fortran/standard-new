module standardir_dependencies
    !! Compute rule references and transitive StandardIR profile closure.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    integer, parameter, public :: dependency_max_rules = 1024
    integer, parameter, public :: dependency_max_refs = 256
    integer, parameter :: dependency_rule_length = 16
    integer, parameter :: dependency_name_length = 256

    type, public :: dependency_rule_t
        character(len=dependency_rule_length) :: rule = ''
        character(len=dependency_name_length) :: lhs = ''
        integer :: reference_count = 0
        character(len=dependency_name_length) :: references(dependency_max_refs) = ''
        integer :: occurrences = 0
    end type dependency_rule_t

    type, public :: dependency_table_t
        integer :: rule_count = 0
        type(dependency_rule_t) :: rules(dependency_max_rules)
    end type dependency_table_t

    public :: dependency_add_syntax
    public :: dependency_compute
    public :: dependency_write

contains

    subroutine dependency_add_syntax(table, node, is_syntax, ok, message)
        type(dependency_table_t), intent(inout) :: table
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: is_syntax, ok
        character(len=*), intent(out) :: message

        character(len=dependency_rule_length) :: rule
        character(len=dependency_name_length) :: lhs
        character(len=dependency_name_length) :: references(dependency_max_refs)
        integer :: reference_count, index

        is_syntax = .false.
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            ok = .true.
            return
        end if
        if (node%child_count < 4) then
            ok = .true.
            return
        end if
        if (.not. atom_is(node%children(1), 'syntax')) then
            ok = .true.
            return
        end if
        is_syntax = .true.
        call read_atom(node%children(2), rule, ok, message)
        if (.not. ok) return
        call read_lhs(node%children(3), lhs, ok, message)
        if (.not. ok) return

        references = ''
        reference_count = 0
        call collect_references(node%children(4), references, reference_count, ok, message)
        if (.not. ok) return

        index = find_rule(table, trim(rule))
        if (index == 0) then
            if (table%rule_count >= dependency_max_rules) then
                ok = .false.
                message = 'dependency rule table is full'
                return
            end if
            table%rule_count = table%rule_count + 1
            index = table%rule_count
            table%rules(index)%rule = trim(rule)
            table%rules(index)%lhs = trim(lhs)
            table%rules(index)%reference_count = reference_count
            table%rules(index)%references = references
            table%rules(index)%occurrences = 1
        else
            if (.not. same_rule_shape(table%rules(index), lhs, references, reference_count)) then
                ok = .false.
                message = 'repeated rule has a different grammar shape: '//trim(rule)
                return
            end if
            table%rules(index)%occurrences = table%rules(index)%occurrences + 1
        end if
        ok = .true.
    end subroutine dependency_add_syntax

    subroutine dependency_compute(table, roots, root_count, closure, closure_count, &
            unresolved_count, ok, message)
        type(dependency_table_t), intent(in) :: table
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        integer, intent(out) :: closure(:)
        integer, intent(out) :: closure_count, unresolved_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        logical :: selected(dependency_max_rules), processed(dependency_max_rules)
        integer :: queue(dependency_max_rules)
        integer :: head, tail, i, j, index, target, match_count
        character(len=dependency_name_length) :: reference

        selected = .false.
        processed = .false.
        queue = 0
        closure = 0
        closure_count = 0
        unresolved_count = 0
        ok = .false.
        message = ''
        if (root_count < 1) then
            message = 'profile has no roots'
            return
        end if
        if (size(closure) < table%rule_count) then
            message = 'closure output array is too small'
            return
        end if

        head = 1
        tail = 0
        do i = 1, root_count
            index = find_rule(table, trim(roots(i)))
            if (index == 0) then
                message = 'profile root is not present: '//trim(roots(i))
                return
            end if
            if (.not. selected(index)) then
                selected(index) = .true.
                tail = tail + 1
                queue(tail) = index
            end if
        end do

        do while (head <= tail)
            index = queue(head)
            head = head + 1
            if (processed(index)) cycle
            processed(index) = .true.
            do j = 1, table%rules(index)%reference_count
                reference = table%rules(index)%references(j)
                call find_lhs(table, trim(reference), target, match_count)
                if (match_count == 0) then
                    unresolved_count = unresolved_count + 1
                else if (match_count > 1) then
                    message = 'reference names multiple rules: '//trim(reference)
                    return
                else
                    if (.not. selected(target)) then
                        selected(target) = .true.
                        tail = tail + 1
                        queue(tail) = target
                    end if
                end if
            end do
        end do

        do i = 1, table%rule_count
            if (selected(i)) then
                closure_count = closure_count + 1
                closure(closure_count) = i
            end if
        end do
        ok = .true.
    end subroutine dependency_compute

    subroutine dependency_write(unit, profile, source_hash, roots, root_count, table, &
            closure, closure_count, unresolved_count)
        integer, intent(in) :: unit, root_count, closure_count, unresolved_count
        character(len=*), intent(in) :: profile, source_hash, roots(:)
        type(dependency_table_t), intent(in) :: table
        integer, intent(in) :: closure(:)

        integer :: i, j, duplicate_count

        duplicate_count = 0
        do i = 1, table%rule_count
            if (table%rules(i)%occurrences > 1) duplicate_count = duplicate_count + 1
        end do

        write (unit, '(a)') '(dependencies (format 1) (origin MECHANICAL) '// &
            '(source (document J3-24-007) (source-sha256 '//trim(source_hash)//')))'
        do i = 1, table%rule_count
            write (unit, '(a)', advance='no') '(dependency '
            write (unit, '(a)', advance='no') trim(table%rules(i)%rule)
            write (unit, '(a)', advance='no') ' (lhs '
            write (unit, '(a)', advance='no') trim(table%rules(i)%lhs)
            write (unit, '(a)', advance='no') ') (refs'
            do j = 1, table%rules(i)%reference_count
                write (unit, '(a)', advance='no') ' (ref '
                write (unit, '(a)', advance='no') trim(table%rules(i)%references(j))
                write (unit, '(a)', advance='no') ')'
            end do
            write (unit, '(a,i0,a)') ') (occurrences ', table%rules(i)%occurrences, '))'
        end do

        do i = 1, root_count
            write (unit, '(a)', advance='no') '(profile '
            write (unit, '(a)', advance='no') trim(profile)
            write (unit, '(a)', advance='no') ' (root '
            write (unit, '(a)', advance='no') trim(roots(i))
            write (unit, '(a)') '))'
        end do
        do i = 1, closure_count
            write (unit, '(a)', advance='no') '(profile '
            write (unit, '(a)', advance='no') trim(profile)
            write (unit, '(a)', advance='no') ' (member '
            write (unit, '(a)', advance='no') trim(table%rules(closure(i))%rule)
            write (unit, '(a)') '))'
        end do
        write (unit, '(a,i0,a,i0,a,i0,a,i0,a,i0,a)') &
            '(summary (source-records ', count_records(table), &
            ') (unique-rules ', table%rule_count, ') (duplicate-rule-ids ', &
            duplicate_count, ') (closure-rules ', closure_count, &
            ') (unresolved-references ', unresolved_count, '))'
    end subroutine dependency_write

    integer function count_records(table)
        type(dependency_table_t), intent(in) :: table
        integer :: i

        count_records = 0
        do i = 1, table%rule_count
            count_records = count_records + table%rules(i)%occurrences
        end do
    end function count_records

    recursive subroutine collect_references(node, references, reference_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(inout) :: references(:)
        integer, intent(inout) :: reference_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=dependency_name_length) :: label, reference
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            ok = .true.
            return
        end if
        if (node%child_count < 1) then
            ok = .true.
            return
        end if
        call read_atom(node%children(1), label, ok, message)
        if (.not. ok) return
        if (trim(label) == 'ref') then
            if (node%child_count /= 2) then
                ok = .false.
                message = 'ref node has the wrong field count'
                return
            end if
            call read_atom(node%children(2), reference, ok, message)
            if (.not. ok) return
            call add_reference(references, reference_count, reference, ok, message)
            return
        end if
        do i = 1, node%child_count
            call collect_references(node%children(i), references, reference_count, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine collect_references

    subroutine add_reference(references, reference_count, reference, ok, message)
        character(len=*), intent(inout) :: references(:)
        integer, intent(inout) :: reference_count
        character(len=*), intent(in) :: reference
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        do i = 1, reference_count
            if (trim(references(i)) == trim(reference)) then
                ok = .true.
                return
            end if
        end do
        if (reference_count >= size(references)) then
            message = 'too many references in one rule'
            return
        end if
        reference_count = reference_count + 1
        references(reference_count) = trim(reference)
        ok = .true.
    end subroutine add_reference

    logical function same_rule_shape(rule, lhs, references, reference_count)
        type(dependency_rule_t), intent(in) :: rule
        character(len=*), intent(in) :: lhs, references(:)
        integer, intent(in) :: reference_count
        integer :: i

        same_rule_shape = .false.
        if (trim(rule%lhs) /= trim(lhs)) return
        if (rule%reference_count /= reference_count) return
        do i = 1, reference_count
            if (.not. has_reference(rule%references, rule%reference_count, references(i))) &
                return
        end do
        same_rule_shape = .true.
    end function same_rule_shape

    logical function has_reference(references, reference_count, reference)
        character(len=*), intent(in) :: references(:), reference
        integer, intent(in) :: reference_count
        integer :: i

        has_reference = .false.
        do i = 1, reference_count
            if (trim(references(i)) == trim(reference)) then
                has_reference = .true.
                return
            end if
        end do
    end function has_reference

    integer function find_rule(table, rule)
        type(dependency_table_t), intent(in) :: table
        character(len=*), intent(in) :: rule
        integer :: i

        find_rule = 0
        do i = 1, table%rule_count
            if (trim(table%rules(i)%rule) == trim(rule)) then
                find_rule = i
                return
            end if
        end do
    end function find_rule

    subroutine find_lhs(table, lhs, index, match_count)
        type(dependency_table_t), intent(in) :: table
        character(len=*), intent(in) :: lhs
        integer, intent(out) :: index, match_count
        integer :: i

        index = 0
        match_count = 0
        do i = 1, table%rule_count
            if (trim(table%rules(i)%lhs) == trim(lhs)) then
                index = i
                match_count = match_count + 1
            end if
        end do
    end subroutine find_lhs

    subroutine read_lhs(node, lhs, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        lhs = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'lhs field is not a list'
            return
        end if
        if (node%child_count /= 2) then
            message = 'lhs field has the wrong field count'
            return
        end if
        if (.not. atom_is(node%children(1), 'lhs')) then
            message = 'lhs field has no lhs label'
            return
        end if
        call read_atom(node%children(2), lhs, ok, message)
    end subroutine read_lhs

    subroutine read_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'expected an SX atom'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = 'SX atom exceeds dependency buffer'
            return
        end if
        value = trim(node%atom)
        ok = len_trim(value) > 0
        if (.not. ok) message = 'SX atom is empty'
    end subroutine read_atom

    logical function atom_is(node, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: expected

        atom_is = .false.
        if (node%kind /= sx_atom) return
        atom_is = trim(node%atom) == expected
    end function atom_is

end module standardir_dependencies

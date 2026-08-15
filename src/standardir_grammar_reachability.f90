module standardir_grammar_reachability
    !! Generic post-normalization reachability for target grammar rules.

    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_sequence
    use standardir_grammar_targetnorm, only: standardir_target_expression_t, &
        standardir_target_provenance_t, standardir_target_rule_t, standardir_target_source_witness_t
    implicit none
    private

    type, public :: standardir_target_reachability_witness_t
        character(len=128) :: rule_id = ''
        character(len=128) :: lhs = ''
        integer :: alternative = 0
        character(len=4096) :: roots = ''
        character(len=128) :: reason = ''
        type(standardir_source_ref_t) :: source
        type(standardir_target_provenance_t), allocatable :: provenance(:)
        type(standardir_target_source_witness_t), allocatable :: source_witnesses(:)
        character(len=64) :: target_expression_sha256 = ''
    end type standardir_target_reachability_witness_t

    public :: standardir_grammar_select_reachable
    public :: standardir_grammar_validate_reachability

contains

    subroutine standardir_grammar_select_reachable(values, roots, retained, pruned, witness, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: roots(:)
        type(standardir_target_rule_t), allocatable, intent(out) :: retained(:), pruned(:)
        type(standardir_target_reachability_witness_t), allocatable, intent(out) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128), allocatable :: names(:)
        logical, allocatable :: edges(:,:), reachable(:)
        integer :: i, j
        type(standardir_target_reachability_witness_t) :: item

        if (allocated(retained)) deallocate (retained)
        if (allocated(pruned)) deallocate (pruned)
        if (allocated(witness)) deallocate (witness)
        allocate (retained(0), pruned(0), witness(0))
        ok = .false.
        message = ''
        if (size(values) < 1) then
            message = 'target reachability input is empty'
            return
        end if
        if (size(roots) < 1) then
            message = 'target reachability has no roots'
            return
        end if

        call collect_names(values, names)
        call build_edges(values, names, edges, ok, message)
        if (.not. ok) return
        call compute_reachable(edges, names, roots, reachable, ok, message)
        if (.not. ok) return

        do i = 1, size(values)
            j = find_name(names, trim(values(i)%lhs))
            if (reachable(j)) then
                call append_rule(retained, values(i))
            else
                call append_rule(pruned, values(i))
                item = standardir_target_reachability_witness_t()
                item%rule_id = trim(values(i)%id)
                item%lhs = trim(values(i)%lhs)
                item%alternative = values(i)%alternative
                item%roots = root_text(roots)
                if (size(roots) == 1) then
                    item%reason = 'not-reachable-from-selected-root'
                else
                    item%reason = 'not-reachable-from-declared-roots'
                end if
                item%source = values(i)%source
                item%target_expression_sha256 = values(i)%target_expression_sha256
                if (allocated(values(i)%provenance)) then
                    item%provenance = values(i)%provenance
                end if
                if (allocated(values(i)%source_witnesses)) then
                    item%source_witnesses = values(i)%source_witnesses
                end if
                call append_witness(witness, item)
            end if
        end do
        ok = .true.
        message = ''
    end subroutine standardir_grammar_select_reachable

    subroutine build_edges(values, names, edges, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), intent(in) :: names(:)
        logical, allocatable, intent(out) :: edges(:,:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, owner

        allocate (edges(size(names), size(names)))
        edges = .false.
        ok = .false.
        message = ''
        do i = 1, size(values)
            owner = find_name(names, trim(values(i)%lhs))
            if (owner == 0) then
                message = 'target rule has an unknown left-hand side'
                return
            end if
            call expression_edges(values(i)%expression, names, owner, edges)
        end do
        ok = .true.
    end subroutine build_edges

    subroutine compute_reachable(edges, names, roots, reachable, ok, message)
        logical, intent(in) :: edges(:,:)
        character(len=128), intent(in) :: names(:)
        character(len=*), intent(in) :: roots(:)
        logical, allocatable, intent(out) :: reachable(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, allocatable :: stack(:)
        integer :: i, j, current, root_index, top

        allocate (reachable(size(names)), stack(size(names)))
        reachable = .false.
        ok = .false.
        message = ''
        do i = 1, size(roots)
            if (len_trim(roots(i)) == 0) then
                message = 'target reachability contains an empty root'
                return
            end if
            root_index = find_name(names, trim(roots(i)))
            if (root_index == 0) then
                message = 'target reachability root is not a normalized rule: '//trim(roots(i))
                return
            end if
            if (.not. reachable(root_index)) then
                reachable(root_index) = .true.
                top = 1
                stack(top) = root_index
                do while (top > 0)
                    current = stack(top)
                    top = top - 1
                    do j = 1, size(names)
                        if (edges(current, j)) then
                            if (.not. reachable(j)) then
                                reachable(j) = .true.
                                top = top + 1
                                stack(top) = j
                            end if
                        end if
                    end do
                end do
            end if
        end do
        ok = .true.
    end subroutine compute_reachable

    subroutine standardir_grammar_validate_reachability(values, roots, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: roots(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: retained(:), pruned(:)
        type(standardir_target_reachability_witness_t), allocatable :: witness(:)

        call standardir_grammar_select_reachable(values, roots, retained, pruned, witness, ok, message)
        if (.not. ok) return
        if (size(pruned) > 0) then
            ok = .false.
            message = 'target reachability found an unreachable retained rule: '//trim(pruned(1)%lhs)
            return
        end if
        ok = .true.
        message = ''
    end subroutine standardir_grammar_validate_reachability

    subroutine collect_names(values, names)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), allocatable, intent(out) :: names(:)
        integer :: i

        allocate (names(0))
        do i = 1, size(values)
            if (find_name(names, trim(values(i)%lhs)) == 0) then
                call append_name(names, trim(values(i)%lhs))
            end if
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

    recursive subroutine expression_edges(expression, names, owner, edges)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: owner
        logical, intent(inout) :: edges(:,:)
        integer :: i, target

        select case (expression%kind)
        case (standardir_grammar_reference)
            target = find_name(names, trim(expression%name))
            if (target > 0) edges(owner, target) = .true.
        case (standardir_grammar_sequence, standardir_grammar_choice, &
                standardir_grammar_optional, standardir_grammar_repeat)
            if (allocated(expression%children)) then
                do i = 1, size(expression%children)
                    call expression_edges(expression%children(i), names, owner, edges)
                end do
            end if
        end select
    end subroutine expression_edges

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

    subroutine append_rule(values, value)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), intent(in) :: value
        type(standardir_target_rule_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_rule

    subroutine append_witness(values, value)
        type(standardir_target_reachability_witness_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_reachability_witness_t), intent(in) :: value
        type(standardir_target_reachability_witness_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_witness

    function root_text(roots) result(value)
        character(len=*), intent(in) :: roots(:)
        character(len=4096) :: value
        integer :: i, position, length

        value = ''
        position = 1
        do i = 1, size(roots)
            if (i > 1) then
                value(position:position) = ','
                position = position + 1
            end if
            length = len_trim(roots(i))
            value(position:position + length - 1) = trim(roots(i))
            position = position + length
        end do
    end function root_text

end module standardir_grammar_reachability

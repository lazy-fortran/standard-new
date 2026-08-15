module standardir_grammar_target_support
    !! Generic target records and source-backed role-family factoring.

    use standardir_grammar_producer, only: standardir_grammar_reference
    use standardir_grammar_target_records, only: append_expression, append_target, contains_expression, &
        same_expression, standardir_target_expression_t, standardir_target_provenance_t, &
        standardir_target_role_family_config_t, standardir_target_role_family_factored, &
        standardir_target_role_family_rejected, standardir_target_role_family_witness_t, &
        standardir_target_rule_t
    use standardir_grammar_target_identity, only: same_provenance, same_provenance_list, same_roles, &
        same_target_rule
    use standardir_grammar_target_fingerprint, only: standardir_target_expression_sha256
    implicit none
    private

    public :: standardir_grammar_factor_role_family
    public :: standardir_grammar_validate_role_family_witness
    public :: standardir_target_expression_t, standardir_target_provenance_t, standardir_target_rule_t
    public :: standardir_target_role_family_config_t, standardir_target_role_family_factored
    public :: standardir_target_role_family_rejected, standardir_target_role_family_witness_t
    public :: append_expression, append_name, append_target
    public :: collect_lhs_names, contains_expression
    public :: merge_provenance, merge_roles, same_expression, same_provenance

contains

    subroutine standardir_grammar_factor_role_family(values, config, factored, witness, ok, message, &
            protected_lhs)
        type(standardir_target_rule_t), intent(in) :: values(:)
        type(standardir_target_role_family_config_t), intent(in) :: config
        type(standardir_target_rule_t), allocatable, intent(out) :: factored(:)
        type(standardir_target_role_family_witness_t), allocatable, intent(out) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: protected_lhs(:)

        character(len=128), allocatable :: family_roles(:)
        type(standardir_target_provenance_t), allocatable :: family_provenance(:)
        logical, allocatable :: removed(:)
        call initialize_factor_outputs(factored, witness, ok, message)
        call validate_factor_request(values, config, ok, message)
        if (.not. ok) return
        if (.not. config%enabled) then
            factored = values
            ok = .true.
            return
        end if
        allocate (removed(size(values)), family_roles(0), family_provenance(0))
        removed = .false.
        call collect_representative_family(values, trim(config%representative), family_roles, &
            family_provenance)
        call classify_role_family(values, config%representative, protected_lhs, family_roles, &
            family_provenance, removed, witness)

        if (.not. any(removed)) then
            factored = values
            call finish_witness(witness, family_roles, family_provenance, &
                representative_target_hash(values, trim(config%representative)))
            ok = .true.
            return
        end if
        call materialize_factored_family(values, removed, config%representative, family_roles, &
            family_provenance, factored, witness)
        ok = .true.
        message = ''
    end subroutine standardir_grammar_factor_role_family

    subroutine validate_factor_request(values, config, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        type(standardir_target_role_family_config_t), intent(in) :: config
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: representative_index
        integer :: representative_count

        representative_index = 0
        ok = .false.
        message = ''
        if (size(values) < 1) then
            message = 'role-family factoring input is empty'
            return
        end if
        if (.not. config%enabled) then
            ok = .true.
            return
        end if
        if (len_trim(config%representative) == 0) then
            message = 'role-family factoring representative is empty'
            return
        end if
        representative_index = find_first_lhs(values, trim(config%representative))
        if (representative_index == 0) then
            message = 'role-family factoring representative is not retained: '// &
                trim(config%representative)
            return
        end if
        representative_count = count_lhs(values, trim(config%representative))
        if (representative_count == 1 .and. is_whole_unit_alias(values(representative_index))) then
            message = 'role-family factoring representative is itself an alias'
            return
        end if
        ok = .true.
    end subroutine validate_factor_request

    subroutine classify_role_family(values, representative, protected_lhs, family_roles, &
            family_provenance, removed, witness)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: representative
        character(len=*), intent(in), optional :: protected_lhs(:)
        character(len=128), allocatable, intent(inout) :: family_roles(:)
        type(standardir_target_provenance_t), allocatable, intent(inout) :: family_provenance(:)
        logical, intent(inout) :: removed(:)
        type(standardir_target_role_family_witness_t), allocatable, intent(inout) :: witness(:)
        character(len=128), allocatable :: family_names(:), merged_roles(:)
        type(standardir_target_provenance_t), allocatable :: merged_provenance(:)
        character(len=128) :: final_target
        integer :: i, count, alias_index
        logical :: safe, cycle, protected

        allocate (family_names(0))
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(representative)) cycle
            if (find_name(family_names, trim(values(i)%lhs)) > 0) cycle
            call append_name(family_names, trim(values(i)%lhs))
            count = count_lhs(values, trim(values(i)%lhs))
            alias_index = i
            if (count == 1) then
                call resolve_alias_target(values, trim(values(i)%lhs), trim(representative), &
                    final_target, safe, cycle)
                if (safe .and. trim(final_target) == trim(representative)) then
                    protected = is_protected_lhs(trim(values(i)%lhs), protected_lhs)
                    if (protected) then
                        call add_role_family_witness(witness, values(alias_index), family_roles, &
                            family_provenance, trim(representative), &
                            standardir_target_role_family_rejected, 'protected-reachability-root')
                    else
                        removed(i) = .true.
                        call merge_roles(family_roles, values(i)%source_roles, merged_roles)
                        call move_alloc(merged_roles, family_roles)
                        call merge_provenance(family_provenance, values(i)%provenance, merged_provenance)
                        call move_alloc(merged_provenance, family_provenance)
                    end if
                else if (cycle) then
                    call add_role_family_witness(witness, values(alias_index), family_roles, &
                        family_provenance, trim(representative), &
                        standardir_target_role_family_rejected, 'cyclic-unit-alias')
                end if
            else
                alias_index = unique_unit_alias_to_representative(values, trim(values(i)%lhs), &
                    trim(representative))
                if (alias_index > 0) then
                    call add_role_family_witness(witness, values(alias_index), family_roles, &
                        family_provenance, trim(representative), &
                        standardir_target_role_family_rejected, 'multi-alternative-alias')
                end if
            end if
        end do
    end subroutine classify_role_family

    subroutine materialize_factored_family(values, removed, representative, family_roles, &
            family_provenance, factored, witness)
        type(standardir_target_rule_t), intent(in) :: values(:)
        logical, intent(in) :: removed(:)
        character(len=*), intent(in) :: representative
        character(len=128), allocatable, intent(inout) :: family_roles(:)
        type(standardir_target_provenance_t), allocatable, intent(inout) :: family_provenance(:)
        type(standardir_target_rule_t), allocatable, intent(inout) :: factored(:)
        type(standardir_target_role_family_witness_t), allocatable, intent(inout) :: witness(:)
        type(standardir_target_provenance_t), allocatable :: alias_provenance(:), merged_provenance(:)
        integer :: i

        allocate (alias_provenance(0))
        do i = 1, size(values)
            if (removed(i)) then
                call merge_provenance(alias_provenance, values(i)%provenance, merged_provenance)
                call move_alloc(merged_provenance, alias_provenance)
            end if
        end do
        do i = 1, size(values)
            if (removed(i)) cycle
            call append_factored_rule(factored, values(i), values, removed, trim(representative), &
                family_roles, alias_provenance)
        end do
        deallocate (family_roles, family_provenance)
        allocate (family_roles(0), family_provenance(0))
        call collect_representative_family(factored, trim(representative), family_roles, family_provenance)
        call finish_witness(witness, family_roles, family_provenance, &
            representative_target_hash(factored, trim(representative)))
        do i = 1, size(values)
            if (removed(i)) call add_role_family_witness(witness, values(i), family_roles, &
                family_provenance, trim(representative), standardir_target_role_family_factored, &
                'whole-unit-alias')
        end do
        call finish_witness(witness, family_roles, family_provenance, &
            representative_target_hash(factored, trim(representative)))
        call sort_role_family_witness(witness)
    end subroutine materialize_factored_family

    subroutine initialize_factor_outputs(factored, witness, ok, message)
        type(standardir_target_rule_t), allocatable, intent(out) :: factored(:)
        type(standardir_target_role_family_witness_t), allocatable, intent(out) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        allocate (factored(0), witness(0))
        ok = .false.
        message = ''
    end subroutine initialize_factor_outputs

    subroutine collect_representative_family(values, representative, roles, provenance)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: representative
        character(len=128), allocatable, intent(inout) :: roles(:)
        type(standardir_target_provenance_t), allocatable, intent(inout) :: provenance(:)
        character(len=128), allocatable :: merged_roles(:)
        type(standardir_target_provenance_t), allocatable :: merged_provenance(:)
        integer :: i

        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(representative)) cycle
            call merge_roles(roles, values(i)%source_roles, merged_roles)
            call move_alloc(merged_roles, roles)
            call merge_provenance(provenance, values(i)%provenance, merged_provenance)
            call move_alloc(merged_provenance, provenance)
        end do
    end subroutine collect_representative_family

    subroutine append_factored_rule(factored, value, values, removed, representative, roles, alias_provenance)
        type(standardir_target_rule_t), allocatable, intent(inout) :: factored(:)
        type(standardir_target_rule_t), intent(in) :: value, values(:)
        logical, intent(in) :: removed(:)
        character(len=*), intent(in) :: representative
        character(len=128), allocatable, intent(in) :: roles(:)
        type(standardir_target_provenance_t), allocatable, intent(in) :: alias_provenance(:)
        type(standardir_target_rule_t) :: candidate
        character(len=128), allocatable :: merged_roles(:)
        type(standardir_target_provenance_t), allocatable :: merged_provenance(:)
        character(len=256) :: local_message
        logical :: local_ok

        candidate = value
        call replace_aliases(candidate%expression, values, removed, representative)
        if (trim(candidate%lhs) == trim(representative)) then
            call merge_roles(candidate%source_roles, roles, merged_roles)
            call move_alloc(merged_roles, candidate%source_roles)
            call merge_provenance(candidate%provenance, alias_provenance, merged_provenance)
            call move_alloc(merged_provenance, candidate%provenance)
        end if
        call standardir_target_expression_sha256(candidate%expression, candidate%target_expression_sha256, &
            local_ok, local_message)
        if (.not. local_ok) candidate%target_expression_sha256 = ''
        call append_target(factored, candidate)
    end subroutine append_factored_rule

    subroutine finish_witness(values, roles, provenance, representative_hash)
        type(standardir_target_role_family_witness_t), intent(inout) :: values(:)
        character(len=128), allocatable, intent(in) :: roles(:)
        type(standardir_target_provenance_t), allocatable, intent(in) :: provenance(:)
        character(len=*), intent(in) :: representative_hash
        integer :: i

        do i = 1, size(values)
            values(i)%source_roles = roles
            values(i)%representative_provenance = provenance
            values(i)%representative_target_expression_sha256 = trim(representative_hash)
        end do
    end subroutine finish_witness

    subroutine standardir_grammar_validate_role_family_witness(before, after, witness, ok, message)
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        type(standardir_target_role_family_witness_t), intent(in) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j

        ok = .false.
        message = ''
        if (size(before) < 1 .or. size(after) < 1) then
            message = 'role-family witness has an empty source or target'
            return
        end if
        if (size(witness) == 0) then
            call validate_identity(before, after, ok, message)
            return
        end if
        do i = 1, size(witness)
            do j = 1, i - 1
                if (trim(witness(j)%alias_role) == trim(witness(i)%alias_role)) then
                    message = 'role-family witness repeats a source role'
                    return
                end if
            end do
            if (i > 1) then
                if (trim(witness(i)%representative_role) /= trim(witness(1)%representative_role)) then
                    message = 'role-family witness uses multiple representatives'
                    return
                end if
            end if
            call validate_witness_item(before, after, witness, i, ok, message)
            if (.not. ok) return
        end do
        call validate_target_records(before, after, witness, ok, message)
        if (.not. ok) return
        call validate_witness_coverage(before, after, witness, ok, message)
    end subroutine standardir_grammar_validate_role_family_witness

    subroutine validate_identity(before, after, ok, message)
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = size(before) == size(after)
        if (.not. ok) then
            message = 'role-family identity changed the number of target alternatives'
            return
        end if
        do i = 1, size(before)
            ok = same_target_rule(before(i), after(i))
            if (.not. ok) then
                message = 'role-family identity changed a target record'
                return
            end if
        end do
        message = ''
    end subroutine validate_identity

    subroutine validate_target_records(before, after, witness, ok, message)
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        type(standardir_target_role_family_witness_t), intent(in) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_target_rule_t), allocatable :: expected(:)
        character(len=128), allocatable :: roles(:), merged_roles(:)
        type(standardir_target_provenance_t), allocatable :: provenance(:), alias_provenance(:)
        type(standardir_target_provenance_t), allocatable :: merged_provenance(:)
        logical, allocatable :: removed(:)
        logical :: expression_changed
        type(standardir_target_rule_t) :: candidate
        integer :: i, j, index
        character(len=128) :: representative

        ok = .false.
        message = ''
        representative = ''
        allocate (expected(0), removed(size(before)), roles(0), provenance(0), alias_provenance(0))
        removed = .false.
        do i = 1, size(witness)
            if (witness(i)%disposition /= standardir_target_role_family_factored) cycle
            representative = trim(witness(i)%representative_role)
            index = find_first_lhs(before, trim(witness(i)%alias_role))
            if (index == 0) then
                message = 'role-family witness names an absent source role'
                return
            end if
            removed(index) = .true.
        end do
        if (len_trim(representative) > 0) then
            call collect_representative_family(before, representative, roles, provenance)
            do i = 1, size(before)
                if (.not. removed(i)) cycle
                call merge_roles(roles, before(i)%source_roles, merged_roles)
                call move_alloc(merged_roles, roles)
            end do
        end if
        do i = 1, size(before)
            if (removed(i)) cycle
            candidate = before(i)
            expression_changed = .false.
            if (len_trim(representative) > 0) then
                call replace_aliases(candidate%expression, before, removed, representative)
                expression_changed = .not. same_expression(candidate%expression, before(i)%expression)
                if (trim(candidate%lhs) == representative) then
                    call merge_roles(candidate%source_roles, roles, merged_roles)
                    call move_alloc(merged_roles, candidate%source_roles)
                    do j = 1, size(before)
                        if (removed(j)) then
                            call merge_provenance(alias_provenance, before(j)%provenance, merged_provenance)
                            call move_alloc(merged_provenance, alias_provenance)
                        end if
                    end do
                    call merge_provenance(candidate%provenance, alias_provenance, merged_provenance)
                    call move_alloc(merged_provenance, candidate%provenance)
                end if
            end if
            if (expression_changed) then
                call standardir_target_expression_sha256(candidate%expression, &
                    candidate%target_expression_sha256, ok, message)
                if (.not. ok) return
            end if
            call append_target(expected, candidate)
        end do
        ok = size(expected) == size(after)
        if (ok) then
            do i = 1, size(expected)
                if (.not. same_target_rule(expected(i), after(i))) then
                    ok = .false.
                    exit
                end if
            end do
        end if
        if (.not. ok) message = 'role-family factoring changed an unexpected target record'
    end subroutine validate_target_records

    subroutine validate_witness_item(before, after, witness, item_index, ok, message)
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        type(standardir_target_role_family_witness_t), intent(in) :: witness(:)
        integer, intent(in) :: item_index
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_target_role_family_witness_t) :: item
        character(len=128), allocatable :: expected_roles(:)
        type(standardir_target_provenance_t), allocatable :: expected_provenance(:)
        integer :: alias_index, representative_index, count, selected
        character(len=128) :: final_target
        logical :: safe, cycle

        item = witness(item_index)
        ok = .false.
        if (item%disposition /= standardir_target_role_family_factored .and. &
            item%disposition /= standardir_target_role_family_rejected) then
            message = 'role-family witness has an invalid disposition'
            return
        end if
        alias_index = find_first_lhs(before, trim(item%alias_role))
        representative_index = find_first_lhs(after, trim(item%representative_role))
        if (alias_index == 0 .or. representative_index == 0) then
            message = 'role-family witness names an absent source or target role'
            return
        end if
        allocate (expected_roles(0), expected_provenance(0))
        call collect_representative_family(after, trim(item%representative_role), expected_roles, &
            expected_provenance)
        ok = same_roles(item%source_roles, expected_roles) .and. &
            same_provenance_list(item%representative_provenance, expected_provenance) .and. &
            trim(item%representative_target_expression_sha256) == &
            trim(representative_target_hash(after, trim(item%representative_role)))
        if (.not. ok) then
            message = 'role-family witness does not match representative target records'
            return
        end if
        count = count_lhs(before, trim(item%alias_role))
        if (trim(item%reason) == 'whole-unit-alias') then
            ok = item%disposition == standardir_target_role_family_factored .and. count == 1 .and. &
                is_whole_unit_alias(before(alias_index))
            call resolve_alias_target(before, trim(item%alias_role), trim(item%representative_role), &
                final_target, safe, cycle)
            ok = ok .and. safe .and. trim(final_target) == trim(item%representative_role)
            ok = ok .and. count_lhs(after, trim(item%alias_role)) == 0
        else if (trim(item%reason) == 'multi-alternative-alias') then
            selected = unique_unit_alias_to_representative(before, trim(item%alias_role), &
                trim(item%representative_role))
            ok = item%disposition == standardir_target_role_family_rejected .and. count > 1 .and. &
                selected > 0
            if (ok) then
                alias_index = selected
                ok = same_provenance_list(item%alias_provenance, before(selected)%provenance)
            end if
        else if (trim(item%reason) == 'cyclic-unit-alias') then
            ok = item%disposition == standardir_target_role_family_rejected .and. count == 1 .and. &
                is_whole_unit_alias(before(alias_index))
            call resolve_alias_target(before, trim(item%alias_role), trim(item%representative_role), &
                final_target, safe, cycle)
            ok = ok .and. cycle
        else if (trim(item%reason) == 'protected-reachability-root') then
            ok = item%disposition == standardir_target_role_family_rejected .and. count == 1 .and. &
                is_whole_unit_alias(before(alias_index))
            call resolve_alias_target(before, trim(item%alias_role), trim(item%representative_role), &
                final_target, safe, cycle)
            ok = ok .and. safe .and. trim(final_target) == trim(item%representative_role) .and. &
                count_lhs(after, trim(item%alias_role)) > 0
        else
            ok = .false.
            message = 'role-family witness has an unknown reason'
            return
        end if
        if (ok) ok = same_provenance_list(item%alias_provenance, before(alias_index)%provenance)
        if (ok) ok = trim(item%alias_target_expression_sha256) == &
            trim(before(alias_index)%target_expression_sha256)
        if (ok) ok = rule_source_in_lineage(before(alias_index), item%alias_provenance)
        if (.not. ok) then
            message = 'role-family witness does not match the exact source alternative'
            return
        end if
        message = ''
    end subroutine validate_witness_item

    logical function rule_source_in_lineage(rule, lineage)
        type(standardir_target_rule_t), intent(in) :: rule
        type(standardir_target_provenance_t), allocatable, intent(in) :: lineage(:)
        type(standardir_target_provenance_t) :: expected
        integer :: i

        rule_source_in_lineage = .false.
        if (.not. allocated(lineage)) return
        expected%source = rule%source
        expected%alternative = rule%alternative
        do i = 1, size(lineage)
            expected%source_expression_present = lineage(i)%source_expression_present
            expected%source_expression_sha256 = lineage(i)%source_expression_sha256
            if (same_provenance(lineage(i), expected)) then
                rule_source_in_lineage = .true.
                return
            end if
        end do
    end function rule_source_in_lineage

    subroutine validate_witness_coverage(before, after, witness, ok, message)
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        type(standardir_target_role_family_witness_t), intent(in) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j
        logical :: covered

        ok = .false.
        do i = 1, size(before)
            covered = .false.
            do j = 1, size(witness)
                if (trim(witness(j)%alias_role) == trim(before(i)%lhs)) then
                    covered = .true.
                    if (witness(j)%disposition == standardir_target_role_family_rejected .and. &
                        count_lhs(after, trim(before(i)%lhs)) > 0) then
                        covered = target_carries_source(after, before(i))
                    end if
                    exit
                end if
            end do
            if (.not. covered) then
                covered = target_carries_source(after, before(i))
                if (.not. covered .and. count_lhs(after, trim(before(i)%lhs)) == 0) then
                    message = 'role-family witness does not cover source role '//trim(before(i)%lhs)
                    return
                end if
            end if
        end do
        do i = 1, size(after)
            if (contains_removed_reference(after(i)%expression, witness)) then
                message = 'factored alias remains referenced in target output'
                return
            end if
        end do
        ok = .true.
        message = ''
    end subroutine validate_witness_coverage

    logical function target_carries_source(values, source)
        type(standardir_target_rule_t), intent(in) :: values(:), source
        integer :: i

        target_carries_source = .false.
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(source%lhs)) cycle
            if (provenance_contained(values(i)%provenance, source%provenance)) then
                target_carries_source = .true.
                return
            end if
        end do
    end function target_carries_source

    recursive logical function contains_removed_reference(expression, witness) result(found)
        type(standardir_target_expression_t), intent(in) :: expression
        type(standardir_target_role_family_witness_t), intent(in) :: witness(:)
        integer :: i

        found = .false.
        if (expression%kind == standardir_grammar_reference) then
            do i = 1, size(witness)
                if (witness(i)%disposition == standardir_target_role_family_factored .and. &
                    trim(expression%name) == trim(witness(i)%alias_role)) then
                    found = .true.
                    return
                end if
            end do
        end if
        if (.not. allocated(expression%children)) return
        do i = 1, size(expression%children)
            if (contains_removed_reference(expression%children(i), witness)) then
                found = .true.
                return
            end if
        end do
    end function contains_removed_reference

    logical function is_whole_unit_alias(value)
        type(standardir_target_rule_t), intent(in) :: value

        is_whole_unit_alias = value%expression%kind == standardir_grammar_reference .and. &
            len_trim(value%expression%name) > 0 .and. .not. has_children(value%expression)
    end function is_whole_unit_alias

    logical function has_children(expression)
        type(standardir_target_expression_t), intent(in) :: expression

        has_children = allocated(expression%children)
        if (has_children) has_children = size(expression%children) > 0
    end function has_children

    integer function find_first_lhs(values, lhs)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i

        find_first_lhs = 0
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(lhs)) then
                find_first_lhs = i
                return
            end if
        end do
    end function find_first_lhs

    integer function count_lhs(values, lhs)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i

        count_lhs = 0
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(lhs)) count_lhs = count_lhs + 1
        end do
    end function count_lhs

    integer function find_name(values, value)
        character(len=128), intent(in) :: values(:)
        character(len=*), intent(in) :: value
        integer :: i

        find_name = 0
        do i = 1, size(values)
            if (trim(values(i)) == trim(value)) then
                find_name = i
                return
            end if
        end do
    end function find_name

    integer function unique_unit_alias_to_representative(values, lhs, representative)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, representative
        character(len=128) :: final_target
        logical :: safe, cycle
        integer :: i, candidate_count

        unique_unit_alias_to_representative = 0
        candidate_count = 0
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(lhs)) cycle
            if (.not. is_whole_unit_alias(values(i))) cycle
            call resolve_alias_target(values, trim(values(i)%expression%name), trim(representative), &
                final_target, safe, cycle)
            if (safe .and. trim(final_target) == trim(representative)) then
                candidate_count = candidate_count + 1
                unique_unit_alias_to_representative = i
            end if
        end do
        if (candidate_count /= 1) unique_unit_alias_to_representative = 0
    end function unique_unit_alias_to_representative

    subroutine resolve_alias_target(values, start, representative, final_target, safe, cycle)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: start, representative
        character(len=128), intent(out) :: final_target
        logical, intent(out) :: safe, cycle
        logical, allocatable :: visited(:)
        character(len=128) :: current, next
        integer :: index

        allocate (visited(size(values)))
        visited = .false.
        current = trim(start)
        final_target = ''
        safe = .false.
        cycle = .false.
        do
            if (trim(current) == trim(representative)) then
                final_target = current
                safe = .true.
                return
            end if
            index = find_first_lhs(values, current)
            if (index == 0 .or. count_lhs(values, current) /= 1) return
            if (.not. is_whole_unit_alias(values(index))) return
            if (visited(index)) then
                cycle = .true.
                return
            end if
            visited(index) = .true.
            next = trim(values(index)%expression%name)
            current = next
        end do
    end subroutine resolve_alias_target

    logical function is_protected_lhs(lhs, protected_lhs)
        character(len=*), intent(in) :: lhs
        character(len=*), intent(in), optional :: protected_lhs(:)
        integer :: i

        is_protected_lhs = .false.
        if (.not. present(protected_lhs)) return
        do i = 1, size(protected_lhs)
            if (trim(lhs) == trim(protected_lhs(i))) then
                is_protected_lhs = .true.
                return
            end if
        end do
    end function is_protected_lhs

    recursive subroutine replace_aliases(expression, values, removed, representative)
        type(standardir_target_expression_t), intent(inout) :: expression
        type(standardir_target_rule_t), intent(in) :: values(:)
        logical, intent(in) :: removed(:)
        character(len=*), intent(in) :: representative
        integer :: i, index

        if (expression%kind == standardir_grammar_reference) then
            index = find_first_lhs(values, trim(expression%name))
            if (index > 0) then
                if (removed(index)) expression%name = trim(representative)
            end if
            return
        end if
        if (.not. allocated(expression%children)) return
        do i = 1, size(expression%children)
            call replace_aliases(expression%children(i), values, removed, representative)
        end do
    end subroutine replace_aliases

    subroutine add_role_family_witness(values, alias, family_roles, family_provenance, representative, &
            disposition, reason)
        type(standardir_target_role_family_witness_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), intent(in) :: alias
        character(len=128), intent(in) :: family_roles(:)
        type(standardir_target_provenance_t), intent(in) :: family_provenance(:)
        character(len=*), intent(in) :: representative, reason
        integer, intent(in) :: disposition
        type(standardir_target_role_family_witness_t) :: item

        item = standardir_target_role_family_witness_t()
        item%alias_role = trim(alias%lhs)
        item%representative_role = trim(representative)
        item%disposition = disposition
        item%reason = trim(reason)
        item%source_roles = family_roles
        item%representative_provenance = family_provenance
        item%alias_provenance = alias%provenance
        item%alias_target_expression_sha256 = alias%target_expression_sha256
        call append_role_family_witness(values, item)
    end subroutine add_role_family_witness

    function representative_target_hash(values, representative) result(hash)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: representative
        character(len=64) :: hash
        integer :: i

        hash = ''
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(representative)) then
                hash = values(i)%target_expression_sha256
                return
            end if
        end do
    end function representative_target_hash

    subroutine append_role_family_witness(values, value)
        type(standardir_target_role_family_witness_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_role_family_witness_t), intent(in) :: value
        type(standardir_target_role_family_witness_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_role_family_witness

    subroutine sort_role_family_witness(values)
        type(standardir_target_role_family_witness_t), intent(inout) :: values(:)
        type(standardir_target_role_family_witness_t) :: item
        integer :: i, j

        do i = 2, size(values)
            item = values(i)
            j = i - 1
            do while (j > 0)
                if (trim(values(j)%alias_role) <= trim(item%alias_role)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = item
        end do
    end subroutine sort_role_family_witness

    subroutine merge_roles(left, right, merged)
        character(len=128), allocatable, intent(in) :: left(:), right(:)
        character(len=128), allocatable, intent(out) :: merged(:)
        integer :: i

        allocate (merged(0))
        if (allocated(left)) then
            do i = 1, size(left)
                call append_unique_role(merged, left(i))
            end do
        end if
        if (allocated(right)) then
            do i = 1, size(right)
                call append_unique_role(merged, right(i))
            end do
        end if
    end subroutine merge_roles

    subroutine append_unique_role(values, value)
        character(len=128), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: value
        character(len=128), allocatable :: expanded(:)
        integer :: i, n

        if (len_trim(value) == 0) return
        do i = 1, size(values)
            if (trim(values(i)) == trim(value)) return
        end do
        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = trim(value)
        call move_alloc(expanded, values)
    end subroutine append_unique_role

    logical function provenance_contained(values, expected)
        type(standardir_target_provenance_t), allocatable, intent(in) :: values(:), expected(:)
        integer :: i, j

        provenance_contained = allocated(values) .and. allocated(expected)
        if (.not. provenance_contained) return
        do i = 1, size(expected)
            j = 1
            do while (j <= size(values))
                if (same_provenance(values(j), expected(i))) exit
                j = j + 1
            end do
            if (j > size(values)) then
                provenance_contained = .false.
                return
            end if
        end do
    end function provenance_contained

    subroutine merge_provenance(left, right, merged)
        type(standardir_target_provenance_t), allocatable, intent(in) :: left(:), right(:)
        type(standardir_target_provenance_t), allocatable, intent(out) :: merged(:)
        integer :: i

        allocate (merged(0))
        if (allocated(left)) then
            do i = 1, size(left)
                call append_provenance(merged, left(i))
            end do
        end if
        if (allocated(right)) then
            do i = 1, size(right)
                call append_provenance(merged, right(i))
            end do
        end if
    end subroutine merge_provenance

    subroutine append_provenance(values, value)
        type(standardir_target_provenance_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_provenance_t), intent(in) :: value
        type(standardir_target_provenance_t), allocatable :: expanded(:)
        integer :: i, n

        do i = 1, size(values)
            if (same_provenance(values(i), value)) return
        end do
        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_provenance

    subroutine collect_lhs_names(values, names, name_count)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=128), allocatable, intent(out) :: names(:)
        integer, intent(out) :: name_count
        integer :: i

        allocate (names(0))
        name_count = 0
        do i = 1, size(values)
            if (find_name(names, trim(values(i)%lhs)) == 0) then
                call append_name(names, trim(values(i)%lhs))
                name_count = name_count + 1
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

end module standardir_grammar_target_support

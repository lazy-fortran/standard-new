module standardir_grammar_transformation_witness
    !! Emit the target lowering provenance as deterministic JSONL.

    use standardir_export, only: standardir_validate_source_ref
    use standardir_grammar_export_support, only: standardir_grammar_apply_role_family
    use standardir_grammar_producer, only: standardir_grammar_origin_differential, &
        standardir_grammar_origin_mechanical, &
        standardir_grammar_origin_search, standardir_grammar_origin_smt, &
        standardir_grammar_origin_llm, standardir_grammar_origin_llm_repair, &
        standardir_grammar_origin_human, standardir_grammar_origin_imported, &
        standardir_grammar_rule_t
    use standardir_grammar_reachability, only: standardir_grammar_select_reachable, &
        standardir_target_reachability_witness_t
    use standardir_grammar_targetnorm, only: standardir_grammar_normalize, &
        standardir_target_provenance_t, standardir_target_role_family_config_t, &
        standardir_target_role_family_factored, standardir_target_role_family_rejected, &
        standardir_target_role_family_witness_t, standardir_target_rule_t
    implicit none
    private

    public :: standardir_grammar_emit_transformation_witness
    public :: standardir_grammar_validate_transformation_witness

contains

    subroutine standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
            selected_root, roots, role_family)
        integer, intent(in) :: unit
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        type(standardir_target_role_family_config_t), intent(in), optional :: role_family

        type(standardir_target_rule_t), allocatable :: normalized(:), pruned(:), before_role(:)
        type(standardir_target_rule_t), allocatable :: retained(:)
        type(standardir_target_reachability_witness_t), allocatable :: reachability(:)
        type(standardir_target_role_family_witness_t), allocatable :: role_witness(:)
        character(len=4096) :: profile
        character(len=128), allocatable :: profile_roots(:)
        logical :: reachability_mode, role_mode
        integer :: i

        ok = .false.
        message = ''
        if (present(selected_root) .and. present(roots)) then
            message = 'transformation witness cannot select a root and a root set together'
            return
        end if
        if (size(rules) < 1) then
            message = 'transformation witness input is empty'
            return
        end if
        call standardir_grammar_normalize(rules, normalized, pruned, ok, message)
        if (.not. ok) return
        call standardir_grammar_validate_transformation_witness(normalized, ok, message)
        if (.not. ok) return

        reachability_mode = present(selected_root) .or. present(roots)
        allocate (reachability(0))
        if (reachability_mode) then
            if (present(selected_root)) then
                allocate (profile_roots(1))
                profile_roots(1) = trim(selected_root)
            else
                if (size(roots) < 1) then
                    message = 'transformation witness root set is empty'
                    return
                end if
                allocate (profile_roots(size(roots)))
                profile_roots = roots
            end if
            call standardir_grammar_select_reachable(normalized, profile_roots, retained, pruned, &
                reachability, ok, message)
            if (.not. ok) return
        else
            retained = normalized
        end if
        before_role = retained
        if (present(role_family)) then
            call standardir_grammar_apply_role_family(retained, selected_root, roots, &
                reachability_mode, role_family, role_mode, role_witness, ok, message)
        else
            allocate (role_witness(0))
            role_mode = .false.
            ok = .true.
            message = ''
        end if
        if (.not. ok) return
        call standardir_grammar_validate_transformation_witness(retained, ok, message)
        if (.not. ok) return

        call make_profile(selected_root, roots, profile)
        write (unit, '(a)') &
            '{"kind":"transformation-witness-header","format":1,"origin":"MECHANICAL"}'
        do i = 1, size(retained)
            call emit_target_row(unit, retained(i), profile, ok, message)
            if (.not. ok) return
        end do
        do i = 1, size(reachability)
            call emit_reachability_row(unit, reachability(i), normalized, profile, ok, message)
            if (.not. ok) return
        end do
        if (role_mode .and. present(role_family)) then
            do i = 1, size(role_witness)
                call emit_role_row(unit, role_witness(i), before_role, retained, profile, ok, message)
                if (.not. ok) return
            end do
        end if
        ok = .true.
        message = ''
    end subroutine standardir_grammar_emit_transformation_witness

    subroutine standardir_grammar_validate_transformation_witness(values, ok, message)
        type(standardir_target_rule_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j, k
        logical :: found

        ok = .false.
        message = ''
        if (size(values) < 1) then
            message = 'transformation witness target set is empty'
            return
        end if
        do i = 1, size(values)
            if (len_trim(values(i)%id) == 0) then
                message = 'transformation witness target has an empty id or lhs'
                return
            end if
            if (len_trim(values(i)%lhs) == 0) then
                message = 'transformation witness target has an empty id or lhs'
                return
            end if
            if (values(i)%alternative < 1) then
                message = 'transformation witness target alternative is invalid'
                return
            end if
            if (len_trim(values(i)%target_expression_sha256) == 0) then
                message = 'transformation witness target lacks an expression hash'
                return
            end if
            if (values(i)%origin < standardir_grammar_origin_mechanical .or. &
                values(i)%origin > standardir_grammar_origin_differential) then
                message = 'transformation witness target origin is invalid'
                return
            end if
            call validate_provenance(values(i), ok, message)
            if (.not. ok) return
            if (allocated(values(i)%source_witnesses)) then
                do j = 1, size(values(i)%source_witnesses)
                    if (len_trim(values(i)%source_witnesses(j)%reason) == 0) then
                        message = 'transformation witness source reason is empty'
                        return
                    end if
                    if (trim(values(i)%source_witnesses(j)%target_rule_id) /= trim(values(i)%id) .or. &
                        trim(values(i)%source_witnesses(j)%target_lhs) /= trim(values(i)%lhs) .or. &
                        values(i)%source_witnesses(j)%target_alternative /= values(i)%alternative .or. &
                        trim(values(i)%source_witnesses(j)%target_expression_sha256) /= &
                        trim(values(i)%target_expression_sha256)) then
                        message = 'transformation witness source target is ambiguous'
                        return
                    end if
                    call standardir_validate_source_ref(values(i)%source_witnesses(j)%source%source, ok, &
                        message)
                    if (.not. ok) return
                    found = .false.
                    do k = 1, size(values(i)%provenance)
                        if (same_source_location(values(i)%source_witnesses(j)%source, &
                            values(i)%provenance(k))) found = .true.
                    end do
                    if (.not. found) then
                        message = 'transformation witness source is absent from target lineage'
                        return
                    end if
                    do k = 1, j - 1
                        if (same_source_location(values(i)%source_witnesses(j)%source, &
                            values(i)%source_witnesses(k)%source)) then
                            message = 'transformation witness source lineage is ambiguous'
                            return
                        end if
                    end do
                end do
            end if
        end do
        do i = 1, size(values)
            do j = 1, i - 1
                if (trim(values(i)%id) == trim(values(j)%id) .and. &
                    values(i)%alternative == values(j)%alternative) then
                    message = 'transformation witness target identity is ambiguous'
                    return
                end if
            end do
        end do
        ok = .true.
        message = ''
    end subroutine standardir_grammar_validate_transformation_witness

    subroutine validate_provenance(value, ok, message)
        type(standardir_target_rule_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j

        ok = .false.
        message = ''
        if (.not. allocated(value%provenance)) then
            message = 'transformation witness target has no lineage'
            return
        end if
        if (size(value%provenance) < 1) then
            message = 'transformation witness target has empty lineage'
            return
        end if
        do i = 1, size(value%provenance)
            if (value%provenance(i)%alternative < 1) then
                message = 'transformation witness source alternative is invalid'
                return
            end if
            call standardir_validate_source_ref(value%provenance(i)%source, ok, message)
            if (.not. ok) return
            if (value%provenance(i)%source_expression_present .and. &
                len_trim(value%provenance(i)%source_expression_sha256) == 0) then
                message = 'transformation witness source-backed lineage lacks a hash'
                return
            end if
            if (.not. value%provenance(i)%source_expression_present .and. &
                len_trim(value%provenance(i)%source_expression_sha256) > 0) then
                message = 'transformation witness source-less lineage carries a hash'
                return
            end if
            do j = 1, i - 1
                if (same_source_location(value%provenance(i), value%provenance(j))) then
                    message = 'transformation witness lineage is ambiguous'
                    return
                end if
            end do
        end do
        ok = .true.
        message = ''
    end subroutine validate_provenance

    logical function same_source_location(left, right)
        type(standardir_target_provenance_t), intent(in) :: left, right

        same_source_location = left%alternative == right%alternative .and. &
            trim(left%source%document) == trim(right%source%document) .and. &
            trim(left%source%clause) == trim(right%source%clause) .and. &
            trim(left%source%rule) == trim(right%source%rule) .and. &
            left%source%page == right%source%page .and. &
            left%source%end_page == right%source%end_page .and. &
            left%source%byte_start == right%source%byte_start .and. &
            left%source%byte_length == right%source%byte_length .and. &
            trim(left%source%source_hash) == trim(right%source%source_hash)
    end function same_source_location

    subroutine emit_target_row(unit, value, profile, ok, message)
        integer, intent(in) :: unit
        type(standardir_target_rule_t), intent(in) :: value
        character(len=*), intent(in) :: profile
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: alternatives, lineage, hashes
        character(len=64) :: transformation, reason

        call provenance_text(value%provenance, alternatives, lineage, hashes)
        reason = ''
        if (allocated(value%source_witnesses)) then
            if (size(value%source_witnesses) > 0) reason = trim(value%source_witnesses(1)%reason)
        end if
        if (any_source_less(value%provenance)) then
            transformation = 'generated-helper'
            reason = 'generated-helper'
        else if (size(value%provenance) > 1) then
            transformation = 'merged-provenance'
            reason = 'merged-provenance'
        else if (trim(hashes) == trim(value%target_expression_sha256)) then
            transformation = 'identity'
            if (len_trim(reason) == 0) reason = 'source-alternative-preservation'
        else
            transformation = 'normalized'
            reason = 'target-normalization'
        end if
        call write_row_start(unit, 'target', transformation, profile)
        call write_json_field(unit, 'source_alternative', alternatives)
        call write_json_field(unit, 'source_lineage', lineage)
        call write_json_field(unit, 'source_expression_sha256', hashes)
        call write_json_field(unit, 'target_rule', trim(value%id))
        call write_json_field(unit, 'target_lhs', trim(value%lhs))
        call write_json_field(unit, 'target_expression_sha256', trim(value%target_expression_sha256))
        call write_json_field(unit, 'reason', trim(reason))
        call write_json_field(unit, 'origin', origin_text(value%origin))
        call write_json_end(unit)
        ok = .true.
        message = ''
    end subroutine emit_target_row

    subroutine emit_reachability_row(unit, value, all_values, profile, ok, message)
        integer, intent(in) :: unit
        type(standardir_target_reachability_witness_t), intent(in) :: value
        type(standardir_target_rule_t), intent(in) :: all_values(:)
        character(len=*), intent(in) :: profile
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: alternatives, lineage, hashes
        integer :: origin, index

        call provenance_text(value%provenance, alternatives, lineage, hashes)
        index = find_lhs(all_values, trim(value%lhs))
        if (index == 0) then
            message = 'transformation witness omitted target is absent'
            ok = .false.
            return
        end if
        origin = all_values(index)%origin
        call write_row_start(unit, 'omitted', 'omitted-reachability', profile)
        call write_json_field(unit, 'source_alternative', alternatives)
        call write_json_field(unit, 'source_lineage', lineage)
        call write_json_field(unit, 'source_expression_sha256', hashes)
        call write_json_field(unit, 'target_rule', trim(value%rule_id))
        call write_json_field(unit, 'target_lhs', trim(value%lhs))
        call write_json_field(unit, 'target_expression_sha256', trim(value%target_expression_sha256))
        call write_json_field(unit, 'reason', trim(value%reason))
        call write_json_field(unit, 'origin', origin_text(origin))
        call write_json_end(unit)
        ok = .true.
        message = ''
    end subroutine emit_reachability_row

    subroutine emit_role_row(unit, value, before, after, profile, ok, message)
        integer, intent(in) :: unit
        type(standardir_target_role_family_witness_t), intent(in) :: value
        type(standardir_target_rule_t), intent(in) :: before(:), after(:)
        character(len=*), intent(in) :: profile
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=:), allocatable :: alternatives, lineage, hashes
        integer :: index, origin
        character(len=32) :: disposition
        type(standardir_target_rule_t) :: target

        if (value%disposition == standardir_target_role_family_factored) then
            index = find_lhs(after, trim(value%representative_role))
            if (index > 0) then
                target = after(index)
                origin = target%origin
            end if
        else if (value%disposition == standardir_target_role_family_rejected) then
            index = find_lhs(before, trim(value%alias_role))
            if (index > 0) then
                target = before(index)
                origin = target%origin
            end if
        else
            index = 0
        end if
        if (index == 0) then
            message = 'transformation witness role target is absent'
            ok = .false.
            return
        end if
        call provenance_text(value%alias_provenance, alternatives, lineage, hashes)
        disposition = role_disposition_text(value%disposition)
        call write_row_start(unit, 'role', 'role-family', profile)
        call write_json_field(unit, 'source_alternative', alternatives)
        call write_json_field(unit, 'source_lineage', lineage)
        call write_json_field(unit, 'source_expression_sha256', hashes)
        call write_json_field(unit, 'target_rule', trim(target%id))
        call write_json_field(unit, 'target_lhs', trim(target%lhs))
        call write_json_field(unit, 'target_expression_sha256', &
            trim(target%target_expression_sha256))
        call write_json_field(unit, 'reason', trim(value%reason))
        call write_json_field(unit, 'origin', origin_text(origin))
        call write_json_field(unit, 'role_alias', trim(value%alias_role))
        call write_json_field(unit, 'role_representative', trim(value%representative_role))
        call write_json_field(unit, 'role_disposition', trim(disposition))
        call write_json_end(unit)
        ok = .true.
        message = ''
    end subroutine emit_role_row

    subroutine write_row_start(unit, row_kind, transformation, profile)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: row_kind, transformation, profile

        write (unit, '(a)', advance='no') '{"kind":"transformation-witness","row_kind":'
        call json_string(unit, row_kind)
        write (unit, '(a)', advance='no') ',"transformation":'
        call json_string(unit, transformation)
        write (unit, '(a)', advance='no') ',"profile":'
        call json_string(unit, profile)
    end subroutine write_row_start

    subroutine write_json_field(unit, key, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: key, value

        write (unit, '(a)', advance='no') ',"'//trim(key)//'":'
        call json_string(unit, value)
    end subroutine write_json_field

    subroutine write_json_end(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') '}'
    end subroutine write_json_end

    subroutine json_string(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        integer :: i

        write (unit, '(a)', advance='no') '"'
        do i = 1, len_trim(value)
            select case (value(i:i))
            case ('"', '\')
                write (unit, '(a)', advance='no') '\'//value(i:i)
            case (achar(10))
                write (unit, '(a)', advance='no') '\n'
            case (achar(13))
                write (unit, '(a)', advance='no') '\r'
            case (achar(9))
                write (unit, '(a)', advance='no') '\t'
            case default
                write (unit, '(a)', advance='no') value(i:i)
            end select
        end do
        write (unit, '(a)', advance='no') '"'
    end subroutine json_string

    subroutine provenance_text(values, alternatives, lineage, hashes)
        type(standardir_target_provenance_t), allocatable, intent(in) :: values(:)
        character(len=:), allocatable, intent(out) :: alternatives, lineage, hashes
        character(len=256) :: item
        integer :: i

        alternatives = 'none'
        lineage = 'none'
        hashes = 'none'
        if (.not. allocated(values)) return
        if (size(values) < 1) return
        alternatives = ''
        lineage = ''
        hashes = ''
        do i = 1, size(values)
            if (i > 1) then
                alternatives = alternatives//','
                lineage = lineage//','
                hashes = hashes//','
            end if
            write (item, '(i0)') values(i)%alternative
            alternatives = alternatives//trim(item)
            write (item, '(a,":",i0,"@",i0,"+",i0)') trim(values(i)%source%rule), &
                values(i)%alternative, values(i)%source%byte_start, values(i)%source%byte_length
            lineage = lineage//trim(item)
            if (values(i)%source_expression_present) then
                hashes = hashes//trim(values(i)%source_expression_sha256)
            else
                hashes = hashes//'none'
            end if
        end do
    end subroutine provenance_text

    logical function any_source_less(values)
        type(standardir_target_provenance_t), allocatable, intent(in) :: values(:)
        integer :: i

        any_source_less = .false.
        if (.not. allocated(values)) return
        do i = 1, size(values)
            if (.not. values(i)%source_expression_present) then
                any_source_less = .true.
                return
            end if
        end do
    end function any_source_less

    subroutine make_profile(selected_root, roots, profile)
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        character(len=*), intent(out) :: profile
        integer :: i

        profile = 'all-rules'
        if (present(selected_root)) then
            profile = 'selected-root:'//trim(selected_root)
        else if (present(roots)) then
            profile = 'all-roots:'
            do i = 1, size(roots)
                if (i > 1) profile = trim(profile)//','
                profile = trim(profile)//trim(roots(i))
            end do
        end if
    end subroutine make_profile

    integer function find_lhs(values, lhs)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i

        find_lhs = 0
        do i = 1, size(values)
            if (trim(values(i)%lhs) == trim(lhs)) then
                find_lhs = i
                return
            end if
        end do
    end function find_lhs

    function origin_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        select case (value)
        case (standardir_grammar_origin_mechanical)
            text = 'MECHANICAL'
        case (standardir_grammar_origin_search)
            text = 'SEARCH'
        case (standardir_grammar_origin_smt)
            text = 'SMT'
        case (standardir_grammar_origin_llm)
            text = 'LLM'
        case (standardir_grammar_origin_llm_repair)
            text = 'LLM_REPAIR'
        case (standardir_grammar_origin_human)
            text = 'HUMAN'
        case (standardir_grammar_origin_imported)
            text = 'IMPORTED'
        case (standardir_grammar_origin_differential)
            text = 'DIFFERENTIAL'
        case default
            text = 'INVALID'
        end select
        text = trim(text)
    end function origin_text

    function role_disposition_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        select case (value)
        case (standardir_target_role_family_factored)
            text = 'factored'
        case (standardir_target_role_family_rejected)
            text = 'rejected'
        case default
            text = 'invalid'
        end select
        text = trim(text)
    end function role_disposition_text

end module standardir_grammar_transformation_witness

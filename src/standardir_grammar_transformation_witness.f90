module standardir_grammar_transformation_witness
    !! Emit the target lowering provenance as deterministic JSONL.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_export, only: standardir_validate_source_ref
    use standardir_grammar_correspondence, only: standardir_correspondence_ambiguous, &
        standardir_correspondence_mapped, standardir_correspondence_suppressed, &
        standardir_correspondence_unsupported, standardir_grammar_correspondence_trace_t
    use standardir_grammar_export_support, only: standardir_grammar_apply_role_family
    use standardir_grammar_producer, only: standardir_grammar_origin_differential, &
        standardir_grammar_origin_mechanical, &
        standardir_grammar_origin_search, standardir_grammar_origin_smt, &
        standardir_grammar_origin_llm, standardir_grammar_origin_llm_repair, &
        standardir_grammar_origin_human, standardir_grammar_origin_imported, &
        standardir_grammar_reference, standardir_grammar_rule_t, standardir_grammar_token, &
        standardir_grammar_sequence, standardir_grammar_choice, standardir_grammar_optional, &
        standardir_grammar_repeat
    use standardir_grammar_reachability, only: standardir_grammar_select_reachable, &
        standardir_target_reachability_witness_t
    use standardir_grammar_targetnorm, only: standardir_grammar_normalize, &
        standardir_target_provenance_t, standardir_target_role_family_config_t, &
        standardir_target_role_family_factored, standardir_target_role_family_rejected, &
        standardir_target_role_family_witness_t, standardir_target_rule_t, &
        standardir_target_source_witness_t
    use standardir_grammar_treesitter, only: standardir_grammar_lower_treesitter
    implicit none
    private

    public :: standardir_grammar_emit_transformation_witness
    public :: standardir_grammar_emit_correspondence_witness
    public :: standardir_grammar_validate_correspondence_trace
    public :: standardir_grammar_validate_transformation_witness
    public :: standardir_grammar_validate_source_disposition_witnesses

contains

    subroutine standardir_grammar_emit_correspondence_witness(unit, rules, ok, message)
        integer, intent(in) :: unit
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
        type(standardir_grammar_correspondence_trace_t), allocatable :: trace(:), ordered(:)
        integer :: i

        ok = .false.
        message = ''
        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message, trace)
        if (.not. ok) return
        call standardir_grammar_validate_correspondence_trace(trace, ok, message)
        if (.not. ok) return
        ordered = trace
        call sort_correspondence_trace(ordered)
        do i = 1, size(ordered)
            call emit_correspondence_row(unit, ordered(i))
        end do
        ok = .true.
        message = ''
    end subroutine standardir_grammar_emit_correspondence_witness

    subroutine standardir_grammar_validate_correspondence_trace(values, ok, message)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = .false.
        message = ''
        if (size(values) < 1) then
            message = 'correspondence witness trace is empty'
            return
        end if
        do i = 1, size(values)
            call validate_correspondence_row(values(i), ok, message)
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine standardir_grammar_validate_correspondence_trace

    subroutine validate_correspondence_row(value, ok, message)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        call standardir_validate_source_ref(value%source, ok, message)
        if (.not. ok) return
        ok = .false.
        if (value%source%end_page < 0) then
            message = 'correspondence witness source end page is invalid'
            return
        end if
        if (value%source%end_page > 0) then
            if (value%source%end_page < value%source%page) then
                message = 'correspondence witness source page range is invalid'
                return
            end if
        end if
        if (value%source%byte_start < 0_int64 .or. value%source%byte_length < 0_int64) then
            message = 'correspondence witness source byte range is invalid'
            return
        end if
        if (value%source_alternative < 1) then
            message = 'correspondence witness source alternative is invalid'
            return
        end if
        if (len_trim(value%raw_source_expression_path) == 0) then
            message = 'correspondence witness raw source path is empty'
            return
        end if
        select case (value%source_node_kind)
        case (standardir_grammar_reference, standardir_grammar_token, &
                standardir_grammar_sequence, &
                standardir_grammar_choice, standardir_grammar_optional, standardir_grammar_repeat)
        case default
            message = 'correspondence witness source node kind is invalid'
            return
        end select
        if (len_trim(value%source_node_name) == 0) then
            message = 'correspondence witness source node name is empty'
            return
        end if
        if (len_trim(value%source_boundary_role) == 0) then
            message = 'correspondence witness source boundary role is empty'
            return
        end if
        if (len_trim(value%target_rule_id) == 0 .or. len_trim(value%target_lhs) == 0) then
            message = 'correspondence witness target identity is incomplete'
            return
        end if
        if (value%target_alternative < 1) then
            message = 'correspondence witness target alternative is invalid'
            return
        end if
        if (len_trim(value%target_expression_path) > 0) then
            call validate_target_path(value%target_expression_path, ok, message)
            if (.not. ok) return
            ok = .false.
        end if
        if (value%target_sequence_boundary_slot < 0) then
            message = 'correspondence witness target sequence slot is invalid'
            return
        end if
        if (len_trim(value%transformation) == 0) then
            message = 'correspondence witness transformation is empty'
            return
        end if
        if (len_trim(value%input_expression_sha256) == 0 .or. &
            len_trim(value%output_expression_sha256) == 0) then
            message = 'correspondence witness expression hash is incomplete'
            return
        end if
        if (len_trim(value%source_expression_sha256) == 0 .or. &
            len_trim(value%target_expression_sha256) == 0) then
            message = 'correspondence witness source or target expression hash is incomplete'
            return
        end if
        if (.not. valid_correspondence_disposition(value%disposition)) then
            message = 'correspondence witness disposition is invalid'
            return
        end if
        if (len_trim(value%reason) == 0) then
            message = 'correspondence witness reason is empty'
            return
        end if
        ok = .true.
        message = ''
    end subroutine validate_correspondence_row

    logical function valid_correspondence_disposition(value)
        character(len=*), intent(in) :: value

        valid_correspondence_disposition = .false.
        if (trim(value) == standardir_correspondence_mapped) then
            valid_correspondence_disposition = .true.
            return
        end if
        if (trim(value) == standardir_correspondence_ambiguous) then
            valid_correspondence_disposition = .true.
            return
        end if
        if (trim(value) == standardir_correspondence_suppressed) then
            valid_correspondence_disposition = .true.
            return
        end if
        if (trim(value) == standardir_correspondence_unsupported) then
            valid_correspondence_disposition = .true.
        end if
    end function valid_correspondence_disposition

    subroutine validate_target_path(path, ok, message)
        character(len=*), intent(in) :: path
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (len_trim(path) < 3) then
            message = 'correspondence witness target path is invalid'
            return
        end if
        if (path(1:3) /= 'rhs') then
            message = 'correspondence witness target path is invalid'
            return
        end if
        if (len_trim(path) > 3) then
            if (path(4:4) /= '/') then
                message = 'correspondence witness target path is invalid'
                return
            end if
        end if
        ok = .true.
    end subroutine validate_target_path

    subroutine sort_correspondence_trace(values)
        type(standardir_grammar_correspondence_trace_t), intent(inout) :: values(:)
        type(standardir_grammar_correspondence_trace_t) :: item
        integer :: i, j

        do i = 2, size(values)
            item = values(i)
            j = i - 1
            do while (j >= 1)
                if (.not. correspondence_less(item, values(j))) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = item
        end do
    end subroutine sort_correspondence_trace

    logical function correspondence_less(left, right)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: left, right
        integer :: comparison

        correspondence_less = .false.
        comparison = compare_text(left%source%document, right%source%document)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%source%clause, right%source%clause)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%source%rule, right%source%rule)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%source%page, right%source%page)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%source%end_page, right%source%end_page)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_int64(left%source%byte_start, right%source%byte_start)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_int64(left%source%byte_length, right%source%byte_length)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%source%source_hash, right%source%source_hash)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%source_alternative, right%source_alternative)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%raw_source_expression_path, &
            right%raw_source_expression_path)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%source_node_kind, right%source_node_kind)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%source_node_name, right%source_node_name)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%source_boundary_role, right%source_boundary_role)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%target_rule_id, right%target_rule_id)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%target_lhs, right%target_lhs)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%target_alternative, right%target_alternative)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%target_expression_path, right%target_expression_path)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_integer(left%target_sequence_boundary_slot, &
            right%target_sequence_boundary_slot)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%transformation, right%transformation)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%input_expression_sha256, right%input_expression_sha256)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%output_expression_sha256, right%output_expression_sha256)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        comparison = compare_text(left%disposition, right%disposition)
        if (comparison /= 0) then
            correspondence_less = comparison < 0
            return
        end if
        correspondence_less = compare_text(left%reason, right%reason) < 0
    end function correspondence_less

    integer function compare_text(left, right)
        character(len=*), intent(in) :: left, right

        compare_text = 0
        if (trim(left) < trim(right)) compare_text = -1
        if (trim(left) > trim(right)) compare_text = 1
    end function compare_text

    integer function compare_integer(left, right)
        integer, intent(in) :: left, right

        compare_integer = 0
        if (left < right) compare_integer = -1
        if (left > right) compare_integer = 1
    end function compare_integer

    integer function compare_int64(left, right)
        integer(int64), intent(in) :: left, right

        compare_int64 = 0
        if (left < right) compare_int64 = -1
        if (left > right) compare_int64 = 1
    end function compare_int64

    subroutine emit_correspondence_row(unit, value)
        integer, intent(in) :: unit
        type(standardir_grammar_correspondence_trace_t), intent(in) :: value

        write (unit, '(a)', advance='no') '{"kind":"correspondence-witness"'
        call write_json_field(unit, 'source_document', trim(value%source%document))
        call write_json_field(unit, 'source_clause', trim(value%source%clause))
        call write_json_field(unit, 'source_rule', trim(value%source%rule))
        call write_json_integer(unit, 'source_page', value%source%page)
        call write_json_integer(unit, 'source_end_page', value%source%end_page)
        call write_json_int64(unit, 'source_byte_start', value%source%byte_start)
        call write_json_int64(unit, 'source_byte_length', value%source%byte_length)
        call write_json_field(unit, 'source_hash', trim(value%source%source_hash))
        call write_json_integer(unit, 'source_alternative', value%source_alternative)
        call write_json_field(unit, 'raw_source_path', trim(value%raw_source_expression_path))
        call write_json_integer(unit, 'source_node_kind', value%source_node_kind)
        call write_json_field(unit, 'source_node_name', trim(value%source_node_name))
        call write_json_field(unit, 'source_boundary_role', trim(value%source_boundary_role))
        call write_json_field(unit, 'target_rule', trim(value%target_rule_id))
        call write_json_field(unit, 'target_lhs', trim(value%target_lhs))
        call write_json_integer(unit, 'target_alternative', value%target_alternative)
        call write_json_field(unit, 'target_path', trim(value%target_expression_path))
        call write_json_integer(unit, 'target_sequence_slot', value%target_sequence_boundary_slot)
        call write_json_field(unit, 'transformation', trim(value%transformation))
        call write_json_field(unit, 'source_expression_sha256', &
            trim(value%source_expression_sha256))
        call write_json_field(unit, 'target_expression_sha256', &
            trim(value%target_expression_sha256))
        call write_json_field(unit, 'input_expression_sha256', trim(value%input_expression_sha256))
        call write_json_field(unit, 'output_expression_sha256', &
            trim(value%output_expression_sha256))
        call write_json_field(unit, 'disposition', trim(value%disposition))
        call write_json_field(unit, 'reason', trim(value%reason))
        call write_json_end(unit)
    end subroutine emit_correspondence_row

    subroutine write_json_integer(unit, key, value)
        integer, intent(in) :: unit, value
        character(len=*), intent(in) :: key
        character(len=32) :: text

        write (text, '(i0)') value
        write (unit, '(a)', advance='no') ',"'//trim(key)//'":'//trim(text)
    end subroutine write_json_integer

    subroutine write_json_int64(unit, key, value)
        integer, intent(in) :: unit
        integer(int64), intent(in) :: value
        character(len=*), intent(in) :: key
        character(len=64) :: text

        write (text, '(i0)') value
        write (unit, '(a)', advance='no') ',"'//trim(key)//'":'//trim(text)
    end subroutine write_json_int64

    subroutine standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
            selected_root, roots, role_family, pre_lowering_witnesses, treesitter_lowering)
        integer, intent(in) :: unit
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        type(standardir_target_role_family_config_t), intent(in), optional :: role_family
        type(standardir_target_source_witness_t), intent(in), optional :: pre_lowering_witnesses(:)
        logical, intent(in), optional :: treesitter_lowering

        type(standardir_target_rule_t), allocatable :: normalized(:), pruned(:), before_role(:)
        type(standardir_target_rule_t), allocatable :: retained(:)
        type(standardir_target_reachability_witness_t), allocatable :: reachability(:)
        type(standardir_target_role_family_witness_t), allocatable :: role_witness(:)
        character(len=4096) :: profile
        character(len=128), allocatable :: profile_roots(:)
        logical :: reachability_mode, role_mode
        logical :: entry_nullable
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
        if (present(treesitter_lowering)) then
            if (treesitter_lowering) then
                if (present(selected_root)) then
                    call standardir_grammar_lower_treesitter(retained, selected_root, entry_nullable, ok, message)
                else
                    call standardir_grammar_lower_treesitter(retained, &
                        entry_nullable=entry_nullable, ok=ok, message=message)
                end if
                if (.not. ok) return
            end if
        end if
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
        if (present(pre_lowering_witnesses)) then
            call validate_source_witnesses(pre_lowering_witnesses, ok, message)
            if (.not. ok) return
            do i = 1, size(pre_lowering_witnesses)
                call emit_pre_lowering_row(unit, pre_lowering_witnesses(i), profile, ok, message)
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

    subroutine standardir_grammar_validate_source_disposition_witnesses(expected, actual, ok, message)
        type(standardir_target_source_witness_t), intent(in) :: expected(:), actual(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j
        logical :: found

        ok = .false.
        message = ''
        call validate_source_witnesses(actual, ok, message)
        if (.not. ok) return
        do i = 1, size(expected)
            found = .false.
            do j = 1, size(actual)
                if (same_source_witness(expected(i), actual(j))) found = .true.
            end do
            if (.not. found) then
                ok = .false.
                message = 'source disposition witness coverage is incomplete'
                return
            end if
        end do
        ok = size(expected) == size(actual)
        if (.not. ok) message = 'source disposition witness coverage is ambiguous'
    end subroutine standardir_grammar_validate_source_disposition_witnesses

    subroutine validate_source_witnesses(values, ok, message)
        type(standardir_target_source_witness_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j

        ok = .false.
        message = ''
        do i = 1, size(values)
            if (len_trim(values(i)%target_rule_id) == 0 .or. &
                len_trim(values(i)%target_lhs) == 0) then
                message = 'source disposition witness target is incomplete'
                return
            end if
            if (values(i)%target_alternative < 1 .or. values(i)%source%alternative < 1) then
                message = 'source disposition witness alternative is invalid'
                return
            end if
            if (len_trim(values(i)%reason) == 0 .or. &
                len_trim(values(i)%target_expression_sha256) == 0) then
                message = 'source disposition witness reason or target hash is incomplete'
                return
            end if
            if (.not. values(i)%source%source_expression_present .or. &
                len_trim(values(i)%source%source_expression_sha256) == 0) then
                message = 'source disposition witness lacks a source hash'
                return
            end if
            call standardir_validate_source_ref(values(i)%source%source, ok, message)
            if (.not. ok) return
            do j = 1, i - 1
                if (same_source_witness(values(i), values(j))) then
                    message = 'source disposition witness is duplicated'
                    return
                end if
            end do
        end do
        ok = .true.
        message = ''
    end subroutine validate_source_witnesses

    logical function same_source_witness(left, right)
        type(standardir_target_source_witness_t), intent(in) :: left, right

        same_source_witness = .false.
        if (.not. same_source_location(left%source, right%source)) return
        if (left%source%source_expression_present .neqv. right%source%source_expression_present) return
        if (trim(left%source%source_expression_sha256) /= &
            trim(right%source%source_expression_sha256)) return
        if (trim(left%target_rule_id) /= trim(right%target_rule_id)) return
        if (trim(left%target_lhs) /= trim(right%target_lhs)) return
        if (left%target_alternative /= right%target_alternative) return
        if (trim(left%reason) /= trim(right%reason)) return
        if (trim(left%target_expression_sha256) /= trim(right%target_expression_sha256)) return
        same_source_witness = .true.
    end function same_source_witness

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
        else if (trim(reason) == 'tree-sitter-nullable-lowering') then
            transformation = 'tree-sitter-nullable-lowering'
        else
            transformation = 'normalized'
            if (len_trim(reason) == 0) reason = 'target-normalization'
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

    subroutine emit_pre_lowering_row(unit, value, profile, ok, message)
        integer, intent(in) :: unit
        type(standardir_target_source_witness_t), intent(in) :: value
        character(len=*), intent(in) :: profile
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_target_provenance_t), allocatable :: provenance(:)
        character(len=:), allocatable :: alternatives, lineage, hashes

        allocate (provenance(1))
        provenance(1) = value%source
        call provenance_text(provenance, alternatives, lineage, hashes)
        call write_row_start(unit, 'omitted', 'omitted-before-target-lowering', profile)
        call write_json_field(unit, 'source_alternative', alternatives)
        call write_json_field(unit, 'source_lineage', lineage)
        call write_json_field(unit, 'source_expression_sha256', hashes)
        call write_json_field(unit, 'target_rule', trim(value%target_rule_id))
        call write_json_field(unit, 'target_lhs', trim(value%target_lhs))
        call write_json_field(unit, 'target_expression_sha256', &
            trim(value%target_expression_sha256))
        call write_json_field(unit, 'reason', trim(value%reason))
        call write_json_field(unit, 'origin', origin_text(standardir_grammar_origin_mechanical))
        call write_json_end(unit)
        ok = .true.
        message = ''
    end subroutine emit_pre_lowering_row

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

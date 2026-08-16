module standardir_statement_boundary_source_mapping
    !! Map statement-boundary witnesses against authoritative raw StandardIR SX.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_choice, standardir_grammar_optional, &
        standardir_grammar_reference, standardir_grammar_repeat, standardir_grammar_sequence, &
        standardir_grammar_token
    use standardir_grammar_sx_adapter_support, only: read_syntax
    use standardir_statement_boundary, only: standardir_statement_boundary_plan_t, &
        standardir_statement_boundary_site_t
    use standardir_statement_boundary_mapping, only: standardir_boundary_ambiguous, standardir_boundary_mapped, &
        standardir_boundary_unsupported, standardir_statement_boundary_mapping_t
    use standardir_statement_sequence, only: standardir_statement_sequence_candidate_t
    implicit none
    private
    public :: standardir_statement_boundary_map_sx

contains

    subroutine standardir_statement_boundary_map_sx(plan, nodes, mappings, ok, message)
        type(standardir_statement_boundary_plan_t), intent(in) :: plan
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_statement_boundary_mapping_t), allocatable, intent(out) :: mappings(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_statement_boundary_mapping_t) :: value
        type(sx_node_t) :: expression
        type(standardir_source_ref_t) :: source
        character(len=128) :: rule, lhs
        integer :: i, j, matches, node_index, alternative
        logical :: local_ok

        ok = .false.; message = ''
        if (.not. allocated(plan%sites)) then; message = 'raw source mapping requires an allocated plan'; return; end if
        allocate (mappings(0))
        do i = 1, size(plan%sites)
            value = standardir_statement_boundary_mapping_t(); allocate (value%alternatives(0))
            value%candidate = plan%sites(i)%candidate; call copy_evidence(value, plan%sites(i)); matches = 0
            do j = 1, size(nodes)
                call read_syntax(nodes(j), rule, lhs, expression, source, local_ok, message)
                if (.not. local_ok) return
                if (.not. same_source(value%candidate, rule, lhs, source)) cycle
                call locate_raw(expression, value%candidate%expression_path, node_index, alternative, local_ok, message)
                if (.not. local_ok) then
                    value%disposition = standardir_boundary_unsupported; value%reason = trim(message); exit
                end if
                matches = matches + 1; value%source_node_index = node_index
                value%source_node_kind = raw_kind(expression, value%candidate%expression_path)
                value%source_node_name = raw_name(expression, value%candidate%expression_path)
                value%alternative = alternative; call append_int(value%alternatives, alternative)
            end do
            if (len_trim(value%disposition) == 0 .and. matches == 0) then
                value%disposition = standardir_boundary_unsupported; value%reason = 'source occurrence and raw path were not found'
            else if (len_trim(value%disposition) == 0 .and. matches > 1) then
                value%disposition = standardir_boundary_ambiguous
                value%reason = 'multiple raw source occurrences match the complete occurrence and path'
                value%alternative = 0
            else if (len_trim(value%disposition) == 0) then
                value%disposition = standardir_boundary_mapped
                value%reason = 'raw source occurrence and path resolved structurally'
            end if
            call append_mapping(mappings, value)
        end do
        ok = .true.
    end subroutine standardir_statement_boundary_map_sx

    subroutine copy_evidence(value, site)
        type(standardir_statement_boundary_mapping_t), intent(inout) :: value
        type(standardir_statement_boundary_site_t), intent(in) :: site

        if (allocated(site%evidence)) then
            value%evidence = site%evidence
        else
            allocate (value%evidence(1))
            value%evidence(1)%kind = trim(site%candidate%kind)
            value%evidence(1)%item = trim(site%candidate%item)
            value%evidence(1)%derivation = trim(site%candidate%derivation)
            value%evidence(1)%status = trim(site%candidate%status)
        end if
    end subroutine copy_evidence

    logical function same_source(candidate, rule, lhs, source)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        character(len=*), intent(in) :: rule, lhs
        type(standardir_source_ref_t), intent(in) :: source
        integer(int64) :: page, byte_start
        logical :: ok

        call decimal(candidate%source_page, page, ok); if (.not. ok) then; same_source = .false.; return; end if
        call decimal(candidate%source_byte_start, byte_start, ok); if (.not. ok) then; same_source = .false.; return; end if
        same_source = trim(candidate%source_rule) == trim(rule) .and. trim(candidate%source_lhs) == trim(lhs) .and. &
            trim(candidate%source_document) == trim(source%document) .and. &
            trim(candidate%source_clause) == trim(source%clause) .and. &
            trim(candidate%source_hash) == trim(source%source_hash) .and. &
            page == int(source%page, int64) .and. byte_start == source%byte_start
    end function same_source

    subroutine locate_raw(expression, text, index, alternative, ok, message)
        type(sx_node_t), intent(in) :: expression
        character(len=*), intent(in) :: text
        integer, intent(out) :: index, alternative
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, allocatable :: path(:)
        type(sx_node_t) :: node
        integer :: i, child

        call parse_path(text, path, ok, message); if (.not. ok) return
        node = expression; index = 1; alternative = 1
        if (label(node) == 'alt' .and. size(path) > 0) alternative = path(1)
        do i = 1, size(path)
            if (node%kind /= sx_list) then
                ok = .false.; message = 'raw source path is missing from the source tree'; return
            end if
            if (path(i) < 1 .or. path(i) >= node%child_count) then
                ok = .false.; message = 'raw source path is missing from the source tree'; return
            end if
            child = path(i) + 1; index = index + 1 + sibling_size(node, path(i)); node = node%children(child)
        end do
        ok = .true.; message = ''
    end subroutine locate_raw

    integer function sibling_size(node, number)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: number
        integer :: i
        sibling_size = 0
        do i = 2, number; sibling_size = sibling_size + tree_size(node%children(i)); end do
        end function sibling_size

        recursive integer function tree_size(node) result(size_value)
            type(sx_node_t), intent(in) :: node
            integer :: i
            size_value = 1
            if (node%kind /= sx_list) return
            do i = 2, node%child_count
                if (node%children(i)%kind == sx_list) size_value = size_value + tree_size(node%children(i))
            end do
        end function tree_size

        integer function raw_kind(expression, text)
            type(sx_node_t), intent(in) :: expression
            character(len=*), intent(in) :: text
            type(sx_node_t) :: node
            integer, allocatable :: path(:)
            logical :: ok
            character(len=256) :: message
            integer :: i
            call parse_path(text, path, ok, message); node = expression
            do i = 1, size(path); node = node%children(path(i) + 1); end do
                select case (label(node)); case ('ref'); raw_kind = standardir_grammar_reference
                case ('token'); raw_kind = standardir_grammar_token
                case ('seq'); raw_kind = standardir_grammar_sequence; case ('alt'); raw_kind = standardir_grammar_choice
                case ('optional'); raw_kind = standardir_grammar_optional; case ('repeat'); raw_kind = standardir_grammar_repeat
                case default; raw_kind = 0; end select
                end function raw_kind

                function raw_name(expression, text) result(name)
                    type(sx_node_t), intent(in) :: expression
                    character(len=*), intent(in) :: text
                    character(len=128) :: name
                    type(sx_node_t) :: node
                    integer, allocatable :: path(:)
                    logical :: ok
                    character(len=256) :: message
                    integer :: i
                    call parse_path(text, path, ok, message); node = expression
                    do i = 1, size(path); node = node%children(path(i) + 1); end do
                        name = '-'; if (label(node) == 'ref' .or. label(node) == 'token') name = trim(node%children(2)%atom)
                    end function raw_name

                    function label(node) result(value)
                        type(sx_node_t), intent(in) :: node
                        character(len=32) :: value
                        value = ''; if (node%kind /= sx_list) return; if (node%child_count < 1) return
                        if (node%children(1)%kind == sx_atom) value = trim(node%children(1)%atom)
                    end function label

                    subroutine parse_path(text, path, ok, message)
                        character(len=*), intent(in) :: text
                        integer, allocatable, intent(out) :: path(:)
                        logical, intent(out) :: ok
                        character(len=*), intent(out) :: message
                        character(len=512) :: value
                        integer :: i, first, number, ios, length
                        allocate (path(0)); value = trim(text); length = len_trim(value); ok = .false.; message = ''
                        if (length < 3 .or. value(:3) /= 'rhs') then
                            message = 'candidate canonical expression path is malformed'; return
                        end if
                        if (length == 3) then; ok = .true.; return; end if
                        i = 4
                        do while (i <= length)
                            if (value(i:i) /= '/') then
                                message = 'candidate canonical expression path is malformed'; return
                            end if
                            i = i + 1; first = i
                            do while (i <= length)
                                if (value(i:i) < '0' .or. value(i:i) > '9') exit
                                i = i + 1
                            end do
                            if (i == first .or. value(first:first) == '0') then
                                message = 'candidate canonical expression path is malformed'; return
                            end if
                            read (value(first:i - 1), *, iostat=ios) number
                            if (ios /= 0 .or. number < 1) then
                                message = 'candidate canonical expression path is malformed'; return
                            end if
                            call append_int(path, number)
                        end do
                        ok = .true.
                    end subroutine parse_path

                    subroutine decimal(text, value, ok)
                        character(len=*), intent(in) :: text
                        integer(int64), intent(out) :: value
                        logical, intent(out) :: ok
                        integer :: i, ios
                        value = 0_int64; ok = len_trim(text) > 0; if (.not. ok) return
                        do i = 1, len_trim(text)
                            if (text(i:i) < '0' .or. text(i:i) > '9') then; ok = .false.; return; end if
                        end do
                        read (text, *, iostat=ios) value; ok = ios == 0
                    end subroutine decimal

                    subroutine append_int(values, value)
                        integer, allocatable, intent(inout) :: values(:)
                        integer, intent(in) :: value
                        integer, allocatable :: expanded(:)
                        integer :: old_size
                        old_size = size(values); allocate (expanded(old_size + 1))
                        if (old_size > 0) expanded(:old_size) = values
                        expanded(old_size + 1) = value; call move_alloc(expanded, values)
                    end subroutine append_int

                    subroutine append_mapping(values, value)
                        type(standardir_statement_boundary_mapping_t), allocatable, intent(inout) :: values(:)
                        type(standardir_statement_boundary_mapping_t), intent(in) :: value
                        type(standardir_statement_boundary_mapping_t), allocatable :: expanded(:)
                        integer :: old_size
                        old_size = size(values); allocate (expanded(old_size + 1))
                        if (old_size > 0) expanded(:old_size) = values
                        expanded(old_size + 1) = value; call move_alloc(expanded, values)
                    end subroutine append_mapping

                end module standardir_statement_boundary_source_mapping

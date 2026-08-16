program sxstatementboundarymap
    !! Map source statement-boundary candidates to typed grammar nodes.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_origin_mechanical, &
        standardir_grammar_reference, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_repeat, standardir_grammar_token
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
    use standardir_grammar_sx_adapter_support, only: read_syntax
    use standardir_statement_boundary, only: standardir_statement_boundary_build_plan, &
        standardir_statement_boundary_plan_t
    use standardir_statement_boundary_mapping, only: standardir_statement_boundary_map, &
        standardir_statement_boundary_mapping_t
    use standardir_statement_sequence, only: standardir_statement_sequence_candidate_t
    implicit none

    character(len=4096) :: standardir_path, candidates_path, output_path, message
    type(sx_node_t), allocatable :: nodes(:)
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    type(standardir_statement_sequence_candidate_t), allocatable :: candidates(:), active(:)
    type(standardir_statement_boundary_plan_t) :: plan
    type(standardir_statement_boundary_mapping_t), allocatable :: mappings(:)
    type(standardir_statement_boundary_mapping_t), allocatable :: typed_mappings(:)
    integer :: argc
    logical :: ok

    argc = command_argument_count()
    if (argc /= 3) then
        print '(a)', 'usage: sxstatementboundarymap <standardir.sx> <candidates.tsv> <output.tsv>'
        stop 2
    end if
    call get_command_argument(1, standardir_path)
    call get_command_argument(2, candidates_path)
    call get_command_argument(3, output_path)
    call refuse_existing_output(output_path)

    call read_syntax_file(standardir_path, nodes, ok, message)
    if (.not. ok) call fail(trim(message))
    call adapt_rules(nodes, rules, ok, message)
    if (.not. ok) call fail(trim(message))
    call read_candidates(candidates_path, candidates, ok, message)
    if (.not. ok) call fail(trim(message))
    call active_candidates(candidates, active)
    !
    call standardir_statement_boundary_build_plan(active, plan, ok, message)
    if (.not. ok) call fail(trim(message))
    ! The typed call remains a validation gate.  Raw SX is authoritative for
    ! witness paths because adapter alternatives deliberately flatten roots.
    call standardir_statement_boundary_map(plan, rules, typed_mappings, ok, message)
    if (.not. ok) call fail(trim(message))
    call map_raw_sources(plan, nodes, mappings, ok, message)
    if (.not. ok) call fail(trim(message))
    call append_suppressed(candidates, mappings)
    call write_output(output_path, mappings, ok, message)
    if (.not. ok) call fail(trim(message))

contains

    subroutine map_raw_sources(plan, nodes, values, ok, message)
        type(standardir_statement_boundary_plan_t), intent(in) :: plan
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_statement_boundary_mapping_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_statement_boundary_mapping_t) :: value
        type(sx_node_t) :: expression
        type(standardir_source_ref_t) :: source
        character(len=128) :: rule, lhs
        integer :: i, j, matches, node_index, source_alternative
        logical :: local_ok

        allocate (values(0)); ok = .false.; message = ''
        do i = 1, size(plan%sites)
            value = standardir_statement_boundary_mapping_t()
            allocate (value%alternatives(0))
            value%candidate = plan%sites(i)%candidate
            matches = 0; source_alternative = 0
            do j = 1, size(nodes)
                call read_syntax(nodes(j), rule, lhs, expression, source, local_ok, message)
                if (.not. local_ok) return
                if (.not. same_candidate_source(value%candidate, rule, lhs, source)) cycle
                call raw_path_node(expression, value%candidate%expression_path, node_index, &
                    source_alternative, local_ok, message)
                if (.not. local_ok) then
                    value%disposition = 'unsupported'; value%reason = trim(message); exit
                end if
                matches = matches + 1
                value%source_node_index = node_index
                value%source_node_kind = raw_node_kind(expression, value%candidate%expression_path)
                value%source_node_name = raw_node_name(expression, value%candidate%expression_path)
                value%alternative = source_alternative
                call append_integer_value(value%alternatives, source_alternative)
            end do
            if (trim(value%candidate%status) == 'suppressed') then
                value%disposition = 'suppressed'; value%reason = 'candidate status is suppressed'
            else if (len_trim(value%disposition) == 0 .and. matches == 0) then
                value%disposition = 'unsupported'; value%reason = 'source occurrence and raw path were not found'
            else if (len_trim(value%disposition) == 0 .and. matches > 1) then
                value%disposition = 'ambiguous'
                value%reason = 'multiple raw source occurrences match the complete occurrence and path'
                value%alternative = 0
            else if (len_trim(value%disposition) == 0) then
                value%disposition = 'mapped'; value%reason = 'raw source occurrence and path resolved structurally'
            end if
            call append_mapping(values, value)
        end do
        ok = .true.
    end subroutine map_raw_sources

    logical function same_candidate_source(candidate, rule, lhs, source)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        character(len=*), intent(in) :: rule, lhs
        type(standardir_source_ref_t), intent(in) :: source
        integer(int64) :: page, byte_start

        call parse_int(candidate%source_page, page, same_candidate_source)
        if (.not. same_candidate_source) return
        call parse_int(candidate%source_byte_start, byte_start, same_candidate_source)
        if (.not. same_candidate_source) return
        same_candidate_source = trim(candidate%source_rule) == trim(rule) .and. &
            trim(candidate%source_lhs) == trim(lhs) .and. trim(candidate%source_document) == trim(source%document) .and. &
            trim(candidate%source_clause) == trim(source%clause) .and. &
            trim(candidate%source_hash) == trim(source%source_hash) .and. &
            page == int(source%page, int64) .and. byte_start == source%byte_start
    end function same_candidate_source

    subroutine raw_path_node(expression, path_text, index, alternative, ok, message)
        type(sx_node_t), intent(in) :: expression
        character(len=*), intent(in) :: path_text
        integer, intent(out) :: index, alternative
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer, allocatable :: path(:)
        type(sx_node_t) :: node
        integer :: i, child

        call parse_raw_path(path_text, path, ok, message)
        if (.not. ok) return
        node = expression; index = 1; alternative = 1
        if (trim_raw_label(node) == 'alt' .and. size(path) > 0) alternative = path(1)
        do i = 1, size(path)
            if (node%kind /= sx_list .or. path(i) < 1 .or. path(i) >= node%child_count) then
                ok = .false.; message = 'raw source path is missing from the source tree'; return
            end if
            child = path(i) + 1
            index = index + 1
            if (path(i) > 1) index = index + raw_sibling_size(node, path(i))
            node = node%children(child)
        end do
        ok = .true.; message = ''
    end subroutine raw_path_node

    integer function raw_sibling_size(node, child_number)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: child_number
        integer :: i

        raw_sibling_size = 0
        do i = 2, child_number
            raw_sibling_size = raw_sibling_size + raw_tree_size(node%children(i))
        end do
    end function raw_sibling_size

    recursive integer function raw_tree_size(node) result(total)
        type(sx_node_t), intent(in) :: node
        integer :: i

        total = 1
        if (node%kind /= sx_list) return
        do i = 2, node%child_count
            total = total + raw_tree_size(node%children(i))
        end do
    end function raw_tree_size

    integer function raw_node_kind(expression, path_text)
        type(sx_node_t), intent(in) :: expression
        character(len=*), intent(in) :: path_text
        integer, allocatable :: path(:)
        type(sx_node_t) :: node
        logical :: ok
        character(len=256) :: message
        integer :: i

        call parse_raw_path(path_text, path, ok, message); node = expression
        do i = 1, size(path)
            node = node%children(path(i) + 1)
        end do
        select case (trim_raw_label(node))
        case ('ref'); raw_node_kind = standardir_grammar_reference
        case ('token'); raw_node_kind = standardir_grammar_token
        case ('seq'); raw_node_kind = standardir_grammar_sequence
        case ('alt'); raw_node_kind = standardir_grammar_choice
        case ('optional'); raw_node_kind = standardir_grammar_optional
        case ('repeat'); raw_node_kind = standardir_grammar_repeat
        case default; raw_node_kind = 0
        end select
    end function raw_node_kind

    function raw_node_name(expression, path_text) result(name)
        type(sx_node_t), intent(in) :: expression
        character(len=*), intent(in) :: path_text
        character(len=128) :: name
        integer, allocatable :: path(:)
        type(sx_node_t) :: node
        logical :: ok
        character(len=256) :: message
        integer :: i

        call parse_raw_path(path_text, path, ok, message); node = expression
        do i = 1, size(path); node = node%children(path(i) + 1); end do
            name = '-'
            if (trim_raw_label(node) == 'ref' .or. trim_raw_label(node) == 'token') name = trim(node%children(2)%atom)
        end function raw_node_name

        subroutine parse_raw_path(text, path, ok, message)
            character(len=*), intent(in) :: text
            integer, allocatable, intent(out) :: path(:)
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
            integer :: i, first, number, ios, length
            character(len=512) :: value

            allocate (path(0)); ok = .false.; message = ''; value = trim(text); length = len_trim(value)
            if (length < 3 .or. value(:3) /= 'rhs') then
                message = 'candidate canonical expression path is malformed'; return
            end if
            if (length == 3) then; ok = .true.; return; end if
            i = 4
            do while (i <= length)
                if (value(i:i) /= '/') then; message = 'candidate canonical expression path is malformed'; return; end if
                i = i + 1; first = i
                do while (i <= length)
                    if (value(i:i) < '0' .or. value(i:i) > '9') exit
                    i = i + 1
                end do
                if (i == first .or. value(first:first) == '0') then
                    message = 'candidate canonical expression path is malformed'; return
                end if
                read (value(first:i - 1), *, iostat=ios) number
                if (ios /= 0 .or. number < 1) then; message = 'candidate canonical expression path is malformed'; return; end if
                call append_integer_value(path, number)
            end do
            ok = .true.
        end subroutine parse_raw_path

        subroutine parse_int(text, value, ok)
            character(len=*), intent(in) :: text
            integer(int64), intent(out) :: value
            logical, intent(out) :: ok
            integer :: i, ios

            value = 0_int64; ok = len_trim(text) > 0
            if (.not. ok) return
            do i = 1, len_trim(text)
                if (text(i:i) < '0' .or. text(i:i) > '9') then; ok = .false.; return; end if
            end do
            read (text, *, iostat=ios) value; ok = ios == 0
        end subroutine parse_int

        subroutine append_integer_value(values, value)
            integer, allocatable, intent(inout) :: values(:)
            integer, intent(in) :: value
            integer, allocatable :: expanded(:)
            integer :: old_size

            old_size = size(values); allocate (expanded(old_size + 1))
            if (old_size > 0) expanded(:old_size) = values
            expanded(old_size + 1) = value; call move_alloc(expanded, values)
        end subroutine append_integer_value

        subroutine append_mapping(values, value)
            type(standardir_statement_boundary_mapping_t), allocatable, intent(inout) :: values(:)
            type(standardir_statement_boundary_mapping_t), intent(in) :: value
            type(standardir_statement_boundary_mapping_t), allocatable :: expanded(:)
            integer :: old_size

            old_size = size(values); allocate (expanded(old_size + 1))
            if (old_size > 0) expanded(:old_size) = values
            expanded(old_size + 1) = value; call move_alloc(expanded, values)
        end subroutine append_mapping

        function trim_raw_label(node) result(label)
            type(sx_node_t), intent(in) :: node
            character(len=32) :: label

            label = ''
            if (node%kind == sx_list .and. node%child_count > 0 .and. node%children(1)%kind == sx_atom) &
                label = trim(node%children(1)%atom)
        end function trim_raw_label

        subroutine read_syntax_file(path, values, ok, message)
            character(len=*), intent(in) :: path
            type(sx_node_t), allocatable, intent(out) :: values(:)
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
            character(len=262144) :: line, detail
            type(sx_node_t) :: node
            integer :: unit, ios, line_number

            allocate (values(0)); ok = .false.; message = ''; line_number = 0
            open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
            if (ios /= 0) then
                message = 'cannot open StandardIR SX: '//trim(path)
                return
            end if
            do
                read (unit, '(a)', iostat=ios) line
                if (ios < 0) exit
                line_number = line_number + 1
                if (ios > 0) then
                    close (unit); call input_error(path, line_number, 'could not read SX record', message)
                    return
                end if
                if (len_trim(line) == 0) cycle
                call sx_parse(trim(line), node, ok, detail)
                if (.not. ok) then
                    close (unit); call input_error(path, line_number, trim(detail), message)
                    return
                end if
                if (is_label(node, 'standardir')) cycle
                if (.not. is_label(node, 'syntax')) then
                    close (unit); call input_error(path, line_number, 'expected syntax record', message)
                    return
                end if
                call append_node(values, node)
            end do
            close (unit)
            if (size(values) == 0) then
                message = trim(path)//': no StandardIR syntax records'
                return
            end if
            ok = .true.
        end subroutine read_syntax_file

        subroutine adapt_rules(nodes, values, ok, message)
            type(sx_node_t), intent(in) :: nodes(:)
            type(standardir_grammar_rule_t), allocatable, intent(out) :: values(:)
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
            type(standardir_grammar_rule_t), allocatable :: one(:)
            integer :: i

            allocate (values(0)); ok = .false.; message = ''
            do i = 1, size(nodes)
                call standardir_grammar_adapt_sx(nodes(i), standardir_grammar_origin_mechanical, &
                    standardir_grammar_resolution_resolved, one, ok, message, nodes(i))
                if (.not. ok) then
                    message = 'cannot adapt StandardIR syntax record '//itoa(i)//': '//trim(message)
                    return
                end if
                call append_rules(values, one)
            end do
            ok = size(values) > 0
            if (.not. ok) message = 'StandardIR SX adapted no grammar rules'
        end subroutine adapt_rules

        subroutine read_candidates(path, values, ok, message)
            character(len=*), intent(in) :: path
            type(standardir_statement_sequence_candidate_t), allocatable, intent(out) :: values(:)
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
            character(len=262144) :: line
            character(len=4096) :: fields(12), detail
            type(standardir_statement_sequence_candidate_t) :: value
            integer :: unit, ios, line_number, count

            allocate (values(0)); ok = .false.; message = ''; line_number = 0
            open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
            if (ios /= 0) then
                message = 'cannot open statement-sequence TSV: '//trim(path)
                return
            end if
            read (unit, '(a)', iostat=ios) line; line_number = 1
            if (ios /= 0) then
                close (unit); message = trim(path)//': invalid statement-sequence TSV header'; return
            end if
            call split_fields(trim(line), fields, count, detail)
            if (count /= 12 .or. trim(fields(1)) /= 'rule' .or. trim(fields(2)) /= 'container') then
                close (unit); message = trim(path)//': invalid statement-sequence TSV header'; return
            end if
            do
                read (unit, '(a)', iostat=ios) line
                if (ios < 0) exit
                line_number = line_number + 1
                if (ios > 0) then
                    close (unit); call input_error(path, line_number, 'could not read TSV record', message); return
                end if
                if (len_trim(line) == 0) cycle
                call split_fields(trim(line), fields, count, detail)
                if (count /= 12) then
                    close (unit); call input_error(path, line_number, trim(detail), message); return
                end if
                value = standardir_statement_sequence_candidate_t()
                value%source_rule = trim(fields(1)); value%source_lhs = trim(fields(2))
                value%source_document = trim(fields(3)); value%source_clause = trim(fields(4))
                value%source_page = trim(fields(5)); value%source_byte_start = trim(fields(6))
                value%source_hash = trim(fields(7)); value%kind = trim(fields(8))
                value%expression_path = trim(fields(9)); value%item = trim(fields(10))
                value%derivation = trim(fields(11)); value%status = trim(fields(12))
                if (len_trim(value%source_rule) == 0 .or. len_trim(value%source_lhs) == 0 .or. &
                    len_trim(value%status) == 0) then
                    close (unit); call input_error(path, line_number, 'candidate has empty required fields', message); return
                end if
                call append_candidate(values, value)
            end do
            close (unit)
            if (size(values) == 0) then
                message = trim(path)//': no candidate records'
                return
            end if
            ok = .true.
        end subroutine read_candidates

        subroutine active_candidates(all_values, values)
            type(standardir_statement_sequence_candidate_t), intent(in) :: all_values(:)
            type(standardir_statement_sequence_candidate_t), allocatable, intent(out) :: values(:)
            integer :: i

            allocate (values(0))
            do i = 1, size(all_values)
                if (trim(all_values(i)%status) /= 'suppressed') call append_candidate(values, all_values(i))
            end do
        end subroutine active_candidates

        subroutine append_suppressed(all_values, values)
            type(standardir_statement_sequence_candidate_t), intent(in) :: all_values(:)
            type(standardir_statement_boundary_mapping_t), allocatable, intent(inout) :: values(:)
            type(standardir_statement_boundary_mapping_t), allocatable :: expanded(:)
            integer :: i, old_size

            do i = 1, size(all_values)
                if (trim(all_values(i)%status) /= 'suppressed') cycle
                old_size = size(values); allocate (expanded(old_size + 1))
                if (old_size > 0) expanded(:old_size) = values
                expanded(old_size + 1) = standardir_statement_boundary_mapping_t()
                expanded(old_size + 1)%candidate = all_values(i)
                expanded(old_size + 1)%disposition = 'suppressed'
                expanded(old_size + 1)%reason = 'candidate status is suppressed'
                call move_alloc(expanded, values)
            end do
        end subroutine append_suppressed

        subroutine write_output(path, values, ok, message)
            character(len=*), intent(in) :: path
            type(standardir_statement_boundary_mapping_t), intent(in) :: values(:)
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
            integer :: unit, ios, i

            ok = .false.; message = ''
            open (newunit=unit, file=trim(path), status='new', action='write', iostat=ios)
            if (ios /= 0) then
                message = 'refusing to overwrite or create output: '//trim(path); return
            end if
            write (unit, '(a)', iostat=ios) output_header()
            do i = 1, size(values)
                if (ios /= 0) exit
                write (unit, '(a)', iostat=ios) trim(mapping_line(values(i)))
            end do
            close (unit)
            if (ios /= 0) then
                message = 'could not write boundary mapping TSV: '//trim(path); return
            end if
            ok = .true.
        end subroutine write_output

        function mapping_line(value) result(line)
            type(standardir_statement_boundary_mapping_t), intent(in) :: value
            character(len=16384) :: line
            character(len=1) :: tab

            tab = achar(9)
            line = trim(candidate_line(value%candidate))//tab//trim(value%disposition)//tab// &
                trim(itoa(value%source_node_index))//tab//trim(itoa(value%source_node_kind))//tab// &
                trim(value%source_node_name)//tab//trim(itoa(value%alternative))//tab// &
                alternatives_text(value%alternatives)//tab//trim(value%reason)
        end function mapping_line

        function candidate_line(value) result(line)
            type(standardir_statement_sequence_candidate_t), intent(in) :: value
            character(len=8192) :: line
            character(len=1) :: tab

            tab = achar(9)
            line = trim(value%source_rule)//tab//trim(value%source_lhs)//tab//trim(value%source_document)//tab// &
                trim(value%source_clause)//tab//trim(value%source_page)//tab//trim(value%source_byte_start)//tab// &
                trim(value%source_hash)//tab//trim(value%kind)//tab//trim(value%expression_path)//tab// &
                trim(value%item)//tab//trim(value%derivation)//tab//trim(value%status)
        end function candidate_line

        function alternatives_text(values) result(text)
            integer, intent(in), allocatable :: values(:)
            character(len=1024) :: text
            integer :: i

            text = ''
            if (.not. allocated(values)) return
            do i = 1, size(values)
                if (i > 1) text = trim(text)//','
                text = trim(text)//itoa(values(i))
            end do
        end function alternatives_text

        subroutine split_fields(line, fields, count, message)
            character(len=*), intent(in) :: line
            character(len=*), intent(out) :: fields(:)
            integer, intent(out) :: count
            character(len=*), intent(out) :: message
            integer :: i, start, length

            fields = ''; count = 0; message = 'malformed TSV record: expected 12 tab-separated fields'
            length = len_trim(line); start = 1
            do i = 1, length
                if (line(i:i) == achar(9)) then
                    count = count + 1
                    if (count <= size(fields)) fields(count) = line(start:i - 1)
                    start = i + 1
                end if
            end do
            count = count + 1
            if (count <= size(fields)) fields(count) = line(start:length)
        end subroutine split_fields

        subroutine append_node(values, value)
            type(sx_node_t), allocatable, intent(inout) :: values(:)
            type(sx_node_t), intent(in) :: value
            type(sx_node_t), allocatable :: expanded(:)
            integer :: old_size

            old_size = size(values); allocate (expanded(old_size + 1))
            if (old_size > 0) expanded(:old_size) = values
            expanded(old_size + 1) = value; call move_alloc(expanded, values)
        end subroutine append_node

        subroutine append_rules(values, extra)
            type(standardir_grammar_rule_t), allocatable, intent(inout) :: values(:)
            type(standardir_grammar_rule_t), intent(in) :: extra(:)
            type(standardir_grammar_rule_t), allocatable :: expanded(:)
            integer :: old_size, extra_size

            old_size = size(values); extra_size = size(extra); allocate (expanded(old_size + extra_size))
            if (old_size > 0) expanded(:old_size) = values
            if (extra_size > 0) expanded(old_size + 1:) = extra
            call move_alloc(expanded, values)
        end subroutine append_rules

        subroutine append_candidate(values, value)
            type(standardir_statement_sequence_candidate_t), allocatable, intent(inout) :: values(:)
            type(standardir_statement_sequence_candidate_t), intent(in) :: value
            type(standardir_statement_sequence_candidate_t), allocatable :: expanded(:)
            integer :: old_size

            old_size = size(values); allocate (expanded(old_size + 1))
            if (old_size > 0) expanded(:old_size) = values
            expanded(old_size + 1) = value; call move_alloc(expanded, values)
        end subroutine append_candidate

        function expected_header() result(text)
            character(len=4096) :: text
            text = 'rule'//achar(9)//'container'//achar(9)//'source_document'//achar(9)//'source_clause'//achar(9)// &
                'page'//achar(9)//'byte_start'//achar(9)//'source_sha256'//achar(9)//'kind'//achar(9)//'path'//achar(9)// &
                'item'//achar(9)//'derivation'//achar(9)//'status'
        end function expected_header

        function output_header() result(text)
            character(len=4096) :: text
            text = trim(expected_header())//achar(9)//'disposition'//achar(9)//'source_node_index'//achar(9)// &
                'source_node_kind'//achar(9)//'source_node_name'//achar(9)//'alternative'//achar(9)// &
                'alternatives'//achar(9)//'reason'
        end function output_header

        logical function is_label(node, label)
            type(sx_node_t), intent(in) :: node
            character(len=*), intent(in) :: label

            is_label = node%kind == sx_list .and. node%child_count > 0
            if (.not. is_label) return
            is_label = node%children(1)%kind == sx_atom
            if (is_label) is_label = trim(node%children(1)%atom) == trim(label)
        end function is_label

        function itoa(value) result(text)
            integer, intent(in) :: value
            character(len=32) :: text

            write (text, '(i0)') value
        end function itoa

        subroutine input_error(path, line_number, detail, message)
            character(len=*), intent(in) :: path, detail
            integer, intent(in) :: line_number
            character(len=*), intent(out) :: message

            write (message, '(a,i0,a,a)') trim(path), line_number, ': ', trim(detail)
        end subroutine input_error

        subroutine refuse_existing_output(path)
            character(len=*), intent(in) :: path
            logical :: exists

            inquire (file=trim(path), exist=exists)
            if (exists) call fail('refusing to overwrite output: '//trim(path))
        end subroutine refuse_existing_output

        subroutine fail(text)
            character(len=*), intent(in) :: text

            print '(a)', 'error: '//trim(text)
            stop 1
        end subroutine fail

    end program sxstatementboundarymap

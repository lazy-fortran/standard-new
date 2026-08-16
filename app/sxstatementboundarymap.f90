program sxstatementboundarymap
    !! Map source statement-boundary candidates to typed grammar nodes.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    use standardir_statement_boundary, only: standardir_statement_boundary_build_plan, &
        standardir_statement_boundary_plan_t
    use standardir_statement_boundary_mapping, only: standardir_statement_boundary_mapping_t
    use standardir_statement_boundary_source_mapping, only: standardir_statement_boundary_map_sx
    use standardir_statement_sequence, only: standardir_statement_sequence_candidate_t
    implicit none

    character(len=4096) :: standardir_path, candidates_path, output_path, message
    type(sx_node_t), allocatable :: nodes(:)
    type(standardir_statement_sequence_candidate_t), allocatable :: candidates(:), active(:)
    type(standardir_statement_boundary_plan_t) :: plan
    type(standardir_statement_boundary_mapping_t), allocatable :: mappings(:)
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
    call read_candidates(candidates_path, candidates, ok, message)
    if (.not. ok) call fail(trim(message))
    call active_candidates(candidates, active)
    call standardir_statement_boundary_build_plan(active, plan, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_statement_boundary_map_sx(plan, nodes, mappings, ok, message)
    if (.not. ok) call fail(trim(message))
    call append_non_candidates(candidates, mappings)
    call write_output(output_path, mappings, ok, message)
    if (.not. ok) call fail(trim(message))

contains

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
            select case (trim(value%status))
            case ('candidate', 'suppressed', 'unsupported')
                continue
            case default
                close (unit); call input_error(path, line_number, 'candidate has an unknown status', message); return
            end select
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
            if (trim(all_values(i)%status) == 'candidate') call append_candidate(values, all_values(i))
        end do
    end subroutine active_candidates

    subroutine append_non_candidates(all_values, values)
        type(standardir_statement_sequence_candidate_t), intent(in) :: all_values(:)
        type(standardir_statement_boundary_mapping_t), allocatable, intent(inout) :: values(:)
        type(standardir_statement_boundary_mapping_t), allocatable :: expanded(:)
        integer :: i, old_size

        do i = 1, size(all_values)
            if (trim(all_values(i)%status) == 'candidate') cycle
            old_size = size(values); allocate (expanded(old_size + 1))
            if (old_size > 0) expanded(:old_size) = values
            expanded(old_size + 1) = standardir_statement_boundary_mapping_t()
            expanded(old_size + 1)%candidate = all_values(i)
            expanded(old_size + 1)%disposition = trim(all_values(i)%status)
            expanded(old_size + 1)%reason = 'input candidate status is '//trim(all_values(i)%status)
            call move_alloc(expanded, values)
        end do
    end subroutine append_non_candidates

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
        write (unit, '(a)', iostat=ios) trim(output_header())
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
            trim(alternatives_text(value%alternatives))//tab//trim(value%reason)
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

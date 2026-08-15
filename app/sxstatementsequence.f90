program sxstatementsequence
    !! Derive a source-backed statement-sequence witness from line-oriented SX.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    use standardir_lexical_layout, only: standardir_layout_add, standardir_layout_reset, &
        standardir_layout_t, standardir_layout_validate
    use standardir_statement_sequence, only: standardir_statement_sequence_analyze, &
        standardir_statement_sequence_candidate_t
    implicit none

    character(len=4096) :: standardir_path, layout_path, output_path, message
    type(sx_node_t), allocatable :: nodes(:)
    type(standardir_layout_t) :: layout
    type(standardir_statement_sequence_candidate_t), allocatable :: values(:)
    integer :: argc
    logical :: ok, analysis_ok

    argc = command_argument_count()
    if (argc /= 3) then
        print '(a)', 'usage: sxstatementsequence <standardir.sx> <layout.sx> <output.tsv>'
        stop 2
    end if
    call get_command_argument(1, standardir_path)
    call get_command_argument(2, layout_path)
    call get_command_argument(3, output_path)
    call refuse_existing_output(output_path)

    call read_syntax_file(standardir_path, nodes, ok, message)
    if (.not. ok) call fail(trim(message))
    call read_layout_file(layout_path, layout, ok, message)
    if (.not. ok) call fail(trim(message))

    call standardir_statement_sequence_analyze(nodes, layout, values, analysis_ok, message)
    if (.not. analysis_ok) then
        if (.not. allocated(values)) then
            call fail('statement-sequence analysis failed: '//trim(message))
        end if
        if (size(values) == 0) then
            call fail('statement-sequence analysis failed: '//trim(message))
        end if
        call write_tsv(output_path, values, ok, message)
        if (.not. ok) call fail(trim(message))
        print '(a)', 'error: '//trim(message)
        stop 3
    end if

    call write_tsv(output_path, values, ok, message)
    if (.not. ok) call fail(trim(message))
contains

    subroutine read_syntax_file(path, values, ok, message)
        character(len=*), intent(in) :: path
        type(sx_node_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=262144) :: line
        character(len=262144) :: detail
        type(sx_node_t) :: node
        integer :: unit, ios, line_number

        allocate (values(0))
        ok = .false.; message = ''; line_number = 0
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
                close (unit)
                call input_error(path, line_number, 'could not read SX record', message)
                return
            end if
            if (len_trim(line) == 0) cycle
            call sx_parse(trim(line), node, ok, detail)
            if (.not. ok) then
                close (unit)
                call input_error(path, line_number, trim(detail), message)
                return
            end if
            if (is_label(node, 'syntax')) call append_node(values, node)
        end do
        close (unit)
        if (size(values) == 0) then
            message = trim(path)//': no StandardIR syntax records'
            return
        end if
        ok = .true.
    end subroutine read_syntax_file

    subroutine read_layout_file(path, layout, ok, message)
        character(len=*), intent(in) :: path
        type(standardir_layout_t), intent(out) :: layout
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=262144) :: line
        character(len=262144) :: detail
        type(sx_node_t) :: node
        integer :: unit, ios, line_number

        call standardir_layout_reset(layout)
        ok = .false.; message = ''; line_number = 0
        open (newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open lexical layout SX: '//trim(path)
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios < 0) exit
            line_number = line_number + 1
            if (ios > 0) then
                close (unit)
                call input_error(path, line_number, 'could not read SX record', message)
                return
            end if
            if (len_trim(line) == 0) cycle
            call sx_parse(trim(line), node, ok, detail)
            if (.not. ok) then
                close (unit)
                call input_error(path, line_number, trim(detail), message)
                return
            end if
            if (.not. is_layout_record(node)) cycle
            call standardir_layout_add(node, layout, ok, detail)
            if (.not. ok) then
                close (unit)
                call input_error(path, line_number, trim(detail), message)
                return
            end if
        end do
        close (unit)
        if (layout%count == 0) then
            message = trim(path)//': no lexical-layout records'
            return
        end if
        call standardir_layout_validate(layout, ok, message)
        if (.not. ok) message = trim(path)//': invalid v2 lexical layout: '//trim(message)
    end subroutine read_layout_file

    subroutine write_tsv(path, values, ok, message)
        character(len=*), intent(in) :: path
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: unit, ios, i

        ok = .false.; message = ''
        open (newunit=unit, file=trim(path), status='new', action='write', iostat=ios)
        if (ios /= 0) then
            message = 'refusing to overwrite or create output: '//trim(path)
            return
        end if
        write (unit, '(a)', iostat=ios) 'rule'//achar(9)//'container'//achar(9)// &
            'source_document'//achar(9)//'source_clause'//achar(9)//'page'//achar(9)// &
            'byte_start'//achar(9)//'source_sha256'//achar(9)//'kind'//achar(9)//'path'//achar(9)// &
            'item'//achar(9)//'derivation'//achar(9)//'status'
        if (ios == 0) then
            do i = 1, size(values)
                write (unit, '(a)', iostat=ios) trim(candidate_line(values(i)))
                if (ios /= 0) exit
            end do
        end if
        close (unit)
        if (ios /= 0) then
            message = 'could not write statement-sequence TSV: '//trim(path)
            return
        end if
        ok = .true.
    end subroutine write_tsv

    function candidate_line(value) result(line)
        type(standardir_statement_sequence_candidate_t), intent(in) :: value
        character(len=8192) :: line
        character(len=1) :: tab

        tab = achar(9)
        line = trim(value%source_rule)//tab//trim(value%source_lhs)//tab// &
            trim(value%source_document)//tab//trim(value%source_clause)//tab// &
            trim(value%source_page)//tab//trim(value%source_byte_start)//tab// &
            trim(value%source_hash)//tab//trim(value%kind)//tab//trim(value%expression_path)//tab// &
            trim(value%item)//tab//trim(value%derivation)//tab//trim(value%status)
    end function candidate_line

    subroutine append_node(values, value)
        type(sx_node_t), allocatable, intent(inout) :: values(:)
        type(sx_node_t), intent(in) :: value
        type(sx_node_t), allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_node

    logical function is_layout_record(node)
        type(sx_node_t), intent(in) :: node
        character(len=32) :: label

        is_layout_record = .false.
        if (node%kind /= sx_list .or. node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        label = trim(node%children(1)%atom)
        select case (label)
        case ('statement-boundary', 'statement-class-suffix', 'continuation', 'keyword-name-policy')
            is_layout_record = .true.
        end select
    end function is_layout_record

    logical function is_label(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label

        is_label = .false.
        if (node%kind /= sx_list .or. node%child_count < 1) return
        if (node%children(1)%kind /= sx_atom) return
        is_label = trim(node%children(1)%atom) == trim(label)
    end function is_label

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

end program sxstatementsequence

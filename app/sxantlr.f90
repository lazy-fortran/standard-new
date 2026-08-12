program sxantlr
    !! Emit a combined ANTLR4 grammar from StandardIR syntax objects grouped by lhs.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar, only: standardir_emit_antlr_group
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_records, standardir_max_syntax_groups
    implicit none

    character(len=4096) :: input_path, output_path, message
    character(len=65536) :: line
    type(sx_node_t) :: node, nodes(standardir_max_syntax_records)
    type(standardir_group_t) :: groups(standardir_max_syntax_groups)
    integer :: argc, input_unit, output_unit, ios, records, group_count, i
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <output.g4>'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open SX input')
    open (newunit=output_unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (input_unit)
        call fail('cannot open ANTLR output')
    end if

    write (output_unit, '(a)') 'grammar Fortran2023;'
    write (output_unit, '(a)')
    records = 0
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail(trim(message))
        if (is_header(node)) cycle
        if (records >= standardir_max_syntax_records) call fail('syntax record limit exceeded')
        records = records + 1
        nodes(records) = node
    end do
    call standardir_group_syntax(nodes, records, groups, group_count, ok, message)
    if (.not. ok) call fail(trim(message))
    do i = 1, group_count
        call standardir_emit_antlr_group(output_unit, nodes, groups(i), ok, message)
        if (.not. ok) call fail(trim(message))
    end do
    close (input_unit)
    close (output_unit)
    print '(a,i0,a,i0,a)', 'emitted ', group_count, ' ANTLR productions from ', records, ' records'

contains

    logical function is_header(node)
        type(sx_node_t), intent(in) :: node

        is_header = .false.
        if (node%kind /= 2 .or. node%child_count < 1) return
        if (node%children(1)%kind /= 1) return
        is_header = trim(node%children(1)%atom) == 'standardir'
    end function is_header

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'error: '//trim(text)
        stop 1
    end subroutine fail

end program sxantlr

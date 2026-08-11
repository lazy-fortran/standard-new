program sxnormalize
    !! Normalize StandardIR SX syntax objects into production JSONL.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_normalize, only: standardir_normalize_syntax
    implicit none

    character(len=4096) :: input_path, output_path, message
    character(len=65536) :: line, rule, lhs, notation
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios, records
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <output.jsonl>'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open SX input'
        stop 1
    end if
    open (newunit=output_unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (input_unit)
        print '(a)', 'error: cannot open normalized output'
        stop 1
    end if

    write (output_unit, '(a)') &
        '{"kind":"normalized-production-header","format":1,"origin":"MECHANICAL"}'
    records = 0
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail(input_unit, output_unit, message)
        if (is_header(node)) cycle
        call standardir_normalize_syntax(node, rule, lhs, notation, ok, message)
        if (.not. ok) call fail(input_unit, output_unit, message)
        write (output_unit, '(a)', advance='no') '{"kind":"normalized-production","rule":'
        call json_string(output_unit, rule)
        write (output_unit, '(a)', advance='no') ',"lhs":'
        call json_string(output_unit, lhs)
        write (output_unit, '(a)', advance='no') ',"notation":'
        call json_string(output_unit, notation)
        write (output_unit, '(a)') ',"origin":"MECHANICAL"}'
        records = records + 1
    end do
    close (input_unit)
    close (output_unit)
    print '(a,i0,a)', 'normalized ', records, ' StandardIR syntax objects'

contains

    logical function is_header(node)
        type(sx_node_t), intent(in) :: node

        is_header = .false.
        if (node%kind /= 2) return
        if (node%child_count < 1) return
        if (node%children(1)%kind /= 1) return
        if (trim(node%children(1)%atom) == 'standardir') is_header = .true.
    end function is_header

    subroutine json_string(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        integer :: i

        write (unit, '(a)', advance='no') '"'
        do i = 1, len_trim(value)
            if (value(i:i) == '"' .or. value(i:i) == achar(92)) then
                write (unit, '(a)', advance='no') achar(92)
            end if
            write (unit, '(a)', advance='no') value(i:i)
        end do
        write (unit, '(a)', advance='no') '"'
    end subroutine json_string

    subroutine fail(input_unit, output_unit, message)
        integer, intent(in) :: input_unit, output_unit
        character(len=*), intent(in) :: message

        close (input_unit)
        close (output_unit, status='delete')
        print '(a)', 'error: '//trim(message)
        stop 1
    end subroutine fail

end program sxnormalize

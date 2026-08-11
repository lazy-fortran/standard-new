program sxroundtrip
    !! Parse every SX record and rewrite it in canonical form.

    use fortsx, only: sx_node_t, sx_parse, sx_write
    implicit none

    character(len=4096) :: input_path, output_path, message
    character(len=16384) :: line
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios, records
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <output.sx>'
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
        print '(a)', 'error: cannot open SX output'
        stop 1
    end if

    records = 0
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) then
            close (input_unit)
            close (output_unit, status='delete')
            print '(a)', 'error: '//trim(message)
            stop 1
        end if
        call sx_write(output_unit, node, ok, message)
        if (.not. ok) then
            close (input_unit)
            close (output_unit, status='delete')
            print '(a)', 'error: '//trim(message)
            stop 1
        end if
        records = records + 1
    end do
    close (input_unit)
    close (output_unit)
    print '(a,i0,a)', 'round-tripped ', records, ' SX records'
end program sxroundtrip

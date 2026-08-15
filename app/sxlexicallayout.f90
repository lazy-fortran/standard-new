program sxlexicallayout
    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexical_layout, only: standardir_layout_add, standardir_layout_reset, &
        standardir_layout_t, standardir_layout_write
    implicit none
    character(len=4096) :: input_path, output_path, line, message
    type(standardir_layout_t) :: layout
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) error stop 'usage: sxlexicallayout <layout.sx> <layout.jsonl>'
    call get_command_argument(1, input_path); call get_command_argument(2, output_path)
    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', iostat=ios)
    if (ios /= 0) error stop 'cannot invalidate layout output'
    close (output_unit)
    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) error stop 'cannot open layout SX'
    call standardir_layout_reset(layout)
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail_input(input_unit, message)
        call standardir_layout_add(node, layout, ok, message)
        if (.not. ok) call fail_input(input_unit, message)
    end do
    close (input_unit)
    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', iostat=ios)
    if (ios /= 0) error stop 'cannot open layout output'
    call standardir_layout_write(layout, output_unit, ok, message)
    close (output_unit)
    if (.not. ok) error stop trim(message)

contains
    subroutine fail_input(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        close (unit); error stop trim(text)
    end subroutine fail_input
end program sxlexicallayout

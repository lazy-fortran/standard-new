program sxtreesitter
    !! Emit a tree-sitter grammar.js projection from StandardIR syntax objects.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_treesitter, only: standardir_emit_treesitter
    implicit none

    character(len=4096) :: input_path, output_path, message
    character(len=65536) :: line
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios, records
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <grammar.js>'
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
        call fail('cannot open tree-sitter output')
    end if

    write (output_unit, '(a)') '// Generated from StandardIR syntax'
    write (output_unit, '(a)') 'module.exports = grammar({'
    write (output_unit, '(a)') '  name: ''fortran2023'','
    write (output_unit, '(a)') '  rules: {'
    records = 0
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail(trim(message))
        if (is_header(node)) cycle
        call standardir_emit_treesitter(output_unit, node, ok, message)
        if (.not. ok) call fail(trim(message))
        records = records + 1
    end do
    write (output_unit, '(a)') '  }'
    write (output_unit, '(a)') '});'
    close (input_unit)
    close (output_unit)
    print '(a,i0,a)', 'emitted ', records, ' tree-sitter productions'

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

end program sxtreesitter

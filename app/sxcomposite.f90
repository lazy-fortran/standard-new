program sxcomposite
    !! Export a composite StandardIR stream containing syntax and lexical facts.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_composite, only: standardir_composite_add, standardir_composite_emit_antlr, &
        standardir_composite_emit_bison, standardir_composite_emit_ebnf, &
        standardir_composite_emit_treesitter, standardir_composite_reset, &
        standardir_composite_t
    implicit none

    character(len=4096) :: input_path, format, output_path, message
    character(len=65536) :: line
    type(standardir_composite_t) :: composite
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios
    logical :: ok

    argc = command_argument_count()
    if (argc /= 3) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <format> <output>'
        print '(a)', 'format is one of ebnf, antlr, bison, treesitter'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, format)
    call get_command_argument(3, output_path)

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open SX input')
    call standardir_composite_reset(composite)
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail(trim(message))
        call standardir_composite_add(composite, node, ok, message)
        if (.not. ok) call fail(trim(message))
    end do
    close (input_unit)

    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open composite output')
    select case (trim(format))
    case ('ebnf')
        call standardir_composite_emit_ebnf(output_unit, composite, ok, message)
    case ('antlr')
        call standardir_composite_emit_antlr(output_unit, composite, ok, message)
    case ('bison')
        call standardir_composite_emit_bison(output_unit, composite, ok, message)
    case ('treesitter')
        call standardir_composite_emit_treesitter(output_unit, composite, ok, message)
    case default
        close (output_unit)
        call fail('unknown composite format')
    end select
    close (output_unit)
    if (.not. ok) call fail(trim(message))
    print '(a,i0,a,i0,a)', 'emitted composite output with ', composite%syntax_count, &
        ' syntax records and ', composite%lexical%count, ' lexical facts'

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'error: '//trim(text)
        stop 1
    end subroutine fail

end program sxcomposite

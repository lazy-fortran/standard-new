program sxlexercontract
    !! Project source lexical-facts SX into a target-neutral JSONL contract.
    !!
    !! Usage: sxlexercontract <lexical-facts.sx> <lexer-contract.jsonl>
    !! Non-lexical top-level SX records are ignored, as they are by sxgrammar.
    !! A lexical-fact record is parsed by standardir_lexical_add; unknown fields,
    !! invalid provenance, and ambiguous facts fail without producing output.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexer_contract, only: standardir_lexer_contract_origin_mechanical, &
        standardir_lexer_contract_project, standardir_lexer_contract_t, &
        standardir_lexer_contract_write
    use standardir_lexical, only: standardir_lexical_add, standardir_lexical_facts_t, &
        standardir_lexical_reset
    implicit none

    character(len=4096) :: input_path, output_path, message
    character(len=65536) :: line
    type(standardir_lexical_facts_t) :: facts
    type(standardir_lexer_contract_t) :: contract
    type(sx_node_t) :: node
    integer :: argc, input_unit, output_unit, ios
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <lexical-facts.sx> <lexer-contract.jsonl>'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open lexical fact SX')
    call standardir_lexical_reset(facts)
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        call sx_parse(line, node, ok, message)
        if (.not. ok) call fail_input(input_unit, trim(message))
        if (is_label(node, 'lexical-fact')) then
            call standardir_lexical_add(node, facts, ok, message)
            if (.not. ok) call fail_input(input_unit, trim(message))
        end if
    end do
    close (input_unit)

    call standardir_lexer_contract_project(facts, standardir_lexer_contract_origin_mechanical, &
        contract, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open lexer contract output')
    call standardir_lexer_contract_write(contract, output_unit, ok, message)
    close (output_unit)
    if (.not. ok) call fail(trim(message))
    print '(a,i0,a)', 'emitted lexer contract with ', contract%count, ' tokens'

contains

    logical function is_label(value, label)
        type(sx_node_t), intent(in) :: value
        character(len=*), intent(in) :: label

        is_label = .false.
        if (value%kind /= 2) return
        if (value%child_count < 1) return
        if (value%children(1)%kind /= 1) return
        is_label = trim(value%children(1)%atom) == trim(label)
    end function is_label

    subroutine fail_input(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text

        close (unit)
        call fail(text)
    end subroutine fail_input

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'error: '//trim(text)
        stop 1
    end subroutine fail

end program sxlexercontract

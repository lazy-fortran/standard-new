program test_standardir_grammar
    !! Fixed EBNF is the independent oracle for the grammar projection.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar, only: standardir_emit_ebnf
    implicit none

    character(len=*), parameter :: input = &
        '(syntax R501 (lhs program) (rhs (seq (ref program-unit) '// &
        '(repeat (ref program-unit) 0 unbounded))) (source '// &
        '(document J3-24-007) (clause 5-15) (rule R501) (page 53) '// &
        '(source-sha256 abcdef)))'
    character(len=*), parameter :: expected_comment = &
        '(* rule=R501 document=J3-24-007 clause=5-15 page=53 '// &
        'source-sha256=abcdef *)'
    character(len=*), parameter :: expected_rule = &
        'program ::= program-unit { program-unit } ;'
    character(len=256) :: line, message
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call sx_parse(input, node, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/test_standardir_grammar.ebnf', &
        status='replace', action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open output fixture')
    call standardir_emit_ebnf(unit, node, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))

    open (newunit=unit, file='build/test_standardir_grammar.ebnf', &
        action='read', iostat=ios)
    if (ios /= 0) call fail('cannot read output fixture')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_comment) call fail('provenance differs')
    read (unit, '(a)', iostat=ios) line
    close (unit)
    if (ios /= 0 .or. trim(line) /= expected_rule) call fail('EBNF differs')
    print '(a)', 'StandardIR grammar test passed'

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

end program test_standardir_grammar

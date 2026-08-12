program test_standardir_grammar
    !! Fixed EBNF is the independent oracle for the grammar projection.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_bison, only: standardir_emit_bison
    use standardir_grammar, only: standardir_emit_antlr, standardir_emit_ebnf
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
    character(len=*), parameter :: expected_antlr_comment = &
        '// rule=R501 document=J3-24-007 clause=5-15 page=53 source-sha256=abcdef'
    character(len=*), parameter :: expected_antlr_name = 'r_program'
    character(len=*), parameter :: expected_antlr_rule = &
        '    : r_program_x2D_unit ( r_program_x2D_unit )*'
    character(len=*), parameter :: expected_bison_comment = &
        '/* rule=R501 document=J3-24-007 clause=5-15 page=53 source-sha256=abcdef */'
    character(len=*), parameter :: expected_bison_rule = 'r_program:'
    character(len=*), parameter :: expected_bison_rhs = &
        '    r_program_x2D_unit h_R501_1'
    character(len=*), parameter :: expected_bison_helper = 'h_R501_1:'
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

    open (newunit=unit, file='build/test_standardir_grammar.g4', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open ANTLR fixture')
    call standardir_emit_antlr(unit, node, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/test_standardir_grammar.g4', action='read', iostat=ios)
    if (ios /= 0) call fail('cannot read ANTLR fixture')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_antlr_comment) call fail('ANTLR provenance differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_antlr_name) call fail('ANTLR name differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_antlr_rule) call fail('ANTLR rule differs')
    close (unit)
    open (newunit=unit, file='build/test_standardir_grammar.y', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open Bison fixture')
    call standardir_emit_bison(unit, node, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/test_standardir_grammar.y', action='read', iostat=ios)
    if (ios /= 0) call fail('cannot read Bison fixture')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_bison_comment) call fail('Bison provenance differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_bison_rule) call fail('Bison name differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_bison_rhs) call fail('Bison rhs differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= '  ;') call fail('Bison rule terminator differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_bison_helper) call fail('Bison helper differs')
    close (unit)
    print '(a)', 'StandardIR grammar tests passed'

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

end program test_standardir_grammar

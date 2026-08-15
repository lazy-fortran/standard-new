program test_standardir_grammar
    !! Fixed EBNF is the independent oracle for the grammar projection.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir_bison, only: standardir_emit_bison
    use standardir_grammar, only: standardir_emit_antlr, standardir_emit_ebnf
    use standardir_lexical, only: standardir_lexical_facts_t
    use standardir_treesitter, only: standardir_emit_treesitter
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_groups
    implicit none

    character(len=*), parameter :: input = &
        '(syntax R501 (lhs program) (rhs (seq (ref program-unit) '// &
        '(repeat (ref program-unit) 0 unbounded))) (source '// &
        '(document J3-24-007) (clause 5-15) (rule R501) (page 53) '// &
        '(source-sha256 abcdef)))'
    character(len=*), parameter :: duplicate_input = &
        '(syntax R502 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5-15) (rule R502) (page 53) '// &
        '(source-sha256 abcdef)))'
    character(len=*), parameter :: expected_comment = &
        '(* rule=R501 document=J3-24-007 clause=5-15 page=53 '// &
        'source-canonical-text-sha256=abcdef *)'
    character(len=*), parameter :: expected_rule = &
        'program ::= program-unit { program-unit } ;'
    character(len=*), parameter :: expected_antlr_comment = &
        '// rule=R501 document=J3-24-007 clause=5-15 page=53 source-canonical-text-sha256=abcdef'
    character(len=*), parameter :: expected_antlr_name = 'r_program'
    character(len=*), parameter :: expected_antlr_rule = &
        '    : r_program_x2D_unit ( r_program_x2D_unit )*'
    character(len=*), parameter :: expected_bison_comment = &
        '/* rule=R501 document=J3-24-007 clause=5-15 page=53 source-canonical-text-sha256=abcdef */'
    character(len=*), parameter :: expected_bison_rule = 'r_program:'
    character(len=*), parameter :: expected_bison_rhs = &
        '    r_program_x2D_unit h_r_R501_1'
    character(len=*), parameter :: expected_bison_helper = 'h_r_R501_1:'
    character(len=*), parameter :: expected_bison_repeat = &
        '  | r_program_x2D_unit h_r_R501_1'
    character(len=*), parameter :: expected_treesitter_comment = &
        '// rule=R501 document=J3-24-007 clause=5-15 page=53 source-canonical-text-sha256=abcdef'
    character(len=*), parameter :: expected_treesitter_rule = &
        'r_program: $ => seq($.r_program_x2D_unit, repeat($.r_program_x2D_unit)),'
    character(len=256) :: line, message
    character(len=2048) :: lexical_input
    character(len=3) :: lexical_source
    type(sx_node_t) :: node, nodes(2)
    type(standardir_lexical_facts_t) :: lexical
    type(standardir_group_t) :: groups(standardir_max_syntax_groups)
    integer :: unit, ios, group_count
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
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= '    %empty') call fail('Bison repeat base differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_bison_repeat) call fail('Bison repeat direction differs')
    close (unit)
    open (newunit=unit, file='build/test_standardir_grammar.js', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open tree-sitter fixture')
    call standardir_emit_treesitter(unit, node, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/test_standardir_grammar.js', action='read', iostat=ios)
    if (ios /= 0) call fail('cannot read tree-sitter fixture')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_treesitter_comment) &
        call fail('tree-sitter provenance differs')
    read (unit, '(a)', iostat=ios) line
    if (ios /= 0 .or. trim(line) /= expected_treesitter_rule) &
        call fail('tree-sitter rule differs')
    close (unit)
    call sx_parse(input, nodes(1), ok, message)
    if (.not. ok) call fail(trim(message))
    call sx_parse(duplicate_input, nodes(2), ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_group_syntax(nodes, 2, groups, group_count, ok, message)
    if (.not. ok) call fail(trim(message))
    if (group_count /= 1 .or. groups(1)%count /= 2 .or. &
        groups(1)%indices(1) /= 1 .or. groups(1)%indices(2) /= 2) &
        call fail('duplicate lhs grouping differs')

    lexical_source = achar(194)//achar(164)
    lexical_input = '(syntax RULE-LEXICAL (lhs lexical) (rhs (seq (ref '// &
        lexical_source//') (token '//lexical_source//') (ref ordinary))) '// &
        '(source (document DOC) (clause C) (rule RULE-LEXICAL) (page 1) '// &
        '(source-sha256 HASH)))'
    call sx_parse(trim(lexical_input), node, ok, message)
    if (.not. ok) call fail(trim(message))
    call make_lexical_facts(lexical, lexical_source)
    open (newunit=unit, file='build/test_standardir_grammar_lexical.ebnf', &
        status='replace', action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open lexical EBNF fixture')
    call standardir_emit_ebnf(unit, node, ok, message, lexical)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/test_standardir_grammar_lexical.ebnf', &
        action='read', iostat=ios)
    if (ios /= 0) call fail('cannot read lexical EBNF fixture')
    read (unit, '(a)', iostat=ios) line
    read (unit, '(a)', iostat=ios) line
    close (unit)
    if (ios /= 0 .or. trim(line) /= 'lexical ::= - "-" ordinary ;') &
        call fail('lexical EBNF canonical spelling differs')

    lexical%facts(1)%canonical_spelling = lexical_source
    open (newunit=unit, status='scratch', action='write', iostat=ios)
    if (ios /= 0) call fail('cannot open lexical negative control')
    call standardir_emit_ebnf(unit, node, ok, message, lexical)
    close (unit)
    if (ok) call fail('invalid canonical lexical spelling was accepted')
    print '(a)', 'StandardIR grammar tests passed'

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

    subroutine make_lexical_facts(facts, source)
        type(standardir_lexical_facts_t), intent(out) :: facts
        character(len=*), intent(in) :: source

        facts%count = 1
        facts%facts(1)%source_term = source
        facts%facts(1)%canonical_spelling = '-'
        facts%facts(1)%class_name = 'constructed-class'
        facts%facts(1)%target_name = 'GENERIC_DASH'
        facts%facts(1)%source_rule = 'SOURCE-RULE'
        facts%facts(1)%source_page = '1'
        facts%facts(1)%document = 'DOC'
        facts%facts(1)%clause = 'C'
        facts%facts(1)%source_hash = repeat('a', 64)
        facts%facts(1)%codepoint = 'U+00A4'
        facts%facts(1)%range_count = 1
        facts%facts(1)%range_first(1) = 164_int64
        facts%facts(1)%range_last(1) = 164_int64
    end subroutine make_lexical_facts

end program test_standardir_grammar

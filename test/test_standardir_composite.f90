program test_standardir_composite
    !! Independent fixed facts verify the composite lexical export boundary.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_composite, only: standardir_composite_add, &
        standardir_composite_emit_antlr, standardir_composite_emit_bison, &
        standardir_composite_emit_ebnf, standardir_composite_emit_treesitter, &
        standardir_composite_reset, &
        standardir_composite_t
    use standardir_lexical, only: standardir_lexical_validate
    implicit none

    character(len=8192) :: message
    character(len=65536) :: input, line
    type(standardir_composite_t) :: composite
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call standardir_composite_reset(composite)
    input = '(syntax R501 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (rule R501) (page 53) '// &
        '(source-sha256 7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2) ))'
    call sx_parse(trim(input), node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_composite_add(composite, node, ok, message)
    if (.not. ok) call fail(trim(message))

    input = '(lexical-fact (source-term "–") (canonical-spelling "-") '// &
        '(class unicode-lexical) '// &
        '(target EN_DASH) (rule R1010) (codepoint U+2013) (source '// &
        '(document J3-24-007) (clause R1010) (page 69) '// &
        '(source-sha256 7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2) ))'
    call sx_parse(trim(input), node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_composite_add(composite, node, ok, message)
    if (.not. ok) call fail(trim(message))

    input = '(lexical-fact (source-term "’") (canonical-spelling "''") '// &
        '(class unicode-lexical) '// &
        '(target RIGHT_SINGLE_QUOTE) (rule R724) (codepoint U+2019) (source '// &
        '(document J3-24-007) (clause R724) (page 85) '// &
        '(source-sha256 7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2) ))'
    call sx_parse(trim(input), node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_composite_add(composite, node, ok, message)
    if (.not. ok) call fail(trim(message))

    input = '(syntax RULE-LEXICAL (lhs lexical) (rhs (seq (ref '// &
        achar(226)//achar(128)//achar(147)//') (token '// &
        achar(226)//achar(128)//achar(147)//'))) (source '// &
        '(document J3-24-007) (clause lexical) (rule RULE-LEXICAL) (page 1) '// &
        '(source-sha256 7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2) ))'
    call sx_parse(trim(input), node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_composite_add(composite, node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_lexical_validate(composite%lexical, ok, message)
    if (.not. ok) call fail(trim(message))

    if (composite%lexical%count /= 2) call fail('lexical fact count differs')
    if (iachar(composite%lexical%facts(1)%source_term(1:1)) /= 226 .or. &
        iachar(composite%lexical%facts(1)%source_term(2:2)) /= 128 .or. &
        iachar(composite%lexical%facts(1)%source_term(3:3)) /= 147) &
        call fail('en dash UTF-8 bytes differ')
    if (iachar(composite%lexical%facts(2)%source_term(3:3)) /= 153) &
        call fail('right quote UTF-8 bytes differ')

    open (newunit=unit, file='build/test_standardir_composite.g4', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open ANTLR output')
    call standardir_composite_emit_antlr(unit, composite, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    call require_contains('build/test_standardir_composite.g4', 'grammar StandardIR;', &
        'ANTLR grammar identity is not neutral')
    call require_not_contains('build/test_standardir_composite.g4', 'Fortran2023', &
        'ANTLR grammar retained the old identity')
    call require_contains('build/test_standardir_composite.g4', "EN_DASH : '-' ;", &
        'ANTLR en dash export differs')
    call require_contains('build/test_standardir_composite.g4', &
        "RIGHT_SINGLE_QUOTE : '\'' ;", 'ANTLR quote export differs')

    open (newunit=unit, file='build/test_standardir_composite.y', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open Bison output')
    call standardir_composite_emit_bison(unit, composite, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    call require_contains('build/test_standardir_composite.y', &
        '%token EN_DASH "-" RIGHT_SINGLE_QUOTE "''"', 'Bison lexical export differs')

    open (newunit=unit, file='build/test_standardir_composite.ebnf', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open EBNF output')
    call standardir_composite_emit_ebnf(unit, composite, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    call require_contains('build/test_standardir_composite.ebnf', &
        'codepoint=U+2013 target=EN_DASH canonical-spelling=-', &
        'EBNF lexical provenance differs')
    call require_contains('build/test_standardir_composite.ebnf', '- "-" ;', &
        'EBNF lexical grammar spelling differs')

    open (newunit=unit, file='build/test_standardir_composite.js', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open tree-sitter output')
    call standardir_composite_emit_treesitter(unit, composite, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    call require_contains('build/test_standardir_composite.js', 'name: "standardir",', &
        'tree-sitter grammar identity is not neutral')
    call require_not_contains('build/test_standardir_composite.js', 'fortran2023', &
        'tree-sitter grammar retained the old identity')
    call require_contains('build/test_standardir_composite.js', &
        "EN_DASH: $ => '-',", 'tree-sitter en dash export differs')
    call require_contains('build/test_standardir_composite.js', &
        "RIGHT_SINGLE_QUOTE: $ => '\'',", 'tree-sitter quote export differs')

    open (newunit=unit, status='scratch', action='write', iostat=ios)
    if (ios /= 0) call fail('could not open control output')
    composite%lexical%facts(1)%canonical_spelling = ''
    call standardir_lexical_validate(composite%lexical, ok, message)
    if (.not. ok) call fail('missing optional canonical spelling was rejected')
    call standardir_composite_emit_antlr(unit, composite, ok, message)
    if (ok) call fail('missing canonical spelling was silently normalized')
    composite%lexical%facts(1)%canonical_spelling = '☃'
    call standardir_composite_emit_treesitter(unit, composite, ok, message)
    if (ok) call fail('unrepresentable canonical spelling was accepted')
    close (unit)
    composite%lexical%facts(1)%source_term = '-'
    composite%lexical%facts(1)%canonical_spelling = '-'
    call standardir_lexical_validate(composite%lexical, ok, message)
    if (ok) call fail('ASCII mutation was accepted')
    print '(a)', 'StandardIR composite tests passed'

contains

    subroutine require_contains(path, expected, failure)
        character(len=*), intent(in) :: path, expected, failure
        integer :: read_unit, read_status

        open (newunit=read_unit, file=path, action='read', iostat=read_status)
        if (read_status /= 0) call fail('could not read output')
        do
            read (read_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (index(line, expected) > 0) then
                close (read_unit)
                return
            end if
        end do
        close (read_unit)
        call fail(failure)
    end subroutine require_contains

    subroutine require_not_contains(path, unexpected, failure)
        character(len=*), intent(in) :: path, unexpected, failure
        integer :: read_unit, read_status

        open (newunit=read_unit, file=path, action='read', iostat=read_status)
        if (read_status /= 0) call fail('could not read output')
        do
            read (read_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (index(line, unexpected) > 0) then
                close (read_unit)
                call fail(failure)
            end if
        end do
        close (read_unit)
    end subroutine require_not_contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

end program test_standardir_composite

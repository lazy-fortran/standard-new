program test_standardir_lexer_contract
    !! Source facts and mutations are the independent lexer-contract witness.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexer_contract, only: standardir_lexer_contract_origin_mechanical, &
        standardir_lexer_contract_project, standardir_lexer_contract_t, &
        standardir_lexer_contract_write
    use standardir_lexical, only: standardir_lexical_add, standardir_lexical_facts_t, &
        standardir_lexical_reset
    implicit none

    character(len=*), parameter :: hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=65536) :: input, message, actual, second
    type(standardir_lexical_facts_t) :: facts, ambiguous
    type(standardir_lexer_contract_t) :: contract
    type(sx_node_t) :: node
    integer :: ios, unit
    logical :: ok

    call standardir_lexical_reset(facts)
    input = '(lexical-fact (source-term "–") (canonical-spelling "-") '// &
        '(class unicode-lexical) (target EN_DASH) (rule R1010) '// &
        '(codepoint U+2013) (source (document J3-24-007) (clause R1010) '// &
        '(page 69) (source-sha256 '//hash//')))'
    call add_fact(input, facts)
    input = '(lexical-fact (source-term letter) (class lexical-class) '// &
        '(target LETTER) (rule P6.1.2-3) (codepoint U+0041-U+005A,U+0061-U+007A) '// &
        '(source (document J3-24-007) (clause P6.1.2-3) (page 53) '// &
        '(source-sha256 '//hash//')) )'
    call add_fact(input, facts)

    call standardir_lexer_contract_project(facts, standardir_lexer_contract_origin_mechanical, &
        contract, ok, message)
    call require(ok, message)
    call require(contract%count == 2, 'token count differs')
    call require(trim(contract%tokens(1)%token_name) == 'EN_DASH', 'token name differs')
    call require(trim(contract%tokens(1)%source_term) == '–', 'source term differs')
    call require(trim(contract%tokens(1)%canonical_spelling) == '-', &
        'canonical spelling differs')
    call require(trim(contract%tokens(1)%lexical_class) == 'unicode-lexical', &
        'lexical class differs')
    call require(contract%tokens(2)%range_count == 2 .and. &
        contract%tokens(2)%range_first(1) == 65 .and. &
        contract%tokens(2)%range_last(2) == 122, 'Unicode ranges differ')
    call require(trim(contract%tokens(2)%document) == 'J3-24-007' .and. &
        trim(contract%tokens(2)%clause) == 'P6.1.2-3' .and. &
        trim(contract%tokens(2)%source_rule) == 'P6.1.2-3' .and. &
        trim(contract%tokens(2)%source_page) == '53' .and. &
        trim(contract%tokens(2)%source_hash) == hash, 'provenance differs')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open contract scratch output')
    call standardir_lexer_contract_write(contract, unit, ok, message)
    call require(ok, message)
    rewind (unit)
    actual = ''
    do
        read (unit, '(a)', iostat=ios) second
        if (ios /= 0) exit
        actual = trim(actual)//trim(second)//achar(10)
    end do
    close (unit)
    call require(index(actual, '"canonical_spelling":"-"') > 0, &
        'canonical spelling was not serialized')
    call require(index(actual, '"pattern":"U+0041-U+005A,U+0061-U+007A"') > 0, &
        'source pattern was not serialized')
    call require(index(actual, '"ranges":[{"first":65,"last":90},{"first":97,"last":122}]') > 0, &
        'structured ranges were not serialized')
    call require(index(actual, '"document":"J3-24-007"') > 0 .and. &
        index(actual, '"rule":"P6.1.2-3"') > 0 .and. &
        index(actual, '"source_hash":"'//hash//'"') > 0, &
        'provenance was not serialized')
    call require(index(actual, '"origin":"MECHANICAL"') > 0, 'origin was not serialized')

    call standardir_lexical_reset(ambiguous)
    input = '(lexical-fact (source-term a) (canonical-spelling "-") '// &
        '(class exact) (target A) (rule RA) (codepoint U+0061) '// &
        '(source (document D) (clause C) (page 1) (source-sha256 '//hash//')) )'
    call add_fact(input, ambiguous)
    input = '(lexical-fact (source-term b) (canonical-spelling "-") '// &
        '(class exact) (target B) (rule RB) (codepoint U+0062) '// &
        '(source (document D) (clause C) (page 1) (source-sha256 '//hash//')) )'
    call add_fact(input, ambiguous)
    call standardir_lexer_contract_project(ambiguous, &
        standardir_lexer_contract_origin_mechanical, contract, ok, message)
    call require(.not. ok .and. index(message, 'ambiguous') > 0, &
        'ambiguous canonical spellings were accepted')

    call standardir_lexical_reset(facts)
    input = '(lexical-fact (source-term a) (class exact) (target A) (rule RA) '// &
        '(codepoint U+0061) (source (document D) (clause C) (page 1) '// &
        '(source-sha256 '//hash//')) (unknown value))'
    call sx_parse(trim(input), node, ok, message)
    call require(ok, message)
    call standardir_lexical_add(node, facts, ok, message)
    call require(.not. ok .and. index(message, 'unknown') > 0, &
        'unknown lexical field was accepted')

    input = '(lexical-fact (source-term a) (class exact) (target A) (rule RA) '// &
        '(codepoint U+0061) (source (document D) (clause C) (page 1) '// &
        '(source-sha256 '//hash//') (unknown value)))'
    call sx_parse(trim(input), node, ok, message)
    call require(ok, message)
    call standardir_lexical_add(node, facts, ok, message)
    call require(.not. ok .and. index(message, 'unknown') > 0, &
        'unknown source child was accepted')

    call standardir_lexical_reset(facts)
    input = '(lexical-fact (source-term a) (class exact) (target A) (rule RA) '// &
        '(codepoint U+110000) (source (document D) (clause C) (page 1) '// &
        '(source-sha256 '//hash//')) )'
    call sx_parse(trim(input), node, ok, message)
    call require(ok, message)
    call standardir_lexical_add(node, facts, ok, message)
    call require(.not. ok, 'out-of-range Unicode input was accepted')

    print '(a)', 'StandardIR lexer contract test passed'

contains

    subroutine add_fact(text, output)
        character(len=*), intent(in) :: text
        type(standardir_lexical_facts_t), intent(inout) :: output

        call sx_parse(trim(text), node, ok, message)
        call require(ok, message)
        call standardir_lexical_add(node, output, ok, message)
        call require(ok, message)
    end subroutine add_fact

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_lexer_contract

program test_standardir_grammar_sx_adapter
    !! Fixed raw SX and mutation/depth controls are the independent oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_origin_mechanical, standardir_grammar_optional, &
        standardir_grammar_reference, standardir_grammar_repeat, &
        standardir_grammar_resolution_resolved, standardir_grammar_sequence, &
        standardir_grammar_token, standardir_grammar_rule_t
    use standardir_grammar_export, only: standardir_grammar_normalize, &
        standardir_target_rule_t
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
    use standardir_grammar_source_fingerprint, only: standardir_grammar_source_expression_sha256
    implicit none

    character(len=*), parameter :: syntax_text = &
        '(syntax RULE-A (lhs lhs-a) (rhs (alt '// &
        '(seq (ref first) (alt (token X) (token Y))) '// &
        '(optional (repeat (ref second) 0 unbounded)))) '// &
        '(source (document DOC) (clause C) (rule RULE-A) (page 42) '// &
        '(end-page 43) (byte-start 100) (byte-length 20) (source-sha256 HASH)))'
    character(len=*), parameter :: bad_source_text = &
        '(syntax RULE-A (lhs lhs-a) (rhs (seq (token X))) '// &
        '(source (document DOC) (clause C) (rule RULE-A) (page 42) '// &
        '(source-sha256 )))'
    character(len=*), parameter :: bad_expression_text = &
        '(syntax RULE-A (lhs lhs-a) (rhs (bogus (token X))) '// &
        '(source (document DOC) (clause C) (rule RULE-A) (page 42) '// &
        '(source-sha256 HASH)))'
    character(len=*), parameter :: non_ascii_expression_text = '(seq (token –))'
    character(len=*), parameter :: lexical_source_text = &
        '(syntax R1010 (lhs unicode) (rhs (seq (token –))) '// &
        '(source (document DOC) (clause 5) (rule R1010) (page 69) '// &
        '(byte-start 701) (byte-length 18) (source-sha256 HASH)))'
    character(len=*), parameter :: program_text = &
        '(syntax R502 (lhs program-unit) (rhs (alt (seq (ref main-program)) '// &
        '(seq (ref external-subprogram)) (seq (ref module)) '// &
        '(seq (ref submodule)) (seq (ref block-data)))) '// &
        '(source (document DOC) (clause 5) (rule R502) (page 53) '// &
        '(source-sha256 HASH)))'
    character(len=65536) :: deep_text
    character(len=256) :: message
    type(sx_node_t) :: node, raw_node, target_node
    type(standardir_grammar_rule_t), allocatable :: values(:)
    type(standardir_grammar_rule_t), allocatable :: wrong_values(:)
    type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
    logical :: ok
    character(len=64) :: expected_fingerprint
    character(len=64) :: raw_utf8_expression_text
    integer :: i, position, depth

    call sx_parse(syntax_text, node, ok, message)
    call require(ok, message)
    call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message)
    call require(ok, message)
    call require(allocated(values) .and. size(values) == 2, &
        'top-level alternatives were not split')
    call require(trim(values(1)%id) == 'RULE-A' .and. values(1)%alternative == 1 .and. &
        trim(values(1)%lhs) == 'lhs-a', 'rule identity differs')
    call require(trim(values(1)%source%document) == 'DOC' .and. &
        trim(values(1)%source%clause) == 'C' .and. &
        trim(values(1)%source%rule) == 'RULE-A' .and. values(1)%source%page == 42 .and. &
        trim(values(1)%source%source_hash) == 'HASH', 'source provenance differs')
    call standardir_grammar_source_expression_sha256(node%children(4)%children(2)%children(2), &
        expected_fingerprint, ok, message)
    call require(ok .and. trim(values(1)%source_expression_sha256) == trim(expected_fingerprint), &
        'source-expression fingerprint is not the canonical SX hash')
    call sx_parse(non_ascii_expression_text, node, ok, message)
    call require(ok, message)
    call standardir_grammar_source_expression_sha256(node, expected_fingerprint, ok, message)
    call require(ok .and. trim(expected_fingerprint) == &
        '2fa8772f66ee6732895a9ac09271e5c0729e0e676482cb8245def04a91e166df', &
        'non-ASCII source-expression fingerprint differs from the independent UTF-8 oracle')
    raw_utf8_expression_text = '(seq (token '//achar(226)//achar(128)//achar(147)//'))'
    call sx_parse(trim(raw_utf8_expression_text), node, ok, message)
    call require(ok, message)
    call standardir_grammar_source_expression_sha256(node, expected_fingerprint, ok, message)
    call require(ok .and. trim(expected_fingerprint) == &
        '2fa8772f66ee6732895a9ac09271e5c0729e0e676482cb8245def04a91e166df', &
        'raw UTF-8 bytes were reinterpreted before source-expression hashing')
    call require(values(1)%nodes%values(1)%kind == standardir_grammar_sequence .and. &
        values(1)%nodes%values(1)%child_count == 2 .and. &
        values(1)%nodes%values(2)%kind == standardir_grammar_reference .and. &
        trim(values(1)%nodes%values(2)%name) == 'first', 'sequence/reference structure differs')
    call require(values(1)%nodes%values(3)%kind == standardir_grammar_choice .and. &
        values(1)%nodes%values(3)%child_count == 2 .and. &
        values(1)%nodes%values(4)%kind == standardir_grammar_token .and. &
        trim(values(1)%nodes%values(5)%name) == 'Y', 'nested alt/token structure differs')
    call require(values(2)%alternative == 2 .and. &
        values(2)%nodes%values(1)%kind == standardir_grammar_optional .and. &
        values(2)%nodes%values(2)%kind == standardir_grammar_repeat .and. &
        values(2)%nodes%values(2)%minimum == 0 .and. &
        values(2)%nodes%values(2)%unbounded .and. &
        values(2)%nodes%values(3)%kind == standardir_grammar_reference, &
        'optional/repeat structure differs')
    wrong_values = values
    wrong_values(1)%source_expression_sha256 = repeat('0', 64)
    call standardir_grammar_normalize(wrong_values, normalized, suppressed, ok, message)
    call require(ok .and. trim(normalized(1)%provenance(1)%source_expression_sha256) == repeat('0', 64), &
        'source identity was recomputed from a target tree')
    deallocate (wrong_values)
    call standardir_grammar_normalize(values, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 2 .and. size(suppressed) == 0, &
        'normalization collapsed distinct source alternatives: '//trim(message))
    call require(normalized(1)%expression%kind /= normalized(2)%expression%kind .or. &
        normalized(1)%expression%name /= normalized(2)%expression%name .or. &
        normalized(1)%expression%children(1)%kind /= normalized(2)%expression%children(1)%kind, &
        'normalized alternatives lost their distinct structure')
    deallocate (values)

    call sx_parse(lexical_source_text, raw_node, ok, message)
    call require(ok, message)
    target_node = raw_node
    target_node%children(4)%children(2)%children(2)%children(1)%atom = 'ref'
    call standardir_grammar_adapt_sx(target_node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message, source_node=raw_node)
    call require(ok .and. size(values) == 1, 'lexical target adaptation failed')
    call standardir_grammar_source_expression_sha256(raw_node%children(4)%children(2), &
        expected_fingerprint, ok, message)
    call require(ok .and. trim(values(1)%source_expression_sha256) == trim(expected_fingerprint), &
        'lexical rewrite lost the raw source expression identity')
    call standardir_grammar_normalize(values, normalized, suppressed, ok, message)
    call require(ok .and. trim(normalized(1)%provenance(1)%source_expression_sha256) == &
        trim(expected_fingerprint) .and. trim(normalized(1)%target_expression_sha256) /= &
        trim(expected_fingerprint), 'lexical source and target identities were conflated')
    deallocate (values)

    call sx_parse(program_text, node, ok, message)
    call require(ok, message)
    call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message)
    call require(ok .and. size(values) == 5, 'five-way alternatives were not split')
    call require(trim(values(1)%nodes%values(2)%name) == 'main-program' .and. &
        trim(values(2)%nodes%values(2)%name) == 'external-subprogram' .and. &
        trim(values(5)%nodes%values(2)%name) == 'block-data', &
        'five-way adapter structure changed')
    call standardir_grammar_normalize(values, normalized, suppressed, ok, message)
    call require(ok, 'five-way normalization failed: '//trim(message))
    if (ok) then
        call require(size(normalized) == 5 .and. size(suppressed) == 0, &
            'five-way alternatives collapsed during normalization')
        if (size(normalized) >= 5) then
            call require(trim(normalized(1)%expression%name) == 'main-program' .and. &
                trim(normalized(2)%expression%name) == 'external-subprogram' .and. &
                trim(normalized(5)%expression%name) == 'block-data', &
                'five-way alternative structure changed')
        end if
    end if
    deallocate (values)

    call sx_parse(bad_expression_text, node, ok, message)
    call require(ok, message)
    call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message)
    call require(.not. ok .and. .not. allocated(values), &
        'unsupported expression was accepted transactionally')

    call sx_parse(bad_source_text, node, ok, message)
    call require(ok, message)
    call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message)
    call require(.not. ok .and. .not. allocated(values), &
        'incomplete source provenance was accepted transactionally')

    deep_text = ''
    position = 1
    call append_text(deep_text, position, &
        '(syntax DEEP (lhs deep) (rhs ')
    depth = 0
    do i = 1, 300
        call append_text(deep_text, position, '(optional ')
        depth = depth + 1
    end do
    call append_text(deep_text, position, '(token X)')
    do i = 1, depth
        call append_text(deep_text, position, ')')
    end do
    call append_text(deep_text, position, &
        ' (source (document DOC) (clause C) (rule DEEP) (page 1) (source-sha256 HASH))))')
    call sx_parse(deep_text(:position - 1), node, ok, message)
    call require(ok, 'deep fixture parse failed: '//trim(message))
    call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, values, ok, message)
    call require(.not. ok .and. .not. allocated(values), &
        'depth/cycle guard accepted non-convergent expression')

    print '(a)', 'StandardIR grammar SX adapter test passed'

contains

    subroutine append_text(buffer, cursor, text)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: cursor
        character(len=*), intent(in) :: text
        integer :: length

        length = len(text)
        if (cursor + length - 1 > len(buffer)) call fail('deep fixture exceeded buffer')
        buffer(cursor:cursor + length - 1) = text(:length)
        cursor = cursor + length
    end subroutine append_text

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) call fail(trim(text))
    end subroutine require

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

end program test_standardir_grammar_sx_adapter

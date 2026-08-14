program test_standardir_grammar_sx_adapter
    !! Fixed raw SX and mutation/depth controls are the independent oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_origin_mechanical, standardir_grammar_optional, &
        standardir_grammar_reference, standardir_grammar_repeat, &
        standardir_grammar_resolution_resolved, standardir_grammar_sequence, &
        standardir_grammar_token, standardir_grammar_rule_t
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
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
    character(len=65536) :: deep_text
    character(len=256) :: message
    type(sx_node_t) :: node
    type(standardir_grammar_rule_t), allocatable :: values(:)
    logical :: ok
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

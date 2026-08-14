program test_standardir_grammar_closure
    !! Independent end-to-end controls for source-backed grammar closure.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_closure, only: standardir_grammar_close_sx
    use standardir_grammar_producer, only: standardir_grammar_rule_t
    use standardir_lexical, only: standardir_lexical_facts_t
    use standardir_reference_closure, only: closure_classification_t, closure_kind_list, &
        closure_kind_semantic_only, closure_kind_unresolved
    implicit none

    character(len=*), parameter :: hash = &
        '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
    character(len=*), parameter :: first_text = &
        '(syntax R1 (lhs program) (rhs (seq (ref widget-list) (ref semantic-hole))) '// &
        '(source (document DOC) (clause 5) (rule R1) (page 1) '// &
        '(source-sha256 '//hash//')))'
    character(len=*), parameter :: second_text = &
        '(syntax R2 (lhs widget) (rhs (seq (token W))) '// &
        '(source (document DOC) (clause 5) (rule R2) (page 2) '// &
        '(source-sha256 '//hash//')))'
    character(len=*), parameter :: duplicate_text = &
        '(syntax R1 (lhs duplicate) (rhs (seq (token D))) '// &
        '(source (document DOC) (clause 5) (rule R1) (page 3) '// &
        '(source-sha256 '//hash//')))'
    type(sx_node_t) :: nodes(3)
    type(closure_classification_t) :: facts(2)
    type(standardir_lexical_facts_t) :: lexical
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    type(standardir_source_ref_t) :: source
    character(len=128) :: roots(2), message
    integer :: i, semantic_skipped, lexical_closed
    logical :: ok

    call parse_node(first_text, nodes(1))
    call parse_node(second_text, nodes(2))
    call parse_node(duplicate_text, nodes(3))
    lexical = standardir_lexical_facts_t()
    call make_source(source, 'R401', 10)
    facts = closure_classification_t()
    facts(1)%name = 'widget-list'
    facts(1)%kind = closure_kind_list
    facts(1)%target = 'widget'
    facts(1)%separator = ','
    facts(1)%source = source
    facts(2)%name = 'semantic-hole'
    facts(2)%kind = closure_kind_semantic_only
    facts(2)%source = source
    roots(1) = 'program'
    roots(2) = 'duplicate'

    call standardir_grammar_close_sx(nodes, 3, facts, 2, roots, 2, lexical, rules, &
        semantic_skipped, lexical_closed, ok, message)
    call require(ok, message)
    call require(semantic_skipped == 2, 'semantic-only source records were not isolated')
    call require(lexical_closed == 0, 'unexpected lexical closure count')
    call require(allocated(rules), 'closed rule array is unallocated')
    if (size(rules) /= 3) then
        write (message, '("closed rule count is ",i0)') size(rules)
        call require(.false., message)
    end if
    call require(trim(rules(1)%lhs) == 'duplicate' .and. trim(rules(2)%lhs) == 'widget' .and. &
        trim(rules(3)%lhs) == 'widget-list', 'closed rules were not sorted by lhs')
    call require(trim(rules(3)%id) == 'derived-widget-list', &
        'derived rule identity is not occurrence-independent')
    deallocate (rules)

    facts(1) = closure_classification_t()
    facts(1)%name = 'not-resolved'
    facts(1)%kind = closure_kind_unresolved
    facts(1)%source = source
    roots(1) = 'program'
    roots(2) = 'duplicate'
    call standardir_grammar_close_sx(nodes, 3, facts, 1, roots, 2, lexical, rules, &
        semantic_skipped, lexical_closed, ok, message)
    call require(.not. ok .and. .not. allocated(rules), &
        'unresolved closure classification was exported')

    print '(a)', 'StandardIR grammar closure test passed'

contains

    subroutine parse_node(text, node)
        character(len=*), intent(in) :: text
        type(sx_node_t), intent(out) :: node
        logical :: local_ok
        character(len=256) :: local_message

        call sx_parse(text, node, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine parse_node

    subroutine make_source(value, rule, page)
        type(standardir_source_ref_t), intent(out) :: value
        character(len=*), intent(in) :: rule
        integer, intent(in) :: page

        value = standardir_source_ref_t()
        value%document = 'DOC'
        value%clause = '5'
        value%rule = trim(rule)
        value%page = page
        value%source_hash = hash
    end subroutine make_source

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_grammar_closure

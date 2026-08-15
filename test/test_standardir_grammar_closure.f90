program test_standardir_grammar_closure
    !! Independent end-to-end controls for source-backed grammar closure.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_closure, only: standardir_grammar_close_sx
    use standardir_grammar_producer, only: standardir_grammar_reference, standardir_grammar_rule_t
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
    type(sx_node_t) :: nodes(3), lexical_node, lexical_nodes(1)
    type(closure_classification_t) :: facts(2)
    type(standardir_lexical_facts_t) :: lexical
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    type(standardir_source_ref_t) :: source
    character(len=128) :: roots(2), lexical_roots(1), message
    character(len=128), allocatable :: semantic_skipped_names(:)
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
        semantic_skipped, lexical_closed, ok, message, semantic_skipped_names)
    call require(ok, message)
    call require(semantic_skipped == 2, 'semantic-only source records were not isolated')
    call require(size(semantic_skipped_names) == 2, &
        'semantic-only names were not retained')
    call require(trim(semantic_skipped_names(1)) == 'program' .and. &
        trim(semantic_skipped_names(2)) == 'semantic-hole', &
        'semantic-only names have the wrong provenance order')
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

    call parse_node('(syntax R3 (lhs unicode) (rhs (seq (token –))) '// &
        '(source (document DOC) (clause 5) (rule R3) (page 3) '// &
        '(source-sha256 '//hash//')))', lexical_node)
    lexical = standardir_lexical_facts_t()
    lexical%count = 1
    lexical%facts(1)%source_term = '–'
    lexical%facts(1)%class_name = 'unicode-lexical'
    lexical%facts(1)%target_name = 'EN_DASH'
    lexical%facts(1)%source_rule = 'R1010'
    lexical%facts(1)%source_page = '69'
    lexical%facts(1)%document = 'DOC'
    lexical%facts(1)%clause = '5'
    lexical%facts(1)%source_hash = hash
    lexical%facts(1)%codepoint = 'U+2013'
    lexical%facts(1)%range_count = 1
    lexical%facts(1)%range_first(1) = int(z'2013', int64)
    lexical%facts(1)%range_last(1) = int(z'2013', int64)
    lexical_roots(1) = 'unicode'
    lexical_nodes(1) = lexical_node
    call standardir_grammar_close_sx(lexical_nodes, 1, facts, 0, lexical_roots, 1, lexical, &
        rules, semantic_skipped, lexical_closed, ok, message)
    call require(ok, 'lexical token linkage control failed: '//trim(message))
    call require(lexical_closed == 1, 'lexical fact was not closed')
    call require(size(rules) == 1, 'lexical linkage changed normative rule count')
    call require(rules(1)%nodes%values(2)%kind == standardir_grammar_reference .and. &
        trim(rules(1)%nodes%values(2)%name) == '–', &
        'source lexical token did not become a target reference')

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

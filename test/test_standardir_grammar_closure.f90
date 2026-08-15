program test_standardir_grammar_closure
    !! Independent end-to-end controls for source-backed grammar closure.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_closure, only: standardir_grammar_close_selected_sx, &
        standardir_grammar_close_sx, standardir_grammar_disposition_omitted_helper, &
        standardir_grammar_disposition_omitted_root, standardir_grammar_disposition_selected, &
        standardir_grammar_disposition_t
    use standardir_grammar_producer, only: standardir_grammar_reference, standardir_grammar_rule_t
    use standardir_grammar_source_fingerprint, only: standardir_grammar_source_expression_sha256
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
    character(len=*), parameter :: helper_text = &
        '(syntax R4 (lhs helper) (rhs (seq (token H))) '// &
        '(source (document DOC) (clause 5) (rule R4) (page 4) '// &
        '(source-sha256 '//hash//')))'
    type(sx_node_t) :: nodes(4), lexical_node, lexical_nodes(1)
    type(closure_classification_t) :: facts(2)
    type(standardir_lexical_facts_t) :: lexical
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    type(standardir_grammar_disposition_t), allocatable :: dispositions(:)
    type(standardir_source_ref_t) :: source
    character(len=128) :: roots(2), lexical_roots(1), message
    character(len=128), allocatable :: semantic_skipped_names(:)
    integer :: i, semantic_skipped, lexical_closed
    logical :: ok
    character(len=64) :: raw_expression_hash

    call parse_node(first_text, nodes(1))
    call parse_node(second_text, nodes(2))
    call parse_node(duplicate_text, nodes(3))
    call parse_node(helper_text, nodes(4))
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

    roots(1) = 'program'
    roots(2) = 'duplicate'
    call standardir_grammar_close_selected_sx(nodes, 4, facts, 2, roots, 2, 'program', lexical, &
        rules, semantic_skipped, lexical_closed, dispositions, ok, message)
    call require(ok, 'selected-root closure failed: '//trim(message))
    call require(size(rules) == 2, 'selected-root closure emitted an unreachable rule')
    call require(trim(rules(1)%lhs) == 'widget' .and. trim(rules(2)%lhs) == 'widget-list', &
        'selected-root closure did not retain only reachable rules')
    call require(rules(1)%origin > 0 .and. trim(rules(1)%source%source_hash) == hash, &
        'selected-root rule lost origin or source provenance')
    call require(disposition_count(dispositions, standardir_grammar_disposition_selected) == 3, &
        'selected-root dispositions did not retain reachable source names')
    call require(has_disposition(dispositions, 'duplicate', standardir_grammar_disposition_omitted_root, &
        'not reachable from selected root'), 'omitted declared root had no disposition')
    call require(has_disposition(dispositions, 'helper', standardir_grammar_disposition_omitted_helper, &
        'not reachable from selected root'), 'omitted helper had no disposition')
    deallocate (rules, dispositions)

    call standardir_grammar_close_selected_sx(nodes, 4, facts, 2, roots, 2, 'missing', lexical, &
        rules, semantic_skipped, lexical_closed, dispositions, ok, message)
    call require(.not. ok, 'selected-root closure accepted a root absent from source records')
    call require(.not. allocated(rules), 'failed selected-root closure retained a rule array')

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
        '(byte-start 703) (byte-length 18) (source-sha256 '//hash//')))', lexical_node)
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
    call standardir_grammar_source_expression_sha256(lexical_node%children(4)%children(2), &
        raw_expression_hash, ok, message)
    call require(ok .and. trim(rules(1)%source_expression_sha256) == trim(raw_expression_hash) .and. &
        rules(1)%source%byte_start == 703 .and. rules(1)%source%byte_length == 18, &
        'lexical closure lost the exact raw source key')

    print '(a)', 'StandardIR grammar closure test passed'

contains

    integer function disposition_count(values, kind)
        type(standardir_grammar_disposition_t), intent(in) :: values(:)
        integer, intent(in) :: kind
        integer :: j

        disposition_count = 0
        do j = 1, size(values)
            if (values(j)%disposition == kind) disposition_count = disposition_count + 1
        end do
    end function disposition_count

    logical function has_disposition(values, name, kind, reason)
        type(standardir_grammar_disposition_t), intent(in) :: values(:)
        character(len=*), intent(in) :: name, reason
        integer, intent(in) :: kind
        integer :: j

        has_disposition = .false.
        do j = 1, size(values)
            if (trim(values(j)%name) == trim(name) .and. values(j)%disposition == kind .and. &
                trim(values(j)%reason) == trim(reason)) then
                has_disposition = .true.
                return
            end if
        end do
    end function has_disposition

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

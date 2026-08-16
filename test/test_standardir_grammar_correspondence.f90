program test_standardir_grammar_correspondence
    !! Tiny typed fixtures are the independent oracle for correspondence paths.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_correspondence, only: standardir_correspondence_mapped, &
        standardir_correspondence_suppressed, standardir_correspondence_unsupported, &
        standardir_grammar_correspondence_trace_t
    use standardir_grammar_export, only: standardir_grammar_normalize, standardir_target_rule_t
    use standardir_grammar_producer, only: standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, standardir_grammar_rule_t
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
    implicit none

    type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
    type(standardir_grammar_correspondence_trace_t), allocatable :: trace(:)
    character(len=512) :: message
    logical :: ok

    call normalize_fixture('(syntax R1 (lhs lhs) (rhs (seq (ref A) (token X))) '// &
        '(source (document DOC) (clause C) (rule R1) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message)
    call require(ok, 'identity fixture failed: '//trim(message))
    call require_trace(trace, 'R1', 1, 'rhs/1', 'rhs/1', 'sequence-element', &
        standardir_correspondence_mapped, 'identity', 1, 'A')

    call normalize_fixture('(syntax R2 (lhs lhs) (rhs (seq (ref A) (seq (token X) (ref B)))) '// &
        '(source (document DOC) (clause C) (rule R2) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message)
    call require(ok, 'sequence fixture failed: '//trim(message))
    call require_trace(trace, 'R2', 1, 'rhs/2/1', 'rhs/2', 'sequence-element', &
        standardir_correspondence_mapped, 'sequence-flatten', 2, 'X')

    call normalize_fixture('(syntax MAIN (lhs main) (rhs (optional (repeat (token N) 0 unbounded))) '// &
        '(source (document DOC) (clause C) (rule MAIN) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message)
    call require(ok, 'optional fixture failed: '//trim(message))
    call require_trace(trace, 'MAIN', 1, 'rhs', '', 'rule-expression', &
        standardir_correspondence_suppressed, 'optional-wrapper-removal', 0, '-')

    call normalize_fixture('(syntax R3 (lhs lhs) (rhs (seq (alt (token X) '// &
        '(alt (token X) (token Y))))) (source (document DOC) (clause C) '// &
        '(rule R3) (page 1) (source-sha256 HASH)))', normalized, suppressed, trace, ok, message)
    call require(ok, 'choice fixture failed: '//trim(message))
    call require_trace(trace, 'R3', 1, 'rhs/1/2/1', '', 'choice-alternative', &
        standardir_correspondence_suppressed, 'choice-deduplicate', 0, 'X')
    call require_trace(trace, 'R3', 1, 'rhs/1/2/2', 'rhs/1/2', 'choice-alternative', &
        standardir_correspondence_mapped, 'choice-flatten', 2, 'Y')

    call normalize_pair('(syntax R5 (lhs first) (rhs (seq (seq (token A) (token B)) (token C))) '// &
        '(source (document DOC) (clause C) (rule R5) (page 1) (byte-start 51) '// &
        '(source-sha256 HASH)))', &
        '(syntax R6 (lhs second) (rhs (seq (alt (token X) (alt (token Y) (token Z))) (token W))) '// &
        '(source (document DOC) (clause C) (rule R6) (page 1) (byte-start 61) '// &
        '(source-sha256 HASH)))', trace, ok, message)
    call require(ok, 'repeated-path fixture failed: '//trim(message))
    call require_operation(trace, 'R5', 'rhs/1', 'sequence-flatten')
    call require_operation(trace, 'R6', 'rhs/1', 'choice-flatten')

    call normalize_fixture('(syntax R4 (lhs expr) (rhs (alt (seq (ref expr) (token X)) '// &
        '(token B))) (source (document DOC) (clause C) (rule R4) (page 1) '// &
        '(source-sha256 HASH)))', normalized, suppressed, trace, ok, message)
    call require(ok, 'unsupported fixture unexpectedly failed normalization: '//trim(message))
    call require_disposition(trace, 'R4', 1, 'rhs/1', 'unsupported', 'left-recursion-elimination')

    print '(a)', 'StandardIR grammar correspondence test passed'

contains

    subroutine normalize_fixture(text, normalized, suppressed, trace, ok, message)
        character(len=*), intent(in) :: text
        type(standardir_target_rule_t), allocatable, intent(out) :: normalized(:), suppressed(:)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(out) :: trace(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(sx_node_t) :: node
        type(standardir_grammar_rule_t), allocatable :: rules(:)

        call sx_parse(text, node, ok, message)
        if (.not. ok) return
        call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
            standardir_grammar_resolution_resolved, rules, ok, message)
        if (.not. ok) return
        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message, trace)
    end subroutine normalize_fixture

    subroutine normalize_pair(first_text, second_text, trace, ok, message)
        character(len=*), intent(in) :: first_text, second_text
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(out) :: trace(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(sx_node_t) :: first_node, second_node
        type(standardir_grammar_rule_t), allocatable :: first_rules(:), second_rules(:), rules(:)
        type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)

        call sx_parse(first_text, first_node, ok, message)
        if (.not. ok) return
        call sx_parse(second_text, second_node, ok, message)
        if (.not. ok) return
        call standardir_grammar_adapt_sx(first_node, standardir_grammar_origin_mechanical, &
            standardir_grammar_resolution_resolved, first_rules, ok, message)
        if (.not. ok) return
        call standardir_grammar_adapt_sx(second_node, standardir_grammar_origin_mechanical, &
            standardir_grammar_resolution_resolved, second_rules, ok, message)
        if (.not. ok) return
        allocate (rules(size(first_rules) + size(second_rules)))
        rules(:size(first_rules)) = first_rules
        rules(size(first_rules) + 1:) = second_rules
        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message, trace)
    end subroutine normalize_pair

    subroutine require_operation(values, source_rule, source_path, operation)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, source_path, operation
        integer :: i
        logical :: found

        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (trim(values(i)%raw_source_expression_path) /= trim(source_path)) cycle
            if (trim(values(i)%transformation) /= trim(operation)) cycle
            found = .true.
            exit
        end do
        call require(found, 'source path operation was cross-contaminated for '//trim(source_rule))
    end subroutine require_operation

    subroutine require_trace(values, source_rule, alternative, source_path, target_path, role, &
            disposition, operation, slot, node_name)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, source_path, target_path, role, disposition, operation
        integer, intent(in) :: alternative, slot
        character(len=*), intent(in) :: node_name
        integer :: i
        logical :: found

        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (values(i)%source_alternative /= alternative) cycle
            if (trim(values(i)%source%document) /= 'DOC') cycle
            if (trim(values(i)%source%clause) /= 'C') cycle
            if (trim(values(i)%source%source_hash) /= 'HASH') cycle
            if (values(i)%source%page /= 1) cycle
            if (trim(values(i)%raw_source_expression_path) /= trim(source_path)) cycle
            if (trim(values(i)%target_expression_path) /= trim(target_path)) cycle
            if (trim(values(i)%source_boundary_role) /= trim(role)) cycle
            if (trim(values(i)%disposition) /= trim(disposition)) cycle
            if (trim(values(i)%transformation) /= trim(operation)) cycle
            if (values(i)%target_sequence_boundary_slot /= slot) cycle
            if (trim(values(i)%source_node_name) /= trim(node_name)) cycle
            if (trim(values(i)%target_rule_id) /= trim(source_rule)) cycle
            if (values(i)%target_alternative /= alternative) cycle
            if (len_trim(values(i)%input_expression_sha256) == 0) cycle
            if (len_trim(values(i)%output_expression_sha256) == 0) cycle
            found = .true.
            exit
        end do
        call require(found, 'correspondence row missing for '//trim(source_rule)//' '//trim(source_path))
    end subroutine require_trace

    subroutine require_disposition(values, source_rule, alternative, source_path, disposition, operation)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, source_path, disposition, operation
        integer, intent(in) :: alternative
        integer :: i
        logical :: found

        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (values(i)%source_alternative /= alternative) cycle
            if (trim(values(i)%raw_source_expression_path) /= trim(source_path)) cycle
            if (trim(values(i)%disposition) /= trim(disposition)) cycle
            if (trim(values(i)%transformation) /= trim(operation)) cycle
            found = .true.
            exit
        end do
        call require(found, 'fail-closed correspondence disposition is missing')
    end subroutine require_disposition

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_grammar_correspondence

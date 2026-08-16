program test_standardir_grammar_correspondence
    !! Tiny typed fixtures are the independent oracle for correspondence paths.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_correspondence, only: standardir_correspondence_mapped, &
        standardir_correspondence_suppressed, standardir_correspondence_unsupported, &
        standardir_grammar_correspondence_trace_t
    use standardir_grammar_export, only: standardir_grammar_normalize, standardir_target_rule_t
    use standardir_grammar_producer, only: standardir_grammar_origin_mechanical, &
        standardir_grammar_resolution_resolved, standardir_grammar_rule_t
    use standardir_grammar_sx_adapter, only: standardir_grammar_adapt_sx
    use standardir_grammar_transformation_witness, only: &
        standardir_grammar_emit_correspondence_witness, &
        standardir_grammar_validate_correspondence_trace
    implicit none

    type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
    type(standardir_grammar_correspondence_trace_t), allocatable :: trace(:)
    type(standardir_grammar_correspondence_trace_t), allocatable :: bad_trace(:)
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    character(len=512) :: message
    logical :: ok
    integer :: i

    call normalize_fixture('(syntax R1 (lhs lhs) (rhs (seq (ref A) (token X))) '// &
        '(source (document DOC) (clause C) (rule R1) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message, rules_out=rules)
    call require(ok, 'identity fixture failed: '//trim(message))
    call require_trace(trace, 'R1', 1, 'rhs/1', 'rhs/1', 'sequence-element', &
        standardir_correspondence_mapped, 'identity', 1, 'A')
    call require_hash_distinction(trace, 'R1', 'rhs/1')
    call require_emitted_row(rules, trace, 'R1', 'rhs/1', 'rhs/1', 1, 'identity', &
        standardir_correspondence_mapped)

    call normalize_fixture('(syntax R2 (lhs lhs) (rhs (seq (ref A) (seq (token X) (ref B)))) '// &
        '(source (document DOC) (clause C) (rule R2) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message, rules_out=rules)
    call require(ok, 'sequence fixture failed: '//trim(message))
    call require_trace(trace, 'R2', 1, 'rhs/2/1', 'rhs/2', 'sequence-element', &
        standardir_correspondence_mapped, 'sequence-flatten', 2, 'X')
    call require_emitted_row(rules, trace, 'R2', 'rhs/2/1', 'rhs/2', 2, 'sequence-flatten', &
        standardir_correspondence_mapped)

    call normalize_fixture('(syntax MAIN (lhs main) (rhs (optional (repeat (token N) 0 unbounded))) '// &
        '(source (document DOC) (clause C) (rule MAIN) (page 1) (source-sha256 HASH)))', &
        normalized, suppressed, trace, ok, message, rules_out=rules)
    call require(ok, 'optional fixture failed: '//trim(message))
    call require_trace(trace, 'MAIN', 1, 'rhs', '', 'rule-expression', &
        standardir_correspondence_suppressed, 'optional-wrapper-removal', 0, '-')
    call require_emitted_row(rules, trace, 'MAIN', 'rhs', '', 0, 'optional-wrapper-removal', &
        standardir_correspondence_suppressed)

    call normalize_fixture('(syntax R3 (lhs lhs) (rhs (seq (alt (token X) '// &
        '(alt (token X) (token Y))))) (source (document DOC) (clause C) '// &
        '(rule R3) (page 1) (source-sha256 HASH)))', normalized, suppressed, trace, ok, message, &
        rules_out=rules)
    call require(ok, 'choice fixture failed: '//trim(message))
    call require_trace(trace, 'R3', 1, 'rhs/1/2/1', '', 'choice-alternative', &
        standardir_correspondence_suppressed, 'choice-deduplicate', 0, 'X')
    call require_trace(trace, 'R3', 1, 'rhs/1/2/2', 'rhs/1/2', 'choice-alternative', &
        standardir_correspondence_mapped, 'choice-flatten', 2, 'Y')
    call require_emitted_row(rules, trace, 'R3', 'rhs/1/2/1', '', 0, 'choice-deduplicate', &
        standardir_correspondence_suppressed)

    call normalize_pair('(syntax R5 (lhs first) (rhs (seq (seq (token A) (token B)) (token C))) '// &
        '(source (document DOC) (clause C) (rule R5) (page 1) (byte-start 51) '// &
        '(source-sha256 HASH)))', &
        '(syntax R6 (lhs second) (rhs (seq (alt (token X) (alt (token Y) (token Z))) (token W))) '// &
        '(source (document DOC) (clause C) (rule R6) (page 1) (byte-start 61) '// &
        '(source-sha256 HASH)))', trace, ok, message, rules_out=rules)
    call require(ok, 'repeated-path fixture failed: '//trim(message))
    call require_operation(trace, 'R5', 'rhs/1', 'sequence-flatten')
    call require_operation(trace, 'R6', 'rhs/1', 'choice-flatten')
    call require_emitted_row(rules, trace, 'R5', 'rhs/1', '', 0, 'sequence-flatten', &
        standardir_correspondence_suppressed, 51_int64)
    call require_emitted_row(rules, trace, 'R6', 'rhs/1', 'rhs/1', 1, 'choice-flatten', &
        standardir_correspondence_mapped, 61_int64)

    call normalize_pair('(syntax KEEP (lhs shared) (rhs (token X)) '// &
        '(source (document DOC) (clause C) (rule KEEP) (page 4) (end-page 5) '// &
        '(byte-start 101) (byte-length 11) (source-sha256 HASH)))', &
        '(syntax DUP (lhs shared) (rhs (token X)) '// &
        '(source (document DOC) (clause C) (rule DUP) (page 6) (end-page 7) '// &
        '(byte-start 202) (byte-length 22) (source-sha256 HASH)))', trace, ok, message, &
        rules_out=rules)
    call require(ok, 'rule deduplication fixture failed: '//trim(message))
    call require_retained_target(trace, 'DUP', 'KEEP', 'rhs', 0, 4, 5, 101_int64, 11_int64)
    call require_emitted_row(rules, trace, 'DUP', 'rhs', '', 0, 'rule-deduplicate', &
        standardir_correspondence_suppressed, 202_int64)
    bad_trace = trace
    do i = 1, size(bad_trace)
        if (trim(bad_trace(i)%source%rule) /= 'DUP') cycle
        if (trim(bad_trace(i)%transformation) /= 'rule-deduplicate') cycle
        bad_trace(i)%retained_target_source%rule = ''
        exit
    end do
    call standardir_grammar_validate_correspondence_trace(bad_trace, ok, message)
    call require(.not. ok .and. index(message, 'retained target') > 0, &
        'missing retained target source was accepted')
    bad_trace = trace
    do i = 1, size(bad_trace)
        if (trim(bad_trace(i)%source%rule) /= 'DUP') cycle
        if (trim(bad_trace(i)%transformation) /= 'rule-deduplicate') cycle
        bad_trace(i)%retained_target_expression_path = 'rhs/1'
        exit
    end do
    call standardir_grammar_validate_correspondence_trace(bad_trace, ok, message)
    call require(.not. ok .and. index(message, 'rule root') > 0, &
        'non-root retained target path was accepted for rule deduplication')

    call normalize_fixture('(syntax R4 (lhs expr) (rhs (alt (seq (ref expr) (token X)) '// &
        '(token B))) (source (document DOC) (clause C) (rule R4) (page 1) '// &
        '(source-sha256 HASH)))', normalized, suppressed, trace, ok, message, rules_out=rules)
    call require(ok, 'unsupported fixture unexpectedly failed normalization: '//trim(message))
    call require_disposition(trace, 'R4', 1, 'rhs/1', 'unsupported', 'left-recursion-elimination')
    call require_emitted_row(rules, trace, 'R4', 'rhs/1', '', 0, 'left-recursion-elimination', &
        standardir_correspondence_unsupported)

    bad_trace = trace
    bad_trace(1)%disposition = 'invalid'
    call standardir_grammar_validate_correspondence_trace(bad_trace, ok, message)
    call require(.not. ok .and. index(message, 'disposition') > 0, &
        'invalid correspondence disposition was accepted')
    bad_trace = trace
    bad_trace(1)%source%document = ''
    call standardir_grammar_validate_correspondence_trace(bad_trace, ok, message)
    call require(.not. ok .and. index(message, 'provenance') > 0, &
        'missing correspondence source field was accepted')

    call require_failed_left_recursion_trace('(syntax UNIT (lhs expr) (rhs (ref expr)) '// &
        '(source (document DOC) (clause C) (rule UNIT) (page 1) (source-sha256 HASH)))', 'UNIT')
    call require_failed_left_recursion_trace( &
        '(syntax NULLABLE (lhs expr) (rhs (seq (ref expr) '// &
        '(optional (token X)))) (source (document DOC) (clause C) (rule NULLABLE) '// &
        '(page 1) (source-sha256 HASH)))', 'NULLABLE')
    call require_failed_left_recursion_trace( &
        '(syntax NESTED (lhs expr) (rhs (optional (ref expr))) '// &
        '(source (document DOC) (clause C) (rule NESTED) (page 1) '// &
        '(source-sha256 HASH)))', 'NESTED')

    print '(a)', 'StandardIR grammar correspondence test passed'

contains

    subroutine normalize_fixture(text, normalized, suppressed, trace, ok, message, rules_out)
        character(len=*), intent(in) :: text
        type(standardir_target_rule_t), allocatable, intent(out) :: normalized(:), suppressed(:)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(out) :: trace(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_grammar_rule_t), allocatable, intent(out), optional :: rules_out(:)
        type(sx_node_t) :: node
        type(standardir_grammar_rule_t), allocatable :: rules(:)

        call sx_parse(text, node, ok, message)
        if (.not. ok) return
        call standardir_grammar_adapt_sx(node, standardir_grammar_origin_mechanical, &
            standardir_grammar_resolution_resolved, rules, ok, message)
        if (.not. ok) return
        if (present(rules_out)) rules_out = rules
        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message, trace)
    end subroutine normalize_fixture

    subroutine normalize_pair(first_text, second_text, trace, ok, message, rules_out)
        character(len=*), intent(in) :: first_text, second_text
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(out) :: trace(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_grammar_rule_t), allocatable, intent(out), optional :: rules_out(:)
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
        if (present(rules_out)) rules_out = rules
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

    subroutine require_retained_target(values, source_rule, retained_rule, path, slot, page, end_page, &
            byte_start, byte_length)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, retained_rule, path
        integer, intent(in) :: slot, page, end_page
        integer(int64), intent(in) :: byte_start, byte_length
        integer :: i
        logical :: found

        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (trim(values(i)%transformation) /= 'rule-deduplicate') cycle
            if (trim(values(i)%disposition) /= standardir_correspondence_suppressed) cycle
            call require(trim(values(i)%source%rule) /= &
                trim(values(i)%retained_target_source%rule), &
                'retained target source was confused with suppressed source')
            call require(trim(values(i)%retained_target_source%rule) == trim(retained_rule), &
                'retained target rule is incorrect')
            call require(trim(values(i)%retained_target_source%document) == 'DOC', &
                'retained target document is incorrect')
            call require(trim(values(i)%retained_target_source%clause) == 'C', &
                'retained target clause is incorrect')
            call require(values(i)%retained_target_source%page == page .and. &
                values(i)%retained_target_source%end_page == end_page, &
                'retained target page span is incorrect')
            call require(values(i)%retained_target_source%byte_start == byte_start .and. &
                values(i)%retained_target_source%byte_length == byte_length, &
                'retained target byte range is incorrect')
            call require(trim(values(i)%retained_target_source%source_hash) == 'HASH', &
                'retained target source hash is incorrect')
            call require(values(i)%retained_target_source_alternative == 1, &
                'retained target alternative is incorrect')
            call require(trim(values(i)%retained_target_expression_path) == trim(path) .and. &
                values(i)%retained_target_sequence_boundary_slot == slot, &
                'retained target occurrence is incorrect')
            found = .true.
            exit
        end do
        call require(found, 'retained target relation is missing')
    end subroutine require_retained_target

    subroutine require_emitted_row(rules, values, source_rule, source_path, target_path, slot, &
            operation, &
            disposition, expected_byte_start)
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, source_path, target_path, operation
        character(len=*), intent(in) :: disposition
        integer, intent(in) :: slot
        integer(int64), intent(in), optional :: expected_byte_start
        type(standardir_grammar_correspondence_trace_t) :: expected
        character(len=8192) :: line
        character(len=256) :: local_message
        integer :: unit, ios
        integer :: i
        logical :: found, ok

        found = .false.
        expected = standardir_grammar_correspondence_trace_t()
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (trim(values(i)%raw_source_expression_path) /= trim(source_path)) cycle
            if (trim(values(i)%target_expression_path) /= trim(target_path)) cycle
            if (values(i)%target_sequence_boundary_slot /= slot) cycle
            if (trim(values(i)%transformation) /= trim(operation)) cycle
            if (trim(values(i)%disposition) /= trim(disposition)) cycle
            expected = values(i)
            found = .true.
            exit
        end do
        call require(found, 'typed correspondence row missing before serialization')
        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open correspondence witness output')
        call standardir_grammar_emit_correspondence_witness(unit, rules, ok, local_message)
        call require(ok, trim(local_message))
        found = .false.
        rewind (unit)
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '"source_rule":"'//trim(source_rule)//'"') == 0) cycle
            if (index(line, '"raw_source_path":"'//trim(source_path)//'"') == 0) cycle
            if (index(line, '"transformation":"'//trim(operation)//'"') == 0) cycle
            if (index(line, '"disposition":"'//trim(disposition)//'"') == 0) cycle
            found = .true.
            exit
        end do
        close (unit)
        call require(found, 'serialized correspondence row is missing')
        call require_json_text(line, 'source_document', expected%source%document)
        call require_json_text(line, 'source_clause', expected%source%clause)
        call require_json_text(line, 'source_rule', expected%source%rule)
        call require_json_integer(line, 'source_page', expected%source%page)
        call require_json_integer(line, 'source_end_page', expected%source%end_page)
        call require_json_int64(line, 'source_byte_start', expected%source%byte_start)
        call require_json_int64(line, 'source_byte_length', expected%source%byte_length)
        call require_json_text(line, 'source_hash', expected%source%source_hash)
        call require_json_integer(line, 'source_alternative', expected%source_alternative)
        call require_json_text(line, 'raw_source_path', expected%raw_source_expression_path)
        call require_json_integer(line, 'source_node_kind', expected%source_node_kind)
        call require_json_text(line, 'source_node_name', expected%source_node_name)
        call require_json_text(line, 'source_boundary_role', expected%source_boundary_role)
        call require_json_text(line, 'target_rule', expected%target_rule_id)
        call require_json_text(line, 'target_lhs', expected%target_lhs)
        call require_json_integer(line, 'target_alternative', expected%target_alternative)
        call require_json_text(line, 'target_path', expected%target_expression_path)
        call require_json_integer(line, 'target_sequence_slot', &
            expected%target_sequence_boundary_slot)
        call require_json_text(line, 'retained_target_source_document', &
            expected%retained_target_source%document)
        call require_json_text(line, 'retained_target_source_clause', &
            expected%retained_target_source%clause)
        call require_json_text(line, 'retained_target_source_rule', &
            expected%retained_target_source%rule)
        call require_json_integer(line, 'retained_target_source_page', &
            expected%retained_target_source%page)
        call require_json_integer(line, 'retained_target_source_end_page', &
            expected%retained_target_source%end_page)
        call require_json_int64(line, 'retained_target_source_byte_start', &
            expected%retained_target_source%byte_start)
        call require_json_int64(line, 'retained_target_source_byte_length', &
            expected%retained_target_source%byte_length)
        call require_json_text(line, 'retained_target_source_hash', &
            expected%retained_target_source%source_hash)
        call require_json_integer(line, 'retained_target_source_alternative', &
            expected%retained_target_source_alternative)
        call require_json_text(line, 'retained_target_path', &
            expected%retained_target_expression_path)
        call require_json_integer(line, 'retained_target_sequence_slot', &
            expected%retained_target_sequence_boundary_slot)
        call require_json_text(line, 'transformation', expected%transformation)
        call require_json_text(line, 'source_expression_sha256', expected%source_expression_sha256)
        call require_json_text(line, 'target_expression_sha256', expected%target_expression_sha256)
        call require_json_text(line, 'input_expression_sha256', expected%input_expression_sha256)
        call require_json_text(line, 'output_expression_sha256', expected%output_expression_sha256)
        call require_json_text(line, 'disposition', expected%disposition)
        call require_json_text(line, 'reason', expected%reason)
        if (present(expected_byte_start)) then
            call require_json_int64(line, 'source_byte_start', expected_byte_start)
        end if
    end subroutine require_emitted_row

    subroutine require_json_text(line, key, value)
        character(len=*), intent(in) :: line, key, value

        call require(index(line, '"'//trim(key)//'":"'//trim(value)//'"') > 0, &
            'serialized JSON field is incorrect: '//trim(key))
    end subroutine require_json_text

    subroutine require_json_integer(line, key, value)
        character(len=*), intent(in) :: line, key
        integer, intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
        call require(index(line, '"'//trim(key)//'":'//trim(text)) > 0, &
            'serialized JSON integer is incorrect: '//trim(key))
    end subroutine require_json_integer

    subroutine require_json_int64(line, key, value)
        character(len=*), intent(in) :: line, key
        integer(int64), intent(in) :: value
        character(len=64) :: text

        write (text, '(i0)') value
        call require(index(line, '"'//trim(key)//'":'//trim(text)) > 0, &
            'serialized JSON integer is incorrect: '//trim(key))
    end subroutine require_json_int64

    subroutine require_hash_distinction(values, source_rule, source_path)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: values(:)
        character(len=*), intent(in) :: source_rule, source_path
        integer :: i
        logical :: found

        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source%rule) /= trim(source_rule)) cycle
            if (trim(values(i)%raw_source_expression_path) /= trim(source_path)) cycle
            call require(len_trim(values(i)%source_expression_sha256) > 0, &
                'source expression hash is missing from child trace')
            call require(len_trim(values(i)%target_expression_sha256) > 0, &
                'target expression hash is missing from child trace')
            call require(trim(values(i)%source_expression_sha256) /= &
                trim(values(i)%input_expression_sha256) .or. &
                trim(values(i)%target_expression_sha256) /= &
                trim(values(i)%output_expression_sha256), &
                'explicit source/target hashes were not distinct from operation hashes')
            found = .true.
            exit
        end do
        call require(found, 'hash distinction trace row is missing')
    end subroutine require_hash_distinction

    subroutine require_failed_left_recursion_trace(text, source_rule)
        character(len=*), intent(in) :: text, source_rule
        type(standardir_target_rule_t), allocatable :: local_normalized(:), local_suppressed(:)
        type(standardir_grammar_correspondence_trace_t), allocatable :: local_trace(:)
        character(len=512) :: local_message
        logical :: local_ok

        call normalize_fixture(text, local_normalized, local_suppressed, local_trace, local_ok, &
            local_message)
        call require(.not. local_ok, 'unsupported left recursion unexpectedly normalized')
        call require_disposition(local_trace, source_rule, 1, 'rhs', 'unsupported', &
            'left-recursion-unsupported')
        call standardir_grammar_validate_correspondence_trace(local_trace, local_ok, local_message)
        call require(local_ok, 'failed left-recursion trace was incomplete: '//trim(local_message))
    end subroutine require_failed_left_recursion_trace

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_grammar_correspondence

program test_standardir_grammar_transformation_witness
    !! Independent checks for the standalone lowering witness JSONL.

    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_node_t, &
        standardir_grammar_origin_mechanical, standardir_grammar_reference, &
        standardir_grammar_resolution_resolved, standardir_grammar_rule_t, &
        standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_targetnorm, only: standardir_target_provenance_t, &
        standardir_target_rule_t, standardir_target_source_witness_t
    use standardir_grammar_transformation_witness, only: &
        standardir_grammar_emit_transformation_witness, &
        standardir_grammar_validate_source_disposition_witnesses, &
        standardir_grammar_validate_transformation_witness
    implicit none

    type(standardir_grammar_rule_t) :: identity(1), recursive(2)
    type(standardir_target_rule_t) :: malformed(1)
    type(standardir_target_source_witness_t) :: expected_dispositions(2), actual_dispositions(1)
    character(len=65536) :: text
    character(len=256) :: message
    integer :: unit, ios
    logical :: ok

    call make_token_rule(identity(1), 'identity-1', 'identity', 1, 'IDENTITY')
    call make_disposition_witness(expected_dispositions(1), 'omitted-R1', 'R1')
    call make_disposition_witness(expected_dispositions(2), 'omitted-R2', 'R2')
    actual_dispositions(1) = expected_dispositions(1)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open identity witness output')
    call standardir_grammar_emit_transformation_witness(unit, identity, ok, message, &
        pre_lowering_witnesses=actual_dispositions)
    call require(ok, trim(message))
    call read_text(unit, text)
    call require(index(text, '"transformation":"identity"') > 0 .and. &
        index(text, '"source_alternative":"1"') > 0 .and. &
        index(text, '"target_rule":"identity-1"') > 0 .and. &
        index(text, '"transformation":"omitted-before-target-lowering"') > 0 .and. &
        index(text, '"target_rule":"omitted-R1"') > 0 .and. &
        index(text, '"origin":"MECHANICAL"') > 0, &
        'source-backed identity witness row is incomplete')
    close (unit)

    call standardir_grammar_validate_source_disposition_witnesses(expected_dispositions, &
        actual_dispositions, ok, message)
    call require(.not. ok .and. index(message, 'incomplete') > 0, &
        'removed pre-lowering witness was accepted')
    actual_dispositions(1)%reason = 'mutated'
    call standardir_grammar_validate_source_disposition_witnesses(expected_dispositions(1:1), &
        actual_dispositions, ok, message)
    call require(.not. ok .and. index(message, 'incomplete') > 0, &
        'mutated pre-lowering witness was accepted')

    call make_recursive_rules(recursive)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open generated-helper witness output')
    call standardir_grammar_emit_transformation_witness(unit, recursive, ok, message)
    call require(ok, trim(message))
    call read_text(unit, text)
    call require(index(text, '"transformation":"generated-helper"') > 0 .and. &
        index(text, '"target_lhs":"expr__left_recursion"') > 0 .and. &
        index(text, '"source_expression_sha256":"none"') > 0, &
        'generated-helper witness row is incomplete')
    close (unit)

    call make_malformed_target(malformed(1))
    call standardir_grammar_validate_transformation_witness(malformed, ok, message)
    call require(.not. ok .and. index(message, 'lacks an expression hash') > 0, &
        'malformed target witness was accepted: '//trim(message))

contains

    subroutine make_token_rule(value, id, lhs, alternative, source_rule)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, source_rule
        integer, intent(in) :: alternative

        value = standardir_grammar_rule_t()
        value%id = trim(id)
        value%alternative = alternative
        value%lhs = trim(lhs)
        value%root = 1
        allocate (value%nodes%values(1))
        value%nodes%values(1) = standardir_grammar_node_t()
        value%nodes%values(1)%kind = standardir_grammar_token
        value%nodes%values(1)%name = 'atom'
        call set_source(value%source, source_rule, alternative)
        value%origin = standardir_grammar_origin_mechanical
        value%resolution = standardir_grammar_resolution_resolved
    end subroutine make_token_rule

    subroutine make_recursive_rules(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        type(standardir_grammar_node_t) :: nodes(3)

        call make_token_rule(values(1), 'recursive-1', 'expr', 1, 'RECURSIVE-1')
        values(1)%nodes%values(1)%kind = standardir_grammar_token
        values(1)%nodes%values(1)%name = 'base'
        call make_token_rule(values(2), 'recursive-2', 'expr', 2, 'RECURSIVE-2')
        nodes = standardir_grammar_node_t()
        nodes(1)%kind = standardir_grammar_sequence
        nodes(1)%name = 'sequence'
        nodes(1)%first_child = 2
        nodes(1)%child_count = 2
        nodes(2)%kind = standardir_grammar_reference
        nodes(2)%name = 'expr'
        nodes(3)%kind = standardir_grammar_token
        nodes(3)%name = 'tail'
        deallocate (values(2)%nodes%values)
        allocate (values(2)%nodes%values(3))
        values(2)%nodes%values = nodes
    end subroutine make_recursive_rules

    subroutine make_malformed_target(value)
        type(standardir_target_rule_t), intent(out) :: value

        value = standardir_target_rule_t()
        value%id = 'malformed'
        value%alternative = 1
        value%lhs = 'malformed'
        value%target_expression_sha256 = ''
        value%origin = standardir_grammar_origin_mechanical
        allocate (value%provenance(1))
        call set_provenance(value%provenance(1), 'AMBIGUOUS', 1)
        value%provenance(1)%source_expression_sha256 = ''
    end subroutine make_malformed_target

    subroutine make_disposition_witness(value, target_rule, source_rule)
        type(standardir_target_source_witness_t), intent(out) :: value
        character(len=*), intent(in) :: target_rule, source_rule

        value = standardir_target_source_witness_t()
        call set_source(value%source%source, source_rule, 1)
        value%source%alternative = 1
        value%source%source_expression_present = .true.
        value%source%source_expression_sha256 = 'SOURCE-EXPRESSION'
        value%target_rule_id = trim(target_rule)
        value%target_lhs = trim(source_rule)
        value%target_alternative = 1
        value%reason = 'omitted-before-target-lowering'
        value%target_expression_sha256 = 'TARGET-EXPRESSION'
    end subroutine make_disposition_witness

    subroutine set_source(value, source_rule, alternative)
        type(standardir_source_ref_t), intent(out) :: value
        character(len=*), intent(in) :: source_rule
        integer, intent(in) :: alternative

        value = standardir_source_ref_t()
        value%document = 'DOC'
        value%clause = '1'
        value%rule = trim(source_rule)
        value%page = alternative
        value%end_page = alternative
        value%source_hash = 'SOURCE-HASH'
    end subroutine set_source

    subroutine set_provenance(value, source_rule, alternative)
        type(standardir_target_provenance_t), intent(out) :: value
        character(len=*), intent(in) :: source_rule
        integer, intent(in) :: alternative

        value = standardir_target_provenance_t()
        call set_source(value%source, source_rule, alternative)
        value%alternative = alternative
        value%source_expression_sha256 = 'SOURCE-EXPRESSION'
    end subroutine set_provenance

    subroutine read_text(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: text
        character(len=4096) :: line
        integer :: ios, used

        text = ''
        used = 0
        rewind (unit)
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (used + len_trim(line) + 1 > len(text)) call fail('witness output is too long')
            if (len_trim(line) > 0) then
                text(used + 1:used + len_trim(line)) = trim(line)
                used = used + len_trim(line)
            end if
            if (used < len(text)) then
                used = used + 1
                text(used:used) = achar(10)
            end if
        end do
    end subroutine read_text

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) call fail(trim(message))
    end subroutine require

    subroutine fail(message)
        character(len=*), intent(in) :: message

        print '(a)', 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_standardir_grammar_transformation_witness

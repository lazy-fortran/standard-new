program test_standardir_grammar_v0_export
    use fortsx, only: sx_node_t, sx_parse, sx_validate, sx_write
    use standardir_grammar_producer, only: standardir_grammar_node_t, &
        standardir_grammar_reference, standardir_grammar_repeat, standardir_grammar_sequence, &
        standardir_grammar_origin_mechanical, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t
    use standardir_grammar_target_records, only: standardir_target_provenance_t, &
        standardir_target_rule_t, standardir_target_expression_t
    use standardir_grammar_v0_export, only: standardir_grammar_v0_produce, &
        standardir_grammar_v0_produce_batch
    implicit none

    character(len=*), parameter :: expected = &
        '(syntax-rule (id R501) (alternative 1) (lhs program) (root 1) '// &
        '(nodes (grammar-nodes (grammar-node sequence - 1 false 2 2) '// &
        '(grammar-node reference program-unit 1 false 0 0) '// &
        '(grammar-node repeat - 1 true 4 1) '// &
        '(grammar-node reference program-unit 1 false 0 0))) '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R501) '// &
        '(page 45) (source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_second = &
        '(syntax-rule (id R502) (alternative 2) (lhs module) (root 1) '// &
        '(nodes (grammar-nodes (grammar-node sequence - 1 false 2 2) '// &
        '(grammar-node reference program-unit 1 false 0 0) '// &
        '(grammar-node repeat - 1 true 4 1) '// &
        '(grammar-node reference program-unit 1 false 0 0))) '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R502) '// &
        '(page 45) (source-hash fixture))) (origin mechanical) (resolution resolved))'
    type(standardir_target_rule_t) :: value
    type(standardir_target_rule_t) :: batch_rules(2)
    type(standardir_target_rule_t), allocatable :: empty_rules(:)
    type(standardir_grammar_rule_t) :: flat_value
    type(sx_node_t) :: actual, expected_node
    type(sx_node_t), allocatable :: batch_nodes(:)
    character(len=256) :: message
    character(len=4096) :: text
    integer :: unit, ios
    logical :: ok

    call make_value(value)
    call standardir_grammar_v0_produce(value, actual, ok, message)
    call require(ok, trim(message))
    call sx_parse(expected, expected_node, ok, message)
    call require(ok, trim(message))
    call sx_validate(actual, ok, message)
    call require(ok, trim(message))
    call open_scratch(unit)
    call sx_write(unit, actual, ok, message)
    call require(ok, trim(message))
    call read_line(unit, text)
    close (unit=unit)
    call require(trim(text) == expected, 'canonical SX shape differs from fixture')

    call make_value(batch_rules(1))
    call make_value(batch_rules(2))
    batch_rules(2)%id = 'R502'; batch_rules(2)%alternative = 2; batch_rules(2)%lhs = 'module'
    batch_rules(2)%provenance(1)%alternative = 2
    batch_rules(2)%provenance(1)%source%rule = 'R502'
    call standardir_grammar_v0_produce_batch(batch_rules, batch_nodes, ok, message)
    call require(ok .and. allocated(batch_nodes) .and. size(batch_nodes) == 2, &
        'two-rule batch was not produced')
    call require_batch_text(batch_nodes(1), expected)
    call require_batch_text(batch_nodes(2), expected_second)
    call sx_validate(batch_nodes(1), ok, message)
    call require(ok, trim(message))
    call sx_validate(batch_nodes(2), ok, message)
    call require(ok, trim(message))

    call standardir_grammar_v0_produce_batch(empty_rules, batch_nodes, ok, message)
    call require(ok .and. allocated(batch_nodes) .and. size(batch_nodes) == 0, &
        'empty batch did not produce an allocated zero-length result')

    call make_value(batch_rules(1))
    call make_value(batch_rules(2))
    deallocate (batch_rules(2)%expression%children)
    call standardir_grammar_v0_produce_batch(batch_rules, batch_nodes, ok, message)
    call require(.not. ok .and. .not. allocated(batch_nodes), &
        'malformed second rule retained partial batch output')

    call make_value(batch_rules(1))
    call make_value(batch_rules(2))
    batch_rules(2)%provenance(1)%source%source_hash = ''
    call stale_batch_output(batch_rules, batch_nodes, 'provenance failure')
    call make_value(batch_rules(2))
    batch_rules(2)%origin = 99
    call stale_batch_output(batch_rules, batch_nodes, 'origin failure')
    call make_value(batch_rules(2))
    batch_rules(2)%resolution = 99
    call stale_batch_output(batch_rules, batch_nodes, 'resolution failure')
    call make_value(batch_rules(2))
    ! The target-rule batch API accepts target rules; its nested expression
    ! validation is exercised here with a malformed second target rule.
    deallocate (batch_rules(2)%expression%children)
    call stale_batch_output(batch_rules, batch_nodes, 'offset/group failure')

    flat_value = standardir_grammar_rule_t()
    flat_value%id = 'R501'; flat_value%alternative = 1; flat_value%lhs = 'program'; flat_value%root = 1
    flat_value%source%document = 'J3-24-007'; flat_value%source%clause = '5'
    flat_value%source%rule = 'R501'; flat_value%source%page = 45; flat_value%source%source_hash = 'fixture'
    flat_value%origin = standardir_grammar_origin_mechanical
    flat_value%resolution = standardir_grammar_resolution_resolved
    allocate (flat_value%nodes%values(1))
    flat_value%nodes%values(1)%kind = standardir_grammar_sequence
    flat_value%nodes%values(1)%minimum = 1; flat_value%nodes%values(1)%first_child = 99
    flat_value%nodes%values(1)%child_count = 1
    call standardir_grammar_v0_produce(flat_value, actual, ok, message)
    call require(.not. ok, 'malformed flat offset was accepted')

    deallocate (value%expression%children)
    call standardir_grammar_v0_produce(value, actual, ok, message)
    call require(.not. ok .and. actual%kind == 0, 'malformed group was accepted or output retained')
    call make_value(value)
    value%provenance(1)%source%source_hash = ''
    call standardir_grammar_v0_produce(value, actual, ok, message)
    call require(.not. ok .and. actual%kind == 0, 'incomplete provenance was accepted')
    call make_value(value)
    value%origin = 99
    call standardir_grammar_v0_produce(value, actual, ok, message)
    call require(.not. ok .and. actual%kind == 0, 'invalid origin was accepted')
    call make_value(value)
    value%resolution = 99
    call standardir_grammar_v0_produce(value, actual, ok, message)
    call require(.not. ok .and. actual%kind == 0, 'invalid resolution was accepted')
    call make_flat_value(flat_value)
    flat_value%nodes%values(1)%first_child = 99
    call standardir_grammar_v0_produce(flat_value, actual, ok, message)
    call require(.not. ok .and. actual%kind == 0, 'malformed flat offset was accepted')

    print '(a)', 'StandardIR grammar-v0 export test passed'

contains

    subroutine make_value(value)
        type(standardir_target_rule_t), intent(out) :: value
        type(standardir_target_expression_t) :: sequence, first, repeated, tail
        value = standardir_target_rule_t()
        value%id = 'R501'; value%alternative = 1; value%lhs = 'program'
        sequence%kind = standardir_grammar_sequence; sequence%name = '-'; sequence%minimum = 1
        allocate (sequence%children(2))
        first%kind = standardir_grammar_reference; first%name = 'program-unit'; first%minimum = 1
        repeated%kind = standardir_grammar_repeat; repeated%name = '-'; repeated%minimum = 1
        repeated%unbounded = .true.; allocate (repeated%children(1))
        tail%kind = standardir_grammar_reference; tail%name = 'program-unit'; tail%minimum = 1
        repeated%children(1) = tail
        sequence%children(1) = first; sequence%children(2) = repeated
        value%expression = sequence
        allocate (value%provenance(1)); value%provenance(1) = standardir_target_provenance_t()
        value%provenance(1)%alternative = 1
        value%provenance(1)%source%document = 'J3-24-007'
        value%provenance(1)%source%clause = '5'; value%provenance(1)%source%rule = 'R501'
        value%provenance(1)%source%page = 45; value%provenance(1)%source%source_hash = 'fixture'
        value%origin = standardir_grammar_origin_mechanical
        value%resolution = standardir_grammar_resolution_resolved
    end subroutine make_value

    subroutine make_flat_value(value)
        type(standardir_grammar_rule_t), intent(out) :: value
        type(standardir_grammar_node_t) :: item
        value = standardir_grammar_rule_t()
        value%id = 'R-FLAT'; value%alternative = 1; value%lhs = 'program'; value%root = 1
        item = standardir_grammar_node_t(); item%kind = standardir_grammar_reference
        item%name = 'program-unit'; item%minimum = 1
        allocate (value%nodes%values(1)); value%nodes%values(1) = item
        value%source%document = 'J3-24-007'; value%source%clause = '5'; value%source%rule = 'R-FLAT'
        value%source%page = 45; value%source%source_hash = 'fixture'
        value%origin = standardir_grammar_origin_mechanical
        value%resolution = standardir_grammar_resolution_resolved
    end subroutine make_flat_value

    subroutine open_scratch(unit)
        integer, intent(out) :: unit
        integer :: ios
        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open scratch output')
    end subroutine open_scratch

    subroutine read_line(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: value
        integer :: ios
        rewind (unit); read (unit, '(a)', iostat=ios) value
        call require(ios == 0, 'could not read SX output')
    end subroutine read_line

    subroutine require_batch_text(node, fixture)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: fixture
        character(len=4096) :: value
        integer :: local_unit, local_ios
        open (newunit=local_unit, status='scratch', action='readwrite', iostat=local_ios)
        call require(local_ios == 0, 'could not open batch scratch output')
        call sx_write(local_unit, node, ok, message)
        call require(ok, trim(message))
        call read_line(local_unit, value)
        close (unit=local_unit)
        call require(trim(value) == fixture, 'batch SX order or shape differs from fixture')
    end subroutine require_batch_text

    subroutine stale_batch_output(rules, nodes, failure)
        type(standardir_target_rule_t), intent(in) :: rules(:)
        type(sx_node_t), allocatable, intent(inout) :: nodes(:)
        character(len=*), intent(in) :: failure
        logical :: local_ok
        character(len=256) :: local_message
        allocate (nodes(1))
        call standardir_grammar_v0_produce_batch(rules, nodes, local_ok, local_message)
        call require(.not. local_ok .and. .not. allocated(nodes), &
            'batch '//trim(failure)//' retained stale output')
    end subroutine stale_batch_output

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message
        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_grammar_v0_export

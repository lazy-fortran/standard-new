program test_standardir_statement_boundary_mapping
    !! Fixed grammar tables are the independent source-path mapping oracle.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_producer, only: standardir_grammar_node_t, standardir_grammar_origin_human, &
        standardir_grammar_reference, standardir_grammar_repeat, standardir_grammar_resolution_resolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence
    use standardir_statement_boundary, only: standardir_statement_boundary_build_plan, &
        standardir_statement_boundary_plan_t
    use standardir_statement_boundary_mapping, only: standardir_boundary_ambiguous, &
        standardir_boundary_mapped, standardir_boundary_unsupported, &
        standardir_statement_boundary_coalesce_mappings, standardir_statement_boundary_map, &
        standardir_statement_boundary_mapping_t
    use standardir_statement_sequence, only: standardir_statement_sequence_candidate_t
    implicit none

    character(len=*), parameter :: hash = repeat('a', 64)
    character(len=*), parameter :: other_hash = repeat('b', 64)
    type(standardir_grammar_rule_t) :: rules(10)
    type(standardir_statement_sequence_candidate_t) :: candidates(8)
    type(standardir_statement_sequence_candidate_t) :: coalesced_candidates(2)
    type(standardir_statement_boundary_plan_t) :: plan, broken
    type(standardir_statement_boundary_plan_t) :: coalesced_plan
    type(standardir_statement_boundary_mapping_t), allocatable :: mappings(:)
    type(standardir_statement_boundary_mapping_t), allocatable :: identity_mappings(:)
    character(len=512) :: message
    logical :: ok
    integer :: index, plan_index

    call make_rules(rules)
    candidates(1) = candidate('R1', 'nested-stmt', 'rhs/2/1/2', '100', 'nested-stmt', 'repeat-item')
    candidates(2) = candidate('R2', 'duplicate-stmt', 'rhs/1', '0200', 'first-stmt', 'repeat-item')
    candidates(3) = candidate('R2', 'duplicate-stmt', 'rhs/1', '201', 'second-stmt', 'repeat-item')
    candidates(4) = candidate('R3', 'alternative-stmt', 'rhs/1', '300', 'alternative-stmt', 'repeat-item')
    candidates(5) = candidate('R4', 'missing-path-stmt', 'rhs/9', '400', 'missing-path-stmt', 'repeat-item')
    candidates(6) = candidate('R5', 'missing-lineage-stmt', 'rhs', '500', 'missing-lineage-stmt', 'repeat-item')
    candidates(7) = candidate('R6', 'unique-stmt', 'rhs', '600', 'unique-stmt', 'repeat-item')
    candidates(8) = candidate('R7', 'malformed-stmt', 'rhs', '700', 'malformed-stmt', 'repeat-item')

    call standardir_statement_boundary_build_plan(candidates, plan, ok, message, coalesce=.true.)
    call require(ok, message)
    call standardir_statement_boundary_map(plan, rules, mappings, ok, message)
    call require(ok, message)
    call require(size(mappings) == size(plan%sites), 'mapping did not retain one record per plan site')
    call require(.not. plan%insertion_supported, 'mapping enabled target token insertion')
    call require(trim(plan%integration_boundary) == &
        'target expression mapping and token insertion remain downstream', &
        'mapping changed the target integration boundary')

    index = find_mapping(mappings, 'R1', '100', 'rhs/2/1/2')
    call require(index > 0, 'nested source path mapping was not retained')
    call require(trim(mappings(index)%disposition) == standardir_boundary_mapped .and. &
        mappings(index)%source_node_index == 6 .and. &
        mappings(index)%source_node_kind == standardir_grammar_repeat .and. &
        trim(mappings(index)%source_node_name) == '-' .and. mappings(index)%alternative == 1, &
        'nested source path did not resolve to the fixed repeat node')

    index = find_mapping(mappings, 'R2', '0200', 'rhs/1')
    call require(index > 0, 'first duplicate occurrence was not retained')
    call require(trim(mappings(index)%disposition) == standardir_boundary_mapped .and. &
        trim(mappings(index)%source_node_name) == 'first-node', &
        'numeric source byte matching selected the wrong duplicate occurrence')
    index = find_mapping(mappings, 'R2', '201', 'rhs/1')
    call require(index > 0, 'second duplicate occurrence was not retained')
    call require(trim(mappings(index)%disposition) == standardir_boundary_mapped .and. &
        trim(mappings(index)%source_node_name) == 'second-node', &
        'source occurrence distinction was lost')

    index = find_mapping(mappings, 'R3', '300', 'rhs/1')
    call require(index > 0, 'multiple-alternative mapping was not retained')
    call require(trim(mappings(index)%disposition) == standardir_boundary_ambiguous .and. &
        mappings(index)%alternative == 0 .and. size(mappings(index)%alternatives) == 2 .and. &
        mappings(index)%alternatives(1) == 1 .and. mappings(index)%alternatives(2) == 2, &
        'multiple matching alternatives were not reported as ambiguous')

    index = find_mapping(mappings, 'R4', '400', 'rhs/9')
    call require(index > 0, 'missing source path disposition was dropped')
    call require(trim(mappings(index)%disposition) == standardir_boundary_unsupported .and. &
        index_of(trim(mappings(index)%reason), 'path is missing') > 0, &
        'missing source path was not retained as unsupported evidence')
    index = find_mapping(mappings, 'R5', '500', 'rhs')
    call require(index > 0, 'missing source lineage disposition was dropped')
    call require(trim(mappings(index)%disposition) == standardir_boundary_unsupported .and. &
        index_of(trim(mappings(index)%reason), 'lineage') > 0 .and. &
        same_candidate(mappings(index)%candidate, candidates(6)), &
        'missing source lineage did not retain candidate provenance')
    index = find_mapping(mappings, 'R7', '700', 'rhs')
    call require(index > 0, 'malformed source tree disposition was dropped')
    call require(trim(mappings(index)%disposition) == standardir_boundary_unsupported .and. &
        index_of(trim(mappings(index)%reason), 'node shape') > 0, &
        'malformed source node shape was not retained as unsupported evidence')

    broken = plan
    plan_index = find_plan_site(plan, 'R1', '100', 'rhs/2/1/2')
    call require(plan_index > 0, 'could not locate nested plan site for malformed-path control')
    broken%sites(plan_index)%candidate%expression_path = 'rhs/0'
    call standardir_statement_boundary_map(broken, rules, mappings, ok, message)
    call require(ok, message)
    index = find_mapping(mappings, 'R1', '100', 'rhs/0')
    call require(index > 0 .and. trim(mappings(index)%disposition) == standardir_boundary_unsupported, &
        'malformed canonical path was accepted or discarded')
    call require(index_of(trim(mappings(index)%reason), 'malformed') > 0, &
        'malformed canonical path did not carry a reason')

    broken = plan
    broken%sites(plan_index)%candidate%source_hash = ''
    call standardir_statement_boundary_map(broken, rules, mappings, ok, message)
    call require(ok, message)
    index = find_mapping(mappings, 'R1', '100', 'rhs/2/1/2')
    call require(index > 0 .and. trim(mappings(index)%disposition) == standardir_boundary_unsupported, &
        'missing candidate lineage was rejected instead of retained')
    call require(same_candidate(mappings(index)%candidate, broken%sites(plan_index)%candidate), &
        'mapping did not preserve the original candidate on failure')

    coalesced_candidates(1) = candidate('R1', 'nested-stmt', 'rhs/2/1/2', '100', 'nested-stmt', 'repeat-item')
    coalesced_candidates(2) = coalesced_candidates(1)
    coalesced_candidates(2)%kind = 'sequence-internal'
    coalesced_candidates(2)%item = 'other-item'
    coalesced_candidates(2)%derivation = 'other-derivation'
    call standardir_statement_boundary_build_plan(coalesced_candidates, coalesced_plan, ok, message, coalesce=.true.)
    call require(ok, message)
    call require(size(coalesced_plan%sites) == 1, 'mapping plan retained duplicate structural sites')
    call require(size(coalesced_plan%sites(1)%evidence) == 2, &
        'mapping plan did not retain all candidate evidence')
    call standardir_statement_boundary_map(coalesced_plan, rules, mappings, ok, message)
    call require(ok, message)
    call require(size(mappings) == 1 .and. size(mappings(1)%evidence) == 2, &
        'mapping did not retain one site and both evidence records')
    call require(trim(mappings(1)%evidence(1)%kind) == 'repeat-item' .and. &
        trim(mappings(1)%evidence(2)%kind) == 'sequence-internal', &
        'mapping evidence order was not deterministic')

    allocate (identity_mappings(2))
    call make_mapping(identity_mappings(1), coalesced_candidates(1), standardir_grammar_repeat)
    call make_mapping(identity_mappings(2), coalesced_candidates(2), standardir_grammar_sequence)
    call standardir_statement_boundary_coalesce_mappings(identity_mappings, ok, message)
    call require(ok .and. size(identity_mappings) == 2, &
        'different source node kinds were silently coalesced')
    deallocate (identity_mappings)

    allocate (identity_mappings(2))
    call make_mapping(identity_mappings(1), coalesced_candidates(1), standardir_grammar_repeat)
    call make_mapping(identity_mappings(2), coalesced_candidates(2), standardir_grammar_repeat)
    call standardir_statement_boundary_coalesce_mappings(identity_mappings, ok, message)
    call require(ok .and. size(identity_mappings) == 1 .and. size(identity_mappings(1)%evidence) == 2, &
        'equal source node kinds did not retain candidate evidence')

    print '(a)', 'StandardIR statement-boundary mapping test passed'

contains

    function candidate(rule, lhs, path, byte_start, item, kind) result(value)
        character(len=*), intent(in) :: rule, lhs, path, byte_start, item, kind
        type(standardir_statement_sequence_candidate_t) :: value

        value = standardir_statement_sequence_candidate_t()
        value%source_rule = trim(rule)
        value%source_lhs = trim(lhs)
        value%source_document = 'DOC'
        value%source_clause = '4.1.4'
        value%source_hash = hash
        value%expression_path = trim(path)
        value%item = trim(item)
        value%kind = trim(kind)
        value%derivation = 'fixed-witness'
        value%source_page = '045'
        value%source_byte_start = trim(byte_start)
        value%status = 'candidate'
    end function candidate

    subroutine make_mapping(value, source_candidate, source_node_kind)
        type(standardir_statement_boundary_mapping_t), intent(out) :: value
        type(standardir_statement_sequence_candidate_t), intent(in) :: source_candidate
        integer, intent(in) :: source_node_kind

        value = standardir_statement_boundary_mapping_t()
        value%candidate = source_candidate
        allocate (value%evidence(1), value%alternatives(1))
        value%evidence(1)%kind = source_candidate%kind
        value%evidence(1)%item = source_candidate%item
        value%evidence(1)%derivation = source_candidate%derivation
        value%evidence(1)%status = source_candidate%status
        value%disposition = standardir_boundary_mapped
        value%source_node_index = 2
        value%source_node_kind = source_node_kind
        value%source_node_name = 'node'
        value%alternative = 1
        value%alternatives(1) = 1
    end subroutine make_mapping

    subroutine make_rules(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_rule(values(1), 'R1', 'nested-stmt', 100_int64, 1, 'nested', hash)
        call make_rule(values(2), 'R2', 'duplicate-stmt', 200_int64, 1, 'first-node', hash)
        call make_rule(values(3), 'R2', 'duplicate-stmt', 201_int64, 1, 'second-node', hash)
        call make_rule(values(4), 'R3', 'alternative-stmt', 300_int64, 1, 'alternative-one', hash)
        call make_rule(values(5), 'R3', 'alternative-stmt', 300_int64, 2, 'alternative-two', hash)
        call make_rule(values(6), 'R4', 'missing-path-stmt', 400_int64, 1, 'simple', hash)
        call make_rule(values(7), 'R5', 'missing-lineage-stmt', 500_int64, 1, 'missing-hash', '')
        call make_rule(values(8), 'R6', 'unique-stmt', 600_int64, 1, 'unique', hash)
        call make_rule(values(9), 'R2', 'duplicate-stmt', 200_int64, 2, 'wrong-hash', other_hash)
        call make_rule(values(10), 'R7', 'malformed-stmt', 700_int64, 1, 'malformed', hash)
        values(10)%nodes%values(1)%kind = 99
    end subroutine make_rules

    subroutine make_rule(value, rule, lhs, byte_start, alternative, child_name, source_hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: rule, lhs, child_name, source_hash
        integer(int64), intent(in) :: byte_start
        integer, intent(in) :: alternative

        value = standardir_grammar_rule_t()
        value%id = trim(rule)
        value%alternative = alternative
        value%lhs = trim(lhs)
        value%root = 1
        value%origin = standardir_grammar_origin_human
        value%resolution = standardir_grammar_resolution_resolved
        value%source = standardir_source_ref_t()
        value%source%document = 'DOC'
        value%source%clause = '4.1.4'
        value%source%rule = trim(rule)
        value%source%page = 45
        value%source%byte_start = byte_start
        value%source%source_hash = trim(source_hash)
        if (trim(rule) == 'R1') then
            allocate (value%nodes%values(7))
            call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 2, 2)
            call set_node(value%nodes%values(2), standardir_grammar_reference, 'prefix', 0, 0)
            call set_node(value%nodes%values(3), standardir_grammar_repeat, '-', 4, 1, 0)
            call set_node(value%nodes%values(4), standardir_grammar_sequence, '-', 5, 2)
            call set_node(value%nodes%values(5), standardir_grammar_reference, 'child-a', 0, 0)
            call set_node(value%nodes%values(6), standardir_grammar_repeat, '-', 7, 1, 0)
            call set_node(value%nodes%values(7), standardir_grammar_reference, 'child-b', 0, 0)
        else if (trim(rule) == 'R6') then
            allocate (value%nodes%values(2))
            call set_node(value%nodes%values(1), standardir_grammar_repeat, '-', 2, 1, 0)
            call set_node(value%nodes%values(2), standardir_grammar_reference, child_name, 0, 0)
        else
            allocate (value%nodes%values(2))
            call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 2, 1)
            call set_node(value%nodes%values(2), standardir_grammar_reference, child_name, 0, 0)
        end if
    end subroutine make_rule

    subroutine set_node(value, kind, name, first_child, child_count, minimum)
        type(standardir_grammar_node_t), intent(out) :: value
        integer, intent(in) :: kind, first_child, child_count
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: minimum

        value = standardir_grammar_node_t()
        value%kind = kind
        value%name = trim(name)
        value%first_child = first_child
        value%child_count = child_count
        value%minimum = 1
        if (present(minimum)) value%minimum = minimum
        value%unbounded = kind == standardir_grammar_repeat
    end subroutine set_node

    integer function find_mapping(values, rule, byte_start, path)
        type(standardir_statement_boundary_mapping_t), intent(in) :: values(:)
        character(len=*), intent(in) :: rule, byte_start, path
        integer :: i

        find_mapping = 0
        do i = 1, size(values)
            if (trim(values(i)%candidate%source_rule) /= trim(rule)) cycle
            if (trim(values(i)%candidate%source_byte_start) /= trim(byte_start)) cycle
            if (trim(values(i)%candidate%expression_path) /= trim(path)) cycle
            find_mapping = i
            return
        end do
    end function find_mapping

    integer function find_plan_site(value, rule, byte_start, path)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        character(len=*), intent(in) :: rule, byte_start, path
        integer :: i

        find_plan_site = 0
        do i = 1, size(value%sites)
            if (trim(value%sites(i)%candidate%source_rule) /= trim(rule)) cycle
            if (trim(value%sites(i)%candidate%source_byte_start) /= trim(byte_start)) cycle
            if (trim(value%sites(i)%candidate%expression_path) /= trim(path)) cycle
            find_plan_site = i
            return
        end do
    end function find_plan_site

    logical function same_candidate(left, right)
        type(standardir_statement_sequence_candidate_t), intent(in) :: left, right

        same_candidate = trim(left%source_rule) == trim(right%source_rule) .and. &
            trim(left%source_lhs) == trim(right%source_lhs) .and. &
            trim(left%source_document) == trim(right%source_document) .and. &
            trim(left%source_clause) == trim(right%source_clause) .and. &
            trim(left%source_hash) == trim(right%source_hash) .and. &
            trim(left%expression_path) == trim(right%expression_path) .and. &
            trim(left%item) == trim(right%item) .and. trim(left%kind) == trim(right%kind) .and. &
            trim(left%derivation) == trim(right%derivation) .and. &
            trim(left%source_page) == trim(right%source_page) .and. &
            trim(left%source_byte_start) == trim(right%source_byte_start) .and. &
            trim(left%status) == trim(right%status)
    end function same_candidate

    integer function index_of(text, fragment)
        character(len=*), intent(in) :: text, fragment
        intrinsic :: index

        index_of = index(trim(text), trim(fragment))
    end function index_of

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_standardir_statement_boundary_mapping

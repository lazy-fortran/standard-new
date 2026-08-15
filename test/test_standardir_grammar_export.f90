program test_standardir_grammar_export
    !! Independent format text is the oracle for the normalized batch boundary.

    use standardir_grammar_export
    use standardir_grammar_producer, only: standardir_grammar_node_t, &
        standardir_grammar_origin_human, standardir_grammar_reference, &
        standardir_grammar_optional, &
        standardir_grammar_repeat, &
        standardir_grammar_resolution_resolved, standardir_grammar_resolution_unresolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_token
    implicit none

    type(standardir_grammar_rule_t) :: rules(3), bad(3), cyclic(3), unresolved(3), interleaved(3)
    type(standardir_grammar_rule_t) :: duplicate(2), same_occurrence(2), merged(2), different(2), &
        invalid_provenance(2), direct(2), multiple_direct(3), mutual(4), wrapped(1), unsupported(1)
    type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
    type(standardir_target_rule_t), allocatable :: retained(:), pruned(:)
    type(standardir_target_reachability_witness_t), allocatable :: reachability_witness(:)
    type(standardir_grammar_rule_t) :: reachability(4)
    type(standardir_grammar_rule_t) :: role_fixture(7), no_alias(2)
    type(standardir_grammar_rule_t) :: role_reordered(7), alias_chain(4)
    type(standardir_target_rule_t), allocatable :: role_normalized(:), role_retained(:)
    type(standardir_target_rule_t), allocatable :: role_pruned(:), role_factored(:)
    type(standardir_target_role_family_witness_t), allocatable :: role_witness(:), broken_witness(:)
    type(standardir_target_reachability_witness_t), allocatable :: role_reachability(:)
    type(standardir_target_role_family_config_t) :: role_config
    character(len=128) :: selected_roots(1)
    character(len=128) :: all_roots(2)
    character(len=128) :: role_roots(1), expected_roles(3)
    integer :: format, unit, ios
    logical :: ok
    character(len=256) :: message, line

    call make_rules(rules)
    do format = standardir_grammar_format_ebnf, standardir_grammar_format_tree_sitter
        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open format scratch output')
        call standardir_grammar_export_batch(unit, rules, format, ok, message)
        call require(ok, message)
        call verify_format(unit, format)
        close (unit)
    end do

    bad = rules
    bad(2)%nodes%values(1)%first_child = 99
    call verify_failure(bad, standardir_grammar_format_ebnf, 'malformed rule')

    cyclic = rules
    cyclic(2)%nodes%values(1)%first_child = 1
    call verify_failure(cyclic, standardir_grammar_format_ebnf, 'cyclic rule')

    unresolved = rules
    unresolved(3)%resolution = standardir_grammar_resolution_unresolved
    call verify_failure(unresolved, standardir_grammar_format_antlr4, 'unresolved rule')

    interleaved = rules
    interleaved(2) = rules(3)
    interleaved(3) = rules(2)
    call verify_failure(interleaved, standardir_grammar_format_bison, 'interleaved LHS groups')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open invalid-format scratch output')
    write (unit, '(a)') 'sentinel'
    call standardir_grammar_export_batch(unit, rules, 99, ok, message)
    call require(.not. ok .and. len_trim(message) > 0, 'invalid format was accepted')
    rewind (unit)
    read (unit, '(a)', iostat=ios) line
    call require(ios == 0 .and. trim(line) == 'sentinel', &
        'invalid format did not leave output untouched')
    close (unit)

    call make_duplicate(duplicate)
    call standardir_grammar_normalize(duplicate, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 1 .and. size(suppressed) == 1, &
        'duplicate alternatives were not deterministically eliminated')
    call require(normalized(1)%alternative == 1 .and. suppressed(1)%alternative == 2, &
        'duplicate alternative provenance was not retained')

    call make_same_occurrence(same_occurrence)
    call standardir_grammar_normalize(same_occurrence, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 1 .and. size(suppressed) == 1 .and. &
        size(normalized(1)%provenance) == 1, &
        'same-body same-occurrence fixture was not merged once: '//trim(message))
    call require(trim(normalized(1)%source%rule) == 'SAME-1' .and. &
        trim(normalized(1)%provenance(1)%source%rule) == 'SAME-1', &
        'same-body canonical primary source changed')

    call make_merged(merged)
    call standardir_grammar_normalize(merged, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 1 .and. size(suppressed) == 1 .and. &
        size(normalized(1)%provenance) == 2, &
        'same-body different-occurrence fixture did not merge lineage: '//trim(message))
    call require(trim(normalized(1)%source%rule) == 'MERGE-1' .and. &
        trim(normalized(1)%provenance(1)%source%rule) == 'MERGE-1' .and. &
        trim(normalized(1)%provenance(2)%source%rule) == 'MERGE-2' .and. &
        trim(normalized(1)%provenance(1)%source%source_hash) == 'HASH-M1' .and. &
        trim(normalized(1)%provenance(2)%source%source_hash) == 'HASH-M2' .and. &
        normalized(1)%provenance(1)%source%page == 2 .and. &
        normalized(1)%provenance(2)%source%page == 3 .and. &
        normalized(1)%provenance(1)%source%byte_start == 101 .and. &
        normalized(1)%provenance(2)%source%byte_start == 202, &
        'merged lineage or canonical primary source is not deterministic')
    do format = standardir_grammar_format_ebnf, standardir_grammar_format_tree_sitter
        call verify_merged_lineage(merged, format)
    end do

    call make_different(different)
    call standardir_grammar_normalize(different, normalized, suppressed, ok, message)
    call require(ok, 'different normalized bodies were not accepted: '//trim(message))
    call require(size(normalized) == 2 .and. size(suppressed) == 0, &
        'different normalized bodies were incorrectly merged')
    call require(size(normalized(1)%provenance) == 1, 'first different body lost provenance')
    call require(size(normalized(2)%provenance) == 1, 'second different body lost provenance')

    call make_invalid_provenance(invalid_provenance)
    call verify_failure(invalid_provenance, standardir_grammar_format_ebnf, 'invalid source provenance')

    call make_wrapped(wrapped(1))
    call standardir_grammar_normalize(wrapped, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 1 .and. size(suppressed) == 0, &
        'nullable normalization failed: '//trim(message))
    call require(normalized(1)%expression%kind == 5, &
        'nullable wrapper and singleton sequence were not simplified')

    call make_direct(direct)
    call standardir_grammar_normalize(direct, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 2 .and. size(suppressed) == 1, &
        'direct normalization failed: '//trim(message))
    call require(trim(normalized(1)%lhs) == 'expr' .and. &
        normalized(1)%expression%kind == standardir_grammar_sequence .and. &
        trim(normalized(2)%lhs) == 'expr__left_recursion', &
        'direct left recursion was not transformed into a generic helper')
    call require(direct_witnesses_preserved(normalized), &
        'direct left-recursion witness structure changed')
    do format = standardir_grammar_format_ebnf, standardir_grammar_format_tree_sitter
        call verify_transform_output(direct, format, 'expr__left_recursion')
    end do

    call make_multiple_direct(multiple_direct)
    call standardir_grammar_normalize(multiple_direct, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 3 .and. size(suppressed) == 2, &
        'multiple direct recursion did not preserve helper alternatives: '//trim(message))
    call require(trim(normalized(2)%lhs) == 'expr__left_recursion' .and. &
        trim(normalized(2)%source%rule) == 'MREC-1' .and. normalized(2)%alternative == 1 .and. &
        trim(normalized(3)%source%rule) == 'MREC-2' .and. normalized(3)%alternative == 2, &
        'helper alternatives lost their source provenance')
    call require(normalized(1)%expression%kind == standardir_grammar_sequence .and. &
        normalized(1)%expression%children(2)%kind == standardir_grammar_repeat, &
        'base alternative does not repeat the generated helper')

    call standardir_grammar_normalize(rules, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 3 .and. size(suppressed) == 0, &
        'non-recursive references were incorrectly expanded: '//trim(message))
    call require(normalized(1)%expression%kind == standardir_grammar_sequence .and. &
        trim(normalized(1)%expression%children(1)%name) == 'term', &
        'non-recursive reference was not retained in normalized grammar')

    call make_mutual(mutual)
    call standardir_grammar_normalize(mutual, normalized, suppressed, ok, message)
    call require(ok .and. size(normalized) == 5 .and. size(suppressed) == 1, &
        'mutual normalization failed: '//trim(message))
    call require(no_left_corner(normalized), 'mutually recursive family remains left recursive')

    call make_unsupported(unsupported(1))
    call standardir_grammar_normalize(unsupported, normalized, suppressed, ok, message)
    call require(ok .and. no_left_corner(normalized), &
        'nullable left recursion was not transformed: '//trim(message))

    call make_reachability(reachability)
    selected_roots(1) = 'root'
    call standardir_grammar_normalize(reachability, normalized, suppressed, ok, message)
    call require(ok, 'reachability fixture did not normalize: '//trim(message))
    call standardir_grammar_select_reachable(normalized, selected_roots, retained, pruned, &
        reachability_witness, ok, message)
    call require(ok .and. size(retained) == 2 .and. size(pruned) == 2 .and. &
        size(reachability_witness) == 2, 'selected-root reachability did not prune two rules: '//trim(message))
    call require(trim(retained(1)%lhs) == 'root' .and. trim(retained(2)%lhs) == 'child' .and. &
        trim(pruned(1)%lhs) == 'orphan' .and. trim(pruned(1)%source%source_hash) == 'HASH-ORPHAN' .and. &
        trim(pruned(2)%lhs) == 'dead' .and. trim(pruned(2)%source%source_hash) == 'HASH-DEAD' .and. &
        trim(reachability_witness(1)%provenance(1)%source%rule) == 'REACH-ORPHAN' .and. &
        trim(reachability_witness(2)%provenance(1)%source%rule) == 'REACH-DEAD', &
        'selected-root pruning lost rule order or source lineage')
    call standardir_grammar_validate_reachability(retained, selected_roots, ok, message)
    call require(ok, 'retained selected grammar failed its reachability check: '//trim(message))
    retained(1)%expression%children(1)%kind = standardir_grammar_token
    retained(1)%expression%children(1)%name = 'MUTATED'
    call standardir_grammar_validate_reachability(retained, selected_roots, ok, message)
    call require(.not. ok, 'reachability mutation control was accepted')
    call verify_reachability_output(reachability)
    all_roots = [character(len=128) :: 'root', 'orphan']
    call verify_all_root_output(reachability, all_roots)

    call make_role_fixture(role_fixture)
    call standardir_grammar_normalize(role_fixture, role_normalized, role_pruned, ok, message)
    call require(ok, 'role-family fixture did not normalize: '//trim(message))
    call require(size(role_pruned) == 0, 'role-family fixture produced suppressed rules')
    role_roots(1) = 'start'
    call standardir_grammar_select_reachable(role_normalized, role_roots, role_retained, role_pruned, &
        role_reachability, ok, message)
    call require(ok, 'role-family fixture reachability failed: '//trim(message))
    call require(size(role_retained) == 7 .and. size(role_pruned) == 0, &
        'role-family fixture did not pass reachability')
    role_config%enabled = .true.
    role_config%representative = 'rep'
    call standardir_grammar_factor_role_family(role_retained, role_config, role_factored, role_witness, &
        ok, message, protected_lhs=role_roots)
    call require(ok, 'role-family factoring failed: '//trim(message))
    call require(size(role_factored) == 5 .and. size(role_witness) == 3, &
        'role-family fixture changed by an unexpected amount')
    call require(trim(role_factored(1)%lhs) == 'start' .and. &
        trim(role_factored(1)%expression%children(1)%name) == 'rep' .and. &
        trim(role_factored(1)%expression%children(2)%name) == 'rep' .and. &
        trim(role_factored(1)%expression%children(3)%name) == 'unsafe', &
        'role-family references were not replaced by the independent fixture expectation')
    expected_roles = [character(len=128) :: 'rep', 'alias_a', 'alias_b']
    call require(trim(role_factored(2)%lhs) == 'rep', 'representative output is not deterministic')
    call require(same_roles(role_factored(2)%source_roles, expected_roles), &
        'retained representative lost source roles')
    call require(size(role_factored(2)%provenance) == 3, 'retained representative lost lineage')
    call require(trim(role_factored(3)%lhs) == 'rep', 'representative alternatives are not retained')
    call require(trim(role_witness(1)%alias_role) == 'alias_a' .and. &
        trim(role_witness(2)%alias_role) == 'alias_b' .and. &
        trim(role_witness(3)%alias_role) == 'unsafe' .and. &
        role_witness(1)%disposition == standardir_target_role_family_factored .and. &
        role_witness(2)%disposition == standardir_target_role_family_factored .and. &
        role_witness(3)%disposition == standardir_target_role_family_rejected .and. &
        trim(role_witness(3)%reason) == 'multi-alternative-alias', &
        'role-family witness did not deterministically reject the multi-alternative alias')
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, role_witness, ok, message)
    call require(ok, 'complete role-family witness failed validation: '//trim(message))
    broken_witness = role_witness
    deallocate (broken_witness(1)%alias_provenance)
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'lost role mapping negative control was accepted')

    broken_witness = role_witness
    broken_witness(1)%alias_role = 'mutated'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated alias identity was accepted')
    broken_witness = role_witness
    broken_witness(1)%representative_role = 'start'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated representative identity was accepted')
    broken_witness = role_witness
    broken_witness(1)%disposition = standardir_target_role_family_rejected
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated witness disposition was accepted')
    broken_witness = role_witness
    broken_witness(1)%reason = 'mutated-reason'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated witness reason was accepted')
    broken_witness = role_witness
    broken_witness(1)%source_roles(1) = 'mutated-role'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated witness source roles were accepted')
    broken_witness = role_witness
    broken_witness(1)%representative_provenance(1)%source%rule = 'MUTATED-REP'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated representative lineage was accepted')
    broken_witness = role_witness
    broken_witness(1)%alias_provenance(1)%source%rule = 'MUTATED-ALIAS'
    call standardir_grammar_validate_role_family_witness(role_retained, role_factored, broken_witness, ok, message)
    call require(.not. ok, 'mutated alias lineage was accepted')

    role_reordered = role_fixture
    role_reordered(6) = role_fixture(7)
    role_reordered(7) = role_fixture(6)
    call standardir_grammar_normalize(role_reordered, role_normalized, role_pruned, ok, message)
    call require(ok, 'alternative-order fixture did not normalize: '//trim(message))
    call standardir_grammar_factor_role_family(role_normalized, role_config, role_factored, role_witness, ok, message)
    call require(ok, 'alternative-order factoring failed: '//trim(message))
    call require(trim(role_witness(3)%alias_role) == 'unsafe' .and. &
        trim(role_witness(3)%alias_provenance(1)%source%rule) == 'ROLE-UNSAFE-ALIAS', &
        'reordered alternatives selected the rejected alternative lineage')
    call standardir_grammar_validate_role_family_witness(role_normalized, role_factored, role_witness, ok, message)
    call require(ok, 'alternative-order witness failed validation: '//trim(message))

    call make_alias_chain(alias_chain)
    call standardir_grammar_normalize(alias_chain, role_normalized, role_pruned, ok, message)
    call require(ok, 'alias-chain fixture did not normalize: '//trim(message))
    call standardir_grammar_factor_role_family(role_normalized, role_config, role_factored, role_witness, ok, message)
    call require(ok .and. size(role_factored) == 2 .and. size(role_witness) == 2, &
        'alias chain was not factored with exact unit-alias lineage')
    call require(trim(role_factored(1)%expression%children(1)%name) == 'rep' .and. &
        trim(role_witness(1)%alias_provenance(1)%source%rule) == 'CHAIN-A' .and. &
        trim(role_witness(2)%alias_provenance(1)%source%rule) == 'CHAIN-B', &
        'alias-chain replacement or lineage is not source-backed')
    call standardir_grammar_validate_role_family_witness(role_normalized, role_factored, role_witness, ok, message)
    call require(ok, 'alias-chain witness failed validation: '//trim(message))

    call make_alias_chain(alias_chain)
    call standardir_grammar_normalize(alias_chain, role_normalized, role_pruned, ok, message)
    call require(ok, 'alias-cycle base fixture did not normalize: '//trim(message))
    role_normalized(2)%expression%name = 'alias_b'
    role_normalized(3)%expression%name = 'alias_a'
    call standardir_grammar_factor_role_family(role_normalized, role_config, role_factored, role_witness, ok, message)
    call require(ok .and. size(role_factored) == 4 .and. size(role_witness) == 2, &
        'alias cycle was not retained with two rejection witnesses')
    call standardir_grammar_validate_role_family_witness(role_normalized, role_factored, role_witness, ok, message)
    call require(ok, 'alias-cycle witness failed validation: '//trim(message))

    call verify_role_family_output(role_fixture, role_config)

    call make_no_alias_fixture(no_alias)
    call standardir_grammar_normalize(no_alias, role_normalized, role_pruned, ok, message)
    call require(ok, 'no-alias fixture did not normalize: '//trim(message))
    call standardir_grammar_factor_role_family(role_normalized, role_config, role_factored, role_witness, &
        ok, message)
    call require(ok .and. size(role_factored) == size(role_normalized) .and. size(role_witness) == 0, &
        'no-safe-family path was not unchanged')

    print '(a)', 'StandardIR grammar export tests passed'

contains

    subroutine make_rules(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        values = standardir_grammar_rule_t()
        call make_nested(values(1), 'R-A1', 1, 'expr', 'DOC-A', '5.1', 10, 'HASH-A1')
        call make_simple(values(2), 'R-A2', 2, 'expr', 'ELSE', 'DOC-A', '5.2', 11, 'HASH-A2')
        call make_simple(values(3), 'R-B1', 1, 'term', 'X', 'DOC-B', '6.1', 20, 'HASH-B1')
    end subroutine make_rules

    subroutine make_reachability(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_sequence_rule(values(1), 'REACH-ROOT', 1, 'root', 'child', 'X', &
            'DOC-REACH', '1', 1, 'HASH-ROOT')
        call make_simple(values(2), 'REACH-CHILD', 1, 'child', 'Y', 'DOC-REACH', '2', 2, &
            'HASH-CHILD')
        call make_simple(values(3), 'REACH-ORPHAN', 1, 'orphan', 'Z', 'DOC-REACH', '3', 3, &
            'HASH-ORPHAN')
        call make_sequence_rule(values(4), 'REACH-DEAD', 1, 'dead', 'missing', 'Q', 'DOC-REACH', '4', 4, &
            'HASH-DEAD')
    end subroutine make_reachability

    subroutine make_role_fixture(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_three_reference_rule(values(1), 'ROLE-START', 1, 'start', 'alias_a', 'alias_b', 'unsafe', &
            'DOC-ROLE', '1', 1, 'HASH-START')
        call make_unit_alias_rule(values(2), 'ROLE-A', 1, 'alias_a', 'rep', 'DOC-ROLE', '2', 2, 'HASH-A')
        call make_unit_alias_rule(values(3), 'ROLE-B', 1, 'alias_b', 'rep', 'DOC-ROLE', '3', 3, 'HASH-B')
        call make_simple(values(4), 'ROLE-REP-X', 1, 'rep', 'X', 'DOC-ROLE', '4', 4, 'HASH-REP-X')
        call make_simple(values(5), 'ROLE-REP-Y', 2, 'rep', 'Y', 'DOC-ROLE', '5', 5, 'HASH-REP-Y')
        call make_unit_alias_rule(values(6), 'ROLE-UNSAFE-ALIAS', 1, 'unsafe', 'rep', 'DOC-ROLE', '6', 6, &
            'HASH-UNSAFE-1')
        call make_simple(values(7), 'ROLE-UNSAFE-TOKEN', 2, 'unsafe', 'Z', 'DOC-ROLE', '7', 7, &
            'HASH-UNSAFE-2')
    end subroutine make_role_fixture

    subroutine make_no_alias_fixture(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'NO-ALIAS-ROOT', 1, 'root', 'X', 'DOC-NO-ALIAS', '1', 1, 'HASH-NO-ROOT')
        call make_simple(values(2), 'NO-ALIAS-REP', 1, 'rep', 'Y', 'DOC-NO-ALIAS', '2', 2, 'HASH-NO-REP')
    end subroutine make_no_alias_fixture

    subroutine make_alias_chain(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_sequence_rule(values(1), 'CHAIN-START', 1, 'start', 'alias_a', 'X', 'DOC-CHAIN', '1', 1, &
            'HASH-CHAIN-START')
        call make_unit_alias_rule(values(2), 'CHAIN-A', 1, 'alias_a', 'alias_b', 'DOC-CHAIN', '2', 2, &
            'HASH-CHAIN-A')
        call make_unit_alias_rule(values(3), 'CHAIN-B', 1, 'alias_b', 'rep', 'DOC-CHAIN', '3', 3, &
            'HASH-CHAIN-B')
        call make_simple(values(4), 'CHAIN-REP', 1, 'rep', 'X', 'DOC-CHAIN', '4', 4, 'HASH-CHAIN-REP')
    end subroutine make_alias_chain

    subroutine make_duplicate(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'DUP-1', 1, 'duplicate', 'X', 'DOC-D', '1', 1, 'HASH-D1')
        call make_simple(values(2), 'DUP-2', 2, 'duplicate', 'X', 'DOC-D', '2', 2, 'HASH-D2')
        values(2)%source = values(1)%source
    end subroutine make_duplicate

    subroutine make_same_occurrence(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'SAME-1', 1, 'same', 'X', 'DOC-SAME', '1', 1, 'HASH-SAME')
        values(2) = values(1)
        values(2)%id = 'SAME-2'
    end subroutine make_same_occurrence

    subroutine make_merged(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'MERGE-1', 1, 'merged', 'X', 'DOC-M1', '2.1', 2, 'HASH-M1')
        call make_simple(values(2), 'MERGE-2', 2, 'merged', 'X', 'DOC-M2', '2.2', 3, 'HASH-M2')
        values(1)%source%byte_start = 101
        values(1)%source%byte_length = 7
        values(2)%source%byte_start = 202
        values(2)%source%byte_length = 8
    end subroutine make_merged

    subroutine make_different(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'DIFF-1', 1, 'different', 'X', 'DOC-D1', '3.1', 4, 'HASH-D1')
        call make_simple(values(2), 'DIFF-2', 2, 'different', 'Y', 'DOC-D2', '3.2', 5, 'HASH-D2')
    end subroutine make_different

    subroutine make_invalid_provenance(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_simple(values(1), 'INVALID-1', 1, 'invalid', 'X', 'DOC-I1', '4.1', 6, 'HASH-I1')
        call make_simple(values(2), 'INVALID-2', 2, 'invalid', 'X', 'DOC-I2', '4.2', 7, 'HASH-I2')
        values(2)%source%source_hash = ''
    end subroutine make_invalid_provenance

    subroutine make_direct(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_sequence_rule(values(1), 'REC-1', 1, 'expr', 'expr', 'X', 'DOC-R', '1', 1, 'HASH-R1')
        call make_simple(values(2), 'REC-2', 2, 'expr', 'Y', 'DOC-R', '2', 2, 'HASH-R2')
    end subroutine make_direct

    subroutine make_multiple_direct(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_sequence_rule(values(1), 'MREC-1', 1, 'expr', 'expr', 'X', 'DOC-MR', '1', 1, &
            'HASH-MR1')
        call make_sequence_rule(values(2), 'MREC-2', 2, 'expr', 'expr', 'Z', 'DOC-MR', '2', 2, &
            'HASH-MR2')
        call make_simple(values(3), 'MREC-3', 3, 'expr', 'Y', 'DOC-MR', '3', 3, 'HASH-MR3')
    end subroutine make_multiple_direct

    subroutine make_mutual(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        call make_sequence_rule(values(1), 'M-A1', 1, 'a', 'b', 'X', 'DOC-M', '1', 1, 'HASH-M1')
        call make_simple(values(2), 'M-A2', 2, 'a', 'Y', 'DOC-M', '2', 2, 'HASH-M2')
        call make_sequence_rule(values(3), 'M-B1', 1, 'b', 'a', 'Z', 'DOC-M', '3', 3, 'HASH-M3')
        call make_simple(values(4), 'M-B2', 2, 'b', 'W', 'DOC-M', '4', 4, 'HASH-M4')
    end subroutine make_mutual

    subroutine make_wrapped(value)
        type(standardir_grammar_rule_t), intent(out) :: value

        value = standardir_grammar_rule_t()
        value%id = 'WRAP-1'
        value%alternative = 1
        value%lhs = 'wrapped'
        value%root = 1
        allocate (value%nodes%values(4))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 1)
        call set_node(value%nodes%values(2), standardir_grammar_optional, '-', 0, .false., 3, 1)
        call set_node(value%nodes%values(3), standardir_grammar_optional, '-', 0, .false., 4, 1)
        call set_node(value%nodes%values(4), standardir_grammar_token, 'X', 1, .false., 0, 0)
        call set_source(value, 'DOC-W', '1', 'WRAP-1', 1, 'HASH-W1')
    end subroutine make_wrapped

    subroutine make_unsupported(value)
        type(standardir_grammar_rule_t), intent(out) :: value

        value = standardir_grammar_rule_t()
        value%id = 'BAD-LEFT'
        value%alternative = 1
        value%lhs = 'bad'
        value%root = 1
        allocate (value%nodes%values(4))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 2)
        call set_node(value%nodes%values(2), standardir_grammar_optional, '-', 0, .false., 3, 1)
        call set_node(value%nodes%values(3), standardir_grammar_reference, 'bad', 1, .false., 0, 0)
        call set_node(value%nodes%values(4), standardir_grammar_token, 'X', 1, .false., 0, 0)
        call set_source(value, 'DOC-U', '1', 'BAD-LEFT', 1, 'HASH-U1')
    end subroutine make_unsupported

    subroutine make_sequence_rule(value, id, alternative, lhs, reference, token, document, clause, &
            page, hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, reference, token, document, clause, hash
        integer, intent(in) :: alternative, page

        value = standardir_grammar_rule_t()
        value%id = id
        value%alternative = alternative
        value%lhs = lhs
        value%root = 1
        allocate (value%nodes%values(3))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 2)
        call set_node(value%nodes%values(2), standardir_grammar_reference, reference, 1, .false., 0, 0)
        call set_node(value%nodes%values(3), standardir_grammar_token, token, 1, .false., 0, 0)
        call set_source(value, document, clause, id, page, hash)
    end subroutine make_sequence_rule

    subroutine make_nested(value, id, alternative, lhs, document, clause, page, hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, document, clause, hash
        integer, intent(in) :: alternative, page

        value = standardir_grammar_rule_t()
        value%id = id
        value%alternative = alternative
        value%lhs = lhs
        value%root = 1
        allocate (value%nodes%values(7))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 4)
        call set_node(value%nodes%values(2), standardir_grammar_reference, 'term', 1, .false., 0, 0)
        call set_node(value%nodes%values(3), standardir_grammar_token, 'IF', 1, .false., 0, 0)
        call set_node(value%nodes%values(4), 5, '-', 0, .false., 5, 1)
        call set_node(value%nodes%values(5), standardir_grammar_token, 'THEN', 1, .false., 0, 0)
        call set_node(value%nodes%values(6), 6, '-', 1, .true., 7, 1)
        call set_node(value%nodes%values(7), standardir_grammar_reference, 'item', 1, .false., 0, 0)
        call set_source(value, document, clause, 'SRC-A1', page, hash)
    end subroutine make_nested

    subroutine make_simple(value, id, alternative, lhs, token, document, clause, page, hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, token, document, clause, hash
        integer, intent(in) :: alternative, page

        value = standardir_grammar_rule_t()
        value%id = id
        value%alternative = alternative
        value%lhs = lhs
        value%root = 1
        allocate (value%nodes%values(2))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 1)
        call set_node(value%nodes%values(2), standardir_grammar_token, token, 1, .false., 0, 0)
        call set_source(value, document, clause, id, page, hash)
    end subroutine make_simple

    subroutine make_unit_alias_rule(value, id, alternative, lhs, reference, document, clause, page, hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, reference, document, clause, hash
        integer, intent(in) :: alternative, page

        value = standardir_grammar_rule_t()
        value%id = id
        value%alternative = alternative
        value%lhs = lhs
        value%root = 1
        allocate (value%nodes%values(1))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_reference, reference, 1, .false., 0, 0)
        call set_source(value, document, clause, id, page, hash)
    end subroutine make_unit_alias_rule

    subroutine make_three_reference_rule(value, id, alternative, lhs, first, second, third, document, clause, &
            page, hash)
        type(standardir_grammar_rule_t), intent(out) :: value
        character(len=*), intent(in) :: id, lhs, first, second, third, document, clause, hash
        integer, intent(in) :: alternative, page

        value = standardir_grammar_rule_t()
        value%id = id
        value%alternative = alternative
        value%lhs = lhs
        value%root = 1
        allocate (value%nodes%values(4))
        value%nodes%values = standardir_grammar_node_t()
        call set_node(value%nodes%values(1), standardir_grammar_sequence, '-', 1, .false., 2, 3)
        call set_node(value%nodes%values(2), standardir_grammar_reference, first, 1, .false., 0, 0)
        call set_node(value%nodes%values(3), standardir_grammar_reference, second, 1, .false., 0, 0)
        call set_node(value%nodes%values(4), standardir_grammar_reference, third, 1, .false., 0, 0)
        call set_source(value, document, clause, id, page, hash)
    end subroutine make_three_reference_rule

    subroutine set_node(node, kind, name, minimum, unbounded, first_child, child_count)
        type(standardir_grammar_node_t), intent(out) :: node
        character(len=*), intent(in) :: name
        integer, intent(in) :: kind, minimum, first_child, child_count
        logical, intent(in) :: unbounded

        node = standardir_grammar_node_t()
        node%kind = kind
        node%name = name
        node%minimum = minimum
        node%unbounded = unbounded
        node%first_child = first_child
        node%child_count = child_count
    end subroutine set_node

    subroutine set_source(value, document, clause, rule, page, hash)
        type(standardir_grammar_rule_t), intent(inout) :: value
        character(len=*), intent(in) :: document, clause, rule, hash
        integer, intent(in) :: page

        value%source%document = document
        value%source%clause = clause
        value%source%rule = rule
        value%source%page = page
        value%source%source_hash = hash
        value%origin = standardir_grammar_origin_human
        value%resolution = standardir_grammar_resolution_resolved
    end subroutine set_source

    logical function direct_witnesses_preserved(values)
        type(standardir_target_rule_t), intent(in) :: values(:)

        direct_witnesses_preserved = .true.
        if (size(values) /= 2) then
            direct_witnesses_preserved = .false.
            return
        end if
        if (values(1)%expression%kind /= standardir_grammar_sequence) then
            direct_witnesses_preserved = .false.
            return
        end if
        if (.not. allocated(values(1)%expression%children)) then
            direct_witnesses_preserved = .false.
            return
        end if
        if (size(values(1)%expression%children) /= 2) then
            direct_witnesses_preserved = .false.
            return
        end if
        if (values(1)%expression%children(2)%kind /= standardir_grammar_repeat .or. &
            values(2)%expression%kind /= standardir_grammar_token .or. &
            trim(values(2)%expression%name) /= 'X') then
            direct_witnesses_preserved = .false.
            return
        end if
    end function direct_witnesses_preserved

    logical function no_left_corner(values)
        type(standardir_target_rule_t), intent(in) :: values(:)
        integer :: i

        no_left_corner = .true.
        do i = 1, size(values)
            if (values(i)%expression%kind == standardir_grammar_reference) then
                if (trim(values(i)%expression%name) == trim(values(i)%lhs)) no_left_corner = .false.
            else if (values(i)%expression%kind == standardir_grammar_sequence) then
                if (size(values(i)%expression%children) > 0) then
                    if (values(i)%expression%children(1)%kind == standardir_grammar_reference .and. &
                        trim(values(i)%expression%children(1)%name) == trim(values(i)%lhs)) &
                        no_left_corner = .false.
                end if
            end if
        end do
    end function no_left_corner

    logical function same_roles(actual, expected)
        character(len=128), allocatable, intent(in) :: actual(:)
        character(len=128), intent(in) :: expected(:)
        integer :: i

        same_roles = .false.
        if (.not. allocated(actual)) return
        if (size(actual) /= size(expected)) return
        do i = 1, size(expected)
            if (trim(actual(i)) /= trim(expected(i))) return
        end do
        same_roles = .true.
    end function same_roles

    subroutine verify_transform_output(values, format, marker)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        integer, intent(in) :: format
        character(len=*), intent(in) :: marker
        character(len=65536) :: text
        character(len=256) :: local_message
        integer :: unit, ios
        logical :: local_ok

        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open transform scratch output')
        call standardir_grammar_export_batch(unit, values, format, local_ok, local_message)
        call require(local_ok, trim(local_message))
        call read_text(unit, text)
        call require(index(text, trim(marker)) > 0, 'left-recursion helper is absent from target output')
        call require(index(text, 'source-lineage=REC-1:') > 0 .and. &
            index(text, 'source-lineage=REC-2:') > 0, &
            'left-recursion source mapping is absent from target output')
        close (unit)
    end subroutine verify_transform_output

    subroutine verify_merged_lineage(values, format)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        integer, intent(in) :: format
        character(len=65536) :: text
        character(len=256) :: local_message
        integer :: unit, ios
        logical :: local_ok

        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open merged-lineage scratch output')
        call standardir_grammar_export_batch(unit, values, format, local_ok, local_message)
        call require(local_ok, trim(local_message))
        call read_text(unit, text)
        call require(index(text, 'source-lineage=MERGE-1:1@101+7,MERGE-2:2@202+8') > 0, &
            'merged lineage is absent from downstream format output')
        close (unit)
    end subroutine verify_merged_lineage

    subroutine verify_reachability_output(values)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        character(len=65536) :: text
        character(len=256) :: local_message
        integer :: unit, ios
        logical :: local_ok
        type(standardir_target_reachability_witness_t), allocatable :: witness(:)

        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open reachability scratch output')
        call standardir_grammar_export_batch(unit, values, standardir_grammar_format_ebnf, local_ok, &
            local_message, selected_root='root', reachability_witness=witness)
        call require(local_ok, trim(local_message))
        call require(size(witness) == 2, 'reachability witness did not report both pruned rules')
        call read_text(unit, text)
        call require(index(text, 'root ::= ') > 0 .and. index(text, 'child ::= ') > 0 .and. &
            index(text, 'orphan ::= ') == 0 .and. index(text, 'dead ::= ') == 0, &
            'selected export retained an unreachable target rule')
        call require(index(text, 'target-disposition=omitted-unreachable root=root lhs=orphan') > 0 .and. &
            index(text, 'target-disposition=omitted-unreachable root=root lhs=dead') > 0 .and. &
            index(text, 'source-lineage=REACH-ORPHAN:1@') > 0 .and. &
            index(text, 'source-lineage=REACH-DEAD:1@') > 0, &
            'selected export omitted-rule witness is not source-backed')
        close (unit)
    end subroutine verify_reachability_output

    subroutine verify_role_family_output(values, config)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        type(standardir_target_role_family_config_t), intent(in) :: config
        character(len=65536) :: text
        character(len=256) :: local_message
        integer :: format, unit, ios
        logical :: local_ok
        type(standardir_target_role_family_witness_t), allocatable :: witness(:)

        do format = standardir_grammar_format_ebnf, standardir_grammar_format_tree_sitter
            open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
            call require(ios == 0, 'could not open role-family scratch output')
            call standardir_grammar_export_batch(unit, values, format, local_ok, local_message, &
                selected_root='start', role_family=config, role_family_witness=witness)
            call require(local_ok, trim(local_message))
            call require(size(witness) == 3, 'role-family export witness count is incorrect')
            call read_text(unit, text)
            select case (format)
            case (standardir_grammar_format_ebnf)
                call require(index(text, 'start ::= ') > 0 .and. index(text, 'rep ::= ') > 0 .and. &
                    index(text, 'alias_a ::= ') == 0 .and. index(text, 'alias_b ::= ') == 0, &
                    'role-family EBNF export did not remove only the safe aliases')
            case (standardir_grammar_format_antlr4)
                call require(index(text, 'r_start') > 0 .and. index(text, 'r_rep') > 0 .and. &
                    index(text, 'r_alias_a') == 0 .and. index(text, 'r_alias_b') == 0, &
                    'role-family ANTLR4 export did not remove only the safe aliases')
            case (standardir_grammar_format_bison)
                call require(index(text, 'r_start:') > 0 .and. index(text, 'r_rep:') > 0 .and. &
                    index(text, 'r_alias_a:') == 0 .and. index(text, 'r_alias_b:') == 0, &
                    'role-family Bison export did not remove only the safe aliases')
            case (standardir_grammar_format_tree_sitter)
                call require(index(text, 'r_start: $ =>') > 0 .and. index(text, 'r_rep: $ =>') > 0 .and. &
                    index(text, 'r_alias_a: $ =>') == 0 .and. index(text, 'r_alias_b: $ =>') == 0, &
                    'role-family tree-sitter export did not remove only the safe aliases')
            end select
            call require(index(text, 'target-role-family alias=alias_a representative=rep disposition=factored') > 0 .and. &
                index(text, 'target-role-family alias=unsafe representative=rep disposition=rejected') > 0 .and. &
                index(text, 'source-roles=rep,alias_a,alias_b') > 0 .and. &
                index(text, 'alias-lineage=ROLE-A:1@') > 0, &
                'role-family export did not emit the machine-readable mapping witness')
            close (unit)
        end do
    end subroutine verify_role_family_output

    subroutine verify_all_root_output(values, roots)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: roots(:)
        character(len=65536) :: text
        character(len=256) :: local_message
        integer :: unit, ios
        logical :: local_ok
        type(standardir_target_reachability_witness_t), allocatable :: witness(:)

        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open all-root scratch output')
        call standardir_grammar_export_batch(unit, values, standardir_grammar_format_ebnf, local_ok, &
            local_message, roots=roots, reachability_witness=witness)
        call require(local_ok, trim(local_message))
        call require(size(witness) == 1, 'all-root export witness count is incorrect')
        call read_text(unit, text)
        call require(index(text, 'root ::= ') > 0 .and. index(text, 'child ::= ') > 0 .and. &
            index(text, 'orphan ::= ') > 0 .and. index(text, 'dead ::= ') == 0, &
            'all-root export did not retain every declared root')
        close (unit)
    end subroutine verify_all_root_output

    subroutine read_text(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: text
        character(len=1024) :: line
        integer :: ios, length

        text = ''
        length = 0
        rewind (unit)
        do
            read (unit, '(a)', iostat=ios) line
            if (ios < 0) exit
            call require(ios == 0 .and. length + len_trim(line) + 1 < len(text), &
                'transform output buffer is full')
            text(length + 1:length + len_trim(line)) = trim(line)
            length = length + len_trim(line) + 1
            text(length:length) = new_line('a')
        end do
    end subroutine read_text

    subroutine verify_format(unit, format)
        integer, intent(in) :: unit, format

        character(len=65536) :: text
        integer :: ios, length

        text = ''
        length = 0
        rewind (unit)
        do
            read (unit, '(a)', iostat=ios) text(length + 1:)
            if (ios < 0) exit
            call require(ios == 0, 'could not read emitted format')
            length = length + len_trim(text(length + 1:)) + 1
            if (length >= len(text) - 256) call fail('format oracle buffer is full')
            text(length:length) = new_line('a')
        end do

        call require(index(text, 'rule=R-A1') > 0, 'first rule provenance is missing')
        call require(index(text, 'source-lineage=SRC-A1:') > 0, 'source rule lineage is missing')
        call require(index(text, 'document=DOC-A') > 0 .and. index(text, 'clause=5.1') > 0, &
            'first source provenance is missing')
        call require(index(text, 'source-canonical-text-sha256=HASH-A1') > 0, &
            'source hash is missing')
        call require(index(text, 'rule=R-A1 document') < index(text, 'rule=R-A2 document') .and. &
            index(text, 'rule=R-A2 document') < index(text, 'rule=R-B1 document'), &
            'rule or alternative order was changed')
        select case (format)
        case (standardir_grammar_format_ebnf)
            call require(index(text, 'expr ::= ') > 0 .and. index(text, '[ "THEN" ]') > 0 .and. &
                index(text, 'item { item }') > 0, 'EBNF structure differs')
        case (standardir_grammar_format_antlr4)
            call require(index(text, 'r_expr') > 0 .and. index(text, "'THEN' )?") > 0 .and. &
                index(text, '( r_item )+') > 0, 'ANTLR4 structure differs')
        case (standardir_grammar_format_bison)
            call require(index(text, 'r_expr:') > 0 .and. index(text, 'h_r_R_x2D_A1_') > 0, &
                'Bison structure differs')
        case (standardir_grammar_format_tree_sitter)
            call require(index(text, 'r_expr: $ =>') > 0 .and. index(text, 'optional(') > 0 .and. &
                index(text, 'repeat1(') > 0, 'tree-sitter structure differs')
        end select
    end subroutine verify_format

    subroutine verify_failure(values, format, description)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        integer, intent(in) :: format
        character(len=*), intent(in) :: description

        integer :: ios, unit
        logical :: ok
        character(len=256) :: message, line

        open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open failure scratch output')
        write (unit, '(a)') 'sentinel'
        call standardir_grammar_export_batch(unit, values, format, ok, message)
        call require(.not. ok .and. len_trim(message) > 0, trim(description)//' was accepted')
        rewind (unit)
        read (unit, '(a)', iostat=ios) line
        call require(ios == 0 .and. trim(line) == 'sentinel', &
            trim(description)//' changed output on failure')
        close (unit)
    end subroutine verify_failure

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) call fail(message)
    end subroutine require

    subroutine fail(message)
        character(len=*), intent(in) :: message

        print '(a)', 'FAIL: '//trim(message)
        stop 1
    end subroutine fail

end program test_standardir_grammar_export

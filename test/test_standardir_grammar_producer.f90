program test_standardir_grammar_producer
    !! Fixed SX and mutation controls are the independent contract witness.

    use fortsx, only: sx_node_t, sx_parse
    use, intrinsic :: iso_fortran_env, only: int64
    use standardir, only: standardir_syntax_t, standardir_add, standardir_start
    use standardir_grammar_producer
    use standardir_export, only: standardir_source_ref_t
    implicit none

    character(len=*), parameter :: expected = &
        '(syntax-rule (id R501) (alternative 1) (lhs program) (root 1) '// &
        '(nodes (grammar-nodes (grammar-node sequence - 1 false 2 2) '// &
        '(grammar-node reference program-unit 1 false 0 0) '// &
        '(grammar-node token IF 1 false 0 0) '// &
        '(grammar-node choice - 1 false 5 1) '// &
        '(grammar-node reference name 1 false 0 0) '// &
        '(grammar-node optional - 0 false 7 1) '// &
        '(grammar-node token THEN 1 false 0 0) '// &
        '(grammar-node repeat statement 1 true 9 1) '// &
        '(grammar-node reference body 1 false 0 0))) '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R501) '// &
        '(page 45) (source-hash fixture))) (origin mechanical) '// &
        '(resolution unresolved))'
    type(standardir_grammar_rule_t) :: value, round_trip, malformed
    type(sx_node_t) :: node
    character(len=2048) :: actual
    character(len=256) :: message
    integer :: ios, unit
    logical :: ok
    type(standardir_syntax_t) :: production
    type(standardir_grammar_rule_t), allocatable :: produced(:)
    type(standardir_syntax_t) :: batch_productions(2), bad_batch(2)
    type(standardir_source_ref_t) :: batch_sources(2), bad_sources(2)
    integer :: batch_origins(2), batch_resolutions(2)
    type(standardir_grammar_rule_t), allocatable :: batch_values(:)

    call make_rule(value)
    call standardir_grammar_validate(value, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open SX output')
    call standardir_grammar_write(value, unit, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read SX output')
    call require(trim(actual) == expected, 'canonical grammar SX differs')

    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_grammar_read(node, round_trip, ok, message)
    call require(ok, message)
    call require(trim(round_trip%id) == 'R501' .and. round_trip%alternative == 1 .and. &
        trim(round_trip%lhs) == 'program', 'rule identity was not preserved')
    call require(size(round_trip%nodes%values) == 9 .and. &
        round_trip%nodes%values(1)%kind == standardir_grammar_sequence .and. &
        round_trip%nodes%values(2)%kind == standardir_grammar_reference .and. &
        round_trip%nodes%values(3)%kind == standardir_grammar_token .and. &
        round_trip%nodes%values(4)%kind == standardir_grammar_choice .and. &
        round_trip%nodes%values(6)%kind == standardir_grammar_optional .and. &
        round_trip%nodes%values(8)%kind == standardir_grammar_repeat, &
        'node kinds or alternative order were not preserved')
    call require(trim(round_trip%source%document) == 'J3-24-007' .and. &
        round_trip%source%page == 45 .and. &
        round_trip%resolution == standardir_grammar_resolution_unresolved, &
        'provenance or unresolved state was not preserved')

    malformed = value
    malformed%nodes%values(1)%first_child = 9
    call standardir_grammar_validate(malformed, ok, message)
    call require(.not. ok, 'malformed child range was accepted')

    malformed = value
    malformed%nodes%values(2)%name = ''
    call standardir_grammar_validate(malformed, ok, message)
    call require(.not. ok, 'empty node name was accepted')

    call standardir_start(production, 'R700', 'fixture', 23, 100_int64, 20_int64, ok, message)
    call require(ok, message)
    call standardir_add(production, 'sequence', 'A', 23, 100_int64, 1_int64, ok, message)
    call require(ok, message)
    call standardir_grammar_produce(production, 'J3-24-007', '5', 'R700', 23, 'hash', &
        standardir_grammar_origin_human, standardir_grammar_resolution_resolved, produced, &
        ok, message)
    call require(ok .and. size(produced) == 1, 'simple sequence was not produced')
    call require(produced(1)%nodes%values(1)%kind == standardir_grammar_sequence .and. &
        produced(1)%nodes%values(2)%kind == standardir_grammar_token, &
        'simple sequence structure differs')
    call require(trim(produced(1)%source%document) == 'J3-24-007' .and. &
        trim(produced(1)%source%rule) == 'R700' .and. produced(1)%origin == &
        standardir_grammar_origin_human .and. produced(1)%resolution == &
        standardir_grammar_resolution_resolved, 'caller provenance was not preserved')
    deallocate (produced)

    call standardir_start(production, 'R701', 'nested', 24, 100_int64, 20_int64, ok, message)
    call require(ok, message)
    call standardir_add(production, 'sequence', 'A [ B [ C ] ] . . .', 24, 100_int64, &
        19_int64, &
        ok, message)
    call require(ok, message)
    call standardir_grammar_produce(production, 'doc', 'clause', 'source-rule', 24, 'hash', &
        standardir_grammar_origin_mechanical, standardir_grammar_resolution_unresolved, &
        produced, ok, message)
    call require(ok .and. size(produced) == 1, 'nested production was not produced')
    call require(produced(1)%nodes%values(3)%kind == standardir_grammar_repeat .and. &
        produced(1)%nodes%values(4)%kind == standardir_grammar_sequence .and. &
        produced(1)%nodes%values(6)%kind == standardir_grammar_optional, &
        'optional/repeat nesting was not preserved')
    deallocate (produced)

    call standardir_start(production, 'R702', 'choice', 25, 100_int64, 20_int64, ok, message)
    call require(ok, message)
    call standardir_add(production, 'sequence', 'A', 25, 100_int64, 1_int64, ok, message)
    call require(ok, message)
    call standardir_add(production, 'or', 'B', 25, 102_int64, 1_int64, ok, message)
    call require(ok, message)
    call standardir_grammar_produce(production, 'doc', 'clause', 'R702', 25, 'hash', &
        standardir_grammar_origin_search, standardir_grammar_resolution_disputed, produced, &
        ok, message)
    call require(ok .and. size(produced) == 2, 'alternative order was not preserved')
    call require(trim(produced(1)%nodes%values(2)%name) == 'A' .and. &
        trim(produced(2)%nodes%values(2)%name) == 'B', 'alternative values were reordered')
    deallocate (produced)

    call standardir_start(batch_productions(1), 'R810', 'first', 30, 100_int64, 20_int64, &
        ok, message)
    call require(ok, message)
    call standardir_add(batch_productions(1), 'sequence', 'A', 30, 100_int64, 1_int64, &
        ok, message)
    call require(ok, message)
    call standardir_add(batch_productions(1), 'or', 'B', 30, 102_int64, 1_int64, ok, message)
    call require(ok, message)
    call standardir_start(batch_productions(2), 'R811', 'second', 31, 200_int64, 20_int64, &
        ok, message)
    call require(ok, message)
    call standardir_add(batch_productions(2), 'sequence', 'X [ Y [ Z ] ] ...', 31, &
        200_int64, 19_int64, ok, message)
    call require(ok, message)
    call make_source(batch_sources(1), 'doc-one', 'clause-one', 'source-one', 30, 'hash-one')
    call make_source(batch_sources(2), 'doc-two', 'clause-two', 'source-two', 31, 'hash-two')
    batch_origins = [standardir_grammar_origin_human, standardir_grammar_origin_search]
    batch_resolutions = [standardir_grammar_resolution_resolved, &
        standardir_grammar_resolution_disputed]
    call standardir_grammar_produce_batch(batch_productions, batch_sources, batch_origins, &
        batch_resolutions, 3, batch_values, ok, message)
    call require(ok, message)
    call require(allocated(batch_values), 'successful batch did not allocate output')
    call require(size(batch_values) == 3, 'batch output count differs')
    call require(trim(batch_values(1)%id) == 'R810' .and. batch_values(1)%alternative == 1, &
        'first record order was not preserved')
    call require(trim(batch_values(2)%id) == 'R810' .and. batch_values(2)%alternative == 2, &
        'first record alternatives were not preserved')
    call require(trim(batch_values(3)%id) == 'R811' .and. batch_values(3)%alternative == 1, &
        'second record order was not preserved')
    call require(trim(batch_values(1)%nodes%values(2)%name) == 'A' .and. &
        trim(batch_values(2)%nodes%values(2)%name) == 'B', &
        'batch alternative values were reordered')
    call require(batch_values(3)%nodes%values(3)%kind == standardir_grammar_repeat .and. &
        batch_values(3)%nodes%values(4)%kind == standardir_grammar_sequence .and. &
        batch_values(3)%nodes%values(6)%kind == standardir_grammar_optional, &
        'batch nested structure was not preserved')
    call require(trim(batch_values(1)%source%document) == 'doc-one' .and. &
        trim(batch_values(2)%source%clause) == 'clause-one' .and. &
        trim(batch_values(3)%source%rule) == 'source-two' .and. &
        batch_values(3)%source%page == 31 .and. &
        trim(batch_values(3)%source%source_hash) == 'hash-two', &
        'batch source provenance was not preserved')
    call require(batch_values(1)%origin == standardir_grammar_origin_human .and. &
        batch_values(2)%resolution == standardir_grammar_resolution_resolved .and. &
        batch_values(3)%origin == standardir_grammar_origin_search .and. &
        batch_values(3)%resolution == standardir_grammar_resolution_disputed, &
        'batch origin or resolution was not preserved')
    deallocate (batch_values)

    call standardir_grammar_produce(batch_productions(1), batch_sources(1)%document, &
        batch_sources(1)%clause, batch_sources(1)%rule, batch_sources(1)%page, &
        batch_sources(1)%source_hash, batch_origins(1), batch_resolutions(1), produced, ok, &
        message)
    call require(ok, message)
    bad_batch = batch_productions
    bad_batch(2)%alternatives(1)%items(1)%kind = 99
    call standardir_grammar_produce_batch(bad_batch, batch_sources, batch_origins, &
        batch_resolutions, 3, produced, ok, message)
    call require(.not. ok, 'malformed batch was accepted')
    call require(.not. allocated(produced), 'malformed batch retained output')

    call standardir_grammar_produce(batch_productions(1), batch_sources(1)%document, &
        batch_sources(1)%clause, batch_sources(1)%rule, batch_sources(1)%page, &
        batch_sources(1)%source_hash, batch_origins(1), batch_resolutions(1), produced, ok, &
        message)
    call require(ok, message)
    bad_batch = batch_productions
    bad_batch(2)%incomplete = .true.
    call standardir_grammar_produce_batch(bad_batch, batch_sources, batch_origins, &
        batch_resolutions, 3, produced, ok, message)
    call require(.not. ok, 'incomplete batch was accepted')
    call require(.not. allocated(produced), 'incomplete batch retained output')

    call standardir_grammar_produce(batch_productions(1), batch_sources(1)%document, &
        batch_sources(1)%clause, batch_sources(1)%rule, batch_sources(1)%page, &
        batch_sources(1)%source_hash, batch_origins(1), batch_resolutions(1), produced, ok, &
        message)
    call require(ok, message)
    bad_sources = batch_sources
    bad_sources(2)%source_hash = ''
    call standardir_grammar_produce_batch(batch_productions, bad_sources, batch_origins, &
        batch_resolutions, 3, produced, ok, message)
    call require(.not. ok, 'invalid-provenance batch was accepted')
    call require(.not. allocated(produced), 'invalid-provenance batch retained output')

    call standardir_grammar_produce(batch_productions(1), batch_sources(1)%document, &
        batch_sources(1)%clause, batch_sources(1)%rule, batch_sources(1)%page, &
        batch_sources(1)%source_hash, batch_origins(1), batch_resolutions(1), produced, ok, &
        message)
    call require(ok, message)
    call standardir_grammar_produce_batch(batch_productions, batch_sources, batch_origins, &
        batch_resolutions, 2, produced, ok, message)
    call require(.not. ok, 'capacity-limited batch was accepted')
    call require(.not. allocated(produced), 'capacity failure retained output')

    call standardir_start(production, 'R703', 'broken', 26, 100_int64, 20_int64, ok, message)
    call require(ok, message)
    call standardir_add(production, 'sequence', '[ unfinished', 26, 100_int64, 12_int64, &
        ok, message)
    call require(ok, message)
    call standardir_grammar_produce(production, 'doc', 'clause', 'R703', 26, 'hash', &
        standardir_grammar_origin_mechanical, standardir_grammar_resolution_resolved, produced, &
        ok, message)
    call require(.not. ok .and. .not. allocated(produced), &
        'incomplete production did not clear output')

    call standardir_start(production, 'R704', 'malformed', 27, 100_int64, 20_int64, ok, message)
    call require(ok, message)
    production%alternatives(1)%item_count = 1
    production%alternatives(1)%items(1)%kind = 99
    call standardir_grammar_produce(production, 'doc', 'clause', 'R704', 27, 'hash', &
        standardir_grammar_origin_mechanical, standardir_grammar_resolution_resolved, produced, &
        ok, message)
    call require(.not. ok .and. .not. allocated(produced), &
        'malformed production did not clear output')

    print '(a)', 'StandardIR grammar producer test passed'

contains

    subroutine make_source(value, document, clause, rule, page, source_hash)
        type(standardir_source_ref_t), intent(out) :: value
        character(len=*), intent(in) :: document, clause, rule, source_hash
        integer, intent(in) :: page
        value = standardir_source_ref_t(document, clause, rule, page, source_hash)
    end subroutine make_source

    subroutine make_rule(value)
        type(standardir_grammar_rule_t), intent(out) :: value
        value%id = 'R501'
        value%alternative = 1
        value%lhs = 'program'
        value%root = 1
        allocate (value%nodes%values(9))
        value%nodes%values(1) = standardir_grammar_node_t( &
            standardir_grammar_sequence, '-', 1, .false., 2, 2)
        value%nodes%values(2) = standardir_grammar_node_t( &
            standardir_grammar_reference, 'program-unit', 1, .false., 0, 0)
        value%nodes%values(3) = standardir_grammar_node_t( &
            standardir_grammar_token, 'IF', 1, .false., 0, 0)
        value%nodes%values(4) = standardir_grammar_node_t( &
            standardir_grammar_choice, '-', 1, .false., 5, 1)
        value%nodes%values(5) = standardir_grammar_node_t( &
            standardir_grammar_reference, 'name', 1, .false., 0, 0)
        value%nodes%values(6) = standardir_grammar_node_t( &
            standardir_grammar_optional, '-', 0, .false., 7, 1)
        value%nodes%values(7) = standardir_grammar_node_t( &
            standardir_grammar_token, 'THEN', 1, .false., 0, 0)
        value%nodes%values(8) = standardir_grammar_node_t( &
            standardir_grammar_repeat, 'statement', 1, .true., 9, 1)
        value%nodes%values(9) = standardir_grammar_node_t( &
            standardir_grammar_reference, 'body', 1, .false., 0, 0)
        value%source = standardir_source_ref_t('J3-24-007', '5', 'R501', 45, 'fixture')
        value%origin = standardir_grammar_origin_mechanical
        value%resolution = standardir_grammar_resolution_unresolved
    end subroutine make_rule

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message
        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_grammar_producer

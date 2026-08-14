program test_standardir_grammar_export
    !! Independent format text is the oracle for the normalized batch boundary.

    use standardir_grammar_export
    use standardir_grammar_producer, only: standardir_grammar_node_t, &
        standardir_grammar_origin_human, standardir_grammar_reference, &
        standardir_grammar_resolution_resolved, standardir_grammar_resolution_unresolved, &
        standardir_grammar_rule_t, standardir_grammar_sequence, standardir_grammar_token
    implicit none

    type(standardir_grammar_rule_t) :: rules(3), bad(3), cyclic(3), unresolved(3), interleaved(3)
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

    interleaved = [rules(1), rules(3), rules(2)]
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

    print '(a)', 'StandardIR grammar export tests passed'

contains

    subroutine make_rules(values)
        type(standardir_grammar_rule_t), intent(out) :: values(:)

        values = standardir_grammar_rule_t()
        call make_nested(values(1), 'R-A1', 1, 'expr', 'DOC-A', '5.1', 10, 'HASH-A1')
        call make_simple(values(2), 'R-A2', 2, 'expr', 'ELSE', 'DOC-A', '5.2', 11, 'HASH-A2')
        call make_simple(values(3), 'R-B1', 1, 'term', 'X', 'DOC-B', '6.1', 20, 'HASH-B1')
    end subroutine make_rules

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
        call require(index(text, 'source-rule=SRC-A1') > 0, 'source rule annotation is missing')
        call require(index(text, 'document=DOC-A') > 0 .and. index(text, 'clause=5.1') > 0, &
            'first source provenance is missing')
        call require(index(text, 'source-sha256=HASH-A1') > 0, 'source hash is missing')
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
            call require(index(text, 'r_expr:') > 0 .and. index(text, 'h_R-A1_') > 0, &
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

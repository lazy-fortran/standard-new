program test_standardir_grammar_fact
    !! Fixed SX is the independent oracle for the concrete frontend fact.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_integer_type_spec_fact, &
        standardir_generate_real_type_spec_fact, standardir_generate_double_precision_type_spec_fact, &
        standardir_generate_complex_type_spec_fact, standardir_generate_logical_type_spec_fact, &
        standardir_generate_character_type_spec_fact, &
        standardir_generate_program_grammar_fact, standardir_generate_assignment_stmt_grammar_fact, &
        standardir_generate_intrinsic_type_spec_lookup
    use standardir_program_grammar_fact, only: standardir_consume_program_grammar_fact, &
        standardir_write_program_grammar_fact
    use standardir_assignment_stmt_grammar_fact, only: &
        standardir_consume_assignment_stmt_grammar_fact, standardir_write_assignment_stmt_grammar_fact
    use standardir_grammar_fact, only: standardir_consume_integer_type_spec_fact, &
        standardir_write_integer_type_spec_fact
    use standardir_real_type_spec_fact, only: standardir_consume_real_type_spec_fact, &
        standardir_write_real_type_spec_fact
    use standardir_double_precision_type_spec_fact, only: &
        standardir_consume_double_precision_type_spec_fact, &
        standardir_write_double_precision_type_spec_fact
    use standardir_complex_type_spec_fact, only: &
        standardir_consume_complex_type_spec_fact, standardir_write_complex_type_spec_fact
    use standardir_logical_type_spec_fact, only: &
        standardir_consume_logical_type_spec_fact, standardir_write_logical_type_spec_fact
    use standardir_character_type_spec_fact, only: &
        standardir_consume_character_type_spec_fact, standardir_write_character_type_spec_fact
    use standardir_intrinsic_type_spec_generated, only: &
        standardir_intrinsic_type_spec_t, standardir_lookup_intrinsic_type_spec
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=*), parameter :: expected = &
        '(grammar-fact (id R705) (expression "INTEGER [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_program = &
        '(grammar-fact (id R501) (expression "program-unit [ program-unit ] ...") '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R501) '// &
        '(page 53) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_real = &
        '(grammar-fact (id R706) (expression "REAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R706) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_double_precision = &
        '(grammar-fact (id R707) (expression "DOUBLE PRECISION") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R707) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_complex = &
        '(grammar-fact (id R704) (expression "or COMPLEX [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_logical = &
        '(grammar-fact (id R704) (expression "or LOGICAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_character = &
        '(grammar-fact (id R704) (expression "or CHARACTER [ char-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_assignment_stmt = &
        '(grammar-fact (id R1033) (expression "variable = expr") '// &
        '(source (source-ref (document J3-24-007) (clause 10) (rule R1033) '// &
        '(page 188) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=512) :: actual, message
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call check_generated_source_freshness()
    call check_generator_uses_declarative_expression()
    call check_intrinsic_type_spec_lookup()
    call check_assignment_stmt_generated_source_freshness()

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open program fact output')
    call standardir_write_program_grammar_fact(unit, 'J3-24-007', '5', 'R501', 53, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read program fact output')
    call require(trim(actual) == expected_program, 'canonical program grammar fact differs')

    call sx_parse(expected_program, node, ok, message)
    call require(ok, message)
    call standardir_consume_program_grammar_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R501) (expression "program is program-unit") '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R501) '// &
        '(page 53) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_program_grammar_fact(node, ok, message)
    call require(.not. ok, 'altered R501 expression reached the frontend consumer')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open assignment-stmt fact output')
    call standardir_write_assignment_stmt_grammar_fact(unit, 'J3-24-007', '10', 'R1033', 188, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read assignment-stmt fact output')
    call require(trim(actual) == expected_assignment_stmt, 'canonical assignment-stmt grammar fact differs')

    call sx_parse(expected_assignment_stmt, node, ok, message)
    call require(ok, message)
    call standardir_consume_assignment_stmt_grammar_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R1033) (expression "variable = mutated") '// &
        '(source (source-ref (document J3-24-007) (clause 10) (rule R1033) '// &
        '(page 188) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_assignment_stmt_grammar_fact(node, ok, message)
    call require(.not. ok, 'mutated assignment-stmt expression reached the frontend consumer')

    call sx_parse('(grammar-fact (id R1033) (expression "variable = expr") '// &
        '(source (source-ref (document J3-24-007) (clause 10) (rule R1032) '// &
        '(page 188) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated assignment-stmt rule output')
    call standardir_generate_assignment_stmt_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated assignment-stmt source rule reached the generator')

    call sx_parse('(grammar-fact (id R1033) (expression "variable = expr") '// &
        '(source (source-ref (document J3-24-007) (clause 10) (rule R1033) '// &
        '(page 189) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated assignment-stmt page output')
    call standardir_generate_assignment_stmt_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated assignment-stmt source page reached the generator')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open fact output')
    call standardir_write_integer_type_spec_fact(unit, 'J3-24-007', '7', 'R705', 67, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read fact output')
    call require(trim(actual) == expected, 'canonical type-spec fact differs')

    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_consume_integer_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R704) (expression "INTEGER [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_integer_type_spec_fact(node, ok, message)
    call require(.not. ok, 'wrong source rule reached the frontend consumer')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open REAL fact output')
    call standardir_write_real_type_spec_fact(unit, 'J3-24-007', '7', 'R706', 67, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read REAL fact output')
    call require(trim(actual) == expected_real, 'canonical REAL type-spec fact differs')

    call sx_parse(expected_real, node, ok, message)
    call require(ok, message)
    call standardir_consume_real_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R705) (expression "REAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_real_type_spec_fact(node, ok, message)
    call require(.not. ok, 'R705 reached the REAL frontend consumer')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open DOUBLE PRECISION fact output')
    call standardir_write_double_precision_type_spec_fact(unit, 'J3-24-007', '7', 'R707', 67, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read DOUBLE PRECISION fact output')
    call require(trim(actual) == expected_double_precision, &
        'canonical DOUBLE PRECISION type-spec fact differs')

    call sx_parse(expected_double_precision, node, ok, message)
    call require(ok, message)
    call standardir_consume_double_precision_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R706) (expression "DOUBLE PRECISION") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R706) '// &
        '(page 67) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_double_precision_type_spec_fact(node, ok, message)
    call require(.not. ok, 'R706 reached the DOUBLE PRECISION frontend consumer')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open COMPLEX fact output')
    call standardir_write_complex_type_spec_fact(unit, 'J3-24-007', '7', 'R704', 80, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read COMPLEX fact output')
    call require(trim(actual) == expected_complex, 'canonical COMPLEX type-spec fact differs')

    call sx_parse(expected_complex, node, ok, message)
    call require(ok, message)
    call standardir_consume_complex_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R704) (expression "COMPLEX [ mutated ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_complex_type_spec_fact(node, ok, message)
    call require(.not. ok, 'mutated COMPLEX expression reached the frontend consumer')

    call sx_parse('(grammar-fact (id R704) (expression "or COMPLEX [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated COMPLEX fact output')
    call standardir_generate_complex_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated COMPLEX source rule reached the generator')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open LOGICAL fact output')
    call standardir_write_logical_type_spec_fact(unit, 'J3-24-007', '7', 'R704', 80, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read LOGICAL fact output')
    call require(trim(actual) == expected_logical, 'canonical LOGICAL type-spec fact differs')

    call sx_parse(expected_logical, node, ok, message)
    call require(ok, message)
    call standardir_consume_logical_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R704) (expression "or LOGICAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 81) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated LOGICAL fact output')
    call standardir_generate_logical_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated LOGICAL source page reached the generator')

    call sx_parse('(grammar-fact (id R704) (expression "or LOGICAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call standardir_generate_logical_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated LOGICAL source rule reached the generator')

    call sx_parse('(grammar-fact (id R704) (expression "or LOGICAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash mutated-source))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call standardir_generate_logical_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated LOGICAL source hash reached the generator')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open CHARACTER fact output')
    call standardir_write_character_type_spec_fact(unit, 'J3-24-007', '7', 'R704', 80, &
        source_hash, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read CHARACTER fact output')
    call require(trim(actual) == expected_character, 'canonical CHARACTER type-spec fact differs')

    call sx_parse(expected_character, node, ok, message)
    call require(ok, message)
    call standardir_consume_character_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R704) (expression "or CHARACTER [ mutated ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_character_type_spec_fact(node, ok, message)
    call require(.not. ok, 'mutated CHARACTER expression reached the frontend consumer')

    call sx_parse('(grammar-fact (id R704) (expression "or CHARACTER [ char-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 80) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated CHARACTER rule output')
    call standardir_generate_character_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated CHARACTER source rule reached the generator')

    call sx_parse('(grammar-fact (id R704) (expression "or CHARACTER [ char-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 81) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated CHARACTER page output')
    call standardir_generate_character_type_spec_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated CHARACTER source page reached the generator')

    print '(a)', 'StandardIR grammar fact test passed'

contains

    subroutine check_intrinsic_type_spec_lookup()
        type(standardir_intrinsic_type_spec_t) :: value
        logical :: found

        call standardir_lookup_intrinsic_type_spec('INTEGER [ kind-selector ]', value, found)
        call require(found, 'INTEGER intrinsic lookup failed')
        call require(trim(value%canonical_name) == 'integer', 'INTEGER canonical name differs')
        call require(trim(value%source_rule) == 'R705', 'INTEGER source rule differs')
        call require(trim(value%document) == 'J3-24-007' .and. trim(value%clause) == '7' .and. &
            value%page == 67 .and. trim(value%source_hash) == source_hash, &
            'INTEGER provenance differs')

        call standardir_lookup_intrinsic_type_spec('REAL [ kind-selector ]', value, found)
        call require(found .and. trim(value%canonical_name) == 'real' .and. &
            trim(value%source_rule) == 'R706', 'REAL intrinsic lookup failed')
        call standardir_lookup_intrinsic_type_spec('DOUBLE PRECISION', value, found)
        call require(found .and. trim(value%canonical_name) == 'double_precision' .and. &
            trim(value%source_rule) == 'R707', 'DOUBLE PRECISION intrinsic lookup failed')
        call standardir_lookup_intrinsic_type_spec('COMPLEX', value, found)
        call require(found .and. trim(value%canonical_name) == 'complex' .and. &
            trim(value%source_rule) == 'R704' .and. value%page == 80, &
            'COMPLEX intrinsic lookup failed')
        call standardir_lookup_intrinsic_type_spec('LOGICAL [ kind-selector ]', value, found)
        call require(found .and. trim(value%canonical_name) == 'logical' .and. &
            trim(value%source_rule) == 'R704' .and. value%page == 80, &
            'LOGICAL intrinsic lookup failed')
        call standardir_lookup_intrinsic_type_spec('CHARACTER [ char-selector ]', value, found)
        call require(found .and. trim(value%canonical_name) == 'character' .and. &
            trim(value%source_rule) == 'R704' .and. value%page == 80, &
            'CHARACTER intrinsic lookup failed')

        call standardir_lookup_intrinsic_type_spec('REAL [ mutated ]', value, found)
        call require(.not. found, 'mutated intrinsic spelling was accepted')
    end subroutine check_intrinsic_type_spec_lookup

    subroutine check_generator_uses_declarative_expression()
        character(len=*), parameter :: mutated_source = &
            '(grammar-fact (id R501) (expression "program-unit [ mutated ] ...") '// &
            '(source (document J3-24-007) (clause 5) (rule R501) (page 53) '// &
            '(source-sha256 '//source_hash//')) (origin mechanical) (resolution resolved))'
        character(len=256) :: line
        integer :: local_unit, local_ios
        logical :: local_ok, found

        call sx_parse(mutated_source, node, local_ok, message)
        call require(local_ok, message)
        open (newunit=local_unit, status='scratch', action='readwrite', iostat=local_ios)
        call require(local_ios == 0, 'could not open mutated grammar-fact output')
        call standardir_generate_program_grammar_fact(node, local_unit, local_ok, message)
        call require(local_ok, message)
        rewind (local_unit)
        found = .false.
        do
            read (local_unit, '(a)', iostat=local_ios) line
            if (local_ios /= 0) exit
            if (index(line, 'program-unit [ mutated ] ...') > 0) found = .true.
        end do
        close (local_unit)
        call require(found, 'generator ignored declarative grammar-fact expression')
    end subroutine check_generator_uses_declarative_expression

    subroutine check_generated_source_freshness()
        character(len=256) :: line, fresh_program(512), checked_program(512), &
            fresh_integer(512), checked_integer(512), &
            fresh_real(512), checked_real(512), fresh_double_precision(512), &
            checked_double_precision(512), fresh_complex(512), checked_complex(512), &
            fresh_logical(512), checked_logical(512), &
            fresh_character(512), checked_character(512), &
            fresh_intrinsic(512), checked_intrinsic(512)
        character(len=1024) :: source
        integer :: input_unit, fresh_program_unit, fresh_integer_unit, fresh_real_unit, &
            fresh_double_precision_unit, fresh_logical_unit, fresh_character_unit, fresh_intrinsic_unit, ios
        integer :: fresh_program_count, checked_program_count
        integer :: fresh_integer_count, checked_integer_count, fresh_real_count, checked_real_count
        integer :: fresh_double_precision_count, checked_double_precision_count
        integer :: fresh_complex_count, checked_complex_count
        integer :: fresh_logical_count, checked_logical_count
        integer :: fresh_intrinsic_count, checked_intrinsic_count, i
        integer :: fresh_character_count, checked_character_count
        type(sx_node_t) :: intrinsic_nodes(6)
        logical :: local_ok

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not open grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read grammar-facts specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_program_unit, file='build/standardir_program_grammar_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh program grammar-fact output')
        call standardir_generate_program_grammar_fact(node, fresh_program_unit, local_ok, message)
        close (fresh_program_unit)
        call require(local_ok, message)
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read integer grammar-fact specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_integer_unit, file='build/standardir_grammar_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh grammar-fact output')
        call standardir_generate_integer_type_spec_fact(node, fresh_integer_unit, local_ok, message)
        close (fresh_integer_unit)
        call require(local_ok, message)
        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read REAL grammar-fact specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_real_unit, file='build/standardir_real_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh REAL grammar-fact output')
        call standardir_generate_real_type_spec_fact(node, fresh_real_unit, local_ok, message)
        close (fresh_real_unit)
        call require(local_ok, message)
        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read DOUBLE PRECISION grammar-fact specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_double_precision_unit, &
            file='build/standardir_double_precision_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh DOUBLE PRECISION grammar-fact output')
        call standardir_generate_double_precision_type_spec_fact(node, fresh_double_precision_unit, local_ok, &
            message)
        close (fresh_double_precision_unit)
        call require(local_ok, message)
        call read_source('build/standardir_program_grammar_fact_generated.f90', fresh_program, fresh_program_count)
        call read_source('src/standardir_program_grammar_fact_generated.f90', checked_program, checked_program_count)
        call require(fresh_program_count == checked_program_count, 'checked-in program grammar-fact output is stale')
        call require(all(fresh_program(:fresh_program_count) == checked_program(:checked_program_count)), &
            'checked-in program grammar-fact output differs from specification')
        call read_source('build/standardir_grammar_fact_generated.f90', fresh_integer, fresh_integer_count)
        call read_source('src/standardir_grammar_fact_generated.f90', checked_integer, checked_integer_count)
        call require(fresh_integer_count == checked_integer_count, 'checked-in integer grammar-fact output is stale')
        call require(all(fresh_integer(:fresh_integer_count) == checked_integer(:checked_integer_count)), &
            'checked-in integer grammar-fact output differs from specification')
        call read_source('build/standardir_real_type_spec_fact_generated.f90', fresh_real, fresh_real_count)
        call read_source('src/standardir_real_type_spec_fact_generated.f90', checked_real, checked_real_count)
        call require(fresh_real_count == checked_real_count, 'checked-in REAL grammar-fact output is stale')
        call require(all(fresh_real(:fresh_real_count) == checked_real(:checked_real_count)), &
            'checked-in REAL grammar-fact output differs from specification')
        call read_source('build/standardir_double_precision_type_spec_fact_generated.f90', &
            fresh_double_precision, fresh_double_precision_count)
        call read_source('src/standardir_double_precision_type_spec_fact_generated.f90', &
            checked_double_precision, checked_double_precision_count)
        call require(fresh_double_precision_count == checked_double_precision_count, &
            'checked-in DOUBLE PRECISION grammar-fact output is stale')
        call require(all(fresh_double_precision(:fresh_double_precision_count) == &
            checked_double_precision(:checked_double_precision_count)), &
            'checked-in DOUBLE PRECISION grammar-fact output differs from specification')

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen COMPLEX grammar-fact specification')
        do i = 1, 5
            read (input_unit, '(a)', iostat=ios) source
            call require(ios == 0, 'could not read COMPLEX grammar-fact specification')
        end do
        close (input_unit)
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=input_unit, file='build/standardir_complex_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh COMPLEX grammar-fact output')
        call standardir_generate_complex_type_spec_fact(node, input_unit, local_ok, message)
        close (input_unit)
        call require(local_ok, message)
        call read_source('build/standardir_complex_type_spec_fact_generated.f90', fresh_complex, &
            fresh_complex_count)
        call read_source('src/standardir_complex_type_spec_fact_generated.f90', checked_complex, &
            checked_complex_count)
        call require(fresh_complex_count == checked_complex_count, &
            'checked-in COMPLEX grammar-fact output is stale')
        call require(all(fresh_complex(:fresh_complex_count) == checked_complex(:checked_complex_count)), &
            'checked-in COMPLEX grammar-fact output differs from specification')

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen LOGICAL grammar-fact specification')
        do i = 1, 6
            read (input_unit, '(a)', iostat=ios) source
            call require(ios == 0, 'could not read LOGICAL grammar-fact specification')
        end do
        close (input_unit)
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_logical_unit, file='build/standardir_logical_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh LOGICAL grammar-fact output')
        call standardir_generate_logical_type_spec_fact(node, fresh_logical_unit, local_ok, message)
        close (fresh_logical_unit)
        call require(local_ok, message)
        call read_source('build/standardir_logical_type_spec_fact_generated.f90', fresh_logical, &
            fresh_logical_count)
        call read_source('src/standardir_logical_type_spec_fact_generated.f90', checked_logical, &
            checked_logical_count)
        call require(fresh_logical_count == checked_logical_count, &
            'checked-in LOGICAL grammar-fact output is stale')
        call require(all(fresh_logical(:fresh_logical_count) == checked_logical(:checked_logical_count)), &
            'checked-in LOGICAL grammar-fact output differs from specification')

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen CHARACTER grammar-fact specification')
        do i = 1, 7
            read (input_unit, '(a)', iostat=ios) source
            call require(ios == 0, 'could not read CHARACTER grammar-fact specification')
        end do
        close (input_unit)
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_character_unit, file='build/standardir_character_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh CHARACTER grammar-fact output')
        call standardir_generate_character_type_spec_fact(node, fresh_character_unit, local_ok, message)
        close (fresh_character_unit)
        call require(local_ok, message)
        call read_source('build/standardir_character_type_spec_fact_generated.f90', fresh_character, &
            fresh_character_count)
        call read_source('src/standardir_character_type_spec_fact_generated.f90', checked_character, &
            checked_character_count)
        call require(fresh_character_count == checked_character_count, &
            'checked-in CHARACTER grammar-fact output is stale')
        call require(all(fresh_character(:fresh_character_count) == checked_character(:checked_character_count)), &
            'checked-in CHARACTER grammar-fact output differs from specification')

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not open intrinsic type-spec specification')
        read (input_unit, '(a)', iostat=ios) line
        call require(ios == 0, 'could not skip program grammar-fact specification')
        do i = 1, 6
            read (input_unit, '(a)', iostat=ios) source
            call require(ios == 0, 'could not read intrinsic type-spec specification')
            call sx_parse(trim(source), intrinsic_nodes(i), local_ok, message)
            call require(local_ok, message)
        end do
        close (input_unit)
        open (newunit=fresh_intrinsic_unit, file='build/standardir_intrinsic_type_spec_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh intrinsic type-spec output')
        call standardir_generate_intrinsic_type_spec_lookup(intrinsic_nodes, fresh_intrinsic_unit, &
            local_ok, message)
        close (fresh_intrinsic_unit)
        call require(local_ok, message)
        call read_source('build/standardir_intrinsic_type_spec_generated.f90', fresh_intrinsic, &
            fresh_intrinsic_count)
        call read_source('src/standardir_intrinsic_type_spec_generated.f90', checked_intrinsic, &
            checked_intrinsic_count)
        call require(fresh_intrinsic_count == checked_intrinsic_count, &
            'checked-in intrinsic type-spec output is stale')
        call require(all(fresh_intrinsic(:fresh_intrinsic_count) == &
            checked_intrinsic(:checked_intrinsic_count)), &
            'checked-in intrinsic type-spec output differs from specification')
    end subroutine check_generated_source_freshness

    subroutine check_assignment_stmt_generated_source_freshness()
        character(len=256) :: fresh(512), checked(512)
        character(len=1024) :: source
        integer :: input_unit, output_unit, ios, fresh_count, checked_count
        logical :: local_ok

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not open assignment-stmt grammar-fact specification')
        do
            read (input_unit, '(a)', iostat=ios) source
            if (ios /= 0) exit
        end do
        close (input_unit)
        call require(len_trim(source) > 0, 'assignment-stmt grammar-fact specification is missing')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=output_unit, file='build/standardir_assignment_stmt_grammar_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh assignment-stmt grammar-fact output')
        call standardir_generate_assignment_stmt_grammar_fact(node, output_unit, local_ok, message)
        close (output_unit)
        call require(local_ok, message)
        call read_source('build/standardir_assignment_stmt_grammar_fact_generated.f90', fresh, fresh_count)
        call read_source('src/standardir_assignment_stmt_grammar_fact_generated.f90', checked, checked_count)
        call require(fresh_count == checked_count, 'checked-in assignment-stmt grammar-fact output is stale')
        call require(all(fresh(:fresh_count) == checked(:checked_count)), &
            'checked-in assignment-stmt grammar-fact output differs from specification')
    end subroutine check_assignment_stmt_generated_source_freshness

    subroutine read_source(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: count
        character(len=256) :: line
        integer :: local_unit, local_ios

        count = 0
        open (newunit=local_unit, file=path, action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open generated grammar-fact source')
        do
            read (local_unit, '(a)', iostat=local_ios) line
            if (local_ios /= 0) exit
            count = count + 1
            call require(count <= size(lines), 'generated grammar-fact source is too long')
            lines(count) = line
        end do
        close (local_unit)
    end subroutine read_source

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_grammar_fact

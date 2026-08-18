program test_schema_generated_api
    !! Fixed bytes plus the generic codec are the independent API oracle.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use fortsx, only: sx_node_t, sx_parse
    use schema_ir, only: schema_parse_text, schema_t
    use schema_value, only: schema_value_canonicalize
    use schema_v0_generated
    use writer, only: writer_close, writer_digest, writer_init_hash, writer_t, &
        writer_write_bytes
    implicit none

    character(len=*), parameter :: schema_text = &
        '(schema standardir-v0 (primitive bool) (primitive int) '// &
        '(primitive status) (primitive name) '// &
        '(record source-ref (document name) (clause name) (rule name) '// &
        '(page int) (source-hash name)) '// &
        '(enum item-kind syntax constraint relation rule definition) '// &
        '(sum item (syntax string) constraint relation rule definition) '// &
        '(enum origin mechanical search smt llm llm-repair human imported differential) '// &
        '(enum resolution resolved unresolved disputed) '// &
        '(record semantic-item (id name) (subject name) (source source-ref) '// &
        '(origin origin) (resolution resolution)) '// &
        '(record grammar-fact (id name) (expression string) (source source-ref) '// &
        '(origin origin) (resolution resolution)) '// &
        '(list semantic-items semantic-item) (list grammar-facts grammar-fact) '// &
        '(list items item) '// &
        '(optional source-ref-option source-ref))'
    character(len=256) :: message, actual, reference
    type(schema_t) :: schema
    type(source_ref_t) :: source_ref, decoded_source_ref
    type(item_t) :: item, decoded_item
    type(items_t) :: items
    type(grammar_fact_t) :: grammar_fact, decoded_grammar_fact
    type(grammar_facts_t) :: grammar_facts
    type(source_ref_option_t) :: optional_ref
    type(sx_node_t) :: node
    logical :: ok

    call schema_parse_text(schema_text, schema, ok, message)
    call require(ok, message)

    source_ref%document = 'J3'
    source_ref%clause = '5'
    source_ref%rule = 'R501'
    source_ref%page = 53
    source_ref%source_hash = 'fixture'
    call write_source_ref(source_ref, '(source-ref (document J3) (clause 5) (rule R501) '// &
        '(page 53) (source-hash fixture))')
    call sx_parse('(source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
        '(source-hash fixture))', node, ok, message)
    call require(ok, message)
    call schema_read_source_ref(node, decoded_source_ref, ok, message)
    call require(ok, message)
    call require(trim(decoded_source_ref%document) == 'J3' .and. &
        trim(decoded_source_ref%clause) == '5' .and. &
        trim(decoded_source_ref%rule) == 'R501' .and. decoded_source_ref%page == 53 .and. &
        trim(decoded_source_ref%source_hash) == 'fixture', 'generated record reader differs')

    item%kind = ITEM_SYNTAX
    allocate (item%syntax)
    item%syntax%value = 'alpha beta'
    call write_item(item, '(syntax "alpha beta")')
    call sx_parse('(syntax "alpha beta")', node, ok, message)
    call require(ok, message)
    call schema_read_item(node, decoded_item, ok, message)
    call require(ok, message)
    call require(decoded_item%kind == ITEM_SYNTAX, 'generated sum kind differs')
    call require(allocated(decoded_item%syntax), 'generated sum payload is absent')
    call require(trim(decoded_item%syntax%value) == 'alpha beta', &
        'generated sum payload differs')

    item%kind = ITEM_CONSTRAINT
    if (allocated(item%syntax)) deallocate (item%syntax)
    call write_item(item, '(constraint)')
    call sx_parse('(constraint)', node, ok, message)
    call require(ok, message)
    call schema_read_item(node, decoded_item, ok, message)
    call require(ok, message)
    call require(decoded_item%kind == ITEM_CONSTRAINT, &
        'generated payload-less sum kind differs')

    allocate (items%values(2))
    items%values(1)%kind = ITEM_SYNTAX
    allocate (items%values(1)%syntax)
    items%values(1)%syntax%value = 'x'
    items%values(2)%kind = ITEM_CONSTRAINT
    call write_items(items, '(items (syntax "x") (constraint))')

    grammar_fact%id = 'R501'
    grammar_fact%expression = 'name [ "," name ]'
    grammar_fact%source = source_ref
    grammar_fact%origin = ORIGIN_MECHANICAL
    grammar_fact%resolution = RESOLUTION_RESOLVED
    call write_grammar_fact(grammar_fact, '(grammar-fact (id R501) '// &
        '(expression "name [ \",\" name ]") (source (source-ref (document J3) '// &
        '(clause 5) (rule R501) (page 53) (source-hash fixture))) '// &
        '(origin mechanical) (resolution resolved))')
    call sx_parse('(grammar-fact (id R501) (expression "name [ \",\" name ]") '// &
        '(source (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved))', node, ok, &
        message)
    call require(ok, message)
    call schema_read_grammar_fact(node, decoded_grammar_fact, ok, message)
    call require(ok, message)
    call require(trim(decoded_grammar_fact%id) == 'R501' .and. &
        trim(decoded_grammar_fact%expression) == 'name [ "," name ]' .and. &
        decoded_grammar_fact%origin == ORIGIN_MECHANICAL .and. &
        decoded_grammar_fact%resolution == RESOLUTION_RESOLVED, &
        'generated grammar fact differs')
    allocate (grammar_facts%values(1))
    grammar_facts%values(1) = grammar_fact
    call write_grammar_facts(grammar_facts, '(grammar-facts (grammar-fact (id R501) '// &
        '(expression "name [ \",\" name ]") (source (source-ref (document J3) '// &
        '(clause 5) (rule R501) (page 53) (source-hash fixture))) '// &
        '(origin mechanical) (resolution resolved)))')

    optional_ref%value = source_ref
    call write_optional(optional_ref, &
        '(some (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
        '(source-hash fixture)))')
    deallocate (optional_ref%value)
    call write_optional(optional_ref, 'none')

    call write_item_kind(ITEM_KIND_SYNTAX, 'syntax')
    call write_bool(.true., 'true')
    call write_int(-7, '-7')
    call write_string('a"b', '"a\"b"')

    call expect_failure('(source-ref (clause 5) (document 1) (rule 501) (page 53) '// &
        '(source-hash fixture))', &
        'generated schema record fields are out of order')
    call expect_item_failure('(syntax)', 'sum variant payload is missing')
    call check_generated_semantics()
    call check_print_and_hash_source_ref(source_ref)
    call check_print_and_hash_item(item)
    call check_print_and_hash_items(items)
    call check_print_and_hash_optional(optional_ref)
    call check_print_and_hash_bool(.true.)

    print '(a)', 'generated schema API test passed'

contains

    subroutine write_source_ref(value, expected)
        type(source_ref_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_source_ref(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('source-ref', '(source-ref (document J3) (clause 5) (rule R501) '// &
            '(page 53) (source-hash fixture))', &
            expected)
    end subroutine write_source_ref

    subroutine write_item(value, expected)
        type(item_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_item(value, unit, ok, message)
        close (unit)
        if (.not. ok) call require(.false., message)
        if (trim(expected) == '(syntax "alpha beta")') then
            call check_output('item', '(syntax "alpha beta")', expected)
        else
            call check_output('item', '(constraint)', expected)
        end if
    end subroutine write_item

    subroutine write_items(value, expected)
        type(items_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_items(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('items', '(items (syntax "x") (constraint))', expected)
    end subroutine write_items

    subroutine write_grammar_fact(value, expected)
        type(grammar_fact_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_grammar_fact(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('grammar-fact', expected, expected)
    end subroutine write_grammar_fact

    subroutine write_grammar_facts(value, expected)
        type(grammar_facts_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_grammar_facts(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('grammar-facts', expected, expected)
    end subroutine write_grammar_facts

    subroutine write_optional(value, expected)
        type(source_ref_option_t), intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_source_ref_option(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        if (trim(expected) == 'none') then
            call check_output('source-ref-option', 'none', expected)
        else
            call check_output('source-ref-option', &
                '(some (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
                '(source-hash fixture)))', expected)
        end if
    end subroutine write_optional

    subroutine write_item_kind(value, expected)
        integer, intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_item_kind(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('item-kind', expected, expected)
    end subroutine write_item_kind

    subroutine write_bool(value, expected)
        logical, intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_bool(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('bool', expected, expected)
    end subroutine write_bool

    subroutine write_int(value, expected)
        integer, intent(in) :: value
        character(len=*), intent(in) :: expected
        integer :: unit

        call open_output(unit)
        call schema_write_int(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('int', expected, expected)
    end subroutine write_int

    subroutine write_string(value, expected)
        character(len=*), intent(in) :: value, expected
        integer :: unit

        call open_output(unit)
        call schema_write_string(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('string', 'a"b', expected)
    end subroutine write_string

    subroutine check_output(root_type, input, expected)
        character(len=*), intent(in) :: root_type, input, expected
        integer :: unit, ios

        open (newunit=unit, file='build/generated_schema_value.sx', action='read', &
            iostat=ios)
        call require(ios == 0, 'could not read generated schema value')
        read (unit, '(a)', iostat=ios) actual
        close (unit)
        call require(ios == 0, 'could not read generated schema value line')
        call require(trim(actual) == expected, 'generated schema bytes differ')

        call open_reference(unit)
        call schema_value_canonicalize(schema, root_type, input, unit, ok, message)
        close (unit)
        call require(ok, message)
        open (newunit=unit, file='build/reference_schema_value.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not read reference schema value')
        read (unit, '(a)', iostat=ios) reference
        close (unit)
        call require(ios == 0, 'could not read reference schema value line')
        call require(trim(actual) == trim(reference), 'generated and reference bytes differ')
    end subroutine check_output

    subroutine expect_failure(input, expected_message)
        character(len=*), intent(in) :: input, expected_message
        integer :: unit

        call sx_parse(input, node, ok, message)
        call require(ok, message)
        call schema_read_source_ref(node, source_ref, ok, message)
        call require(.not. ok, 'invalid generated schema value was accepted')
        call require(trim(message) == expected_message, &
            'invalid generated schema diagnostic differs')
    end subroutine expect_failure

    subroutine expect_item_failure(input, expected_message)
        character(len=*), intent(in) :: input, expected_message

        call sx_parse(input, node, ok, message)
        call require(ok, message)
        call schema_read_item(node, item, ok, message)
        call require(.not. ok, 'invalid generated sum value was accepted')
        call require(trim(message) == expected_message, &
            'invalid generated sum diagnostic differs')
    end subroutine expect_item_failure

    subroutine check_generated_semantics()
        type(source_ref_t) :: source_ref_copy, invalid_source_ref
        type(item_t) :: item_copy, invalid_item
        type(items_t) :: empty_items, allocated_empty_items, items_copy
        type(source_ref_option_t) :: optional_copy
        logical :: local_ok
        character(len=256) :: local_message

        call schema_validate_source_ref(source_ref, local_ok, local_message)
        call require(local_ok, local_message)
        invalid_source_ref = source_ref
        invalid_source_ref%document = 'bad name'
        call schema_validate_source_ref(invalid_source_ref, local_ok, local_message)
        call require(.not. local_ok, 'invalid name passed generated validation')
        call require(trim(local_message) == 'name is not a canonical atom', &
            'invalid name diagnostic differs')

        call schema_validate_item_kind(ITEM_KIND_SYNTAX, local_ok, local_message)
        call require(local_ok, local_message)
        call schema_validate_item_kind(99, local_ok, local_message)
        call require(.not. local_ok, 'invalid enum passed generated validation')
        call require(trim(local_message) == 'unknown enum value: item-kind', &
            'invalid enum diagnostic differs')

        item%kind = ITEM_SYNTAX
        if (allocated(item%syntax)) deallocate (item%syntax)
        allocate (item%syntax)
        item%syntax%value = 'semantic value'
        call schema_validate_item(item, local_ok, local_message)
        call require(local_ok, local_message)
        item_copy = item
        call require(schema_equal_item(item, item_copy), 'equal generated sums differ')
        item_copy%syntax%value = 'different'
        call require(.not. schema_equal_item(item, item_copy), &
            'different generated sums compare equal')
        invalid_item = item
        deallocate (invalid_item%syntax)
        call schema_validate_item(invalid_item, local_ok, local_message)
        call require(.not. local_ok, 'missing sum payload passed generated validation')
        call require(trim(local_message) == 'sum payload is not allocated: syntax', &
            'missing sum payload diagnostic differs')
        invalid_item = item
        invalid_item%kind = ITEM_CONSTRAINT
        call schema_validate_item(invalid_item, local_ok, local_message)
        call require(.not. local_ok, 'inactive sum payload passed generated validation')
        call require(trim(local_message) == 'sum has inactive payload: syntax', &
            'inactive sum payload diagnostic differs')

        call schema_validate_items(items, local_ok, local_message)
        call require(local_ok, local_message)
        items_copy = items
        call require(schema_equal_items(items, items_copy), 'equal generated lists differ')
        items_copy%values(2)%kind = ITEM_SYNTAX
        if (allocated(items_copy%values(2)%syntax)) deallocate (items_copy%values(2)%syntax)
        call require(.not. schema_equal_items(items, items_copy), &
            'different generated lists compare equal')
        call schema_validate_items(items_copy, local_ok, local_message)
        call require(.not. local_ok, 'invalid list element passed generated validation')

        call require(schema_equal_items(empty_items, empty_items), &
            'unallocated empty lists do not compare equal')
        allocate (allocated_empty_items%values(0))
        call require(schema_equal_items(empty_items, allocated_empty_items), &
            'empty list representations are not canonical-equal')
        call schema_validate_items(empty_items, local_ok, local_message)
        call require(local_ok, local_message)

        if (allocated(optional_ref%value)) deallocate (optional_ref%value)
        call schema_validate_source_ref_option(optional_ref, local_ok, local_message)
        call require(local_ok, local_message)
        optional_ref%value = source_ref
        call schema_validate_source_ref_option(optional_ref, local_ok, local_message)
        call require(local_ok, local_message)
        optional_copy = optional_ref
        call require(schema_equal_source_ref_option(optional_ref, optional_copy), &
            'equal generated optionals differ')

        source_ref_copy = source_ref
        source_ref_copy%rule = 'R502'
        call require(.not. schema_equal_source_ref(source_ref, source_ref_copy), &
            'different generated records compare equal')
    end subroutine check_generated_semantics

    subroutine check_print_and_hash_source_ref(value)
        type(source_ref_t), intent(in) :: value
        integer :: unit
        integer(int8) :: generated_digest(32), reference_digest(32)

        call open_output(unit)
        call schema_print_source_ref(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('source-ref', '(source-ref (document J3) (clause 5) (rule R501) '// &
            '(page 53) (source-hash fixture))', &
            '(source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
            '(source-hash fixture))')
        call reference_hash('source-ref', &
            '(source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
            '(source-hash fixture))', reference_digest)
        call schema_hash_source_ref(value, generated_digest, ok, message)
        call require(ok, message)
        call require(all(generated_digest == reference_digest), &
            'generated record hash differs from reference')
    end subroutine check_print_and_hash_source_ref

    subroutine check_print_and_hash_item(value)
        type(item_t), intent(in) :: value
        integer :: unit
        integer(int8) :: generated_digest(32), reference_digest(32)

        call open_output(unit)
        call schema_print_item(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('item', '(syntax "semantic value")', '(syntax "semantic value")')
        call reference_hash('item', '(syntax "semantic value")', reference_digest)
        call schema_hash_item(value, generated_digest, ok, message)
        call require(ok, message)
        call require(all(generated_digest == reference_digest), &
            'generated sum hash differs from reference')
    end subroutine check_print_and_hash_item

    subroutine check_print_and_hash_items(value)
        type(items_t), intent(in) :: value
        integer :: unit
        integer(int8) :: generated_digest(32), reference_digest(32)

        call open_output(unit)
        call schema_print_items(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('items', '(items (syntax "x") (constraint))', &
            '(items (syntax "x") (constraint))')
        call reference_hash('items', '(items (syntax "x") (constraint))', reference_digest)
        call schema_hash_items(value, generated_digest, ok, message)
        call require(ok, message)
        call require(all(generated_digest == reference_digest), &
            'generated list hash differs from reference')
    end subroutine check_print_and_hash_items

    subroutine check_print_and_hash_optional(value)
        type(source_ref_option_t), intent(in) :: value
        integer :: unit
        integer(int8) :: generated_digest(32), reference_digest(32)

        call open_output(unit)
        call schema_print_source_ref_option(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('source-ref-option', &
            '(some (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
            '(source-hash fixture)))', &
            '(some (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
            '(source-hash fixture)))')
        call reference_hash('source-ref-option', &
            '(some (source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
            '(source-hash fixture)))', reference_digest)
        call schema_hash_source_ref_option(value, generated_digest, ok, message)
        call require(ok, message)
        call require(all(generated_digest == reference_digest), &
            'generated optional hash differs from reference')
    end subroutine check_print_and_hash_optional

    subroutine check_print_and_hash_bool(value)
        logical, intent(in) :: value
        integer :: unit
        integer(int8) :: generated_digest(32), reference_digest(32)

        call open_output(unit)
        call schema_print_bool(value, unit, ok, message)
        close (unit)
        call require(ok, message)
        call check_output('bool', 'true', 'true')
        call reference_hash('bool', 'true', reference_digest)
        call schema_hash_bool(value, generated_digest, ok, message)
        call require(ok, message)
        call require(all(generated_digest == reference_digest), &
            'generated primitive hash differs from reference')
    end subroutine check_print_and_hash_bool

    subroutine reference_hash(root_type, input, digest)
        character(len=*), intent(in) :: root_type, input
        integer(int8), intent(out) :: digest(32)
        integer :: unit

        call open_reference(unit)
        call schema_value_canonicalize(schema, root_type, input, unit, ok, message)
        close (unit)
        call require(ok, message)
        call hash_file('build/reference_schema_value.sx', digest)
    end subroutine reference_hash

    subroutine hash_file(path, digest)
        character(len=*), intent(in) :: path
        integer(int8), intent(out) :: digest(32)
        type(writer_t) :: output
        integer(int8), allocatable :: bytes(:)
        integer(int64) :: file_size
        integer :: unit, ios
        logical :: local_ok
        character(len=256) :: local_message

        inquire (file=path, size=file_size)
        allocate (bytes(int(file_size)))
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            action='read', iostat=ios)
        call require(ios == 0, 'could not open reference bytes for hashing')
        if (file_size > 0_int64) read (unit, iostat=ios) bytes
        close (unit)
        call require(ios == 0, 'could not read reference bytes for hashing')
        call writer_init_hash(output, local_ok, local_message)
        call require(local_ok, local_message)
        call writer_write_bytes(output, bytes, local_ok, local_message)
        call require(local_ok, local_message)
        call writer_digest(output, digest, local_ok, local_message)
        call require(local_ok, local_message)
        call writer_close(output, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine hash_file

    subroutine open_output(unit)
        integer, intent(out) :: unit
        integer :: ios

        open (newunit=unit, file='build/generated_schema_value.sx', status='replace', &
            action='write', iostat=ios)
        call require(ios == 0, 'could not open generated schema value')
    end subroutine open_output

    subroutine open_reference(unit)
        integer, intent(out) :: unit
        integer :: ios

        open (newunit=unit, file='build/reference_schema_value.sx', status='replace', &
            action='write', iostat=ios)
        call require(ios == 0, 'could not open reference schema value')
    end subroutine open_reference

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_schema_generated_api

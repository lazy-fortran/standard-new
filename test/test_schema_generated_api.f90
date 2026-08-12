program test_schema_generated_api
    !! Fixed bytes plus the generic codec are the independent API oracle.

    use fortsx, only: sx_node_t, sx_parse
    use schema_ir, only: schema_parse_text, schema_t
    use schema_value, only: schema_value_canonicalize
    use schema_v0_generated
    implicit none

    character(len=*), parameter :: schema_text = &
        '(schema standardir-v0 (primitive bool) (primitive int) '// &
        '(primitive status) (primitive name) '// &
        '(record source-ref (document name) (clause name) (rule name)) '// &
        '(enum item-kind syntax constraint relation rule definition) '// &
        '(sum item (syntax string) constraint relation rule definition) '// &
        '(list items item) (optional source-ref-option source-ref))'
    character(len=256) :: message, actual, reference
    type(schema_t) :: schema
    type(source_ref_t) :: source_ref, decoded_source_ref
    type(item_t) :: item, decoded_item
    type(items_t) :: items
    type(source_ref_option_t) :: optional_ref
    type(sx_node_t) :: node
    logical :: ok

    call schema_parse_text(schema_text, schema, ok, message)
    call require(ok, message)

    source_ref%document = 'J3'
    source_ref%clause = '5'
    source_ref%rule = 'R501'
    call write_source_ref(source_ref, '(source-ref (document J3) (clause 5) (rule R501))')
    call sx_parse('(source-ref (document J3) (clause 5) (rule R501))', node, ok, message)
    call require(ok, message)
    call schema_read_source_ref(node, decoded_source_ref, ok, message)
    call require(ok, message)
    call require(trim(decoded_source_ref%document) == 'J3' .and. &
        trim(decoded_source_ref%clause) == '5' .and. &
        trim(decoded_source_ref%rule) == 'R501', 'generated record reader differs')

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

    optional_ref%value = source_ref
    call write_optional(optional_ref, &
        '(some (source-ref (document J3) (clause 5) (rule R501)))')
    deallocate (optional_ref%value)
    call write_optional(optional_ref, 'none')

    call write_item_kind(ITEM_KIND_SYNTAX, 'syntax')
    call write_bool(.true., 'true')
    call write_int(-7, '-7')
    call write_string('a"b', '"a\"b"')

    call expect_failure('(source-ref (clause 5) (document 1) (rule 501))', &
        'generated schema record fields are out of order')
    call expect_item_failure('(syntax)', 'sum variant payload is missing')

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
        call check_output('source-ref', '(source-ref (document J3) (clause 5) (rule R501))', &
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
                '(some (source-ref (document J3) (clause 5) (rule R501)))', expected)
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

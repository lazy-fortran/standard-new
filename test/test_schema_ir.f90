program test_schema_ir
    !! Fixed schema SX and malformed inputs are the independent oracle.

    use schema_ir, only: schema_enum, schema_list, schema_optional, schema_record, &
        schema_sum, schema_parse_text, schema_t
    implicit none

    character(len=*), parameter :: input = &
        '(schema demo (primitive bool) (primitive int) '// &
        '(record source-ref (document name) (clause name)) '// &
        '(enum item-kind syntax constraint) '// &
        '(sum item (syntax string) constraint) (list items item) '// &
        '(optional maybe-source source-ref))'
    character(len=256) :: message
    type(schema_t) :: schema
    type(schema_t) :: file_schema
    character(len=4096) :: file_text
    integer :: ios, unit
    logical :: ok

    call schema_parse_text(input, schema, ok, message)
    call require(ok, message)
    call require(trim(schema%name) == 'demo', 'schema name differs')
    call require(schema%declaration_count == 7, 'declaration count differs')
    call require(schema%declarations(3)%kind == schema_record, 'record kind differs')
    call require(schema%declarations(3)%member_count == 2, 'record member count differs')
    call require(trim(schema%declarations(3)%members(1)%name) == 'document', &
        'record member name differs')
    call require(trim(schema%declarations(3)%members(1)%type_name) == 'name', &
        'record member type differs')
    call require(schema%declarations(4)%kind == schema_enum, 'enum kind differs')
    call require(trim(schema%declarations(4)%members(2)%name) == 'constraint', &
        'enum member differs')
    call require(schema%declarations(5)%kind == schema_sum, 'sum kind differs')
    call require(trim(schema%declarations(5)%members(1)%name) == 'syntax', &
        'sum variant name differs')
    call require(trim(schema%declarations(5)%members(1)%type_name) == 'string', &
        'sum payload type differs')
    call require(len_trim(schema%declarations(5)%members(2)%type_name) == 0, &
        'payload-less sum variant gained a type')
    call require(schema%declarations(6)%kind == schema_list, 'list kind differs')
    call require(trim(schema%declarations(6)%target_type) == 'item', &
        'list target differs')
    call require(schema%declarations(7)%kind == schema_optional, &
        'optional kind differs')
    call require(trim(schema%declarations(7)%target_type) == 'source-ref', &
        'optional target differs')

    open (newunit=unit, file='specs/schema-v0.sxs', action='read', iostat=ios)
    call require(ios == 0, 'could not open schema specification')
    read (unit, '(a)', iostat=ios) file_text
    close (unit)
    call require(ios == 0, 'could not read schema specification')
    call schema_parse_text(trim(file_text), file_schema, ok, message)
    call require(ok, message)
    call require(trim(file_schema%name) == 'standardir-v0', &
        'schema specification name differs')
    call require(file_schema%declaration_count == 7, &
        'schema specification declaration count differs')
    call require(file_schema%declarations(5)%member_count == 5, &
        'schema specification source reference differs')

    call expect_failure('(schema demo (record bad (only)))', &
        'record member needs a name and type')
    call expect_failure('(schema demo (primitive bool) (primitive bool))', &
        'duplicate schema declaration: bool')
    call expect_failure('(schema demo (record bad (field missing)))', &
        'unknown member type: missing')
    call expect_failure('(schema demo (list values missing))', &
        'unknown target type: missing')

    print '(a)', 'schema IR test passed'

contains

    subroutine expect_failure(text, expected_message)
        character(len=*), intent(in) :: text, expected_message
        type(schema_t) :: local_schema
        character(len=256) :: local_message
        logical :: local_ok

        call schema_parse_text(text, local_schema, local_ok, local_message)
        call require(.not. local_ok, 'invalid schema was accepted')
        call require(trim(local_message) == expected_message, &
            'invalid schema message differs')
    end subroutine expect_failure

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_schema_ir

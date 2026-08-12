program test_schema_value
    !! Fixed canonical values are the independent oracle for schema value output.

    use schema_ir, only: schema_parse_text, schema_t
    use schema_value, only: schema_value_canonicalize
    implicit none

    character(len=*), parameter :: schema_text = &
        '(schema demo (primitive bool) (primitive int) (primitive name) '// &
        '(record source-ref (document int) (clause int) (rule int)) '// &
        '(enum item-kind syntax constraint) '// &
        '(sum item (syntax string) constraint) (list items item) '// &
        '(optional maybe-source source-ref))'
    character(len=256) :: message, value
    type(schema_t) :: schema
    logical :: ok

    call schema_parse_text(schema_text, schema, ok, message)
    call require(ok, message)

    call check_value('source-ref', &
        '(source-ref (document 1) (clause 5) (rule 501))', &
        '(source-ref (document 1) (clause 5) (rule 501))')
    call check_value('item', '(syntax "alpha beta")', '(syntax "alpha beta")')
    call check_value('item', '(constraint)', '(constraint)')
    call check_value('items', '(items (syntax "x") (constraint))', &
        '(items (syntax "x") (constraint))')
    call check_value('maybe-source', 'none', 'none')
    call check_value('maybe-source', '(some (source-ref (document 1) (clause 5) (rule 501)))', &
        '(some (source-ref (document 1) (clause 5) (rule 501)))')
    call check_value('item-kind', 'syntax', 'syntax')
    call check_value('bool', 'true', 'true')
    call check_value('int', '-7', '-7')
    call check_value('string', 'a"b', '"a\"b"')

    call expect_failure('int', '01', 'integer schema value is not canonical decimal')
    call expect_failure('source-ref', &
        '(source-ref (clause 5) (document 1) (rule 501))', &
        'record fields are not in schema order')
    call expect_failure('item', 'syntax', 'sum schema value is not a variant list')

    print '(a)', 'schema value test passed'

contains

    subroutine check_value(root_type, input, expected)
        character(len=*), intent(in) :: root_type, input, expected

        call write_value(root_type, input, ok, message)
        call require(ok, message)
        call require(trim(value) == expected, 'canonical schema value differs')
    end subroutine check_value

    subroutine expect_failure(root_type, input, expected)
        character(len=*), intent(in) :: root_type, input, expected

        call write_value(root_type, input, ok, message)
        call require(.not. ok, 'invalid schema value was accepted')
        call require(trim(message) == expected, 'invalid schema value diagnostic differs')
    end subroutine expect_failure

    subroutine write_value(root_type, input, local_ok, local_message)
        character(len=*), intent(in) :: root_type, input
        logical, intent(out) :: local_ok
        character(len=*), intent(out) :: local_message
        integer :: ios, unit

        open (newunit=unit, file='build/schema_value.out', status='replace', &
            action='write', iostat=ios)
        call require(ios == 0, 'could not open schema value output')
        call schema_value_canonicalize(schema, root_type, input, unit, local_ok, local_message)
        close (unit)
        if (.not. local_ok) return
        open (newunit=unit, file='build/schema_value.out', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen schema value output')
        read (unit, '(a)', iostat=ios) value
        close (unit)
        call require(ios == 0, 'could not read schema value output')
    end subroutine write_value

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_schema_value

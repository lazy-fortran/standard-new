program test_schema_consumer
    !! Fixed SX records are the independent oracle for generated consumers.

    use fortsx, only: sx_node_t, sx_parse
    use schema_v0_generated, only: ITEM_CONSTRAINT, ITEM_SYNTAX, item_t, &
        schema_consume_item, schema_consume_item_kind, schema_consume_source_ref, &
        schema_consume_items_elements, source_ref_t
    implicit none

    type(sx_node_t) :: node
    character(len=256) :: message
    logical :: ok
    integer :: source_calls, item_calls, enum_calls
    integer :: item_kinds(2)

    source_calls = 0
    call sx_parse('(source-ref (document J3) (clause 5) (rule R501) (page 53) '// &
        '(source-hash fixture))', node, ok, message)
    call require(ok, message)
    call schema_consume_source_ref(node, consume_source, ok, message)
    call require(ok, message)
    call require(source_calls == 1, 'valid provenance was not consumed once')

    call sx_parse('(source-ref (clause 5) (document J3) (rule R501))', node, ok, message)
    call require(ok, message)
    call schema_consume_source_ref(node, consume_source, ok, message)
    call require(.not. ok, 'invalid provenance reached the consumer')
    call require(source_calls == 1, 'invalid provenance invoked the consumer')

    item_calls = 0
    call sx_parse('(syntax "program")', node, ok, message)
    call require(ok, message)
    call schema_consume_item(node, consume_item, ok, message)
    call require(ok, message)
    call require(item_calls == 1, 'valid sum value was not consumed once')

    enum_calls = 0
    call sx_parse('unresolved', node, ok, message)
    call require(ok, message)
    call schema_consume_item_kind(node, consume_enum, ok, message)
    call require(.not. ok, 'unknown enum state reached the consumer')
    call require(enum_calls == 0, 'invalid enum state invoked the consumer')

    item_calls = 0
    item_kinds = 0
    call sx_parse('(items (syntax "first") (constraint))', node, ok, message)
    call require(ok, message)
    call schema_consume_items_elements(node, consume_item, ok, message)
    call require(ok, message)
    call require(item_calls == 2, 'list elements were not consumed in full')
    call require(item_kinds(1) == ITEM_SYNTAX, 'first list element changed order')
    call require(item_kinds(2) == ITEM_CONSTRAINT, 'second list element changed order')

    item_calls = 0
    call sx_parse('(items (syntax "first") (syntax))', node, ok, message)
    call require(ok, message)
    call schema_consume_items_elements(node, consume_item, ok, message)
    call require(.not. ok, 'malformed list element was accepted')
    call require(item_calls == 1, 'consumer continued after malformed list element')

    item_calls = 0
    call sx_parse('(wrong-items constraint)', node, ok, message)
    call require(ok, message)
    call schema_consume_items_elements(node, consume_item, ok, message)
    call require(.not. ok, 'wrong list declaration reached the consumer')
    call require(item_calls == 0, 'wrong list declaration invoked the consumer')

    print '(a)', 'schema consumer test passed'

contains

    subroutine consume_source(value, callback_ok, callback_message)
        type(source_ref_t), intent(in) :: value
        logical, intent(out) :: callback_ok
        character(len=*), intent(out) :: callback_message

        callback_ok = trim(value%document) == 'J3' .and. trim(value%clause) == '5' .and. &
            trim(value%rule) == 'R501' .and. value%page == 53 .and. &
            trim(value%source_hash) == 'fixture'
        callback_message = ''
        if (.not. callback_ok) callback_message = 'consumer lost provenance fields'
        if (callback_ok) source_calls = source_calls + 1
    end subroutine consume_source

    subroutine consume_item(value, callback_ok, callback_message)
        type(item_t), intent(in) :: value
        logical, intent(out) :: callback_ok
        character(len=*), intent(out) :: callback_message

        callback_ok = .false.
        if (value%kind == ITEM_SYNTAX) then
            if (allocated(value%syntax)) then
                callback_ok = trim(value%syntax%value) == 'program' .or. &
                    trim(value%syntax%value) == 'first'
            end if
        else if (value%kind == ITEM_CONSTRAINT) then
            callback_ok = .true.
        end if
        callback_message = ''
        if (.not. callback_ok) callback_message = 'consumer lost sum payload'
        if (callback_ok) then
            item_calls = item_calls + 1
            if (item_calls <= size(item_kinds)) item_kinds(item_calls) = value%kind
        end if
    end subroutine consume_item

    subroutine consume_enum(value, callback_ok, callback_message)
        integer, intent(in) :: value
        logical, intent(out) :: callback_ok
        character(len=*), intent(out) :: callback_message

        callback_ok = value == ITEM_CONSTRAINT
        callback_message = ''
        if (.not. callback_ok) callback_message = 'consumer received an invalid enum'
        if (callback_ok) enum_calls = enum_calls + 1
    end subroutine consume_enum

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_schema_consumer

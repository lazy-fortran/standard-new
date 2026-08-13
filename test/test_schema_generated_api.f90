program test_schema_generated_api
    !! Fixed SX values independently verify provenance-bearing generated records.

    use fortsx, only: sx_node_t, sx_parse
    use schema_v0_generated
    implicit none

    type(source_ref_t) :: source, decoded_source
    type(syntax_item_t) :: item, decoded_item
    type(sx_node_t) :: node
    character(len=256) :: message
    logical :: ok

    source%document = 'J3-24-007'
    source%clause = 'P6.1.2-3'
    source%rule = 'P6.1.2-3'
    source%page = 53
    source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    call roundtrip_source(source, decoded_source, ok, message)
    call require(ok, message)
    call require(decoded_source%page == 53 .and. &
        trim(decoded_source%source_hash) == trim(source%source_hash), &
        'generated source provenance differs')

    item%id = 'R501'
    item%lhs = 'program'
    item%source = source
    item%origin = ORIGIN_MECHANICAL
    item%resolution = RESOLUTION_RESOLVED
    call roundtrip_item(item, decoded_item, ok, message)
    call require(ok, message)
    call require(trim(decoded_item%id) == 'R501' .and. &
        decoded_item%origin == ORIGIN_MECHANICAL .and. &
        decoded_item%resolution == RESOLUTION_RESOLVED, &
        'generated syntax item differs')

    call sx_parse('(source-ref (document J3) (clause 1) (rule R1) (page 1))', node, ok, message)
    call require(ok, message)
    call schema_read_source_ref(node, decoded_source, ok, message)
    call require(.not. ok, 'generated source reader accepted missing source hash')

    print '(a)', 'generated schema API test passed'

contains

    subroutine roundtrip_source(value, decoded, value_ok, value_message)
        type(source_ref_t), intent(in) :: value
        type(source_ref_t), intent(out) :: decoded
        logical, intent(out) :: value_ok
        character(len=*), intent(out) :: value_message
        integer :: unit

        open (newunit=unit, status='scratch', action='readwrite')
        call schema_write_source_ref(value, unit, value_ok, value_message)
        if (.not. value_ok) return
        rewind (unit)
        call sx_parse('(source-ref (document J3-24-007) (clause P6.1.2-3) '// &
            '(rule P6.1.2-3) (page 53) (source-hash '//trim(value%source_hash)//'))', &
            node, value_ok, value_message)
        if (value_ok) call schema_read_source_ref(node, decoded, value_ok, value_message)
        close (unit)
    end subroutine roundtrip_source

    subroutine roundtrip_item(value, decoded, value_ok, value_message)
        type(syntax_item_t), intent(in) :: value
        type(syntax_item_t), intent(out) :: decoded
        logical, intent(out) :: value_ok
        character(len=*), intent(out) :: value_message
        type(sx_node_t) :: item_node

        call sx_parse('(syntax-item (id R501) (lhs program) '// &
            '(source (source-ref (document J3-24-007) (clause P6.1.2-3) '// &
            '(rule P6.1.2-3) (page 53) '// &
            '(source-hash 7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2))) '// &
            '(origin mechanical) (resolution resolved))', item_node, value_ok, value_message)
        if (value_ok) call schema_read_syntax_item(item_node, decoded, value_ok, value_message)
    end subroutine roundtrip_item

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_schema_generated_api

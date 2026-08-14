program test_standardir_semantic_consumer
    !! Fixed SX sequences are the independent consumer oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_export, only: standardir_resolution_disputed, &
        standardir_resolution_unresolved, &
        standardir_semantic_item_t
    use standardir_semantic_consumer, only: standardir_consume_semantic_items
    use standardir_semantic_table, only: semantic_table_find_source, semantic_table_t
    implicit none

    type(semantic_table_t) :: table
    type(standardir_semantic_item_t) :: actual
    type(sx_node_t) :: node
    character(len=256) :: message
    logical :: found, ok

    call sx_parse('(semantic-items '// &
        '(semantic-item (id S-unresolved) (subject allocation) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) '// &
        '(rule C1102) (page 88) (source-hash fixture-hash))) '// &
        '(origin human) (resolution unresolved)) '// &
        '(semantic-item (id S-disputed-a) (subject coarray) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) '// &
        '(rule C1103) (page 89) (source-hash fixture-hash))) '// &
        '(origin human) (resolution disputed)) '// &
        '(semantic-item (id S-disputed-b) (subject coarray-alias) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) '// &
        '(rule C1103) (page 89) (source-hash fixture-hash))) '// &
        '(origin human) (resolution disputed)))', node, ok, message)
    call require(ok, message)
    call standardir_consume_semantic_items(node, table, ok, message)
    call require(ok, message)
    call require(table%item_count == 3, 'semantic sequence count differs')

    call require(table%items(1)%id == 'S-unresolved' .and. &
        table%items(1)%resolution == standardir_resolution_unresolved .and. &
        table%items(1)%source%page == 88 .and. &
        table%items(1)%source%source_hash == 'fixture-hash', &
        'unresolved record lost status or provenance')
    call require(table%items(2)%resolution == standardir_resolution_disputed .and. &
        table%items(3)%resolution == standardir_resolution_disputed, &
        'disputed records lost status')
    call require(table%items(2)%source%rule == 'C1103' .and. &
        table%items(3)%source%rule == 'C1103' .and. &
        table%items(2)%source%page == table%items(3)%source%page, &
        'duplicate source provenance was changed')
    call semantic_table_find_source(table, table%items(2)%source, actual, found, ok, message)
    call require(.not. ok .and. .not. found, 'ambiguous source was silently resolved')

    call sx_parse('(semantic-items (semantic-item (id S-bad) (subject allocation) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1104) '// &
        '(page 0) (source-hash fixture-hash))) (origin human) '// &
        '(resolution unresolved)))', node, ok, message)
    call require(ok, message)
    call standardir_consume_semantic_items(node, table, ok, message)
    call require(.not. ok, 'incomplete source provenance was accepted')
    call require(table%item_count == 3, 'failed sequence mutated the table')

    call sx_parse('(semantic-items (semantic-item (id S-bad) (subject allocation) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1104) '// &
        '(page 90) (source-hash fixture-hash))) (origin human) '// &
        '(resolution ambiguous)))', node, ok, message)
    call require(ok, message)
    call standardir_consume_semantic_items(node, table, ok, message)
    call require(.not. ok, 'unknown resolution reached the table')
    call require(table%item_count == 3, 'invalid resolution mutated the table')

    print '(a)', 'StandardIR semantic consumer test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_semantic_consumer

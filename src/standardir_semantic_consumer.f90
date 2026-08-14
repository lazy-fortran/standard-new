module standardir_semantic_consumer
    !! Consume the generated source-linked semantic sequence into its table.

    use schema_v0_generated, only: semantic_item_t, &
        schema_consume_semantic_items_elements
    use standardir_export, only: standardir_semantic_item_t, &
        standardir_resolution_disputed, standardir_resolution_resolved, &
        standardir_resolution_unresolved
    use standardir_semantic_table, only: semantic_table_add, semantic_table_t, &
        semantic_table_validate
    use fortsx, only: sx_node_t
    implicit none
    private

    public :: standardir_consume_semantic_items

contains

    subroutine standardir_consume_semantic_items(node, table, ok, message)
        type(sx_node_t), intent(in) :: node
        type(semantic_table_t), intent(inout) :: table
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(semantic_table_t) :: candidate

        call semantic_table_validate(table, ok, message)
        if (.not. ok) return
        candidate = table
        call schema_consume_semantic_items_elements(node, consume_item, ok, message)
        if (.not. ok) return
        table = candidate
        ok = .true.
        message = ''

    contains

        subroutine consume_item(value, callback_ok, callback_message)
            type(semantic_item_t), intent(in) :: value
            logical, intent(out) :: callback_ok
            character(len=*), intent(out) :: callback_message

            type(standardir_semantic_item_t) :: item

            call convert_item(value, item)
            call semantic_table_add(candidate, item, callback_ok, callback_message)
        end subroutine consume_item

        subroutine convert_item(value, item)
            type(semantic_item_t), intent(in) :: value
            type(standardir_semantic_item_t), intent(out) :: item

            item%id = value%id
            item%subject = value%subject
            item%source%document = value%source%document
            item%source%clause = value%source%clause
            item%source%rule = value%source%rule
            item%source%page = value%source%page
            item%source%source_hash = value%source%source_hash
            item%origin = value%origin
            item%resolution = value%resolution
        end subroutine convert_item

    end subroutine standardir_consume_semantic_items

end module standardir_semantic_consumer

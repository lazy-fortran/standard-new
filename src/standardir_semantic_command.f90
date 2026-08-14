module standardir_semantic_command
    !! Validate and canonicalize one source-backed semantic-items SX record.

    use fortsx, only: sx_node_t, sx_parse
    use schema_v0_generated, only: schema_validate_semantic_items, &
        schema_write_semantic_items, semantic_item_t, semantic_items_t
    use standardir_export, only: standardir_semantic_item_t, &
        standardir_validate_semantic_item
    use standardir_semantic_consumer, only: standardir_consume_semantic_items
    use standardir_semantic_table, only: semantic_table_iterate, &
        semantic_table_t, semantic_table_validate
    implicit none
    private

    public :: standardir_canonicalize_semantic_items

contains

    subroutine standardir_canonicalize_semantic_items(input_path, output_path, ok, message)
        character(len=*), intent(in) :: input_path, output_path
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=65536) :: line
        type(sx_node_t) :: node
        type(semantic_items_t) :: generated
        type(semantic_table_t) :: table
        integer :: input_unit, output_unit, ios
        logical :: input_open, output_open, saw_record

        ok = .false.
        message = ''
        input_open = .false.
        output_open = .false.
        call delete_existing_output(output_path, ok, message)
        if (.not. ok) return

        open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open semantic-items SX input'
            return
        end if
        input_open = .true.
        saw_record = .false.
        do
            read (input_unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            if (saw_record) then
                call fail('semantic-items input must contain one nonempty SX record')
                return
            end if
            call sx_parse(line, node, ok, message)
            if (.not. ok) then
                call fail(message)
                return
            end if
            saw_record = .true.
        end do
        close (input_unit)
        input_open = .false.
        if (.not. saw_record) then
            call fail('semantic-items input contains no SX record')
            return
        end if

        call standardir_consume_semantic_items(node, table, ok, message)
        if (.not. ok) then
            call fail(message)
            return
        end if
        call semantic_table_validate(table, ok, message)
        if (.not. ok) then
            call fail(message)
            return
        end if
        call make_generated_items(table, generated, ok, message)
        if (.not. ok) then
            call fail(message)
            return
        end if
        call schema_validate_semantic_items(generated, ok, message)
        if (.not. ok) then
            call fail(message)
            return
        end if

        open (newunit=output_unit, file=trim(output_path), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open semantic-items SX output'
            return
        end if
        output_open = .true.
        call schema_write_semantic_items(generated, output_unit, ok, message)
        if (.not. ok) then
            call fail(message)
            return
        end if
        close (output_unit)
        output_open = .false.
        ok = .true.
        message = ''

    contains

        subroutine make_generated_items(source, target, result_ok, result_message)
            type(semantic_table_t), intent(in) :: source
            type(semantic_items_t), intent(out) :: target
            logical, intent(out) :: result_ok
            character(len=*), intent(out) :: result_message

            type(standardir_semantic_item_t) :: item
            integer :: cursor, i
            logical :: done

            result_ok = .false.
            result_message = ''
            if (source%item_count > 0) allocate (target%values(source%item_count))
            cursor = 0
            do i = 1, source%item_count
                call semantic_table_iterate(source, cursor, item, done, result_ok, &
                    result_message)
                if (.not. result_ok) return
                if (done) then
                    result_ok = .false.
                    result_message = 'semantic-items table ended before its item count'
                    return
                end if
                call standardir_validate_semantic_item(item, result_ok, result_message)
                if (.not. result_ok) return
                target%values(i)%id = item%id
                target%values(i)%subject = item%subject
                target%values(i)%source%document = item%source%document
                target%values(i)%source%clause = item%source%clause
                target%values(i)%source%rule = item%source%rule
                target%values(i)%source%page = item%source%page
                target%values(i)%source%source_hash = item%source%source_hash
                target%values(i)%origin = item%origin
                target%values(i)%resolution = item%resolution
            end do
            result_ok = .true.
            result_message = ''
        end subroutine make_generated_items

        subroutine delete_existing_output(path, result_ok, result_message)
            character(len=*), intent(in) :: path
            logical, intent(out) :: result_ok
            character(len=*), intent(out) :: result_message

            logical :: exists
            integer :: unit, delete_status

            inquire (file=trim(path), exist=exists)
            if (.not. exists) then
                result_ok = .true.
                result_message = ''
                return
            end if
            open (newunit=unit, file=trim(path), status='old', action='readwrite', &
                iostat=delete_status)
            if (delete_status /= 0) then
                result_ok = .false.
                result_message = 'cannot clear semantic-items SX output'
                return
            end if
            close (unit, status='delete', iostat=delete_status)
            result_ok = delete_status == 0
            result_message = ''
            if (.not. result_ok) result_message = 'cannot clear semantic-items SX output'
        end subroutine delete_existing_output

        subroutine fail(failure_message)
            character(len=*), intent(in) :: failure_message

            if (input_open) close (input_unit)
            if (output_open) close (output_unit, status='delete')
            call delete_existing_output(output_path, ok, message)
            if (.not. ok) return
            ok = .false.
            message = failure_message
        end subroutine fail

    end subroutine standardir_canonicalize_semantic_items

end module standardir_semantic_command

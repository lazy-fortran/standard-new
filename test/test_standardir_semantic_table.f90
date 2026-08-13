program test_standardir_semantic_table
    !! Fixed records are the independent oracle for table preservation.

    use standardir_export, only: standardir_origin_human, &
        standardir_resolution_disputed, standardir_resolution_resolved, &
        standardir_resolution_unresolved, standardir_semantic_item_t
    use standardir_semantic_table, only: semantic_table_add, semantic_table_iterate, &
        semantic_table_max_items, semantic_table_reset, semantic_table_t, &
        semantic_table_validate
    implicit none

    type(semantic_table_t) :: table
    type(standardir_semantic_item_t) :: expected, actual, invalid
    character(len=256) :: message
    character(len=12), parameter :: expected_ids(3) = [character(len=12) :: &
        'S-valid', 'S-unresolved', 'S-disputed']
    character(len=12), parameter :: expected_subjects(3) = [character(len=12) :: &
        'assignment', 'allocation', 'coarray']
    integer, parameter :: expected_resolutions(3) = [standardir_resolution_resolved, &
        standardir_resolution_unresolved, standardir_resolution_disputed]
    integer :: cursor, i
    logical :: ok, done

    call make_item(expected, 'S-valid', 'assignment', standardir_resolution_resolved)
    call semantic_table_add(table, expected, ok, message)
    call require(ok, message)
    call make_item(expected, 'S-unresolved', 'allocation', standardir_resolution_unresolved)
    call semantic_table_add(table, expected, ok, message)
    call require(ok, message)
    call make_item(expected, 'S-disputed', 'coarray', standardir_resolution_disputed)
    call semantic_table_add(table, expected, ok, message)
    call require(ok, message)
    call require(table%item_count == 3, 'table count differs')

    cursor = 0
    do i = 1, 3
        call semantic_table_iterate(table, cursor, actual, done, ok, message)
        call require(ok .and. .not. done, 'iteration ended too early')
        call require(actual%id == expected_ids(i) .and. &
            actual%subject == expected_subjects(i), 'iteration record differs')
        call require(actual%source%document == 'J3-24-007' .and. &
            actual%source%clause == '10.1' .and. actual%source%rule == 'C1102' .and. &
            actual%source%page == 88 .and. actual%source%source_hash == 'fixture-hash' .and. &
            actual%origin == standardir_origin_human .and. &
            actual%resolution == expected_resolutions(i), 'source or state changed')
    end do
    call semantic_table_iterate(table, cursor, actual, done, ok, message)
    call require(ok .and. done, 'iteration did not finish')

    invalid = table%items(1)
    invalid%subject = ''
    call semantic_table_add(table, invalid, ok, message)
    call require(.not. ok, 'invalid mutation was accepted')
    table%items(2)%resolution = 0
    call semantic_table_validate(table, ok, message)
    call require(.not. ok, 'invalid resolution mutation was accepted')
    table%items(2)%resolution = standardir_resolution_unresolved
    call semantic_table_reset(table)
    call require(table%item_count == 0, 'reset did not clear table')
    call semantic_table_validate(table, ok, message)
    call require(ok, message)

    do i = 1, semantic_table_max_items
        call make_item(expected, 'S-capacity', 'subject', standardir_resolution_resolved)
        call semantic_table_add(table, expected, ok, message)
        call require(ok, message)
    end do
    call semantic_table_add(table, expected, ok, message)
    call require(.not. ok, 'full table accepted an item')

    print '(a)', 'StandardIR semantic table test passed'

contains

    subroutine make_item(item, id, subject, resolution)
        type(standardir_semantic_item_t), intent(out) :: item
        character(len=*), intent(in) :: id, subject
        integer, intent(in) :: resolution

        item%id = id
        item%subject = subject
        item%source%document = 'J3-24-007'
        item%source%clause = '10.1'
        item%source%rule = 'C1102'
        item%source%page = 88
        item%source%source_hash = 'fixture-hash'
        item%origin = standardir_origin_human
        item%resolution = resolution
    end subroutine make_item

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_semantic_table

module standardir_semantic_table
    !! Bounded, insertion-ordered storage for source-backed semantic items.

    use standardir_export, only: standardir_semantic_item_t, &
        standardir_validate_semantic_item
    implicit none
    private

    integer, parameter, public :: semantic_table_max_items = 256

    type, public :: semantic_table_t
        integer :: item_count = 0
        type(standardir_semantic_item_t) :: items(semantic_table_max_items)
    end type semantic_table_t

    public :: semantic_table_add
    public :: semantic_table_reset
    public :: semantic_table_validate
    public :: semantic_table_iterate

contains

    subroutine semantic_table_add(table, item, ok, message)
        type(semantic_table_t), intent(inout) :: table
        type(standardir_semantic_item_t), intent(in) :: item
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call semantic_table_validate(table, ok, message)
        if (.not. ok) return
        call standardir_validate_semantic_item(item, ok, message)
        if (.not. ok) return
        if (table%item_count >= semantic_table_max_items) then
            ok = .false.
            message = 'semantic-item table is full'
            return
        end if
        table%item_count = table%item_count + 1
        table%items(table%item_count) = item
        ok = .true.
        message = ''
    end subroutine semantic_table_add

    subroutine semantic_table_reset(table)
        type(semantic_table_t), intent(inout) :: table

        table%item_count = 0
    end subroutine semantic_table_reset

    subroutine semantic_table_validate(table, ok, message)
        type(semantic_table_t), intent(in) :: table
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (table%item_count < 0 .or. table%item_count > semantic_table_max_items) then
            message = 'semantic-item table has an invalid item count'
            return
        end if
        do i = 1, table%item_count
            call standardir_validate_semantic_item(table%items(i), ok, message)
            if (.not. ok) then
                message = 'semantic-item table entry is invalid'
                return
            end if
        end do
        ok = .true.
    end subroutine semantic_table_validate

    subroutine semantic_table_iterate(table, cursor, item, done, ok, message)
        type(semantic_table_t), intent(in) :: table
        integer, intent(inout) :: cursor
        type(standardir_semantic_item_t), intent(out) :: item
        logical, intent(out) :: done, ok
        character(len=*), intent(out) :: message

        call semantic_table_validate(table, ok, message)
        if (.not. ok) then
            done = .false.
            return
        end if
        if (cursor < 0 .or. cursor > table%item_count) then
            ok = .false.
            done = .false.
            message = 'semantic-item table cursor is out of range'
            return
        end if
        if (cursor == table%item_count) then
            ok = .true.
            done = .true.
            item = standardir_semantic_item_t()
            message = ''
            return
        end if
        cursor = cursor + 1
        item = table%items(cursor)
        ok = .true.
        done = .false.
        message = ''
    end subroutine semantic_table_iterate

end module standardir_semantic_table

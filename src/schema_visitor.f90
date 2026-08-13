module schema_visitor
    !! Generic depth-first traversal boundary for schema values.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    public :: schema_visit_callback
    public :: schema_visit

    abstract interface
        subroutine schema_visit_callback(node, entering, ok, message)
            import :: sx_node_t
            type(sx_node_t), intent(in) :: node
            logical, intent(in) :: entering
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
        end subroutine schema_visit_callback
    end interface

contains

    recursive subroutine schema_visit(node, callback, ok, message)
        type(sx_node_t), intent(in) :: node
        procedure(schema_visit_callback) :: callback
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_atom .and. node%kind /= sx_list) then
            message = 'schema visitor received an invalid SX node'
            return
        end if
        if (node%kind == sx_list) then
            if (node%child_count < 0) then
                message = 'schema visitor received a negative child count'
                return
            end if
            if (node%child_count > 0 .and. .not. allocated(node%children)) then
                message = 'schema visitor received missing SX children'
                return
            end if
        end if

        call callback(node, .true., ok, message)
        if (.not. ok) return
        if (node%kind == sx_list) then
            do i = 1, node%child_count
                call schema_visit(node%children(i), callback, ok, message)
                if (.not. ok) return
            end do
        end if
        call callback(node, .false., ok, message)
    end subroutine schema_visit

end module schema_visitor

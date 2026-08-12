module standardir_grouping
    !! Group syntax records by lhs without changing their source order.

    use fortsx, only: sx_node_t
    use standardir_syntax_fields, only: standardir_read_syntax_header
    implicit none
    private

    integer, parameter, public :: standardir_max_syntax_records = 1024
    integer, parameter, public :: standardir_max_syntax_groups = 1024
    integer, parameter, public :: standardir_max_group_members = 128

    type, public :: standardir_group_t
        character(len=256) :: lhs = ''
        integer :: count = 0
        integer :: indices(standardir_max_group_members) = 0
    end type standardir_group_t

    public :: standardir_group_syntax

contains

    subroutine standardir_group_syntax(nodes, node_count, groups, group_count, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: node_count
        type(standardir_group_t), intent(out) :: groups(:)
        integer, intent(out) :: group_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule, lhs, document, clause, page, source_hash
        integer :: i, j, found

        group_count = 0
        ok = .false.
        message = ''
        if (node_count < 0 .or. node_count > size(nodes)) then
            message = 'syntax record count is outside the input array'
            return
        end if
        if (size(groups) < standardir_max_syntax_groups) then
            message = 'syntax group array is too small'
            return
        end if

        do i = 1, node_count
            call standardir_read_syntax_header(nodes(i), rule, lhs, document, clause, page, &
                source_hash, ok, message)
            if (.not. ok) return
            found = 0
            do j = 1, group_count
                if (trim(groups(j)%lhs) == trim(lhs)) then
                    found = j
                    exit
                end if
            end do
            if (found == 0) then
                if (group_count >= size(groups)) then
                    ok = .false.
                    message = 'syntax group limit exceeded'
                    return
                end if
                group_count = group_count + 1
                found = group_count
                groups(found)%lhs = trim(lhs)
                groups(found)%count = 0
                groups(found)%indices = 0
            end if
            if (groups(found)%count >= standardir_max_group_members) then
                ok = .false.
                message = 'syntax group member limit exceeded for '//trim(lhs)
                return
            end if
            groups(found)%count = groups(found)%count + 1
            groups(found)%indices(groups(found)%count) = i
        end do
        ok = .true.
        message = ''
    end subroutine standardir_group_syntax

end module standardir_grouping

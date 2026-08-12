module byte_buffer
    !! Owning, geometrically growing byte storage.
    !!
    !! Any span obtained from a buffer is invalid after a reserve or append
    !! that reallocates the buffer. Callers keep spans only while the storage
    !! remains unchanged.

    use, intrinsic :: iso_fortran_env, only: int8
    use byte_span, only: byte_span_empty, byte_span_from_array, byte_span_t
    implicit none
    private

    type, public :: byte_buffer_t
        private
        ! A pointer is required because byte_span_t is a non-owning view of
        ! this storage. The defined assignment below keeps copies independent.
        integer(int8), pointer :: storage(:) => null()
        integer :: length = 0
        integer :: capacity = 0
    contains
        final :: byte_buffer_finalize
    end type byte_buffer_t

    interface assignment(=)
        module procedure byte_buffer_assign
    end interface assignment(=)

    public :: assignment(=)

    public :: byte_buffer_append
    public :: byte_buffer_append_byte
    public :: byte_buffer_capacity
    public :: byte_buffer_clear
    public :: byte_buffer_init
    public :: byte_buffer_reserve
    public :: byte_buffer_size
    public :: byte_buffer_span

contains

    subroutine byte_buffer_finalize(buffer)
        type(byte_buffer_t), intent(inout) :: buffer

        if (associated(buffer%storage)) deallocate (buffer%storage)
        nullify (buffer%storage)
        buffer%length = 0
        buffer%capacity = 0
    end subroutine byte_buffer_finalize

    subroutine byte_buffer_assign(left, right)
        type(byte_buffer_t), intent(out) :: left
        type(byte_buffer_t), intent(in) :: right
        integer(int8), pointer :: copy(:)
        integer :: stat

        left%length = right%length
        left%capacity = right%capacity
        nullify (left%storage)
        if (right%capacity == 0) return
        allocate (copy(right%capacity), stat=stat)
        if (stat /= 0) error stop 'byte buffer assignment allocation failed'
        if (right%length > 0) then
            copy(1:right%length) = right%storage(1:right%length)
        end if
        left%storage => copy
        nullify (copy)
    end subroutine byte_buffer_assign

    subroutine byte_buffer_init(buffer, initial_capacity, ok, message)
        type(byte_buffer_t), intent(out) :: buffer
        integer, intent(in), optional :: initial_capacity
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: capacity

        buffer%length = 0
        buffer%capacity = 0
        if (associated(buffer%storage)) deallocate (buffer%storage)
        capacity = 0
        if (present(initial_capacity)) capacity = initial_capacity
        call byte_buffer_reserve(buffer, capacity, ok, message)
    end subroutine byte_buffer_init

    subroutine byte_buffer_clear(buffer)
        type(byte_buffer_t), intent(inout) :: buffer

        buffer%length = 0
    end subroutine byte_buffer_clear

    subroutine byte_buffer_reserve(buffer, minimum, ok, message)
        type(byte_buffer_t), intent(inout) :: buffer
        integer, intent(in) :: minimum
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8), pointer :: replacement(:)
        integer :: new_capacity, stat

        ok = .false.
        message = ''
        if (minimum < 0) then
            message = 'byte buffer capacity cannot be negative'
            return
        end if
        if (minimum <= buffer%capacity) then
            ok = .true.
            return
        end if

        new_capacity = buffer%capacity
        if (new_capacity == 0) new_capacity = 1
        do while (new_capacity < minimum)
            if (new_capacity > huge(new_capacity) - new_capacity) then
                new_capacity = minimum
                exit
            end if
            new_capacity = 2 * new_capacity
        end do
        allocate (replacement(new_capacity), stat=stat)
        if (stat /= 0) then
            message = 'could not allocate byte buffer storage'
            return
        end if
        if (buffer%length > 0) then
            replacement(1:buffer%length) = buffer%storage(1:buffer%length)
        end if
        if (associated(buffer%storage)) deallocate (buffer%storage)
        buffer%storage => replacement
        nullify (replacement)
        buffer%capacity = new_capacity
        ok = .true.
    end subroutine byte_buffer_reserve

    subroutine byte_buffer_append_byte(buffer, value, ok, message)
        type(byte_buffer_t), intent(inout) :: buffer
        integer(int8), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_buffer_reserve(buffer, buffer%length + 1, ok, message)
        if (.not. ok) return
        buffer%length = buffer%length + 1
        buffer%storage(buffer%length) = value
    end subroutine byte_buffer_append_byte

    subroutine byte_buffer_append(buffer, values, ok, message)
        type(byte_buffer_t), intent(inout) :: buffer
        integer(int8), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: count, first

        count = size(values)
        if (count == 0) then
            ok = .true.
            message = ''
            return
        end if
        if (buffer%length > huge(buffer%length) - count) then
            ok = .false.
            message = 'byte buffer size overflow'
            return
        end if
        call byte_buffer_reserve(buffer, buffer%length + count, ok, message)
        if (.not. ok) return
        first = buffer%length + 1
        buffer%storage(first:first + count - 1) = values
        buffer%length = buffer%length + count
    end subroutine byte_buffer_append

    integer function byte_buffer_size(buffer) result(length)
        type(byte_buffer_t), intent(in) :: buffer

        length = buffer%length
    end function byte_buffer_size

    integer function byte_buffer_capacity(buffer) result(capacity)
        type(byte_buffer_t), intent(in) :: buffer

        capacity = buffer%capacity
    end function byte_buffer_capacity

    subroutine byte_buffer_span(buffer, span, ok, message)
        type(byte_buffer_t), intent(inout) :: buffer
        type(byte_span_t), intent(out) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_span_empty(span)
        if (.not. associated(buffer%storage)) then
            ok = .true.
            message = ''
            return
        end if
        call byte_span_from_array(buffer%storage, 1, buffer%length, span, ok, message)
    end subroutine byte_buffer_span

end module byte_buffer

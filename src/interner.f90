module interner
    !! Deterministic case-insensitive identity interning for source names.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    implicit none
    private

    integer, parameter :: default_capacity = 16
    integer, parameter :: minimum_capacity = 4
    integer(int64), parameter :: hash_modulus = 4294967296_int64
    integer(int64), parameter :: fnv_offset = 2166136261_int64
    integer(int64), parameter :: fnv_prime = 16777619_int64

    type :: interner_entry_t
        integer :: id = 0
        integer(int8), allocatable :: key(:)
    end type interner_entry_t

    type, public :: interner_t
        private
        type(interner_entry_t), allocatable :: entries(:)
        integer, allocatable :: slots(:)
        integer :: count = 0
    end type interner_t

    public :: interner_count
    public :: interner_init
    public :: interner_intern
    public :: interner_key

contains

    subroutine interner_init(table, requested_capacity, ok, message)
        type(interner_t), intent(out) :: table
        integer, intent(in), optional :: requested_capacity
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: capacity, stat

        table%count = 0
        capacity = default_capacity
        if (present(requested_capacity)) capacity = max(minimum_capacity, requested_capacity)
        allocate (table%entries(capacity), table%slots(capacity), stat=stat)
        if (stat /= 0) then
            ok = .false.
            message = 'could not allocate interner table'
            return
        end if
        table%slots = 0
        ok = .true.
        message = ''
    end subroutine interner_init

    subroutine interner_intern(table, source, id, is_new, ok, message)
        type(interner_t), intent(inout) :: table
        type(byte_span_t), intent(in) :: source
        integer, intent(out) :: id
        logical, intent(out) :: is_new, ok
        character(len=*), intent(out) :: message
        integer(int8), allocatable :: key(:)
        integer :: slot, entry_index

        id = 0
        is_new = .false.
        call make_key(source, key, ok, message)
        if (.not. ok) return
        if (.not. allocated(table%slots)) then
            call interner_init(table, ok=ok, message=message)
            if (.not. ok) return
        end if
        if (10 * (table%count + 1) >= 7 * size(table%slots)) then
            call grow(table, ok, message)
            if (.not. ok) return
        end if

        call lookup(table%entries, table%slots, table%count, key, slot, entry_index)
        if (entry_index /= 0) then
            id = table%entries(entry_index)%id
            ok = .true.
            message = ''
            return
        end if

        table%count = table%count + 1
        table%entries(table%count)%id = table%count
        call move_alloc(key, table%entries(table%count)%key)
        table%slots(slot) = table%count
        id = table%count
        is_new = .true.
        ok = .true.
        message = ''
    end subroutine interner_intern

    subroutine interner_key(table, id, key, ok, message)
        type(interner_t), intent(in) :: table
        integer, intent(in) :: id
        integer(int8), allocatable, intent(out) :: key(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (id < 1 .or. id > table%count) then
            ok = .false.
            message = 'interner ID is outside the table'
            return
        end if
        allocate (key(size(table%entries(id)%key)))
        key = table%entries(id)%key
        ok = .true.
        message = ''
    end subroutine interner_key

    integer function interner_count(table) result(count)
        type(interner_t), intent(in) :: table

        count = table%count
    end function interner_count

    subroutine make_key(source, key, ok, message)
        type(byte_span_t), intent(in) :: source
        integer(int8), allocatable, intent(out) :: key(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8) :: value
        integer :: i, code

        if (byte_span_length(source) == 0) then
            ok = .false.
            message = 'cannot intern an empty source span'
            return
        end if
        allocate (key(byte_span_length(source)))
        do i = 1, size(key)
            call byte_span_get(source, i, value, ok, message)
            if (.not. ok) then
                deallocate (key)
                return
            end if
            code = int(value)
            if (code < 0) code = code + 256
            if (code >= iachar('A') .and. code <= iachar('Z')) code = code + 32
            key(i) = int(code, int8)
        end do
        ok = .true.
        message = ''
    end subroutine make_key

    subroutine lookup(entries, slots, count, key, slot, entry_index)
        type(interner_entry_t), intent(in) :: entries(:)
        integer, intent(in) :: slots(:), count
        integer(int8), intent(in) :: key(:)
        integer, intent(out) :: slot, entry_index
        integer(int64) :: hash
        integer :: probe, candidate

        hash = key_hash(key)
        slot = 1 + int(modulo(hash, int(size(slots), int64)))
        entry_index = 0
        do probe = 1, size(slots)
            candidate = slots(slot)
            if (candidate == 0) return
            if (candidate <= count) then
                if (keys_equal(entries(candidate)%key, key)) then
                    entry_index = candidate
                    return
                end if
            end if
            slot = 1 + modulo(slot, size(slots))
        end do
    end subroutine lookup

    subroutine grow(table, ok, message)
        type(interner_t), intent(inout) :: table
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(interner_entry_t), allocatable :: new_entries(:)
        integer, allocatable :: new_slots(:)
        integer :: capacity, stat, i, slot, unused

        capacity = 2 * size(table%slots)
        allocate (new_entries(capacity), new_slots(capacity), stat=stat)
        if (stat /= 0) then
            ok = .false.
            message = 'could not grow interner table'
            return
        end if
        new_slots = 0
        if (table%count > 0) new_entries(1:table%count) = table%entries(1:table%count)
        do i = 1, table%count
            call lookup(new_entries, new_slots, table%count, new_entries(i)%key, slot, unused)
            new_slots(slot) = i
        end do
        call move_alloc(new_entries, table%entries)
        call move_alloc(new_slots, table%slots)
        ok = .true.
        message = ''
    end subroutine grow

    integer(int64) function key_hash(key) result(hash)
        integer(int8), intent(in) :: key(:)
        integer :: i, value

        hash = fnv_offset
        do i = 1, size(key)
            value = int(key(i))
            if (value < 0) value = value + 256
            hash = ieor(hash, int(value, int64))
            hash = modulo(hash * fnv_prime, hash_modulus)
        end do
    end function key_hash

    logical function keys_equal(left, right) result(equal)
        integer(int8), intent(in) :: left(:), right(:)

        equal = size(left) == size(right)
        if (.not. equal) return
        equal = all(left == right)
    end function keys_equal

end module interner

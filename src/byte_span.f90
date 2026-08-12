module byte_span
    !! A non-owning view over immutable byte storage.

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    type, public :: byte_span_t
        private
        integer(int8), pointer :: bytes(:) => null()
        integer :: first = 1
        integer :: length = 0
    end type byte_span_t

    public :: byte_span_empty
    public :: byte_span_equal
    public :: byte_span_from_array
    public :: byte_span_get
    public :: byte_span_is_empty
    public :: byte_span_length
    public :: byte_span_slice

contains

    subroutine byte_span_empty(span)
        type(byte_span_t), intent(out) :: span

        nullify (span%bytes)
        span%first = 1
        span%length = 0
    end subroutine byte_span_empty

    subroutine byte_span_from_array(bytes, first, length, span, ok, message)
        integer(int8), target, intent(in) :: bytes(:)
        integer, intent(in) :: first, length
        type(byte_span_t), intent(out) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_span_empty(span)
        ok = .false.
        message = ''
        if (first < 1) then
            message = 'byte span starts before the source array'
            return
        end if
        if (length < 0) then
            message = 'byte span has a negative length'
            return
        end if
        if (first > size(bytes) + 1) then
            message = 'byte span starts after the source array'
            return
        end if
        if (length > 0) then
            if (first + length - 1 > size(bytes)) then
                message = 'byte span extends past the source array'
                return
            end if
        end if

        span%bytes => bytes
        span%first = first
        span%length = length
        ok = .true.
    end subroutine byte_span_from_array

    subroutine byte_span_slice(parent, first, length, span, ok, message)
        type(byte_span_t), intent(in) :: parent
        integer, intent(in) :: first, length
        type(byte_span_t), intent(out) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_span_empty(span)
        ok = .false.
        message = ''
        if (first < 1) then
            message = 'byte span slice starts before the parent span'
            return
        end if
        if (length < 0) then
            message = 'byte span slice has a negative length'
            return
        end if
        if (first > parent%length + 1) then
            message = 'byte span slice starts after the parent span'
            return
        end if
        if (length > 0) then
            if (first + length - 1 > parent%length) then
                message = 'byte span slice extends past the parent span'
                return
            end if
        end if
        if (parent%length > 0) then
            if (.not. associated(parent%bytes)) then
                message = 'non-empty byte span has no storage'
                return
            end if
            span%bytes => parent%bytes
        else
            nullify (span%bytes)
        end if
        span%first = parent%first + first - 1
        span%length = length
        ok = .true.
    end subroutine byte_span_slice

    integer function byte_span_length(span) result(length)
        type(byte_span_t), intent(in) :: span

        length = span%length
    end function byte_span_length

    logical function byte_span_is_empty(span) result(is_empty)
        type(byte_span_t), intent(in) :: span

        is_empty = span%length == 0
    end function byte_span_is_empty

    subroutine byte_span_get(span, index, value, ok, message)
        type(byte_span_t), intent(in) :: span
        integer, intent(in) :: index
        integer(int8), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = 0_int8
        ok = .false.
        message = ''
        if (index < 1) then
            message = 'byte span index is below one'
            return
        end if
        if (index > span%length) then
            message = 'byte span index is outside the span'
            return
        end if
        if (.not. associated(span%bytes)) then
            message = 'byte span has no storage'
            return
        end if
        value = span%bytes(span%first + index - 1)
        ok = .true.
    end subroutine byte_span_get

    logical function byte_span_equal(left, right) result(equal)
        type(byte_span_t), intent(in) :: left, right
        integer :: i

        equal = .false.
        if (left%length /= right%length) return
        if (left%length == 0) then
            equal = .true.
            return
        end if
        if (.not. associated(left%bytes)) return
        if (.not. associated(right%bytes)) return
        do i = 1, left%length
            if (left%bytes(left%first + i - 1) /= &
                right%bytes(right%first + i - 1)) return
        end do
        equal = .true.
    end function byte_span_equal

end module byte_span

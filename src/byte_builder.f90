module byte_builder
    !! Byte-oriented construction on top of the owning buffer primitive.

    use, intrinsic :: iso_fortran_env, only: int8
    use byte_buffer, only: byte_buffer_append, byte_buffer_append_byte, &
        byte_buffer_clear, byte_buffer_init, byte_buffer_size, &
        byte_buffer_span, byte_buffer_t
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    implicit none
    private

    type, public :: byte_builder_t
        private
        type(byte_buffer_t) :: buffer
    end type byte_builder_t

    public :: byte_builder_append
    public :: byte_builder_append_ascii
    public :: byte_builder_append_byte
    public :: byte_builder_append_newline
    public :: byte_builder_append_span
    public :: byte_builder_clear
    public :: byte_builder_init
    public :: byte_builder_size
    public :: byte_builder_span

contains

    subroutine byte_builder_init(builder, initial_capacity, ok, message)
        type(byte_builder_t), intent(out) :: builder
        integer, intent(in), optional :: initial_capacity
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (present(initial_capacity)) then
            call byte_buffer_init(builder%buffer, initial_capacity, ok, message)
        else
            call byte_buffer_init(builder%buffer, ok=ok, message=message)
        end if
    end subroutine byte_builder_init

    subroutine byte_builder_clear(builder)
        type(byte_builder_t), intent(inout) :: builder

        call byte_buffer_clear(builder%buffer)
    end subroutine byte_builder_clear

    subroutine byte_builder_append_byte(builder, value, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        integer(int8), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_buffer_append_byte(builder%buffer, value, ok, message)
    end subroutine byte_builder_append_byte

    subroutine byte_builder_append(builder, values, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        integer(int8), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_buffer_append(builder%buffer, values, ok, message)
    end subroutine byte_builder_append

    subroutine byte_builder_append_span(builder, span, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        type(byte_span_t), intent(in) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8) :: value
        integer :: i

        ok = .true.
        message = ''
        do i = 1, byte_span_length(span)
            call byte_span_get(span, i, value, ok, message)
            if (.not. ok) return
            call byte_builder_append_byte(builder, value, ok, message)
            if (.not. ok) return
        end do
    end subroutine byte_builder_append_span

    subroutine byte_builder_append_ascii(builder, text, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: code, i

        ok = .true.
        message = ''
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code > 127) then
                ok = .false.
                message = 'non-ASCII character passed to byte builder'
                return
            end if
            call byte_builder_append_byte(builder, int(code, int8), ok, message)
            if (.not. ok) return
        end do
    end subroutine byte_builder_append_ascii

    subroutine byte_builder_append_newline(builder, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_builder_append_byte(builder, int(10, int8), ok, message)
    end subroutine byte_builder_append_newline

    integer function byte_builder_size(builder) result(length)
        type(byte_builder_t), intent(in) :: builder

        length = byte_buffer_size(builder%buffer)
    end function byte_builder_size

    subroutine byte_builder_span(builder, span, ok, message)
        type(byte_builder_t), intent(inout) :: builder
        type(byte_span_t), intent(out) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call byte_buffer_span(builder%buffer, span, ok, message)
    end subroutine byte_builder_span

end module byte_builder

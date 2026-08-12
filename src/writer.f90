module writer
    !! Byte writer with file, memory, hash and counting backends.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use byte_builder, only: byte_builder_append, byte_builder_append_span, &
        byte_builder_init, byte_builder_size, byte_builder_span, byte_builder_t
    use byte_span, only: byte_span_get, byte_span_length, byte_span_t
    use sha256, only: sha256_context_t, sha256_final, sha256_init, sha256_update
    implicit none
    private

    integer, parameter, public :: writer_file = 1
    integer, parameter, public :: writer_memory = 2
    integer, parameter, public :: writer_hash = 3
    integer, parameter, public :: writer_counting = 4

    type, public :: writer_t
        private
        integer :: backend = 0
        integer :: unit = -1
        integer(int64) :: count = 0_int64
        type(byte_builder_t) :: memory
        type(sha256_context_t) :: hash
        logical :: closed = .true.
    end type writer_t

    public :: writer_close
    public :: writer_digest
    public :: writer_init_counting
    public :: writer_init_file
    public :: writer_init_hash
    public :: writer_init_memory
    public :: writer_memory_span
    public :: writer_size
    public :: writer_write_ascii
    public :: writer_write_byte
    public :: writer_write_bytes
    public :: writer_write_newline
    public :: writer_write_span

contains

    subroutine writer_init_file(output, unit, ok, message)
        type(writer_t), intent(out) :: output
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_reset(output)
        output%backend = writer_file
        output%unit = unit
        output%closed = .false.
        ok = .true.
        message = ''
    end subroutine writer_init_file

    subroutine writer_init_memory(output, initial_capacity, ok, message)
        type(writer_t), intent(out) :: output
        integer, intent(in), optional :: initial_capacity
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_reset(output)
        if (present(initial_capacity)) then
            call byte_builder_init(output%memory, initial_capacity, ok, message)
        else
            call byte_builder_init(output%memory, ok=ok, message=message)
        end if
        if (.not. ok) return
        output%backend = writer_memory
        output%closed = .false.
    end subroutine writer_init_memory

    subroutine writer_init_hash(output, ok, message)
        type(writer_t), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_reset(output)
        call sha256_init(output%hash)
        output%backend = writer_hash
        output%closed = .false.
        ok = .true.
        message = ''
    end subroutine writer_init_hash

    subroutine writer_init_counting(output, ok, message)
        type(writer_t), intent(out) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_reset(output)
        output%backend = writer_counting
        output%closed = .false.
        ok = .true.
        message = ''
    end subroutine writer_init_counting

    subroutine writer_reset(output)
        type(writer_t), intent(inout) :: output

        output%backend = 0
        output%unit = -1
        output%count = 0_int64
        output%closed = .true.
    end subroutine writer_reset

    subroutine writer_close(output, ok, message)
        type(writer_t), intent(inout) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (output%closed) then
            ok = .true.
            message = ''
            return
        end if
        output%closed = .true.
        ok = .true.
        message = ''
    end subroutine writer_close

    subroutine writer_write_byte(output, value, ok, message)
        type(writer_t), intent(inout) :: output
        integer(int8), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8) :: one(1)

        one(1) = value
        call writer_write_bytes(output, one, ok, message)
    end subroutine writer_write_byte

    subroutine writer_write_bytes(output, bytes, ok, message)
        type(writer_t), intent(inout) :: output
        integer(int8), intent(in) :: bytes(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        ok = .false.
        message = ''
        if (output%closed .or. output%backend == 0) then
            message = 'writer is not initialized or is closed'
            return
        end if
        if (size(bytes) == 0) then
            ok = .true.
            return
        end if
        select case (output%backend)
        case (writer_file)
            write (output%unit, iostat=ios) bytes
            if (ios /= 0) then
                message = 'writer file write failed'
                return
            end if
        case (writer_memory)
            call byte_builder_append(output%memory, bytes, ok, message)
            if (.not. ok) return
        case (writer_hash)
            call sha256_update(output%hash, bytes)
        case (writer_counting)
        case default
            message = 'writer backend is invalid'
            return
        end select
        output%count = output%count + int(size(bytes), int64)
        ok = .true.
    end subroutine writer_write_bytes

    subroutine writer_write_span(output, span, ok, message)
        type(writer_t), intent(inout) :: output
        type(byte_span_t), intent(in) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer(int8) :: value
        integer :: i

        ok = .true.
        message = ''
        if (output%closed .or. output%backend == 0) then
            ok = .false.
            message = 'writer is not initialized or is closed'
            return
        end if
        select case (output%backend)
        case (writer_memory)
            call byte_builder_append_span(output%memory, span, ok, message)
            if (ok) output%count = int(byte_builder_size(output%memory), int64)
        case default
            do i = 1, byte_span_length(span)
                call byte_span_get(span, i, value, ok, message)
                if (.not. ok) return
                call writer_write_byte(output, value, ok, message)
                if (.not. ok) return
            end do
        end select
    end subroutine writer_write_span

    subroutine writer_write_ascii(output, text, ok, message)
        type(writer_t), intent(inout) :: output
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
                message = 'non-ASCII character passed to writer'
                return
            end if
            call writer_write_byte(output, int(code, int8), ok, message)
            if (.not. ok) return
        end do
    end subroutine writer_write_ascii

    subroutine writer_write_newline(output, ok, message)
        type(writer_t), intent(inout) :: output
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_write_byte(output, int(10, int8), ok, message)
    end subroutine writer_write_newline

    integer(int64) function writer_size(output) result(count)
        type(writer_t), intent(in) :: output

        count = output%count
    end function writer_size

    subroutine writer_memory_span(output, span, ok, message)
        type(writer_t), intent(inout) :: output
        type(byte_span_t), intent(out) :: span
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (output%backend /= writer_memory) then
            ok = .false.
            message = 'writer does not have a memory backend'
            return
        end if
        call byte_builder_span(output%memory, span, ok, message)
    end subroutine writer_memory_span

    subroutine writer_digest(output, digest, ok, message)
        type(writer_t), intent(in) :: output
        integer(int8), intent(out) :: digest(32)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        digest = 0_int8
        if (output%backend /= writer_hash) then
            ok = .false.
            message = 'writer does not have a hash backend'
            return
        end if
        call sha256_final(output%hash, digest)
        ok = .true.
        message = ''
    end subroutine writer_digest

end module writer

module schema_value_runtime
    !! Generic byte and tree primitives used by generated schema APIs.
    !!
    !! The schema generator owns declaration order, dispatch and field wiring.
    !! This module owns the small operations that are common to every emitted
    !! reader and writer.  It is also deliberately independent of schema_ir.

    use fortsx, only: sx_atom, sx_clear, sx_list, sx_node_t
    implicit none
    private

    public :: schema_runtime_close_list
    public :: schema_runtime_error
    public :: schema_runtime_expect_list
    public :: schema_runtime_finish
    public :: schema_runtime_list_element
    public :: schema_runtime_open_list
    public :: schema_runtime_read_atom
    public :: schema_runtime_read_bool
    public :: schema_runtime_read_int
    public :: schema_runtime_read_name
    public :: schema_runtime_read_optional
    public :: schema_runtime_read_string
    public :: schema_runtime_read_variant
    public :: schema_runtime_record_field
    public :: schema_runtime_validate_name
    public :: schema_runtime_write_atom
    public :: schema_runtime_write_bool
    public :: schema_runtime_write_int
    public :: schema_runtime_write_name
    public :: schema_runtime_write_none
    public :: schema_runtime_write_space
    public :: schema_runtime_write_string

contains

    subroutine schema_runtime_open_list(unit, tag, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: tag
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call write_text(unit, '(', ok, message)
        if (.not. ok) return
        if (len_trim(tag) > 0) call write_text(unit, trim(tag), ok, message)
    end subroutine schema_runtime_open_list

    subroutine schema_runtime_write_space(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call write_text(unit, ' ', ok, message)
    end subroutine schema_runtime_write_space

    subroutine schema_runtime_close_list(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call write_text(unit, ')', ok, message)
    end subroutine schema_runtime_close_list

    subroutine schema_runtime_finish(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        write (unit, '(a)', iostat=ios) ''
        ok = ios == 0
        message = ''
        if (.not. ok) message = 'cannot finish generated schema value'
    end subroutine schema_runtime_finish

    subroutine schema_runtime_write_atom(unit, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (.not. is_plain_atom(value)) then
            call schema_runtime_error('generated atom is not canonical', ok, message)
            return
        end if
        call write_text(unit, trim(value), ok, message)
    end subroutine schema_runtime_write_atom

    subroutine schema_runtime_write_bool(unit, value, ok, message)
        integer, intent(in) :: unit
        logical, intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (value) then
            call write_text(unit, 'true', ok, message)
        else
            call write_text(unit, 'false', ok, message)
        end if
    end subroutine schema_runtime_write_bool

    subroutine schema_runtime_write_int(unit, value, ok, message)
        integer, intent(in) :: unit, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=64) :: text

        write (text, '(i0)') value
        call write_text(unit, trim(text), ok, message)
    end subroutine schema_runtime_write_int

    subroutine schema_runtime_write_string(unit, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call write_text(unit, '"', ok, message)
        if (.not. ok) return
        do i = 1, len_trim(value)
            if (value(i:i) == '"' .or. value(i:i) == achar(92)) then
                call write_text(unit, achar(92), ok, message)
                if (.not. ok) return
            end if
            call write_text(unit, value(i:i), ok, message)
            if (.not. ok) return
        end do
        call write_text(unit, '"', ok, message)
    end subroutine schema_runtime_write_string

    subroutine schema_runtime_write_name(unit, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call schema_runtime_write_atom(unit, value, ok, message)
    end subroutine schema_runtime_write_name

    subroutine schema_runtime_validate_name(value, ok, message)
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (.not. is_plain_atom(value)) then
            call schema_runtime_error('name is not a canonical atom', ok, message)
            return
        end if
        ok = .true.
        message = ''
    end subroutine schema_runtime_validate_name

    subroutine schema_runtime_write_none(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call write_text(unit, 'none', ok, message)
    end subroutine schema_runtime_write_none

    subroutine schema_runtime_read_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'generated schema value is not an atom'
            return
        end if
        value = trim(node%atom)
        ok = .true.
    end subroutine schema_runtime_read_atom

    subroutine schema_runtime_read_bool(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: atom

        call schema_runtime_read_atom(node, atom, ok, message)
        if (.not. ok) return
        select case (trim(atom))
        case ('true')
            value = .true.
        case ('false')
            value = .false.
        case default
            call schema_runtime_error('boolean schema value is not true or false', ok, &
                message)
            return
        end select
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_bool

    subroutine schema_runtime_read_int(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: atom
        integer :: ios

        call schema_runtime_read_atom(node, atom, ok, message)
        if (.not. ok) return
        if (.not. is_decimal_integer(atom)) then
            call schema_runtime_error('integer schema value is not canonical decimal', ok, &
                message)
            return
        end if
        read (atom, *, iostat=ios) value
        if (ios /= 0) then
            call schema_runtime_error('integer schema value is out of range', ok, message)
            return
        end if
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_int

    subroutine schema_runtime_read_string(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: atom

        call schema_runtime_read_atom(node, atom, ok, message)
        if (.not. ok) return
        value = trim(atom)
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_string

    subroutine schema_runtime_read_name(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: atom
        call schema_runtime_read_atom(node, atom, ok, message)
        if (.not. ok) return
        if (.not. is_plain_atom(atom)) then
            call schema_runtime_error('name schema value is not a canonical atom', ok, &
                message)
            return
        end if
        value = trim(atom)
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_name

    subroutine schema_runtime_expect_list(node, tag, expected_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        integer, intent(in) :: expected_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: actual

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'generated schema value is not a list'
            return
        end if
        if (node%child_count < 1) then
            message = 'generated schema list has no declaration tag'
            return
        end if
        if (expected_count >= 0) then
            if (node%child_count /= expected_count) then
                message = 'generated schema list has the wrong field count'
                return
            end if
        end if
        call schema_runtime_read_atom(node%children(1), actual, ok, message)
        if (.not. ok) return
        if (trim(actual) /= trim(tag)) then
            message = 'generated schema list has the wrong declaration tag'
            ok = .false.
            return
        end if
        ok = .true.
        message = ''
    end subroutine schema_runtime_expect_list

    subroutine schema_runtime_record_field(node, index, name, field, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: index
        character(len=*), intent(in) :: name
        type(sx_node_t), intent(out) :: field
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: actual

        call sx_clear(field)
        ok = .false.
        message = ''
        if (index < 1 .or. index + 1 > node%child_count) then
            message = 'generated schema record field index is invalid'
            return
        end if
        if (node%children(index + 1)%kind /= sx_list) then
            message = 'generated schema record field is not a pair'
            return
        end if
        if (node%children(index + 1)%child_count /= 2) then
            message = 'generated schema record field pair has the wrong length'
            return
        end if
        call schema_runtime_read_atom(node%children(index + 1)%children(1), actual, ok, message)
        if (.not. ok) return
        if (trim(actual) /= trim(name)) then
            call schema_runtime_error('generated schema record fields are out of order', ok, &
                message)
            return
        end if
        field = node%children(index + 1)%children(2)
        ok = .true.
        message = ''
    end subroutine schema_runtime_record_field

    subroutine schema_runtime_list_element(node, index, element, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: index
        type(sx_node_t), intent(out) :: element
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call sx_clear(element)
        ok = .false.
        message = ''
        if (index < 1 .or. index + 1 > node%child_count) then
            message = 'generated schema list element index is invalid'
            return
        end if
        element = node%children(index + 1)
        ok = .true.
    end subroutine schema_runtime_list_element

    subroutine schema_runtime_read_variant(node, tag, payload, has_payload, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: tag
        type(sx_node_t), intent(out) :: payload
        logical, intent(out) :: has_payload
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call sx_clear(payload)
        tag = ''
        has_payload = .false.
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'generated schema sum value is not a list'
            return
        end if
        if (node%child_count < 1 .or. node%child_count > 2) then
            message = 'generated schema sum value has the wrong field count'
            return
        end if
        call schema_runtime_read_atom(node%children(1), tag, ok, message)
        if (.not. ok) return
        if (node%child_count == 2) then
            payload = node%children(2)
            has_payload = .true.
        end if
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_variant

    subroutine schema_runtime_read_optional(node, present, payload, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: present
        type(sx_node_t), intent(out) :: payload
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: tag

        call sx_clear(payload)
        present = .false.
        ok = .false.
        message = ''
        if (node%kind == sx_atom) then
            call schema_runtime_read_atom(node, tag, ok, message)
            if (.not. ok) return
            if (trim(tag) /= 'none') then
                call schema_runtime_error('optional absence must be none', ok, message)
                return
            end if
            ok = .true.
            return
        end if
        if (node%kind /= sx_list) then
            message = 'optional value is not none or some'
            return
        end if
        if (node%child_count /= 2) then
            message = 'optional presence must be (some value)'
            return
        end if
        call schema_runtime_read_atom(node%children(1), tag, ok, message)
        if (.not. ok) return
        if (trim(tag) /= 'some') then
            call schema_runtime_error('optional presence must be (some value)', ok, message)
            return
        end if
        payload = node%children(2)
        present = .true.
        ok = .true.
        message = ''
    end subroutine schema_runtime_read_optional

    subroutine schema_runtime_error(text, ok, message)
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = text
    end subroutine schema_runtime_error

    subroutine write_text(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        write (unit, '(a)', advance='no', iostat=ios) text
        ok = ios == 0
        message = ''
        if (.not. ok) message = 'cannot write generated schema value'
    end subroutine write_text

    logical function is_plain_atom(value)
        character(len=*), intent(in) :: value
        integer :: i

        is_plain_atom = len_trim(value) > 0
        if (.not. is_plain_atom) return
        do i = 1, len_trim(value)
            if (value(i:i) == ' ' .or. value(i:i) == achar(9) .or. &
                value(i:i) == '(' .or. value(i:i) == ')' .or. &
                value(i:i) == '"' .or. value(i:i) == achar(92)) then
                is_plain_atom = .false.
                return
            end if
        end do
    end function is_plain_atom

    logical function is_decimal_integer(value)
        character(len=*), intent(in) :: value
        integer :: first, i, n, code

        n = len_trim(value)
        is_decimal_integer = n > 0
        if (.not. is_decimal_integer) return
        first = 1
        if (value(1:1) == '-') then
            first = 2
            if (first > n) then
                is_decimal_integer = .false.
                return
            end if
        end if
        if (n - first + 1 > 1 .and. value(first:first) == '0') then
            is_decimal_integer = .false.
            return
        end if
        do i = first, n
            code = iachar(value(i:i))
            if (code < iachar('0') .or. code > iachar('9')) then
                is_decimal_integer = .false.
                return
            end if
        end do
    end function is_decimal_integer

end module schema_value_runtime

module standardir_grammar_source_fingerprint
    !! Canonical SHA-256 fingerprints for source grammar expressions.

    use, intrinsic :: iso_fortran_env, only: int8, int32
    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_validate
    use writer, only: writer_digest, writer_init_hash, writer_t, writer_write_byte, &
        writer_write_newline
    implicit none
    private

    public :: standardir_grammar_source_expression_sha256

contains

    subroutine standardir_grammar_source_expression_sha256(expression, fingerprint, ok, message)
        type(sx_node_t), intent(in) :: expression
        character(len=*), intent(out) :: fingerprint
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer(int8) :: digest(32)
        type(writer_t) :: output
        integer(int32) :: value, high, low
        integer :: i
        character(len=16), parameter :: digits = '0123456789abcdef'
        logical :: valid

        fingerprint = ''
        ok = .false.
        message = ''
        call sx_validate(expression, valid, message)
        if (.not. valid) return
        call writer_init_hash(output, ok, message)
        if (.not. ok) return
        call write_utf8_compatible_node(output, expression, ok, message)
        if (.not. ok) return
        call writer_write_newline(output, ok, message)
        if (.not. ok) return
        call writer_digest(output, digest, ok, message)
        if (.not. ok) return
        if (len(fingerprint) < 64) then
            message = 'source-expression fingerprint buffer is too short'
            return
        end if
        do i = 1, 32
            value = int(digest(i), int32)
            if (value < 0) value = value + 256
            high = shiftr(value, 4)
            low = iand(value, 15_int32)
            fingerprint(2 * i - 1:2 * i - 1) = digits(high + 1:high + 1)
            fingerprint(2 * i:2 * i) = digits(low + 1:low + 1)
        end do
        fingerprint = fingerprint(:64)
        ok = .true.
        message = ''
    end subroutine standardir_grammar_source_expression_sha256

    recursive subroutine write_utf8_compatible_node(output, node, ok, message)
        type(writer_t), intent(inout) :: output
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        ok = .false.
        message = ''
        select case (node%kind)
        case (sx_atom)
            call write_utf8_compatible_atom(output, node%atom, ok, message)
        case (sx_list)
            call write_byte(output, '(', ok, message)
            if (.not. ok) return
            do i = 1, node%child_count
                if (i > 1) then
                    call write_byte(output, ' ', ok, message)
                    if (.not. ok) return
                end if
                call write_utf8_compatible_node(output, node%children(i), ok, message)
                if (.not. ok) return
            end do
            call write_byte(output, ')', ok, message)
        case default
            message = 'invalid SX node kind'
        end select
    end subroutine write_utf8_compatible_node

    subroutine write_utf8_compatible_atom(output, atom, ok, message)
        type(writer_t), intent(inout) :: output
        character(len=*), intent(in) :: atom
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, n, code
        logical :: quoted

        n = len_trim(atom)
        quoted = n == 0
        do i = 1, n
            if (atom(i:i) == ' ' .or. atom(i:i) == achar(9) .or. &
                atom(i:i) == '(' .or. atom(i:i) == ')' .or. &
                atom(i:i) == '"' .or. atom(i:i) == achar(92)) quoted = .true.
        end do
        ok = .true.
        message = ''
        if (quoted) then
            call write_byte(output, '"', ok, message)
            if (.not. ok) return
        end if
        do i = 1, n
            if (quoted .and. (atom(i:i) == '"' .or. atom(i:i) == achar(92))) then
                call write_byte(output, achar(92), ok, message)
                if (.not. ok) return
            end if
            code = iachar(atom(i:i))
            if (code < 0) then
                ok = .false.
                message = 'SX atom character code is invalid'
                return
            end if
            if (code <= 255) then
                call writer_write_byte(output, int(code, int8), ok, message)
            else
                call write_utf8_codepoint(output, code, ok, message)
            end if
            if (.not. ok) return
        end do
        if (quoted) call write_byte(output, '"', ok, message)
    end subroutine write_utf8_compatible_atom

    subroutine write_utf8_codepoint(output, code, ok, message)
        type(writer_t), intent(inout) :: output
        integer, intent(in) :: code
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: first, second, third, fourth

        ok = .false.
        message = ''
        if (code < 0 .or. code > int(z'10ffff')) then
            message = 'SX atom codepoint is outside Unicode'
            return
        end if
        if (code >= int(z'd800') .and. code <= int(z'dfff')) then
            message = 'SX atom codepoint is a Unicode surrogate'
            return
        end if
        if (code <= 127) then
            call writer_write_byte(output, int(code, int8), ok, message)
        else if (code <= int(z'7ff')) then
            first = 192 + shiftr(code, 6)
            second = 128 + iand(code, 63)
            call writer_write_byte(output, int(first, int8), ok, message)
            if (ok) call writer_write_byte(output, int(second, int8), ok, message)
        else if (code <= int(z'ffff')) then
            first = 224 + shiftr(code, 12)
            second = 128 + iand(shiftr(code, 6), 63)
            third = 128 + iand(code, 63)
            call writer_write_byte(output, int(first, int8), ok, message)
            if (ok) call writer_write_byte(output, int(second, int8), ok, message)
            if (ok) call writer_write_byte(output, int(third, int8), ok, message)
        else
            first = 240 + shiftr(code, 18)
            second = 128 + iand(shiftr(code, 12), 63)
            third = 128 + iand(shiftr(code, 6), 63)
            fourth = 128 + iand(code, 63)
            call writer_write_byte(output, int(first, int8), ok, message)
            if (ok) call writer_write_byte(output, int(second, int8), ok, message)
            if (ok) call writer_write_byte(output, int(third, int8), ok, message)
            if (ok) call writer_write_byte(output, int(fourth, int8), ok, message)
        end if
    end subroutine write_utf8_codepoint

    subroutine write_byte(output, value, ok, message)
        type(writer_t), intent(inout) :: output
        character(len=1), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call writer_write_byte(output, int(iachar(value), int8), ok, message)
    end subroutine write_byte

end module standardir_grammar_source_fingerprint

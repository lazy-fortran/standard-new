program test_fortsx_hash
    !! The SHA-256 vector is independently computed from canonical SX bytes.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use fortsx, only: sx_node_t, sx_parse, sx_validate, sx_write_writer
    use writer, only: writer_digest, writer_init_hash, writer_size, writer_t
    implicit none

    character(len=*), parameter :: input = '(list   "a b" "a\"b"    )'
    integer(int8), parameter :: expected_digest(32) = [ &
        int(z'F8', int8), int(z'16', int8), int(z'4D', int8), int(z'47', int8), &
        int(z'FE', int8), int(z'93', int8), int(z'DD', int8), int(z'03', int8), &
        int(z'EF', int8), int(z'09', int8), int(z'B2', int8), int(z'22', int8), &
        int(z'85', int8), int(z'35', int8), int(z'9E', int8), int(z'A7', int8), &
        int(z'79', int8), int(z'BD', int8), int(z'7C', int8), int(z'0F', int8), &
        int(z'8C', int8), int(z'6E', int8), int(z'86', int8), int(z'7F', int8), &
        int(z'DD', int8), int(z'77', int8), int(z'EB', int8), int(z'7E', int8), &
        int(z'F4', int8), int(z'41', int8), int(z'CE', int8), int(z'7C', int8)]
    integer(int8) :: digest(32)
    type(sx_node_t) :: node, invalid
    type(writer_t) :: output
    logical :: ok
    character(len=256) :: message

    call sx_parse(input, node, ok, message)
    call require(ok, message)
    call sx_validate(node, ok, message)
    call require(ok, message)
    call writer_init_hash(output, ok, message)
    call require(ok, message)
    call sx_write_writer(output, node, ok, message)
    call require(ok, message)
    call writer_digest(output, digest, ok, message)
    call require(ok, message)
    call require(all(digest == expected_digest), 'SX content hash differs from oracle')
    call require(writer_size(output) == int(len('(list "a b" "a\"b")') + 1, int64), &
        'SX canonical byte count differs')

    invalid%kind = 99
    call sx_validate(invalid, ok, message)
    call require(.not. ok, 'invalid SX node was accepted')

    print '(a)', 'fortsx hash test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_fortsx_hash

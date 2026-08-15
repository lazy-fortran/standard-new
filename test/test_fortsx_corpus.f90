program test_fortsx_corpus
    !! Deterministic generated trees and a fixed malformed-input corpus.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_clear, sx_node_t, sx_parse, sx_write
    implicit none

    integer, parameter :: case_count = 64
    integer, parameter :: max_depth = 4
    character(len=*), parameter :: alphabet = 'abC 012()\"'
    character(len=256), parameter :: malformed(10) = [character(len=256) :: &
        '', ')', '(a', '"abc', '"a\q"', '(a) (b)', '(a (b)', '(', '("a\q")', '"a\']
    character(len=256), parameter :: malformed_message(10) = [character(len=256) :: &
        'unexpected end of SX form', 'unexpected closing parenthesis', 'unclosed SX list', &
        'unclosed SX quoted atom', 'unsupported SX escape', 'trailing bytes after SX form', &
        'unclosed SX list', 'unclosed SX list', 'unsupported SX escape', 'unterminated SX escape']
    type(sx_node_t) :: originals(case_count), parsed
    character(len=4096) :: line, message
    integer :: i, ios, unit
    integer(int64) :: state
    logical :: ok

    state = 1729
    do i = 1, case_count
        call make_tree(originals(i), 0, state)
    end do
    open (newunit=unit, file='build/fortsx_fuzz_corpus.sx', status='replace', &
        action='write', iostat=ios)
    call require(ios == 0, 'could not open generated SX corpus')
    do i = 1, case_count
        call sx_write(unit, originals(i), ok, message)
        call require(ok, message)
    end do
    close (unit)

    open (newunit=unit, file='build/fortsx_fuzz_corpus.sx', action='read', iostat=ios)
    call require(ios == 0, 'could not reopen generated SX corpus')
    do i = 1, case_count
        read (unit, '(a)', iostat=ios) line
        call require(ios == 0, 'generated SX corpus ended early')
        call sx_parse(trim(line), parsed, ok, message)
        call require(ok, message)
        call require(nodes_equal(originals(i), parsed), 'generated SX tree changed on round-trip')
        call sx_clear(parsed)
    end do
    close (unit)

    do i = 1, size(malformed)
        call sx_parse(trim(malformed(i)), parsed, ok, message)
        call require(.not. ok, 'malformed SX corpus member was accepted')
        call require(trim(message) == trim(malformed_message(i)), &
            'malformed SX corpus message differs')
        call sx_clear(parsed)
    end do

    print '(a)', 'fortsx corpus test passed'

contains

    recursive subroutine make_tree(node, depth, random_state)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: depth
        integer(int64), intent(inout) :: random_state
        integer :: child, length, random_index

        call sx_clear(node)
        if (depth >= max_depth .or. modulo(next_value(random_state), 4) /= 0) then
            node%kind = 1
            length = 1 + modulo(next_value(random_state), 9)
            node%atom = alphabet(:length)
            do child = 1, length
                random_index = 1 + modulo(next_value(random_state), len(alphabet))
                node%atom(child:child) = alphabet(random_index:random_index)
            end do
            return
        end if
        node%kind = 2
        node%child_count = 1 + modulo(next_value(random_state), 4)
        allocate (node%children(128))
        do child = 1, node%child_count
            call make_tree(node%children(child), depth + 1, random_state)
        end do
    end subroutine make_tree

    integer function next_value(random_state) result(value)
        integer(int64), intent(inout) :: random_state

        random_state = modulo(1103515245_int64 * random_state + 12345_int64, 2147483647_int64)
        value = int(random_state)
    end function next_value

    recursive logical function nodes_equal(left, right) result(equal)
        type(sx_node_t), intent(in) :: left, right
        integer :: child

        equal = left%kind == right%kind .and. left%child_count == right%child_count
        if (.not. equal) return
        if (left%kind == 1) then
            equal = trim(left%atom) == trim(right%atom)
            return
        end if
        do child = 1, left%child_count
            if (.not. nodes_equal(left%children(child), right%children(child))) then
                equal = .false.
                return
            end if
        end do
    end function nodes_equal

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_fortsx_corpus

program test_fortsx_robustness
    !! Independent canonical fixtures and malformed SX expectations.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse, sx_write
    implicit none

    character(len=*), parameter :: canonical_input = &
        '(list   "a b" "a\"b"    )'
    character(len=*), parameter :: canonical_output = '(list "a b" "a\"b")'
    character(len=1024) :: actual, message, many_children
    character(len=257) :: long_atom
    type(sx_node_t) :: first, second
    logical :: ok
    integer :: i, ios, position, unit

    call check_canonical(canonical_input, canonical_output)
    call sx_parse(canonical_output, first, ok, message)
    call require(ok, message)
    open (newunit=unit, file='build/ftsx_property.sx', status='replace', &
        action='write', iostat=ios)
    call require(ios == 0, 'could not open SX property fixture')
    call sx_write(unit, first, ok, message)
    close (unit)
    call require(ok, message)
    open (newunit=unit, file='build/ftsx_property.sx', action='read', iostat=ios)
    call require(ios == 0, 'could not reopen SX property fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read SX property fixture')
    call sx_parse(trim(actual), second, ok, message)
    call require(ok, message)
    call require(nodes_equal(first, second), 'parse/write/parse tree differs')

    call expect_failure('', 'unexpected end of SX form')
    call expect_failure(')', 'unexpected closing parenthesis')
    call expect_failure('(a', 'unclosed SX list')
    call expect_failure('"abc', 'unclosed SX quoted atom')
    call expect_failure('"a\q"', 'unsupported SX escape')
    call expect_failure('(a) (b)', 'trailing bytes after SX form')
    call expect_failure('(a (b)', 'unclosed SX list')

    long_atom = repeat('a', len(long_atom))
    call expect_failure(long_atom, 'SX atom exceeds seed limit')
    many_children = '('
    position = 2
    do i = 1, 129
        many_children(position:position + 1) = ' a'
        position = position + 2
    end do
    many_children(position:position) = ')'
    call expect_failure(many_children, 'SX list exceeds seed limit')

    print '(a)', 'fortsx robustness test passed'

contains

    subroutine check_canonical(input, expected)
        character(len=*), intent(in) :: input, expected
        type(sx_node_t) :: node
        character(len=1024) :: actual, local_message
        integer :: local_unit, local_ios
        logical :: local_ok

        call sx_parse(input, node, local_ok, local_message)
        call require(local_ok, local_message)
        open (newunit=local_unit, file='build/ftsx_canonical.sx', status='replace', &
            action='write', iostat=local_ios)
        call require(local_ios == 0, 'could not open canonical SX fixture')
        call sx_write(local_unit, node, local_ok, local_message)
        close (local_unit)
        call require(local_ok, local_message)
        open (newunit=local_unit, file='build/ftsx_canonical.sx', action='read', &
            iostat=local_ios)
        call require(local_ios == 0, 'could not reopen canonical SX fixture')
        read (local_unit, '(a)', iostat=local_ios) actual
        close (local_unit)
        call require(local_ios == 0, 'could not read canonical SX fixture')
        call require(trim(actual) == expected, 'canonical SX bytes differ from oracle')
    end subroutine check_canonical

    subroutine expect_failure(input, expected_message)
        character(len=*), intent(in) :: input, expected_message
        type(sx_node_t) :: node
        character(len=1024) :: local_message
        logical :: local_ok

        call sx_parse(input, node, local_ok, local_message)
        call require(.not. local_ok, 'malformed SX input was accepted')
        call require(trim(local_message) == expected_message, &
            'malformed SX message differs from oracle')
    end subroutine expect_failure

    recursive logical function nodes_equal(left, right) result(equal)
        type(sx_node_t), intent(in) :: left, right
        integer :: child

        equal = left%kind == right%kind .and. left%child_count == right%child_count
        if (.not. equal) return
        if (left%kind == sx_atom) then
            equal = left%atom == right%atom
            return
        end if
        if (left%kind /= sx_list) then
            equal = .false.
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

end program test_fortsx_robustness

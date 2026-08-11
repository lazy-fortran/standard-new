program test_fortsx
    !! Fixed canonical bytes are the independent oracle for the SX seed.

    use fortsx, only: sx_node_t, sx_parse, sx_write
    implicit none

    character(len=*), parameter :: input = &
        '(syntax R501 (lhs program) (rhs (seq (ref program-unit) '// &
        '(repeat (ref program-unit) 0 unbounded))) (source (page 53)))'
    character(len=4096) :: actual, message
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call sx_parse(input, node, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/ftsx_fixture.sx', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open SX fixture')
    call sx_write(unit, node, ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/ftsx_fixture.sx', action='read', iostat=ios)
    if (ios /= 0) call fail('could not reopen SX fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. trim(actual) /= input) call fail('SX round-trip differs')
    print '(a)', 'fortsx reader/writer test passed'

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message
        print '(a)', 'FAIL: '//trim(message)
        stop 1
    end subroutine fail

end program test_fortsx

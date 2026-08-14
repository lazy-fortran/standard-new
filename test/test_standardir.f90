program test_standardir
    !! Fixed SX bytes are the independent oracle for the StandardIR writer.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir, only: standardir_syntax_t, standardir_start, standardir_add, &
        standardir_emit
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=*), parameter :: expected = &
        '(syntax R501 (lhs program) (rhs (seq (ref program-unit) '// &
        '(repeat (ref program-unit) 0 unbounded))) (source '// &
        '(document J3-24-007) (clause 5) (rule R501) (page 53) '// &
        '(end-page 53) (byte-start 138571) (byte-length 53) '// &
        '(source-sha256 '//source_hash//')))'
    character(len=*), parameter :: expected_multi = &
        '(syntax R705 (lhs integer-type-spec) (rhs (seq (token INTEGER) '// &
        '(optional (ref kind-selector)))) (source (document J3-24-007) '// &
        '(clause 5) (rule R705) (page 67) (end-page 67) (byte-start 10) '// &
        '(byte-length 25) (source-sha256 '//source_hash//')))'
    character(len=4096) :: actual, message
    type(standardir_syntax_t) :: production
    integer(int64) :: start, length
    integer :: unit, ios
    logical :: ok

    start = 138571_int64
    length = 30_int64
    call standardir_start(production, 'R501', 'program', 53, start, length, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', 'program-unit', 53, start, length, &
        ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', '[ program-unit ] . . .', 53, &
        138602_int64, 22_int64, ok, message)
    if (.not. ok) call fail(trim(message))

    open (newunit=unit, file='build/standardir_fixture.sx', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open StandardIR fixture')
    call standardir_emit(unit, production, source_hash, '5', ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))

    open (newunit=unit, file='build/standardir_fixture.sx', action='read', iostat=ios)
    if (ios /= 0) call fail('could not reopen StandardIR fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. trim(actual) /= expected) call fail('SX bytes differ')

    call standardir_start(production, 'R705', 'integer-type-spec', 67, 10_int64, &
        25_int64, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', 'INTEGER [ kind-selector ]', 67, &
        10_int64, 25_int64, ok, message)
    if (.not. ok) call fail(trim(message))

    open (newunit=unit, file='build/standardir_multi_fixture.sx', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open multi-token fixture')
    call standardir_emit(unit, production, source_hash, '5', ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))

    open (newunit=unit, file='build/standardir_multi_fixture.sx', action='read', &
        iostat=ios)
    if (ios /= 0) call fail('could not reopen multi-token fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. trim(actual) /= expected_multi) then
        call fail('multi-token SX bytes differ')
    end if

    call standardir_start(production, 'R760', 'enum-def-stmt', 109, 20_int64, 10_int64, &
        ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', 'ENUM, BIND(C) [ :: enum-type-name ]', &
        109, 20_int64, 40_int64, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/standardir_attached_fixture.sx', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open attached-token fixture')
    call standardir_emit(unit, production, source_hash, '5', ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/standardir_attached_fixture.sx', action='read', &
        iostat=ios)
    if (ios /= 0) call fail('could not reopen attached-token fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. index(actual, '(token ENUM) (token ,) (token BIND)') == 0) then
        call fail('attached punctuation was not lexed mechanically')
    end if

    call standardir_start(production, 'R1315', 'position-edit-desc', 291, 30_int64, &
        5_int64, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', 'Tn', 291, 30_int64, 5_int64, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/standardir_transition_fixture.sx', status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) call fail('could not open transition fixture')
    call standardir_emit(unit, production, source_hash, '5', ok, message)
    close (unit)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/standardir_transition_fixture.sx', action='read', &
        iostat=ios)
    if (ios /= 0) call fail('could not reopen transition fixture')
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. index(actual, '(token T) (ref n)') == 0) then
        call fail('terminal/reference transition was not lexed mechanically')
    end if

    call standardir_start(production, 'R999', 'unfinished', 67, 40_int64, 12_int64, &
        ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_add(production, 'sequence', '[ unfinished', 67, 40_int64, &
        12_int64, ok, message)
    if (.not. ok) call fail(trim(message))
    open (newunit=unit, file='build/standardir_incomplete_fixture.sx', &
        status='replace', action='write', iostat=ios)
    if (ios /= 0) call fail('could not open incomplete fixture')
    call standardir_emit(unit, production, source_hash, '5', ok, message)
    close (unit)
    if (ok) call fail('incomplete grammar group was emitted')
    print '(a)', 'standardir writer test passed'

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message
        print '(a)', 'FAIL: '//trim(message)
        stop 1
    end subroutine fail

end program test_standardir

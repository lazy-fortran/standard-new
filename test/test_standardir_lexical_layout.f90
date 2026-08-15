program test_standardir_lexical_layout
    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexical_layout, only: standardir_layout_add, standardir_layout_reset, &
        standardir_layout_t, standardir_layout_validate, standardir_layout_write
    implicit none
    character(len=*), parameter :: hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=4096) :: text, message, line
    type(standardir_layout_t) :: layout
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call standardir_layout_reset(layout)
    text = '(statement-boundary (source-form free-form) (terminator end-of-line) '// &
        '(source (source-ref (document J3-24-007) (clause 4.1.4) (rule R-statement-boundary) '// &
        '(page 45) (source-hash '//hash//'))) (origin mechanical))'
    call add(text)
    text = '(statement-boundary (source-form free-form) (terminator semicolon) '// &
        '(source (source-ref (document J3-24-007) (clause 4.1.4) (rule R-statement-boundary) '// &
        '(page 45) (source-hash '//hash//'))) (origin mechanical))'
    call add(text)
    text = '(continuation (source-form free-form) (signal trailing-ampersand) '// &
        '(source (source-ref (document J3-24-007) (clause 6.3.2.5) (rule R-continuation) '// &
        '(page 72) (source-hash '//hash//'))) (origin mechanical))'
    call add(text)
    text = '(keyword-name-policy (source-form free-form) (policy not-reserved) '// &
        '(source (source-ref (document J3-24-007) (clause 5.5.2) (rule R-keyword-policy) '// &
        '(page 65) (source-hash '//hash//'))) (origin mechanical))'
    call add(text)
    call require(layout%count == 4, 'three layout record families were not retained')
    call require(trim(layout%records(4)%policy) == 'not-reserved', &
        'keyword-not-reserved fact was not retained')
    call standardir_layout_validate(layout, ok, message)
    call require(ok, message)

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open output witness')
    call standardir_layout_write(layout, unit, ok, message)
    call require(ok, message)
    rewind (unit); line = ''; read (unit, '(a)', iostat=ios) line
    call require(index(line, 'lexical-layout-header') > 0, 'JSONL header missing')
    do; read (unit, '(a)', iostat=ios) line; if (ios /= 0) exit; end do
    close (unit)

    text = '(keyword-name-policy (source-form free-form) (policy reserved) '// &
        '(source (source-ref (document D) (clause C) (rule R) (page 1) (source-hash '//hash//'))) '// &
        '(origin mechanical))'
    call reject(text, 'invalid enum was accepted')
    text = '(statement-boundary (source-form free-form) (terminator end-of-line) (bogus x) '// &
        '(source (source-ref (document D) (clause C) (rule R) (page 1) (source-hash '//hash//'))) '// &
        '(origin mechanical))'
    call reject(text, 'unknown field was accepted')
    print '(a)', 'StandardIR lexical layout test passed'

contains
    subroutine add(value)
        character(len=*), intent(in) :: value
        call sx_parse(value, node, ok, message); call require(ok, message)
        call standardir_layout_add(node, layout, ok, message); call require(ok, message)
    end subroutine add

    subroutine reject(value, failure)
        character(len=*), intent(in) :: value, failure
        call sx_parse(value, node, ok, message); call require(ok, message)
        call standardir_layout_add(node, layout, ok, message)
        call require(.not. ok, failure)
    end subroutine reject

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure
        if (.not. condition) error stop trim(failure)
    end subroutine require
end program test_standardir_lexical_layout

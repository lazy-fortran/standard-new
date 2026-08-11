program test_standardir_normalize
    !! Fixed notation is the independent oracle for the SX normalizer.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_normalize, only: standardir_normalize_syntax
    implicit none

    character(len=*), parameter :: input = &
        '(syntax R705 (lhs integer-type-spec) (rhs (seq (token INTEGER) '// &
        '(optional (ref kind-selector)))) (source (clause 6)))'
    character(len=*), parameter :: expected_rule = 'R705'
    character(len=*), parameter :: expected_lhs = 'integer-type-spec'
    character(len=*), parameter :: expected_notation = &
        'INTEGER [ kind-selector ]'
    character(len=32768) :: rule, lhs, notation, message
    type(sx_node_t) :: node
    logical :: ok

    call sx_parse(input, node, ok, message)
    if (.not. ok) call fail(trim(message))
    call standardir_normalize_syntax(node, rule, lhs, notation, ok, message)
    if (.not. ok) call fail(trim(message))
    if (trim(rule) /= expected_rule) call fail('rule differs')
    if (trim(lhs) /= expected_lhs) call fail('lhs differs')
    if (trim(notation) /= expected_notation) then
        call fail('notation differs: ['//trim(notation)//']')
    end if
    print '(a)', 'StandardIR normalizer test passed'

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message

        print '(a)', 'FAIL: '//trim(message)
        stop 1
    end subroutine fail

end program test_standardir_normalize

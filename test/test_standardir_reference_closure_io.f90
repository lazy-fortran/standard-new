program test_standardir_reference_closure_io
    use fortsx, only: sx_node_t, sx_parse
    use standardir_reference_closure, only: closure_classification_t
    use standardir_reference_closure_io, only: closure_read_classification
    implicit none

    character(len=*), parameter :: text = &
        '(classification (name alpha-list) (kind list) (target alpha) '// &
        '(separator ",") (family R401) (suffix -list) '// &
        '(source (document J3-24-007) (clause 5) (rule R401) (page 45) '// &
        '(source-sha256 HASH)))'
    type(sx_node_t) :: node
    type(closure_classification_t) :: value
    character(len=256) :: message
    logical :: ok

    call sx_parse(text, node, ok, message)
    call require(ok, message)
    call closure_read_classification(node, value, ok, message)
    call require(ok, message)
    call require(trim(value%name) == 'alpha-list' .and. trim(value%source%rule) == 'R401', &
        'closure source parser lost fields')
    print '(a)', 'StandardIR closure IO test passed'

contains

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_reference_closure_io

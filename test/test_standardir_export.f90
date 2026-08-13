program test_standardir_export
    !! Fixed SX is the independent oracle for the frontend-facing record API.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_export
    implicit none

    character(len=*), parameter :: accepted = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document J3-24-007) (clause 1) (rule R501) (page 45) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: rejected = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document J3-24-007) (clause 1) (rule R501) (page 0) '// &
        '(source-hash fixture))) (origin mechanical) (resolution resolved))'
    type(standardir_syntax_item_t) :: item
    type(sx_node_t) :: node
    character(len=256) :: message
    character(len=1024) :: actual
    integer :: unit, ios
    logical :: ok

    call sx_parse(accepted, node, ok, message)
    call require(ok, message)
    call standardir_read_syntax_item(node, item, ok, message)
    call require(ok, message)
    call require(trim(item%id) == 'R501' .and. trim(item%lhs) == 'program', &
        'syntax-item identity differs')
    call require(trim(item%source%document) == 'J3-24-007' .and. &
        trim(item%source%clause) == '1' .and. item%source%page == 45 .and. &
        trim(item%source%source_hash) == 'fixture', 'source provenance differs')
    call require(item%origin == standardir_origin_mechanical .and. &
        item%resolution == standardir_resolution_resolved, 'enum state differs')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open SX output')
    call standardir_write_syntax_item(item, unit, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0 .and. trim(actual) == accepted, 'canonical SX differs')

    call sx_parse(rejected, node, ok, message)
    call require(ok, message)
    call standardir_read_syntax_item(node, item, ok, message)
    call require(.not. ok, 'invalid source page was accepted')

    print '(a)', 'StandardIR export test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message
        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_export

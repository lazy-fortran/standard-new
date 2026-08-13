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
    character(len=*), parameter :: semantic_expected = &
        '(semantic-item (id S501) (subject assignment) (source (source-ref '// &
        '(document J3-24-007) (clause 10.1) (rule C1102) (page 88) '// &
        '(source-hash fixture-hash))) (origin human) (resolution disputed))'
    character(len=*), parameter :: semantic_rejected = &
        '(semantic-item (id S501) (subject assignment) (source (source-ref '// &
        '(document J3-24-007) (clause 10.1) (rule C1102) (page 0) '// &
        '(source-hash fixture-hash))) (origin human) (resolution disputed))'
    type(standardir_syntax_item_t) :: item
    type(standardir_semantic_item_t) :: semantic_item
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

    call sx_parse(semantic_expected, node, ok, message)
    call require(ok, message)
    call standardir_read_semantic_item(node, semantic_item, ok, message)
    call require(ok, message)
    call require(trim(semantic_item%id) == 'S501' .and. &
        trim(semantic_item%subject) == 'assignment' .and. &
        trim(semantic_item%source%document) == 'J3-24-007' .and. &
        trim(semantic_item%source%clause) == '10.1' .and. &
        trim(semantic_item%source%rule) == 'C1102' .and. &
        semantic_item%source%page == 88 .and. &
        trim(semantic_item%source%source_hash) == 'fixture-hash' .and. &
        semantic_item%origin == standardir_origin_human .and. &
        semantic_item%resolution == standardir_resolution_disputed, &
        'semantic-item fields differ')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open semantic SX output')
    call standardir_write_semantic_item_from_fields(unit, 'S501', 'assignment', &
        'J3-24-007', '10.1', 'C1102', 88, 'fixture-hash', standardir_origin_human, &
        standardir_resolution_disputed, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0 .and. trim(actual) == semantic_expected, &
        'semantic-item canonical SX differs')

    call sx_parse(semantic_rejected, node, ok, message)
    call require(ok, message)
    call standardir_read_semantic_item(node, semantic_item, ok, message)
    call require(.not. ok, 'invalid semantic provenance was accepted')

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open semantic invalid-state output')
    call standardir_write_semantic_item_from_fields(unit, 'S501', 'assignment', &
        'J3-24-007', '10.1', 'C1102', 88, 'fixture-hash', 0, &
        standardir_resolution_disputed, ok, message)
    call require(.not. ok, 'invalid semantic origin was accepted')
    call standardir_write_semantic_item_from_fields(unit, 'S501', 'assignment', &
        'J3-24-007', '10.1', 'C1102', 88, 'fixture-hash', standardir_origin_human, &
        0, ok, message)
    call require(.not. ok, 'invalid semantic resolution was accepted')
    call standardir_write_semantic_item_from_fields(unit, 'S501', 'assignment', '', &
        '10.1', 'C1102', 88, 'fixture-hash', standardir_origin_human, &
        standardir_resolution_disputed, ok, message)
    call require(.not. ok, 'incomplete semantic provenance was accepted')
    close (unit)

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

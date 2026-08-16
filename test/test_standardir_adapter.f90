program test_standardir_adapter
    !! Fixed SX is the independent oracle for production-to-record adaptation.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir, only: standardir_syntax_t, standardir_add, standardir_start
    use standardir_export, only: standardir_origin_mechanical, &
        standardir_read_syntax_item, standardir_resolution_resolved, &
        standardir_syntax_item_t, standardir_write_syntax_item_from_production
    implicit none

    character(len=*), parameter :: expected = &
        '(syntax-item (id FIX-RULE) (lhs fixture) (source (source-ref '// &
        '(document fixture-document) (clause fixture-clause) (rule FIX-RULE) '// &
        '(page 23) (source-hash fixture-hash) (end-page 23) (byte-start 100) '// &
        '(byte-length 20))) (origin mechanical) '// &
        '(resolution resolved))'
    type(standardir_syntax_t) :: production, incomplete
    type(standardir_syntax_item_t) :: item
    type(sx_node_t) :: node
    character(len=256) :: message
    character(len=1024) :: actual
    integer :: unit, ios
    logical :: ok

    call standardir_start(production, 'FIX-RULE', 'fixture', 23, 100_int64, 20_int64, &
        ok, message)
    call require(ok, message)
    call standardir_add(production, 'sequence', 'fixture-token', 23, 100_int64, 14_int64, &
        ok, message)
    call require(ok, message)

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open SX output')
    call standardir_write_syntax_item_from_production(unit, production, 'fixture-document', &
        'fixture-clause', 'fixture-hash', standardir_origin_mechanical, &
        standardir_resolution_resolved, ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0 .and. trim(actual) == expected, 'adapter SX differs')

    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_read_syntax_item(node, item, ok, message)
    call require(ok, message)
    call require(trim(item%id) == 'FIX-RULE' .and. trim(item%lhs) == 'fixture' .and. &
        item%source%page == 23 .and. item%source%end_page == 23 .and. &
        item%source%byte_start == 100_int64 .and. item%source%byte_length == 20_int64, &
        'adapter round-trip differs')

    call standardir_write_syntax_item_from_production(unit, production, '', 'fixture-clause', &
        'fixture-hash', standardir_origin_mechanical, standardir_resolution_resolved, ok, &
        message)
    call require(.not. ok, 'incomplete provenance was accepted')

    call standardir_start(incomplete, 'FIX-RULE', 'fixture', 23, 100_int64, 20_int64, ok, &
        message)
    call require(ok, message)
    call standardir_add(incomplete, 'sequence', '[ unfinished', 23, 100_int64, 12_int64, &
        ok, message)
    call require(ok, message)
    call standardir_write_syntax_item_from_production(unit, incomplete, 'fixture-document', &
        'fixture-clause', 'fixture-hash', standardir_origin_mechanical, &
        standardir_resolution_resolved, ok, message)
    call require(.not. ok, 'incomplete production was accepted')

    print '(a)', 'StandardIR adapter test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message
        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_adapter

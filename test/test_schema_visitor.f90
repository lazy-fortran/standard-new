module schema_visitor_test_support
    use fortsx, only: sx_node_t
    implicit none
    character(len=32) :: events(16)
    integer :: event_count
contains
    subroutine record_event(value, entering, event_ok, event_message)
        type(sx_node_t), intent(in) :: value
        logical, intent(in) :: entering
        logical, intent(out) :: event_ok
        character(len=*), intent(out) :: event_message

        event_ok = .false.
        event_message = ''
        if (event_count >= size(events)) then
            event_message = 'visitor event buffer is full'
            return
        end if
        event_count = event_count + 1
        if (value%kind == 1) then
            events(event_count) = merge('enter:', 'leave:', entering)//trim(value%atom)
        else
            events(event_count) = merge('enter:list', 'leave:list', entering)
        end if
        event_ok = .true.
    end subroutine record_event
end module schema_visitor_test_support

program test_schema_visitor
    !! Fixed traversal events are an independent oracle for the generic boundary.

    use fortsx, only: sx_node_t, sx_parse
    use schema_visitor, only: schema_visit, schema_visit_callback
    use schema_visitor_test_support, only: event_count, events, record_event
    implicit none

    type(sx_node_t) :: node, invalid
    character(len=256) :: message
    logical :: ok

    call sx_parse('(record (id R501) (origin mechanical))', node, ok, message)
    call require(ok, message)
    events = ''
    event_count = 0
    call schema_visit(node, record_event, ok, message)
    call require(ok, message)
    call require(event_count == 10, 'visitor event count differs')
    call require(trim(events(1)) == 'enter:list', 'visitor root entry differs')
    call require(trim(events(2)) == 'enter:id', 'visitor first child entry differs')
    call require(trim(events(3)) == 'enter:R501', 'visitor atom entry differs')
    call require(trim(events(4)) == 'leave:R501', 'visitor atom leave differs')
    call require(trim(events(10)) == 'leave:list', 'visitor root leave differs')

    invalid%kind = 99
    call schema_visit(invalid, record_event, ok, message)
    call require(.not. ok, 'visitor accepted an invalid node')

    print '(a)', 'schema visitor test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_schema_visitor

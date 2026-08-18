program test_standardir_grammar_fact
    !! Fixed SX is the independent oracle for the concrete frontend fact.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact, only: standardir_consume_integer_type_spec_fact, &
        standardir_write_integer_type_spec_fact
    implicit none

    character(len=*), parameter :: expected = &
        '(grammar-fact (id R705) (expression "INTEGER [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 67) (source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=512) :: actual, message
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open fact output')
    call standardir_write_integer_type_spec_fact(unit, 'J3-24-007', '7', 'R705', 67, &
        'fixture', ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read fact output')
    call require(trim(actual) == expected, 'canonical type-spec fact differs')

    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_consume_integer_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R704) (expression "INTEGER [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R704) '// &
        '(page 67) (source-hash fixture))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_integer_type_spec_fact(node, ok, message)
    call require(.not. ok, 'wrong source rule reached the frontend consumer')

    print '(a)', 'StandardIR grammar fact test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_grammar_fact

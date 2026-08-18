program test_standardir_grammar_fact
    !! Fixed SX is the independent oracle for the concrete frontend fact.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_integer_type_spec_fact, &
        standardir_generate_real_type_spec_fact
    use standardir_grammar_fact, only: standardir_consume_integer_type_spec_fact, &
        standardir_write_integer_type_spec_fact
    use standardir_real_type_spec_fact, only: standardir_consume_real_type_spec_fact, &
        standardir_write_real_type_spec_fact
    implicit none

    character(len=*), parameter :: expected = &
        '(grammar-fact (id R705) (expression "INTEGER [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 67) (source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=*), parameter :: expected_real = &
        '(grammar-fact (id R706) (expression "REAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R706) '// &
        '(page 67) (source-hash fixture))) (origin mechanical) (resolution resolved))'
    character(len=512) :: actual, message
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok

    call check_generated_source_freshness()

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

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open REAL fact output')
    call standardir_write_real_type_spec_fact(unit, 'J3-24-007', '7', 'R706', 67, &
        'fixture', ok, message)
    call require(ok, message)
    rewind (unit)
    read (unit, '(a)', iostat=ios) actual
    close (unit)
    call require(ios == 0, 'could not read REAL fact output')
    call require(trim(actual) == expected_real, 'canonical REAL type-spec fact differs')

    call sx_parse(expected_real, node, ok, message)
    call require(ok, message)
    call standardir_consume_real_type_spec_fact(node, ok, message)
    call require(ok, message)

    call sx_parse('(grammar-fact (id R705) (expression "REAL [ kind-selector ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R705) '// &
        '(page 67) (source-hash fixture))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    call standardir_consume_real_type_spec_fact(node, ok, message)
    call require(.not. ok, 'R705 reached the REAL frontend consumer')

    print '(a)', 'StandardIR grammar fact test passed'

contains

    subroutine check_generated_source_freshness()
        character(len=256) :: line, fresh_integer(512), checked_integer(512), &
            fresh_real(512), checked_real(512), source
        integer :: input_unit, fresh_integer_unit, fresh_real_unit, ios
        integer :: fresh_integer_count, checked_integer_count, fresh_real_count, checked_real_count
        logical :: local_ok

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not open grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read grammar-facts specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_integer_unit, file='build/standardir_grammar_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh grammar-fact output')
        call standardir_generate_integer_type_spec_fact(node, fresh_integer_unit, local_ok, message)
        close (fresh_integer_unit)
        call require(local_ok, message)
        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not reopen grammar-facts specification')
        read (input_unit, '(a)', iostat=ios) line
        read (input_unit, '(a)', iostat=ios) source
        close (input_unit)
        call require(ios == 0, 'could not read REAL grammar-fact specification')
        call sx_parse(trim(source), node, local_ok, message)
        call require(local_ok, message)
        open (newunit=fresh_real_unit, file='build/standardir_real_type_spec_fact_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh REAL grammar-fact output')
        call standardir_generate_real_type_spec_fact(node, fresh_real_unit, local_ok, message)
        close (fresh_real_unit)
        call require(local_ok, message)
        call read_source('build/standardir_grammar_fact_generated.f90', fresh_integer, fresh_integer_count)
        call read_source('src/standardir_grammar_fact_generated.f90', checked_integer, checked_integer_count)
        call require(fresh_integer_count == checked_integer_count, 'checked-in integer grammar-fact output is stale')
        call require(all(fresh_integer(:fresh_integer_count) == checked_integer(:checked_integer_count)), &
            'checked-in integer grammar-fact output differs from specification')
        call read_source('build/standardir_real_type_spec_fact_generated.f90', fresh_real, fresh_real_count)
        call read_source('src/standardir_real_type_spec_fact_generated.f90', checked_real, checked_real_count)
        call require(fresh_real_count == checked_real_count, 'checked-in REAL grammar-fact output is stale')
        call require(all(fresh_real(:fresh_real_count) == checked_real(:checked_real_count)), &
            'checked-in REAL grammar-fact output differs from specification')
    end subroutine check_generated_source_freshness

    subroutine read_source(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: count
        character(len=256) :: line
        integer :: local_unit, local_ios

        count = 0
        open (newunit=local_unit, file=path, action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open generated grammar-fact source')
        do
            read (local_unit, '(a)', iostat=local_ios) line
            if (local_ios /= 0) exit
            count = count + 1
            call require(count <= size(lines), 'generated grammar-fact source is too long')
            lines(count) = line
        end do
        close (local_unit)
    end subroutine read_source

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            error stop 1
        end if
    end subroutine require

end program test_standardir_grammar_fact

program test_standardir_execution_part_grammar_fact
    !! Fixed SX and source mutations are the independent generator oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_execution_part_grammar_fact
    use standardir_execution_part_grammar_fact, only: standardir_consume_execution_part_grammar_fact
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=*), parameter :: expected = &
        '(grammar-fact (id R509) (expression "executable-construct [ execution-part-construct ] ...") '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R509) '// &
        '(page 45) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=1024) :: message, line, source
    character(len=1024) :: fresh(256), checked(256)
    type(sx_node_t) :: node
    integer :: unit, ios, fresh_count, checked_count
    logical :: ok, found

    open (newunit=unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
    call require(ios == 0, 'could not open grammar-facts specification')
    do
        read (unit, '(a)', iostat=ios) source
        if (ios /= 0) exit
        if (index(source, '(id R509)') > 0) exit
    end do
    close (unit)
    call require(ios == 0, 'could not read R509 grammar-fact specification')
    call sx_parse(trim(source), node, ok, message)
    call require(ok, message)
    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_consume_execution_part_grammar_fact(node, ok, message)
    call require(ok, message)
    call sx_parse(trim(source), node, ok, message)
    call require(ok, message)

    open (newunit=unit, file='build/standardir_execution_part_grammar_fact_generated.f90', &
        status='replace', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open generated output')
    call standardir_generate_execution_part_grammar_fact(node, unit, ok, message)
    call require(ok, message)
    rewind (unit)
    found = .false.
    do
        read (unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'standardir_execution_part_grammar_expression =') > 0) then
            read (unit, '(a)', iostat=ios) line
            call require(ios == 0, 'generated expression is missing')
            found = index(line, 'executable-construct [ execution-part-construct ] ...') > 0
        end if
    end do
    close (unit)
    call require(found, 'generated R509 expression differs')
    call read_source('build/standardir_execution_part_grammar_fact_generated.f90', fresh, fresh_count)
    call read_source('src/standardir_execution_part_grammar_fact_generated.f90', checked, checked_count)
    call require(fresh_count == checked_count, 'checked-in R509 generated source is stale')
    call require(all(fresh(:fresh_count) == checked(:checked_count)), &
        'checked-in R509 generated source differs')

    call sx_parse('(grammar-fact (id R509) (expression "mutated") '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R509) '// &
        '(page 45) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated output')
    call standardir_generate_execution_part_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated R509 expression was accepted')

    call sx_parse('(grammar-fact (id R509) (expression "executable-construct [ execution-part-construct ] ...") '// &
        '(source (source-ref (document J3-24-007) (clause 5) (rule R509) '// &
        '(page 46) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, message)
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated page output')
    call standardir_generate_execution_part_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated R509 page was accepted')

    print '(a)', 'StandardIR R509 execution-part grammar fact test passed'

contains

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure)
            error stop 1
        end if
    end subroutine require

    subroutine read_source(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: count

        integer :: input_unit, input_status

        count = 0
        open (newunit=input_unit, file=path, action='read', iostat=input_status)
        call require(input_status == 0, 'could not open '//trim(path))
        do
            read (input_unit, '(a)', iostat=input_status) line
            if (input_status /= 0) exit
            count = count + 1
            lines(count) = line
        end do
        close (input_unit)
    end subroutine read_source

end program test_standardir_execution_part_grammar_fact

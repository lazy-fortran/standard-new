program test_standardir_int_literal_constant_grammar_fact
    !! Fixed SX and source mutations are the independent generator oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: &
        standardir_generate_int_literal_constant_grammar_fact
    use standardir_int_literal_constant_grammar_fact, only: &
        standardir_consume_int_literal_constant_grammar_fact
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=*), parameter :: expected = &
        '(grammar-fact (id R708) (expression "digit-string [ _ kind-param ]") '// &
        '(source (source-ref (document J3-24-007) (clause 7) (rule R708) '// &
        '(page 66) (source-hash '//source_hash//'))) (origin mechanical) (resolution resolved))'
    character(len=1024) :: message, line, source
    character(len=256) :: fresh(512), checked(512)
    type(sx_node_t) :: node
    integer :: unit, ios
    logical :: ok, found

    open (newunit=unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
    call require(ios == 0, 'could not open grammar-facts specification')
    do
        read (unit, '(a)', iostat=ios) source
        if (ios /= 0) exit
        if (index(source, '(id R708)') > 0) exit
    end do
    close (unit)
    call require(ios == 0, 'could not read R708 grammar-fact specification')
    call sx_parse(trim(source), node, ok, message)
    call require(ok, message)
    call sx_parse(expected, node, ok, message)
    call require(ok, message)
    call standardir_consume_int_literal_constant_grammar_fact(node, ok, message)
    call require(ok, message)
    call sx_parse(trim(source), node, ok, message)
    call require(ok, message)
    open (newunit=unit, file='build/standardir_int_literal_constant_grammar_fact_generated.f90', &
        status='replace', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open generated output')
    call standardir_generate_int_literal_constant_grammar_fact(node, unit, ok, message)
    call require(ok, message)
    rewind (unit)
    found = .false.
    do
        read (unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, "standardir_int_literal_constant_grammar_expression =") > 0) then
            read (unit, '(a)', iostat=ios) line
            call require(ios == 0, 'generated expression is missing')
            found = index(line, 'digit-string [ _ kind-param ]') > 0
        end if
    end do
    close (unit)
    call require(found, 'generated R708 expression differs')
    call read_source('build/standardir_int_literal_constant_grammar_fact_generated.f90', fresh, ios)
    call require(ios > 0, 'generated R708 source is empty')
    call read_source('src/standardir_int_literal_constant_grammar_fact_generated.f90', checked, unit)
    call require(unit == ios, 'checked-in R708 generated source is stale')
    call require(all(fresh(:ios) == checked(:unit)), 'checked-in R708 generated source differs')

    call sx_parse('(grammar-fact (id R708) (expression "digit-string [ _ kind-param ]") '// &
        '(source (document J3-24-007) (clause 7) (rule R708) (page 67) '// &
        '(source-sha256 '//source_hash//')) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, 'mutated page fixture parse failed: '//trim(message))
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated page output')
    call standardir_generate_int_literal_constant_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated R708 page was accepted: '//trim(message))

    call sx_parse('(grammar-fact (id R708) (expression "digit-string [ _ kind-param ]") '// &
        '(source (document J3-24-007) (clause 7) (rule R709) (page 66) '// &
        '(source-sha256 '//source_hash//')) (origin mechanical) (resolution resolved))', &
        node, ok, message)
    call require(ok, 'mutated rule fixture parse failed: '//trim(message))
    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    call require(ios == 0, 'could not open mutated rule output')
    call standardir_generate_int_literal_constant_grammar_fact(node, unit, ok, message)
    close (unit)
    call require(.not. ok, 'mutated R708 source rule was accepted: '//trim(message))

    print '(a)', 'StandardIR R708 int-literal-constant grammar fact test passed'

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

end program test_standardir_int_literal_constant_grammar_fact

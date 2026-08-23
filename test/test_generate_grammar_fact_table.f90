program test_generate_grammar_fact_table
    !! Independent record and query expectations exercise the generated table.

    use fortsx, only: sx_node_t, sx_parse
    use schema_v0_generated, only: ORIGIN_MECHANICAL, RESOLUTION_RESOLVED
    use standardir_grammar_fact_codegen, only: standardir_generate_grammar_fact_table
    use standardir_grammar_fact_table_generated, only: standardir_grammar_fact_table_entry_t, &
        standardir_grammar_fact_table_count, standardir_lookup_grammar_fact, &
        standardir_make_grammar_fact_table
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=128), parameter :: expected_ids(25) = [character(len=128) :: &
        'R501', 'R705', 'R706', 'R707', 'R704', 'R704', 'R704', 'R1033', &
        'R1007', 'R1006', 'R1009', 'R1009', 'R1010', 'R1010', 'R1008', 'R708', &
        'R901', 'R902', 'R903', 'R509', 'R1162', 'R1164', 'R1212', 'R1215', 'R1217']
    character(len=128), parameter :: expected_expressions(25) = [character(len=128) :: &
        'program-unit [ program-unit ] ...', 'INTEGER [ kind-selector ]', &
        'REAL [ kind-selector ]', 'DOUBLE PRECISION', 'or COMPLEX [ kind-selector ]', &
        'or LOGICAL [ kind-selector ]', 'or CHARACTER [ char-selector ]', &
        'variable = expr', '[ [ level-2-expr ] add-op ] add-operand', &
        '[ add-operand mult-op ] mult-operand', '*', '/', '+', '–', '**', &
        'digit-string [ _ kind-param ]', &
        'object-name | array-element | array-section | coindexed-named-object | '// &
        'complex-part-designator | structure-component | substring', &
        'designator | function-reference', 'name', &
        'executable-construct [ execution-part-construct ] ...', &
        'STOP [ stop-code ] [ , QUIET = scalar-logical-expr ]', &
        'scalar-default-char-expr | scalar-int-expr', 'PRINT format [ , output-item-list ]', &
        'default-char-expr | label | *', 'expr | io-implied-do']
    integer, parameter :: expected_pages(25) = [53, 67, 67, 67, 80, 80, 80, 188, &
        155, 155, 155, 155, 155, 155, 155, 66, 150, 150, 150, 45, 214, 214, 242, 244, 248]

    type(sx_node_t) :: nodes(25), malformed
    type(standardir_grammar_fact_table_entry_t) :: values(standardir_grammar_fact_table_count), value
    character(len=1024) :: source, message
    integer :: count, ios, unit, position, i
    logical :: ok, found

    call read_source(nodes, count)
    call require(count == standardir_grammar_fact_table_count, 'table count differs from source oracle')
    call check_fresh_generation(nodes, count)
    call standardir_make_grammar_fact_table(values)
    do i = 1, count
        call require(trim(values(i)%fact%id) == trim(expected_ids(i)), 'ordered identifier differs')
        call require(trim(values(i)%fact%expression) == trim(expected_expressions(i)), &
            'ordered expression differs')
        call require(trim(values(i)%fact%source%document) == 'J3-24-007' .and. &
            trim(values(i)%fact%source%clause) == expected_clause(i) .and. &
            trim(values(i)%fact%source%rule) == trim(expected_ids(i)) .and. &
            values(i)%fact%source%page == expected_pages(i) .and. &
            trim(values(i)%fact%source%source_hash) == source_hash .and. &
            values(i)%fact%origin == ORIGIN_MECHANICAL .and. &
            values(i)%fact%resolution == RESOLUTION_RESOLVED, 'record provenance differs')
    end do

    call standardir_lookup_grammar_fact('R1009', '/', value, found)
    call require(found .and. value%fact%source%page == 155, 'listed duplicate lookup failed')
    call standardir_lookup_grammar_fact('R1009', '**', value, found)
    call require(.not. found, 'unlisted duplicate lookup was accepted')
    call standardir_lookup_grammar_fact('R1010', '–', value, found)
    call require(found .and. trim(value%fact%source%rule) == 'R1010', 'Unicode duplicate lookup failed')

    call sx_parse('(grammar-fact (id R501) (expression "broken") (source (document J3-24-007) '// &
        '(clause 5) (page 53) (source-sha256 '//source_hash//')) (origin mechanical) '// &
        '(resolution resolved))', malformed, ok, message)
    call require(ok, message)
    nodes(1) = malformed
    open (newunit=unit, status='scratch', action='write', iostat=ios)
    call require(ios == 0, 'could not open transactional output')
    call standardir_generate_grammar_fact_table(nodes, unit, ok, message)
    inquire (unit=unit, size=position)
    close (unit)
    call require(.not. ok .and. position == 0, 'malformed input left generated output')
    print '(a)', 'StandardIR grammar-fact table test passed'

contains

    subroutine read_source(output, output_count)
        type(sx_node_t), intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer :: local_unit, local_ios

        open (newunit=local_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open grammar-facts specification')
        output_count = 0
        do
            read (local_unit, '(a)', iostat=local_ios) source
            if (local_ios /= 0) exit
            if (output_count >= size(output)) then
                call require(.false., 'grammar-facts source exceeds fixed oracle capacity')
                return
            end if
            output_count = output_count + 1
            call sx_parse(trim(source), output(output_count), ok, message)
            call require(ok, message)
        end do
        close (local_unit)
    end subroutine read_source

    subroutine check_fresh_generation(input, input_count)
        type(sx_node_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=1024) :: fresh_line, checked_line
        integer :: fresh_unit, checked_unit, fresh_ios, checked_ios

        open (newunit=fresh_unit, file='build/standardir_grammar_fact_table_generated.f90', &
            status='replace', action='write', iostat=fresh_ios)
        call require(fresh_ios == 0, 'could not open fresh grammar-fact output')
        call standardir_generate_grammar_fact_table(input(:input_count), fresh_unit, ok, message)
        close (fresh_unit)
        call require(ok, message)

        open (newunit=fresh_unit, file='build/standardir_grammar_fact_table_generated.f90', &
            action='read', iostat=fresh_ios)
        open (newunit=checked_unit, file='src/standardir_grammar_fact_table_generated.f90', &
            action='read', iostat=checked_ios)
        call require(fresh_ios == 0 .and. checked_ios == 0, 'could not open freshness comparison files')
        do
            read (fresh_unit, '(a)', iostat=fresh_ios) fresh_line
            read (checked_unit, '(a)', iostat=checked_ios) checked_line
            call require((fresh_ios /= 0) .eqv. (checked_ios /= 0), &
                'generated grammar-fact file line counts differ')
            if (fresh_ios /= 0) exit
            call require(fresh_line == checked_line, 'generated grammar-fact file is stale')
        end do
        close (fresh_unit)
        close (checked_unit)
    end subroutine check_fresh_generation

    function expected_clause(index) result(value)
        integer, intent(in) :: index
        character(len=128) :: value

        select case (index)
        case (1, 20)
            value = '5'
        case (2, 3, 4, 16)
            value = '7'
        case (5, 6, 7)
            value = '7'
        case (8:15)
            value = '10'
        case (17:19)
            value = '5-15'
        case (21, 22)
            value = '11'
        case (23)
            value = '12.6.1'
        case (24)
            value = '12.6.2.2'
        case (25)
            value = '12.6.3'
        case default
            value = ''
        end select
    end function expected_clause

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_generate_grammar_fact_table

program test_standardir_print_grammar_fact
    !! Fixed SX and source mutations are the independent generator oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_print_stmt_grammar_fact, &
        standardir_generate_format_grammar_fact, standardir_generate_output_item_grammar_fact
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=1024) :: message, source, line
    character(len=256) :: fresh(256), checked(256)
    type(sx_node_t) :: node
    integer :: input_unit, output_unit, ios, fresh_count, checked_count
    logical :: ok

    abstract interface
        subroutine generate_fact(node, unit, ok, message)
            import :: sx_node_t
            type(sx_node_t), intent(in) :: node
            integer, intent(in) :: unit
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
        end subroutine generate_fact
    end interface

    call check_fact('R1212', 'standardir_print_stmt_grammar_fact_generated.f90', &
        'src/standardir_print_stmt_grammar_fact_generated.f90', &
        standardir_generate_print_stmt_grammar_fact, 'PRINT format [ , output-item-list ]', '12.6.1', 242)
    call check_fact('R1215', 'standardir_format_grammar_fact_generated.f90', &
        'src/standardir_format_grammar_fact_generated.f90', standardir_generate_format_grammar_fact, &
        'default-char-expr | label | *', '12.6.2.2', 244)
    call check_fact('R1217', 'standardir_output_item_grammar_fact_generated.f90', &
        'src/standardir_output_item_grammar_fact_generated.f90', &
        standardir_generate_output_item_grammar_fact, 'expr | io-implied-do', '12.6.3', 248)

    call reject('R1212', 'PRINT format [ , output-item-list ]', '12.6.1', 243, &
        standardir_generate_print_stmt_grammar_fact)
    call reject('R1215', 'default-char-expr | label', '12.6.2.2', 244, standardir_generate_format_grammar_fact)
    call reject('R1217', 'expr | io-implied-do', '12.6.3', 249, standardir_generate_output_item_grammar_fact)

    print '(a)', 'StandardIR R1212/R1215/R1217 PRINT grammar fact test passed'

contains

    subroutine read_fact(rule, result)
        character(len=*), intent(in) :: rule
        type(sx_node_t), intent(out) :: result

        open (newunit=input_unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
        call require(ios == 0, 'could not open grammar-facts specification')
        do
            read (input_unit, '(a)', iostat=ios) source
            if (ios /= 0) exit
            if (index(source, '(id '//trim(rule)//')') > 0) exit
        end do
        close (input_unit)
        call require(ios == 0, 'could not read '//trim(rule)//' grammar-fact specification')
        call sx_parse(trim(source), result, ok, message)
        call require(ok, message)
    end subroutine read_fact

    subroutine check_fact(rule, fresh_path, checked_path, generate, expected, clause, page)
        character(len=*), intent(in) :: rule, fresh_path, checked_path, expected, clause
        procedure(generate_fact) :: generate
        integer, intent(in) :: page

        call read_fact(rule, node)
        open (newunit=output_unit, file='build/'//trim(fresh_path), status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open generated output')
        call generate(node, output_unit, ok, message)
        close (output_unit)
        call require(ok, message)
        call read_source('build/'//trim(fresh_path), fresh, fresh_count)
        call read_source(trim(checked_path), checked, checked_count)
        call require(fresh_count == checked_count, 'generated output is stale')
        call require(all(fresh(:fresh_count) == checked(:checked_count)), &
            'generated output differs from canonical source')
        call require(any(index(fresh(:fresh_count), trim(expected)) > 0), 'generated expression differs')
        call require(any(index(fresh(:fresh_count), "clause = '"//trim(clause)//"'") > 0), &
            'generated clause differs')
        call require(any(index(fresh(:fresh_count), 'page = '//trim(adjustl(itoa(page)))) > 0), &
            'generated page differs')
        call require(any(index(fresh(:fresh_count), trim(source_hash)) > 0), 'generated source hash differs')
    end subroutine check_fact

    subroutine reject(rule, expression, clause, page, generate)
        character(len=*), intent(in) :: rule, expression, clause
        integer, intent(in) :: page
        procedure(generate_fact) :: generate
        character(len=1024) :: mutated

        mutated = '(grammar-fact (id '//trim(rule)//') (expression "'//trim(expression)//'") '// &
            '(source (document J3-24-007) (clause '//trim(clause)//') (rule '//trim(rule)//') (page '// &
            trim(adjustl(itoa(page)))//' ) (source-sha256 '//source_hash//')) '// &
            '(origin mechanical) (resolution resolved))'
        call sx_parse(trim(mutated), node, ok, message)
        call require(ok, message)
        open (newunit=output_unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open mutation output')
        call generate(node, output_unit, ok, message)
        close (output_unit)
        call require(.not. ok, 'mutated PRINT grammar fact was accepted')
    end subroutine reject

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text

        write (text, '(i0)') value
    end function itoa

    subroutine read_source(path, lines, count)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: lines(:)
        integer, intent(out) :: count
        integer :: local_unit, local_ios

        count = 0
        open (newunit=local_unit, file=path, action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open generated source')
        do
            read (local_unit, '(a)', iostat=local_ios) line
            if (local_ios /= 0) exit
            count = count + 1
            lines(count) = line
        end do
        close (local_unit)
    end subroutine read_source

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure)
            error stop 1
        end if
    end subroutine require

end program test_standardir_print_grammar_fact

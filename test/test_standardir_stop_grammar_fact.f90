program test_standardir_stop_grammar_fact
    !! Fixed SX and source mutations are the independent generator oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_stop_code_grammar_fact, &
        standardir_generate_stop_stmt_grammar_fact
    use standardir_stop_code_grammar_fact, only: standardir_consume_stop_code_grammar_fact
    use standardir_stop_stmt_grammar_fact, only: standardir_consume_stop_stmt_grammar_fact
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
        subroutine consume_fact(node, ok, message)
            import :: sx_node_t
            type(sx_node_t), intent(in) :: node
            logical, intent(out) :: ok
            character(len=*), intent(out) :: message
        end subroutine consume_fact
    end interface

    call read_fact('R1162', node)
    call check_generated(node, 'build/standardir_stop_stmt_grammar_fact_generated.f90', &
        'src/standardir_stop_stmt_grammar_fact_generated.f90', &
        standardir_generate_stop_stmt_grammar_fact, &
        'STOP [ stop-code ] [ , QUIET = scalar-logical-expr ]')
    call check_consumer('(grammar-fact (id R1162) (expression "STOP [ stop-code ] '// &
        '[ , QUIET = scalar-logical-expr ]") (source (source-ref (document J3-24-007) '// &
        '(clause 11) (rule R1162) (page 214) (source-hash '//source_hash//'))) '// &
        '(origin mechanical) (resolution resolved))', standardir_consume_stop_stmt_grammar_fact)
    call read_fact('R1164', node)
    call check_generated(node, 'build/standardir_stop_code_grammar_fact_generated.f90', &
        'src/standardir_stop_code_grammar_fact_generated.f90', &
        standardir_generate_stop_code_grammar_fact, &
        'scalar-default-char-expr | scalar-int-expr')
    call check_consumer('(grammar-fact (id R1164) (expression "scalar-default-char-expr | '// &
        'scalar-int-expr") (source (source-ref (document J3-24-007) (clause 11) '// &
        '(rule R1164) (page 214) (source-hash '//source_hash//'))) '// &
        '(origin mechanical) (resolution resolved))', standardir_consume_stop_code_grammar_fact)

    call reject('(grammar-fact (id R1162) (expression "STOP [ stop-code ]") '// &
        '(source (document J3-24-007) (clause 11) (rule R1162) (page 214) '// &
        '(source-sha256 '//source_hash//')) (origin mechanical) (resolution resolved))', &
        standardir_generate_stop_stmt_grammar_fact)
    call reject('(grammar-fact (id R1164) (expression "scalar-default-char-expr | scalar-int-expr") '// &
        '(source (document J3-24-007) (clause 11) (rule R1164) (page 215) '// &
        '(source-sha256 '//source_hash//')) (origin mechanical) (resolution resolved))', &
        standardir_generate_stop_code_grammar_fact)

    print '(a)', 'StandardIR R1162/R1164 stop grammar fact test passed'

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

    subroutine check_generated(value, fresh_path, checked_path, generate, expected)
        type(sx_node_t), intent(in) :: value
        character(len=*), intent(in) :: fresh_path, checked_path, expected
        procedure(generate_fact) :: generate

        open (newunit=output_unit, file=fresh_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open generated output')
        call generate(value, output_unit, ok, message)
        close (output_unit)
        call require(ok, message)
        call read_source(fresh_path, fresh, fresh_count)
        call read_source(checked_path, checked, checked_count)
        call require(fresh_count == checked_count, 'generated output is stale')
        call require(all(fresh(:fresh_count) == checked(:checked_count)), &
            'generated output differs from canonical source')
        call require(any(index(fresh(:fresh_count), trim(expected)) > 0), &
            'generated expression differs from canonical source')
    end subroutine check_generated

    subroutine check_consumer(text, consume)
        character(len=*), intent(in) :: text
        procedure(consume_fact) :: consume

        call sx_parse(text, node, ok, message)
        call require(ok, message)
        call consume(node, ok, message)
        call require(ok, message)
    end subroutine check_consumer

    subroutine reject(text, generate)
        character(len=*), intent(in) :: text
        procedure(generate_fact) :: generate

        call sx_parse(text, node, ok, message)
        call require(ok, message)
        open (newunit=output_unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open mutation output')
        call generate(node, output_unit, ok, message)
        close (output_unit)
        call require(.not. ok, 'mutated stop grammar fact was accepted')
    end subroutine reject

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

end program test_standardir_stop_grammar_fact

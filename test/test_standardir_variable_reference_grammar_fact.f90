program test_standardir_variable_reference_grammar_fact
    !! Canonical R901-R903 rows are the independent source-backed oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: &
        standardir_generate_designator_grammar_fact, standardir_generate_variable_grammar_fact, &
        standardir_generate_variable_name_grammar_fact
    implicit none

    character(len=*), parameter :: hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=1024) :: source, message, line
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

    call read_fact('R901', node)
    call check_generated(node, 'build/standardir_designator_grammar_fact_generated.f90', &
        'src/standardir_designator_grammar_fact_generated.f90', &
        standardir_generate_designator_grammar_fact, &
        'object-name | array-element | array-section | coindexed-named-object | '// &
        'complex-part-designator | structure-component | substring')

    call read_fact('R902', node)
    call check_generated(node, 'build/standardir_variable_grammar_fact_generated.f90', &
        'src/standardir_variable_grammar_fact_generated.f90', standardir_generate_variable_grammar_fact, &
        'designator | function-reference')

    call read_fact('R903', node)
    call check_generated(node, 'build/standardir_variable_name_grammar_fact_generated.f90', &
        'src/standardir_variable_name_grammar_fact_generated.f90', &
        standardir_generate_variable_name_grammar_fact, 'name')

    call read_fact('R901', node)
    node%children(4)%children(5)%children(2)%atom = '151'
    call mutate_and_reject(node, standardir_generate_designator_grammar_fact)
    call read_fact('R902', node)
    node%children(4)%children(4)%children(2)%atom = 'R999'
    call mutate_and_reject(node, standardir_generate_variable_grammar_fact)
    call read_fact('R903', node)
    node%children(3)%children(2)%atom = 'mutated'
    call mutate_and_reject(node, standardir_generate_variable_name_grammar_fact)

    print '(a)', 'StandardIR R901-R903 variable-reference grammar fact test passed'

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
        call require(ios == 0, 'could not read canonical grammar-fact row')
        call sx_parse(trim(source), result, ok, message)
        call require(ok, message)
    end subroutine read_fact

    subroutine check_generated(node, fresh_path, checked_path, generate, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: fresh_path, checked_path, expected
        procedure(generate_fact) :: generate

        open (newunit=output_unit, file=fresh_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open generated output')
        call generate(node, output_unit, ok, message)
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

    subroutine mutate_and_reject(node, generate)
        type(sx_node_t), intent(in) :: node
        procedure(generate_fact) :: generate

        open (newunit=output_unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'could not open mutation output')
        call generate(node, output_unit, ok, message)
        close (output_unit)
        call require(.not. ok, 'mutated grammar fact was accepted')
    end subroutine mutate_and_reject

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
        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_standardir_variable_reference_grammar_fact

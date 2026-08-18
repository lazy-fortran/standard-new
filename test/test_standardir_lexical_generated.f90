program test_standardir_lexical_generated
    !! Declarative SX and mutation cases are the independent generator oracle.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexical, only: standardir_lexical_fact_t, standardir_lexical_facts_t, &
        standardir_lexical_lookup, &
        standardir_lexical_lookup_match, standardir_lexical_resolve_spelling
    use standardir_lexical_codegen, only: standardir_generate_lexical_facts
    use standardir_lexical_facts_generated, only: standardir_make_lexical_facts
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    character(len=4096) :: source, line, message
    character(len=4096), allocatable :: fresh(:), checked(:)
    type(sx_node_t), allocatable :: nodes(:)
    type(sx_node_t) :: node
    type(standardir_lexical_facts_t) :: facts
    type(standardir_lexical_fact_t) :: result
    integer :: input_unit, output_unit, ios, count, i, status
    integer :: fresh_count, checked_count
    logical :: ok
    character(len=256) :: spelling

    call read_source(nodes, count)
    call check_generated_source(nodes, count, fresh, fresh_count)
    call read_lines('src/standardir_lexical_facts_generated.f90', checked, checked_count)
    call require(fresh_count == checked_count, 'generated lexical source is stale')
    call require(all(fresh(:fresh_count) == checked(:checked_count)), &
        'generated lexical source differs from specification')

    call standardir_make_lexical_facts(facts, ok, message)
    call require(ok, message)
    call require(facts%count == 5, 'generated lexical fact count differs')
    call require(trim(facts%facts(4)%source_term) == '–', 'Unicode source spelling differs')
    call require(trim(facts%facts(4)%canonical_spelling) == '-', &
        'Unicode canonical spelling differs')
    call require(trim(facts%facts(4)%codepoint) == 'U+2013', 'Unicode codepoint differs')
    call require(trim(facts%facts(4)%source_hash) == source_hash, 'source hash differs')
    call standardir_lexical_lookup(facts, int(z'2013', int64), result, status, message)
    call require(status == standardir_lexical_lookup_match, 'generated Unicode lookup failed')
    call standardir_lexical_resolve_spelling(result, spelling, ok, message)
    call require(ok .and. trim(spelling) == '-', 'generated canonical spelling failed')

    call mutate_and_reject('(source-term "—")', nodes(4), 'changed Unicode source spelling')
    call mutate_and_reject('(source-hash deadbeef)', nodes(1), 'changed source hash')
    print '(a)', 'StandardIR generated lexical test passed'

contains

    subroutine read_source(output, output_count)
        type(sx_node_t), allocatable, intent(out) :: output(:)
        integer, intent(out) :: output_count
        character(len=4096) :: text
        type(sx_node_t) :: local_node
        integer :: unit, local_ios
        logical :: local_ok

        allocate (output(5))
        open (newunit=unit, file='specs/lexical-facts-v0.sx', action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open lexical specification')
        output_count = 0
        do
            read (unit, '(a)', iostat=local_ios) text
            if (local_ios /= 0) exit
            call sx_parse(trim(text), local_node, local_ok, message)
            call require(local_ok, message)
            output_count = output_count + 1
            output(output_count) = local_node
        end do
        close (unit)
        call require(output_count == 5, 'lexical specification row count differs')
    end subroutine read_source

    subroutine check_generated_source(input, input_count, output, output_count)
        type(sx_node_t), intent(in) :: input(:)
        integer, intent(in) :: input_count
        character(len=4096), allocatable, intent(out) :: output(:)
        integer, intent(out) :: output_count

        open (newunit=output_unit, file='build/standardir_lexical_facts_generated.f90', &
            status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open fresh generated output')
        call standardir_generate_lexical_facts(input(:input_count), output_unit, ok, message)
        close (output_unit)
        call require(ok, message)
        call read_lines('build/standardir_lexical_facts_generated.f90', output, output_count)
    end subroutine check_generated_source

    subroutine mutate_and_reject(replacement, input, description)
        character(len=*), intent(in) :: replacement, description
        type(sx_node_t), intent(in) :: input
        type(sx_node_t) :: mutated
        type(sx_node_t) :: mutation_rows(5)
        integer :: mutation_unit

        call require(input%child_count > 0, 'mutation input is empty')
        mutated = input
        call sx_parse('(lexical-fact (source-term "—") (class lexical-class) '// &
            '(target EN_DASH) (rule R1010) (codepoint U+2013) '// &
            '(source (document J3-24-007) (clause R1010) (page 69) '// &
            '(source-sha256 '//source_hash//')))', mutated, ok, message)
        if (index(replacement, 'source-hash') > 0) then
            call sx_parse('(lexical-fact (source-term letter) (class lexical-class) '// &
                '(target LETTER) (rule P6.1.2-3) (codepoint U+0041-U+005A,U+0061-U+007A) '// &
                '(source (document J3-24-007) (clause P6.1.2-3) (page 53) '// &
                '(source-sha256 deadbeef)))', mutated, ok, message)
        end if
        call require(ok, message)
        mutation_rows = input
        mutation_rows(1) = mutated
        open (newunit=mutation_unit, status='scratch', action='write', iostat=ios)
        call require(ios == 0, 'could not open mutation output')
        call standardir_generate_lexical_facts(mutation_rows, mutation_unit, ok, message)
        close (mutation_unit)
        call require(.not. ok, description//' was accepted')
    end subroutine mutate_and_reject

    subroutine read_lines(path, output, output_count)
        character(len=*), intent(in) :: path
        character(len=4096), allocatable, intent(out) :: output(:)
        integer, intent(out) :: output_count
        integer :: unit, local_ios
        character(len=4096) :: text

        allocate (output(512))
        output_count = 0
        open (newunit=unit, file=path, action='read', iostat=local_ios)
        call require(local_ios == 0, 'could not open generated source')
        do
            read (unit, '(a)', iostat=local_ios) text
            if (local_ios /= 0) exit
            output_count = output_count + 1
            output(output_count) = text
        end do
        close (unit)
    end subroutine read_lines

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_standardir_lexical_generated

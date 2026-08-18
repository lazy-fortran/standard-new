program test_generate_expression_fact_table
    use fortsx, only: sx_node_t, sx_parse
    use standardir_grammar_fact_codegen, only: standardir_generate_expression_fact_table
    use standardir_expression_fact_generated, only: standardir_expression_fact_t, &
        standardir_expression_fact_count, standardir_make_expression_fact_table, &
        standardir_lookup_expression_fact
    implicit none

    type(sx_node_t) :: nodes(32)
    type(standardir_expression_fact_t) :: values(standardir_expression_fact_count), value
    character(len=1024) :: source, message
    integer :: count, ios, unit
    logical :: ok, found

    open (newunit=unit, file='specs/grammar-facts-v0.sx', action='read', iostat=ios)
    if (ios /= 0) error stop 'could not open grammar-facts specification'
    count = 0
    do
        read (unit, '(a)', iostat=ios) source
        if (ios /= 0) exit
        count = count + 1
        call sx_parse(trim(source), nodes(count), ok, message)
        if (.not. ok) error stop trim(message)
    end do
    close (unit)

    call standardir_make_expression_fact_table(values)
    if (trim(values(1)%fact%id) /= 'R1006' .or. &
        trim(values(1)%fact%expression) /= '[ add-operand mult-op ] mult-operand') &
        error stop 'R1006 expression fact differs'
    call standardir_lookup_expression_fact('R1009', '/', value, found)
    if (.not. found .or. value%fact%source%page /= 155) error stop 'R1009 lookup failed'
    call standardir_lookup_expression_fact('R1010', '–', value, found)
    if (.not. found .or. trim(value%fact%source%rule) /= 'R1010') error stop 'R1010 lookup failed'
    call standardir_lookup_expression_fact('R1010', '−', value, found)
    if (found) error stop 'unlisted expression fact was accepted'

    open (newunit=unit, file='build/standardir_expression_fact_generated.f90', &
        status='replace', action='write', iostat=ios)
    if (ios /= 0) error stop 'could not open generated output'
    call standardir_generate_expression_fact_table(nodes(:count), unit, ok, message)
    close (unit)
    if (.not. ok) error stop trim(message)
end program test_generate_expression_fact_table

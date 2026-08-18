program test_power_op_fact
    use standardir_expression_fact_generated, only: standardir_expression_fact_t, &
        standardir_lookup_expression_fact
    implicit none

    type(standardir_expression_fact_t) :: value
    logical :: found

    call standardir_lookup_expression_fact('R1008', '**', value, found)
    if (.not. found) error stop 'R1008 power-op fact is missing'
    if (trim(value%fact%source%document) /= 'J3-24-007' .or. &
        trim(value%fact%source%clause) /= '10' .or. &
        trim(value%fact%source%rule) /= 'R1008' .or. &
        value%fact%source%page /= 155 .or. &
        trim(value%fact%source%source_hash) /= &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2') &
        error stop 'R1008 power-op provenance differs'

    call standardir_lookup_expression_fact('R1008', '*', value, found)
    if (found) error stop 'R1008 accepted a non-power operator'
end program test_power_op_fact

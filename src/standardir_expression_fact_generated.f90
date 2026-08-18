module standardir_expression_fact_generated
    !! Generated from specs/grammar-facts-v0.sx; do not edit.

    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, &
        RESOLUTION_RESOLVED
    implicit none
    private

    type, public :: standardir_expression_fact_t
        type(grammar_fact_t) :: fact
    end type standardir_expression_fact_t

    integer, parameter, public :: standardir_expression_fact_count = 6
    public :: standardir_make_expression_fact_table
    public :: standardir_lookup_expression_fact

contains

    subroutine standardir_make_expression_fact_table(values)
        type(standardir_expression_fact_t), intent(out) :: values(6)

        values(1)%fact%id = 'R1006'
        values(1)%fact%expression = '[ add-operand mult-op ] mult-operand'
        values(1)%fact%source%document = 'J3-24-007'
        values(1)%fact%source%clause = '10'
        values(1)%fact%source%rule = 'R1006'
        values(1)%fact%source%page = 155
        values(1)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(1)%fact%origin = ORIGIN_MECHANICAL
        values(1)%fact%resolution = RESOLUTION_RESOLVED
        values(2)%fact%id = 'R1009'
        values(2)%fact%expression = '*'
        values(2)%fact%source%document = 'J3-24-007'
        values(2)%fact%source%clause = '10'
        values(2)%fact%source%rule = 'R1009'
        values(2)%fact%source%page = 155
        values(2)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(2)%fact%origin = ORIGIN_MECHANICAL
        values(2)%fact%resolution = RESOLUTION_RESOLVED
        values(3)%fact%id = 'R1009'
        values(3)%fact%expression = '/'
        values(3)%fact%source%document = 'J3-24-007'
        values(3)%fact%source%clause = '10'
        values(3)%fact%source%rule = 'R1009'
        values(3)%fact%source%page = 155
        values(3)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(3)%fact%origin = ORIGIN_MECHANICAL
        values(3)%fact%resolution = RESOLUTION_RESOLVED
        values(4)%fact%id = 'R1010'
        values(4)%fact%expression = '+'
        values(4)%fact%source%document = 'J3-24-007'
        values(4)%fact%source%clause = '10'
        values(4)%fact%source%rule = 'R1010'
        values(4)%fact%source%page = 155
        values(4)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(4)%fact%origin = ORIGIN_MECHANICAL
        values(4)%fact%resolution = RESOLUTION_RESOLVED
        values(5)%fact%id = 'R1010'
        values(5)%fact%expression = '–'
        values(5)%fact%source%document = 'J3-24-007'
        values(5)%fact%source%clause = '10'
        values(5)%fact%source%rule = 'R1010'
        values(5)%fact%source%page = 155
        values(5)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(5)%fact%origin = ORIGIN_MECHANICAL
        values(5)%fact%resolution = RESOLUTION_RESOLVED
        values(6)%fact%id = 'R1008'
        values(6)%fact%expression = '**'
        values(6)%fact%source%document = 'J3-24-007'
        values(6)%fact%source%clause = '10'
        values(6)%fact%source%rule = 'R1008'
        values(6)%fact%source%page = 155
        values(6)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(6)%fact%origin = ORIGIN_MECHANICAL
        values(6)%fact%resolution = RESOLUTION_RESOLVED
    end subroutine standardir_make_expression_fact_table

    subroutine standardir_lookup_expression_fact(id, expression, value, found)
        character(len=*), intent(in) :: id, expression
        type(standardir_expression_fact_t), intent(out) :: value
        logical, intent(out) :: found

        type(standardir_expression_fact_t) :: values(6)
        integer :: i

        call standardir_make_expression_fact_table(values)
        value%fact%id = ''
        value%fact%expression = ''
        value%fact%source%document = ''
        value%fact%source%clause = ''
        value%fact%source%rule = ''
        value%fact%source%page = 0
        value%fact%source%source_hash = ''
        value%fact%origin = 0
        value%fact%resolution = 0
        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%fact%id) == trim(id) .and. &
                trim(values(i)%fact%expression) == trim(expression)) then
                value = values(i)
                found = .true.
                return
            end if
        end do
    end subroutine standardir_lookup_expression_fact

end module standardir_expression_fact_generated

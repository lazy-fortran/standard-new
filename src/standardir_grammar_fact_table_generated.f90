module standardir_grammar_fact_table_generated
    !! Generated from specs/grammar-facts-v0.sx; do not edit.

    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, &
        RESOLUTION_RESOLVED
    implicit none
    private

    type, public :: standardir_grammar_fact_table_entry_t
        type(grammar_fact_t) :: fact
    end type standardir_grammar_fact_table_entry_t

    integer, parameter, public :: standardir_grammar_fact_table_count = 25
    public :: standardir_make_grammar_fact_table
    public :: standardir_lookup_grammar_fact

contains

    subroutine standardir_make_grammar_fact_table(values)
        type(standardir_grammar_fact_table_entry_t), intent(out) :: values(25)

        values(1)%fact%id = 'R501'
        values(1)%fact%expression = 'program-unit [ program-unit ] ...'
        values(1)%fact%source%document = 'J3-24-007'
        values(1)%fact%source%clause = '5'
        values(1)%fact%source%rule = 'R501'
        values(1)%fact%source%page = 53
        values(1)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(1)%fact%origin = ORIGIN_MECHANICAL
        values(1)%fact%resolution = RESOLUTION_RESOLVED
        values(2)%fact%id = 'R705'
        values(2)%fact%expression = 'INTEGER [ kind-selector ]'
        values(2)%fact%source%document = 'J3-24-007'
        values(2)%fact%source%clause = '7'
        values(2)%fact%source%rule = 'R705'
        values(2)%fact%source%page = 67
        values(2)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(2)%fact%origin = ORIGIN_MECHANICAL
        values(2)%fact%resolution = RESOLUTION_RESOLVED
        values(3)%fact%id = 'R706'
        values(3)%fact%expression = 'REAL [ kind-selector ]'
        values(3)%fact%source%document = 'J3-24-007'
        values(3)%fact%source%clause = '7'
        values(3)%fact%source%rule = 'R706'
        values(3)%fact%source%page = 67
        values(3)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(3)%fact%origin = ORIGIN_MECHANICAL
        values(3)%fact%resolution = RESOLUTION_RESOLVED
        values(4)%fact%id = 'R707'
        values(4)%fact%expression = 'DOUBLE PRECISION'
        values(4)%fact%source%document = 'J3-24-007'
        values(4)%fact%source%clause = '7'
        values(4)%fact%source%rule = 'R707'
        values(4)%fact%source%page = 67
        values(4)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(4)%fact%origin = ORIGIN_MECHANICAL
        values(4)%fact%resolution = RESOLUTION_RESOLVED
        values(5)%fact%id = 'R704'
        values(5)%fact%expression = 'or COMPLEX [ kind-selector ]'
        values(5)%fact%source%document = 'J3-24-007'
        values(5)%fact%source%clause = '7'
        values(5)%fact%source%rule = 'R704'
        values(5)%fact%source%page = 80
        values(5)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(5)%fact%origin = ORIGIN_MECHANICAL
        values(5)%fact%resolution = RESOLUTION_RESOLVED
        values(6)%fact%id = 'R704'
        values(6)%fact%expression = 'or LOGICAL [ kind-selector ]'
        values(6)%fact%source%document = 'J3-24-007'
        values(6)%fact%source%clause = '7'
        values(6)%fact%source%rule = 'R704'
        values(6)%fact%source%page = 80
        values(6)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(6)%fact%origin = ORIGIN_MECHANICAL
        values(6)%fact%resolution = RESOLUTION_RESOLVED
        values(7)%fact%id = 'R704'
        values(7)%fact%expression = 'or CHARACTER [ char-selector ]'
        values(7)%fact%source%document = 'J3-24-007'
        values(7)%fact%source%clause = '7'
        values(7)%fact%source%rule = 'R704'
        values(7)%fact%source%page = 80
        values(7)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(7)%fact%origin = ORIGIN_MECHANICAL
        values(7)%fact%resolution = RESOLUTION_RESOLVED
        values(8)%fact%id = 'R1033'
        values(8)%fact%expression = 'variable = expr'
        values(8)%fact%source%document = 'J3-24-007'
        values(8)%fact%source%clause = '10'
        values(8)%fact%source%rule = 'R1033'
        values(8)%fact%source%page = 188
        values(8)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(8)%fact%origin = ORIGIN_MECHANICAL
        values(8)%fact%resolution = RESOLUTION_RESOLVED
        values(9)%fact%id = 'R1007'
        values(9)%fact%expression = '[ [ level-2-expr ] add-op ] add-operand'
        values(9)%fact%source%document = 'J3-24-007'
        values(9)%fact%source%clause = '10'
        values(9)%fact%source%rule = 'R1007'
        values(9)%fact%source%page = 155
        values(9)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(9)%fact%origin = ORIGIN_MECHANICAL
        values(9)%fact%resolution = RESOLUTION_RESOLVED
        values(10)%fact%id = 'R1006'
        values(10)%fact%expression = '[ add-operand mult-op ] mult-operand'
        values(10)%fact%source%document = 'J3-24-007'
        values(10)%fact%source%clause = '10'
        values(10)%fact%source%rule = 'R1006'
        values(10)%fact%source%page = 155
        values(10)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(10)%fact%origin = ORIGIN_MECHANICAL
        values(10)%fact%resolution = RESOLUTION_RESOLVED
        values(11)%fact%id = 'R1009'
        values(11)%fact%expression = '*'
        values(11)%fact%source%document = 'J3-24-007'
        values(11)%fact%source%clause = '10'
        values(11)%fact%source%rule = 'R1009'
        values(11)%fact%source%page = 155
        values(11)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(11)%fact%origin = ORIGIN_MECHANICAL
        values(11)%fact%resolution = RESOLUTION_RESOLVED
        values(12)%fact%id = 'R1009'
        values(12)%fact%expression = '/'
        values(12)%fact%source%document = 'J3-24-007'
        values(12)%fact%source%clause = '10'
        values(12)%fact%source%rule = 'R1009'
        values(12)%fact%source%page = 155
        values(12)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(12)%fact%origin = ORIGIN_MECHANICAL
        values(12)%fact%resolution = RESOLUTION_RESOLVED
        values(13)%fact%id = 'R1010'
        values(13)%fact%expression = '+'
        values(13)%fact%source%document = 'J3-24-007'
        values(13)%fact%source%clause = '10'
        values(13)%fact%source%rule = 'R1010'
        values(13)%fact%source%page = 155
        values(13)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(13)%fact%origin = ORIGIN_MECHANICAL
        values(13)%fact%resolution = RESOLUTION_RESOLVED
        values(14)%fact%id = 'R1010'
        values(14)%fact%expression = '–'
        values(14)%fact%source%document = 'J3-24-007'
        values(14)%fact%source%clause = '10'
        values(14)%fact%source%rule = 'R1010'
        values(14)%fact%source%page = 155
        values(14)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(14)%fact%origin = ORIGIN_MECHANICAL
        values(14)%fact%resolution = RESOLUTION_RESOLVED
        values(15)%fact%id = 'R1008'
        values(15)%fact%expression = '**'
        values(15)%fact%source%document = 'J3-24-007'
        values(15)%fact%source%clause = '10'
        values(15)%fact%source%rule = 'R1008'
        values(15)%fact%source%page = 155
        values(15)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(15)%fact%origin = ORIGIN_MECHANICAL
        values(15)%fact%resolution = RESOLUTION_RESOLVED
        values(16)%fact%id = 'R708'
        values(16)%fact%expression = 'digit-string [ _ kind-param ]'
        values(16)%fact%source%document = 'J3-24-007'
        values(16)%fact%source%clause = '7'
        values(16)%fact%source%rule = 'R708'
        values(16)%fact%source%page = 66
        values(16)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(16)%fact%origin = ORIGIN_MECHANICAL
        values(16)%fact%resolution = RESOLUTION_RESOLVED
        values(17)%fact%id = 'R901'
        values(17)%fact%expression = 'object-name | array-element | array-section | coindexed-named-object | complex-part-designator | structure-component | substring'
        values(17)%fact%source%document = 'J3-24-007'
        values(17)%fact%source%clause = '5-15'
        values(17)%fact%source%rule = 'R901'
        values(17)%fact%source%page = 150
        values(17)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(17)%fact%origin = ORIGIN_MECHANICAL
        values(17)%fact%resolution = RESOLUTION_RESOLVED
        values(18)%fact%id = 'R902'
        values(18)%fact%expression = 'designator | function-reference'
        values(18)%fact%source%document = 'J3-24-007'
        values(18)%fact%source%clause = '5-15'
        values(18)%fact%source%rule = 'R902'
        values(18)%fact%source%page = 150
        values(18)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(18)%fact%origin = ORIGIN_MECHANICAL
        values(18)%fact%resolution = RESOLUTION_RESOLVED
        values(19)%fact%id = 'R903'
        values(19)%fact%expression = 'name'
        values(19)%fact%source%document = 'J3-24-007'
        values(19)%fact%source%clause = '5-15'
        values(19)%fact%source%rule = 'R903'
        values(19)%fact%source%page = 150
        values(19)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(19)%fact%origin = ORIGIN_MECHANICAL
        values(19)%fact%resolution = RESOLUTION_RESOLVED
        values(20)%fact%id = 'R509'
        values(20)%fact%expression = 'executable-construct [ execution-part-construct ] ...'
        values(20)%fact%source%document = 'J3-24-007'
        values(20)%fact%source%clause = '5'
        values(20)%fact%source%rule = 'R509'
        values(20)%fact%source%page = 45
        values(20)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(20)%fact%origin = ORIGIN_MECHANICAL
        values(20)%fact%resolution = RESOLUTION_RESOLVED
        values(21)%fact%id = 'R1162'
        values(21)%fact%expression = 'STOP [ stop-code ] [ , QUIET = scalar-logical-expr ]'
        values(21)%fact%source%document = 'J3-24-007'
        values(21)%fact%source%clause = '11'
        values(21)%fact%source%rule = 'R1162'
        values(21)%fact%source%page = 214
        values(21)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(21)%fact%origin = ORIGIN_MECHANICAL
        values(21)%fact%resolution = RESOLUTION_RESOLVED
        values(22)%fact%id = 'R1164'
        values(22)%fact%expression = 'scalar-default-char-expr | scalar-int-expr'
        values(22)%fact%source%document = 'J3-24-007'
        values(22)%fact%source%clause = '11'
        values(22)%fact%source%rule = 'R1164'
        values(22)%fact%source%page = 214
        values(22)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(22)%fact%origin = ORIGIN_MECHANICAL
        values(22)%fact%resolution = RESOLUTION_RESOLVED
        values(23)%fact%id = 'R1212'
        values(23)%fact%expression = 'PRINT format [ , output-item-list ]'
        values(23)%fact%source%document = 'J3-24-007'
        values(23)%fact%source%clause = '12.6.1'
        values(23)%fact%source%rule = 'R1212'
        values(23)%fact%source%page = 242
        values(23)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(23)%fact%origin = ORIGIN_MECHANICAL
        values(23)%fact%resolution = RESOLUTION_RESOLVED
        values(24)%fact%id = 'R1215'
        values(24)%fact%expression = 'default-char-expr | label | *'
        values(24)%fact%source%document = 'J3-24-007'
        values(24)%fact%source%clause = '12.6.2.2'
        values(24)%fact%source%rule = 'R1215'
        values(24)%fact%source%page = 244
        values(24)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(24)%fact%origin = ORIGIN_MECHANICAL
        values(24)%fact%resolution = RESOLUTION_RESOLVED
        values(25)%fact%id = 'R1217'
        values(25)%fact%expression = 'expr | io-implied-do'
        values(25)%fact%source%document = 'J3-24-007'
        values(25)%fact%source%clause = '12.6.3'
        values(25)%fact%source%rule = 'R1217'
        values(25)%fact%source%page = 248
        values(25)%fact%source%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(25)%fact%origin = ORIGIN_MECHANICAL
        values(25)%fact%resolution = RESOLUTION_RESOLVED
    end subroutine standardir_make_grammar_fact_table

    subroutine standardir_lookup_grammar_fact(id, expression, value, found)
        character(len=*), intent(in) :: id, expression
        type(standardir_grammar_fact_table_entry_t), intent(out) :: value
        logical, intent(out) :: found

        type(standardir_grammar_fact_table_entry_t) :: values(25)
        integer :: i

        call standardir_make_grammar_fact_table(values)
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
    end subroutine standardir_lookup_grammar_fact

end module standardir_grammar_fact_table_generated

program test_grammar_fact_table_collection
    use schema_v0_generated, only: ORIGIN_MECHANICAL, RESOLUTION_RESOLVED
    use standardir_grammar_fact_table_generated, only: &
        STANDARDIR_GRAMMAR_FACT_COLLECTION_CAPACITY, &
        STANDARDIR_GRAMMAR_FACT_COLLECTION_MALFORMED, &
        STANDARDIR_GRAMMAR_FACT_COLLECTION_MISSING, &
        STANDARDIR_GRAMMAR_FACT_COLLECTION_SUCCESS, &
        standardir_collect_grammar_facts, standardir_grammar_fact_table_entry_t
    implicit none

    character(len=*), parameter :: source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    type(standardir_grammar_fact_table_entry_t) :: output(3)
    type(standardir_grammar_fact_table_entry_t) :: small_output(2)
    integer :: count, status

    call standardir_collect_grammar_facts('R501', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_SUCCESS .and. count == 1, &
        'unique rule collection failed')
    call require_fact(output(1), 'R501', 'program-unit [ program-unit ] ...', '5', 'R501', 53)

    call seed(output)
    call standardir_collect_grammar_facts('R704', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_SUCCESS .and. count == 3, &
        'duplicate rule collection failed')
    call require_fact(output(1), 'R704', 'or COMPLEX [ kind-selector ]', '7', 'R704', 80)
    call require_fact(output(2), 'R704', 'or LOGICAL [ kind-selector ]', '7', 'R704', 80)
    call require_fact(output(3), 'R704', 'or CHARACTER [ char-selector ]', '7', 'R704', 80)

    call seed(output)
    call standardir_collect_grammar_facts('R999', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_MISSING .and. count == 0, &
        'missing rule outcome failed')
    call require_cleared(output, 'missing rule did not clear output')

    call seed(output)
    call standardir_collect_grammar_facts('', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_MALFORMED .and. count == 0, &
        'empty identifier outcome failed')
    call require_cleared(output, 'empty identifier did not clear output')

    call seed(output)
    call standardir_collect_grammar_facts('R70X', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_MALFORMED .and. count == 0, &
        'malformed identifier outcome failed')
    call require_cleared(output, 'malformed identifier did not clear output')

    call seed(small_output)
    call standardir_collect_grammar_facts('R704', small_output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_CAPACITY .and. count == 0, &
        'capacity outcome failed')
    call require_cleared(small_output, 'capacity outcome did not clear output')

    call seed(output)
    call standardir_collect_grammar_facts('R704', output, count, status)
    call require(status == STANDARDIR_GRAMMAR_FACT_COLLECTION_SUCCESS .and. count == 3, &
        'sufficient-capacity retry failed')
    call require_fact(output(1), 'R704', 'or COMPLEX [ kind-selector ]', '7', 'R704', 80)
    call require_fact(output(2), 'R704', 'or LOGICAL [ kind-selector ]', '7', 'R704', 80)
    call require_fact(output(3), 'R704', 'or CHARACTER [ char-selector ]', '7', 'R704', 80)
    print '(a)', 'grammar-fact table collection test passed'

contains

    subroutine require_fact(value, id, expression, clause, rule, page)
        type(standardir_grammar_fact_table_entry_t), intent(in) :: value
        character(len=*), intent(in) :: id, expression, clause, rule
        integer, intent(in) :: page

        call require(trim(value%fact%id) == id .and. trim(value%fact%expression) == expression, &
            'collected fact identity or expression differs')
        call require(trim(value%fact%source%document) == 'J3-24-007' .and. &
            trim(value%fact%source%clause) == clause .and. &
            trim(value%fact%source%rule) == rule .and. value%fact%source%page == page .and. &
            trim(value%fact%source%source_hash) == source_hash .and. &
            value%fact%origin == ORIGIN_MECHANICAL .and. &
            value%fact%resolution == RESOLUTION_RESOLVED, 'collected provenance differs')
    end subroutine require_fact

    subroutine seed(values)
        type(standardir_grammar_fact_table_entry_t), intent(out) :: values(:)
        integer :: i

        do i = 1, size(values)
            values(i)%fact%id = 'STALE'
            values(i)%fact%expression = 'stale expression'
            values(i)%fact%source%document = 'STALE-DOC'
            values(i)%fact%source%clause = 'stale-clause'
            values(i)%fact%source%rule = 'STALE-RULE'
            values(i)%fact%source%page = -1
            values(i)%fact%source%source_hash = 'stale-hash'
            values(i)%fact%origin = -1
            values(i)%fact%resolution = -1
        end do
    end subroutine seed

    subroutine require_cleared(values, failure)
        type(standardir_grammar_fact_table_entry_t), intent(in) :: values(:)
        character(len=*), intent(in) :: failure
        integer :: i

        do i = 1, size(values)
            call require(trim(values(i)%fact%id) == '' .and. &
                trim(values(i)%fact%expression) == '' .and. &
                trim(values(i)%fact%source%document) == '' .and. &
                trim(values(i)%fact%source%clause) == '' .and. &
                trim(values(i)%fact%source%rule) == '' .and. &
                values(i)%fact%source%page == 0 .and. &
                trim(values(i)%fact%source%source_hash) == '' .and. &
                values(i)%fact%origin == 0 .and. values(i)%fact%resolution == 0, failure)
        end do
    end subroutine require_cleared

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_grammar_fact_table_collection

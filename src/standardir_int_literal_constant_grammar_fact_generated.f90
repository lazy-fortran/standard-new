module standardir_int_literal_constant_grammar_fact
    !! Generated from specs/grammar-facts-v0.sx; do not edit.

    use fortsx, only: sx_node_t
    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, &
        RESOLUTION_RESOLVED, schema_consume_grammar_fact, schema_validate_grammar_fact, &
        schema_write_grammar_fact
    implicit none
    private

    character(len=*), parameter, public :: standardir_int_literal_constant_grammar_id = 'R708'
    character(len=*), parameter, public :: standardir_int_literal_constant_grammar_expression = &
        'digit-string [ _ kind-param ]'
    character(len=*), parameter :: standardir_int_literal_constant_grammar_document = 'J3-24-007'
    character(len=*), parameter :: standardir_int_literal_constant_grammar_clause = '7'
    character(len=*), parameter :: standardir_int_literal_constant_grammar_source_rule = 'R708'
    integer, parameter :: standardir_int_literal_constant_grammar_page = 66
    character(len=*), parameter :: standardir_int_literal_constant_grammar_source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'

    public :: standardir_make_int_literal_constant_grammar_fact
    public :: standardir_write_int_literal_constant_grammar_fact
    public :: standardir_consume_int_literal_constant_grammar_fact

contains

    subroutine standardir_make_int_literal_constant_grammar_fact(document, clause, source_rule, page, &
            source_hash, value, ok, message)
        character(len=*), intent(in) :: document, clause, source_rule, source_hash
        integer, intent(in) :: page
        type(grammar_fact_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (trim(document) /= standardir_int_literal_constant_grammar_document .or. &
            trim(clause) /= standardir_int_literal_constant_grammar_clause .or. &
            trim(source_rule) /= standardir_int_literal_constant_grammar_source_rule .or. &
            page /= standardir_int_literal_constant_grammar_page .or. &
            trim(source_hash) /= standardir_int_literal_constant_grammar_source_hash) then
            ok = .false.
            message = 'grammar-fact source provenance differs'
            return
        end if

        value%id = standardir_int_literal_constant_grammar_id
        value%expression = standardir_int_literal_constant_grammar_expression
        value%source%document = trim(document)
        value%source%clause = trim(clause)
        value%source%rule = trim(source_rule)
        value%source%page = page
        value%source%source_hash = trim(source_hash)
        value%origin = ORIGIN_MECHANICAL
        value%resolution = RESOLUTION_RESOLVED
        call schema_validate_grammar_fact(value, ok, message)
    end subroutine standardir_make_int_literal_constant_grammar_fact

    subroutine standardir_write_int_literal_constant_grammar_fact(unit, document, clause, source_rule, &
            page, source_hash, ok, message)
        integer, intent(in) :: unit, page
        character(len=*), intent(in) :: document, clause, source_rule, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(grammar_fact_t) :: value

        call standardir_make_int_literal_constant_grammar_fact(document, clause, source_rule, page, &
            source_hash, value, ok, message)
        if (.not. ok) return
        call schema_write_grammar_fact(value, unit, ok, message)
    end subroutine standardir_write_int_literal_constant_grammar_fact

    subroutine standardir_consume_int_literal_constant_grammar_fact(node, ok, message)
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call schema_consume_grammar_fact(node, check_fact, ok, message)

    contains

        subroutine check_fact(value, callback_ok, callback_message)
            type(grammar_fact_t), intent(in) :: value
            logical, intent(out) :: callback_ok
            character(len=*), intent(out) :: callback_message

            callback_ok = trim(value%id) == standardir_int_literal_constant_grammar_id .and. &
                trim(value%expression) == standardir_int_literal_constant_grammar_expression
            callback_message = ''
            if (.not. callback_ok) callback_message = 'int-literal-constant type-spec grammar fact differs'
        end subroutine check_fact

    end subroutine standardir_consume_int_literal_constant_grammar_fact

end module standardir_int_literal_constant_grammar_fact

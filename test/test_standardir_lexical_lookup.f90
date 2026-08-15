program test_standardir_lexical_lookup
    !! Constructed facts establish generic scalar lookup behavior.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_lexical, only: standardir_lexical_fact_t, &
        standardir_lexical_facts_t, standardir_lexical_lookup, &
        standardir_lexical_lookup_ambiguous, standardir_lexical_lookup_invalid_facts, &
        standardir_lexical_lookup_invalid_scalar, standardir_lexical_lookup_match, &
        standardir_lexical_lookup_no_match, standardir_lexical_lookup_unsupported, &
        standardir_lexical_reset, standardir_lexical_validate
    implicit none

    type(standardir_lexical_facts_t) :: facts
    type(standardir_lexical_fact_t) :: result
    integer :: status
    character(len=256) :: message
    logical :: ok

    call make_facts(facts)
    call standardir_lexical_lookup(facts, 70_int64, result, status, message)
    call require(status == standardir_lexical_lookup_match, 'range lookup failed')
    call require(trim(result%target_name) == 'TARGET_RANGE', 'range target differs')
    call require(trim(result%class_name) == 'constructed-class', 'range class differs')
    call require(trim(result%source_rule) == 'RULE-range', 'range rule differs')
    call require(trim(result%document) == 'constructed-document', 'range document differs')
    call require(trim(result%clause) == 'constructed-clause', 'range clause differs')
    call require(trim(result%source_hash) == repeat('a', 64), 'range provenance differs')

    call standardir_lexical_lookup(facts, 945_int64, result, status, message)
    call require(status == standardir_lexical_lookup_match, 'exact lookup failed')
    call require(trim(result%target_name) == 'TARGET_EXACT', 'exact target differs')

    call standardir_lexical_lookup(facts, 32_int64, result, status, message)
    call require(status == standardir_lexical_lookup_unsupported, &
        'processor-defined fact was not reported as unsupported')
    call require(trim(result%target_name) == 'TARGET_PROCESSOR', &
        'unsupported fact was not returned')

    facts%count = 2
    call standardir_lexical_lookup(facts, 32_int64, result, status, message)
    call require(status == standardir_lexical_lookup_no_match, 'no-match status differs')

    call standardir_lexical_lookup(facts, -1_int64, result, status, message)
    call require(status == standardir_lexical_lookup_invalid_scalar, &
        'negative scalar was accepted')
    call standardir_lexical_lookup(facts, int(z'd800', int64), result, status, message)
    call require(status == standardir_lexical_lookup_invalid_scalar, &
        'surrogate scalar was accepted')
    call standardir_lexical_lookup(facts, int(z'110000', int64), result, status, message)
    call require(status == standardir_lexical_lookup_invalid_scalar, &
        'out-of-range scalar was accepted')

    call make_facts(facts)
    facts%facts(2)%target_name = facts%facts(1)%target_name
    call standardir_lexical_validate(facts, ok, message)
    call require(.not. ok, 'duplicate target was accepted')

    call make_facts(facts)
    facts%facts(2)%source_term = facts%facts(1)%source_term
    call standardir_lexical_validate(facts, ok, message)
    call require(.not. ok, 'duplicate source term was accepted')

    call make_facts(facts)
    facts%facts(2)%range_first(1) = 70_int64
    facts%facts(2)%range_last(1) = 980_int64
    call standardir_lexical_lookup(facts, 70_int64, result, status, message)
    call require(status == standardir_lexical_lookup_invalid_facts, &
        'overlapping facts were not rejected')
    call require(index(message, 'overlapping') > 0, 'overlap diagnostic differs')
    call require(status /= standardir_lexical_lookup_ambiguous, &
        'invalid overlap was reported as an ordinary ambiguity')

    call make_facts(facts)
    facts%count = 4
    call set_fact(facts%facts(4), '€', 'constructed-class', 'TARGET_EURO', &
        'U+20AC', 8364_int64, 8364_int64)
    call standardir_lexical_validate(facts, ok, message)
    call require(ok, 'generic exact UTF-8 source term was rejected')
    facts%facts(4)%source_term = 'x'
    call standardir_lexical_validate(facts, ok, message)
    call require(.not. ok, 'mismatched exact UTF-8 source term was accepted')

    print '(a)', 'StandardIR lexical lookup test passed'

contains

    subroutine make_facts(output)
        type(standardir_lexical_facts_t), intent(out) :: output

        call standardir_lexical_reset(output)
        output%count = 3
        call set_fact(output%facts(1), 'range', 'constructed-class', 'TARGET_RANGE', &
            'U+0041-U+005A', 65_int64, 90_int64)
        call set_fact(output%facts(2), 'exact', 'constructed-class', 'TARGET_EXACT', &
            'U+03B1', 945_int64, 945_int64)
        call set_fact(output%facts(3), 'processor', 'constructed-class', &
            'TARGET_PROCESSOR', 'processor-defined', 0_int64, 0_int64)
        output%facts(3)%range_count = 0
    end subroutine make_facts

    subroutine set_fact(fact, source_term, class_name, target_name, codepoint, first, last)
        type(standardir_lexical_fact_t), intent(out) :: fact
        character(len=*), intent(in) :: source_term, class_name, target_name, codepoint
        integer(int64), intent(in) :: first, last

        fact = standardir_lexical_fact_t()
        fact%source_term = source_term
        fact%class_name = class_name
        fact%target_name = target_name
        fact%source_rule = 'RULE-'//trim(source_term)
        fact%source_page = '1'
        fact%document = 'constructed-document'
        fact%clause = 'constructed-clause'
        fact%source_hash = repeat('a', 64)
        fact%codepoint = codepoint
        fact%range_count = 1
        fact%range_first(1) = first
        fact%range_last(1) = last
    end subroutine set_fact

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure)
            stop 1
        end if
    end subroutine require

end program test_standardir_lexical_lookup

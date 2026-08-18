module standardir_lexical_facts_generated
    !! Generated from specs/lexical-facts-v0.sx; do not edit.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_lexical, only: standardir_lexical_facts_t, &
        standardir_lexical_reset, standardir_lexical_validate
    implicit none
    private

    public :: standardir_make_lexical_facts

contains

    subroutine standardir_make_lexical_facts(facts, ok, message)
        type(standardir_lexical_facts_t), intent(out) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_lexical_reset(facts)
        facts%count = 5
        facts%facts(1)%source_term = 'letter'
        facts%facts(1)%canonical_spelling = ''
        facts%facts(1)%class_name = 'lexical-class'
        facts%facts(1)%target_name = 'LETTER'
        facts%facts(1)%source_rule = 'P6.1.2-3'
        facts%facts(1)%source_page = '53'
        facts%facts(1)%document = 'J3-24-007'
        facts%facts(1)%clause = 'P6.1.2-3'
        facts%facts(1)%source_hash = &
            '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        facts%facts(1)%codepoint = 'U+0041-U+005A,U+0061-U+007A'
        facts%facts(1)%range_count = 2
        facts%facts(1)%range_first(1) = 65_int64
        facts%facts(1)%range_last(1) = 90_int64
        facts%facts(1)%range_first(2) = 97_int64
        facts%facts(1)%range_last(2) = 122_int64
        facts%facts(2)%source_term = 'digit'
        facts%facts(2)%canonical_spelling = ''
        facts%facts(2)%class_name = 'lexical-class'
        facts%facts(2)%target_name = 'DIGIT'
        facts%facts(2)%source_rule = 'P6.1.3-3'
        facts%facts(2)%source_page = '53'
        facts%facts(2)%document = 'J3-24-007'
        facts%facts(2)%clause = 'P6.1.3-3'
        facts%facts(2)%source_hash = &
            '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        facts%facts(2)%codepoint = 'U+0030-U+0039'
        facts%facts(2)%range_count = 1
        facts%facts(2)%range_first(1) = 48_int64
        facts%facts(2)%range_last(1) = 57_int64
        facts%facts(3)%source_term = 'rep-char'
        facts%facts(3)%canonical_spelling = ''
        facts%facts(3)%class_name = 'lexical-class'
        facts%facts(3)%target_name = 'REP_CHAR'
        facts%facts(3)%source_rule = 'R724-P3'
        facts%facts(3)%source_page = '71'
        facts%facts(3)%document = 'J3-24-007'
        facts%facts(3)%clause = 'R724-P3'
        facts%facts(3)%source_hash = &
            '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        facts%facts(3)%codepoint = 'processor-defined'
        facts%facts(3)%range_count = 0
        facts%facts(4)%source_term = '–'
        facts%facts(4)%canonical_spelling = '-'
        facts%facts(4)%class_name = 'unicode-lexical'
        facts%facts(4)%target_name = 'EN_DASH'
        facts%facts(4)%source_rule = 'R1010'
        facts%facts(4)%source_page = '69'
        facts%facts(4)%document = 'J3-24-007'
        facts%facts(4)%clause = 'R1010'
        facts%facts(4)%source_hash = &
            '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        facts%facts(4)%codepoint = 'U+2013'
        facts%facts(4)%range_count = 1
        facts%facts(4)%range_first(1) = 8211_int64
        facts%facts(4)%range_last(1) = 8211_int64
        facts%facts(5)%source_term = '’'
        facts%facts(5)%canonical_spelling = ''''
        facts%facts(5)%class_name = 'unicode-lexical'
        facts%facts(5)%target_name = 'RIGHT_SINGLE_QUOTE'
        facts%facts(5)%source_rule = 'R724'
        facts%facts(5)%source_page = '85'
        facts%facts(5)%document = 'J3-24-007'
        facts%facts(5)%clause = 'R724'
        facts%facts(5)%source_hash = &
            '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        facts%facts(5)%codepoint = 'U+2019'
        facts%facts(5)%range_count = 1
        facts%facts(5)%range_first(1) = 8217_int64
        facts%facts(5)%range_last(1) = 8217_int64
        call standardir_lexical_validate(facts, ok, message)
    end subroutine standardir_make_lexical_facts

end module standardir_lexical_facts_generated

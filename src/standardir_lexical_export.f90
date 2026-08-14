module standardir_lexical_export
    !! Target-specific projection of source-defined lexical facts.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_lexical, only: standardir_lexical_fact_t, &
        standardir_lexical_facts_t, standardir_lexical_validate
    implicit none
    private

    public :: standardir_lexical_emit_antlr
    public :: standardir_lexical_emit_bison
    public :: standardir_lexical_emit_bison_aliases
    public :: standardir_lexical_emit_ebnf
    public :: standardir_lexical_emit_treesitter

contains

    subroutine standardir_lexical_emit_ebnf(unit, facts, ok, message)
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return
        do i = 1, facts%count
            write (unit, '(a)', advance='no') '(* lexical-fact source-term='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%source_term)
            write (unit, '(a)', advance='no') ' class='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%class_name)
            write (unit, '(a)', advance='no') ' document='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%document)
            write (unit, '(a)', advance='no') ' clause='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%clause)
            write (unit, '(a)', advance='no') ' rule='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%source_rule)
            write (unit, '(a)', advance='no') ' page='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%source_page)
            write (unit, '(a)', advance='no') ' source-sha256='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%source_hash)
            write (unit, '(a)', advance='no') ' codepoint='
            write (unit, '(a)', advance='no') trim(facts%facts(i)%codepoint)
            write (unit, '(a)') ' *)'
        end do
    end subroutine standardir_lexical_emit_ebnf

    subroutine standardir_lexical_emit_antlr(unit, facts, ok, message)
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return
        do i = 1, facts%count
            call emit_antlr_comment(unit, facts%facts(i))
            write (unit, '(a)', advance='no') &
                trim(lexical_reference_name(facts%facts(i)%source_term))
            write (unit, '(a)', advance='no') ' : '
            write (unit, '(a)') trim(facts%facts(i)%target_name)//' ;'
        end do
        do i = 1, facts%count
            write (unit, '(a)', advance='no') trim(facts%facts(i)%target_name)//' : '
            call emit_antlr_pattern(unit, facts%facts(i))
            write (unit, '(a)') ' ;'
        end do
    end subroutine standardir_lexical_emit_antlr

    subroutine standardir_lexical_emit_bison(unit, facts, ok, message)
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return
        do i = 1, facts%count
            call emit_bison_comment(unit, facts%facts(i))
        end do
        write (unit, '(a)', advance='no') '%token'
        do i = 1, facts%count
            write (unit, '(a)', advance='no') ' '//trim(facts%facts(i)%target_name)
        end do
        write (unit, '(a)')
    end subroutine standardir_lexical_emit_bison

    subroutine standardir_lexical_emit_bison_aliases(unit, facts, ok, message)
        !! Emit parser-side aliases for source terms used as references.
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return
        do i = 1, facts%count
            write (unit, '(a)', advance='no') trim(lexical_reference_name( &
                facts%facts(i)%source_term))
            write (unit, '(a)', advance='no') ' : '
            write (unit, '(a)') trim(facts%facts(i)%target_name)//' ;'
        end do
    end subroutine standardir_lexical_emit_bison_aliases

    subroutine standardir_lexical_emit_treesitter(unit, facts, ok, message)
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return
        do i = 1, facts%count
            call emit_treesitter_comment(unit, facts%facts(i))
            write (unit, '(a)', advance='no') '    '
            write (unit, '(a)', advance='no') &
                trim(lexical_reference_name(facts%facts(i)%source_term))
            write (unit, '(a)', advance='no') ': $ => $.'
            write (unit, '(a)', advance='no') trim(facts%facts(i)%target_name)
            write (unit, '(a)') ','
            write (unit, '(a)', advance='no') '    '
            write (unit, '(a)', advance='no') trim(facts%facts(i)%target_name)
            write (unit, '(a)', advance='no') ': $ => '
            call emit_treesitter_pattern(unit, facts%facts(i))
            write (unit, '(a)') ','
        end do
    end subroutine standardir_lexical_emit_treesitter

    character(len=1024) function lexical_reference_name(value)
        character(len=*), intent(in) :: value
        character(len=16) :: encoded
        integer :: code, i, position

        lexical_reference_name = 'r_'
        position = 3
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                (code >= iachar('0') .and. code <= iachar('9')) .or. code == iachar('_')) then
                lexical_reference_name(position:position) = value(i:i)
                position = position + 1
            else
                write (encoded, '("_x",z2.2,"_")') code
                lexical_reference_name(position:position + len_trim(encoded) - 1) = trim(encoded)
                position = position + len_trim(encoded)
            end if
        end do
    end function lexical_reference_name

    subroutine emit_antlr_comment(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact

        write (unit, '(a)', advance='no') '// lexical-fact source-term='
        write (unit, '(a)', advance='no') trim(fact%source_term)
        write (unit, '(a)', advance='no') ' rule='
        write (unit, '(a)', advance='no') trim(fact%source_rule)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(fact%source_page)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(fact%document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(fact%clause)
        write (unit, '(a)', advance='no') ' source-sha256='
        write (unit, '(a)', advance='no') trim(fact%source_hash)
        write (unit, '(a)', advance='no') ' codepoint='
        write (unit, '(a)') trim(fact%codepoint)
    end subroutine emit_antlr_comment

    subroutine emit_treesitter_comment(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact

        write (unit, '(a)', advance='no') '    // lexical-fact source-term='
        write (unit, '(a)', advance='no') trim(fact%source_term)
        write (unit, '(a)', advance='no') ' class='
        write (unit, '(a)', advance='no') trim(fact%class_name)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(fact%document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(fact%clause)
        write (unit, '(a)', advance='no') ' rule='
        write (unit, '(a)', advance='no') trim(fact%source_rule)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(fact%source_page)
        write (unit, '(a)', advance='no') ' source-sha256='
        write (unit, '(a)', advance='no') trim(fact%source_hash)
        write (unit, '(a)', advance='no') ' codepoint='
        write (unit, '(a)') trim(fact%codepoint)
    end subroutine emit_treesitter_comment

    subroutine emit_bison_comment(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact

        write (unit, '(a)', advance='no') '/* lexical-fact source-term='
        write (unit, '(a)', advance='no') trim(fact%source_term)
        write (unit, '(a)', advance='no') ' class='
        write (unit, '(a)', advance='no') trim(fact%class_name)
        write (unit, '(a)', advance='no') ' document='
        write (unit, '(a)', advance='no') trim(fact%document)
        write (unit, '(a)', advance='no') ' clause='
        write (unit, '(a)', advance='no') trim(fact%clause)
        write (unit, '(a)', advance='no') ' rule='
        write (unit, '(a)', advance='no') trim(fact%source_rule)
        write (unit, '(a)', advance='no') ' page='
        write (unit, '(a)', advance='no') trim(fact%source_page)
        write (unit, '(a)', advance='no') ' source-sha256='
        write (unit, '(a)', advance='no') trim(fact%source_hash)
        write (unit, '(a)', advance='no') ' codepoint='
        write (unit, '(a)') trim(fact%codepoint)//' */'
    end subroutine emit_bison_comment

    subroutine emit_antlr_pattern(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact

        if (trim(fact%codepoint) == 'processor-defined') then
            write (unit, '(a)', advance='no') '.'
        else if (fact%range_count == 1 .and. fact%range_first(1) == int(z'2013', int64)) then
            write (unit, '(a)', advance='no') "'\u2013'"
        else if (fact%range_count == 1 .and. fact%range_first(1) == int(z'2019', int64)) then
            write (unit, '(a)', advance='no') "'\u2019'"
        else
            write (unit, '(a)', advance='no') '['
            call emit_antlr_ranges(unit, fact)
            write (unit, '(a)', advance='no') ']'
        end if
    end subroutine emit_antlr_pattern

    subroutine emit_antlr_ranges(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact
        integer :: i

        do i = 1, fact%range_count
            write (unit, '(a)', advance='no') '\u'
            write (unit, '(z4.4)', advance='no') fact%range_first(i)
            if (fact%range_last(i) /= fact%range_first(i)) then
                write (unit, '(a)', advance='no') '-\u'
                write (unit, '(z4.4)', advance='no') fact%range_last(i)
            end if
        end do
    end subroutine emit_antlr_ranges

    subroutine emit_treesitter_pattern(unit, fact)
        integer, intent(in) :: unit
        type(standardir_lexical_fact_t), intent(in) :: fact
        integer :: i

        if (trim(fact%codepoint) == 'processor-defined') then
            write (unit, '(a)', advance='no') '/[\s\S]/'
        else
            write (unit, '(a)', advance='no') '/['
            do i = 1, fact%range_count
                write (unit, '(a)', advance='no') '\u'
                write (unit, '(z4.4)', advance='no') fact%range_first(i)
                if (fact%range_last(i) /= fact%range_first(i)) then
                    write (unit, '(a)', advance='no') '-\u'
                    write (unit, '(z4.4)', advance='no') fact%range_last(i)
                end if
            end do
            write (unit, '(a)', advance='no') ']/'
        end if
    end subroutine emit_treesitter_pattern

end module standardir_lexical_export

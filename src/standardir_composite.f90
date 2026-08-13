module standardir_composite
    !! Composite StandardIR input: syntax records plus typed lexical facts.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_bison, only: standardir_emit_bison_group
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_groups, standardir_max_syntax_records
    use standardir_grammar, only: standardir_emit_antlr_group, standardir_emit_ebnf_group
    use standardir_lexical, only: standardir_lexical_add, standardir_lexical_facts_t, &
        standardir_lexical_reset, standardir_lexical_validate
    use standardir_lexical_export, only: standardir_lexical_emit_antlr, &
        standardir_lexical_emit_bison, standardir_lexical_emit_ebnf, &
        standardir_lexical_emit_treesitter
    use standardir_treesitter, only: standardir_emit_treesitter_group
    implicit none
    private

    type, public :: standardir_composite_t
        integer :: syntax_count = 0
        type(sx_node_t) :: syntax(standardir_max_syntax_records)
        type(standardir_lexical_facts_t) :: lexical
    end type standardir_composite_t

    public :: standardir_composite_add
    public :: standardir_composite_reset
    public :: standardir_composite_validate
    public :: standardir_composite_emit_antlr
    public :: standardir_composite_emit_bison
    public :: standardir_composite_emit_ebnf
    public :: standardir_composite_emit_treesitter

contains

    subroutine standardir_composite_reset(composite)
        type(standardir_composite_t), intent(out) :: composite

        composite%syntax_count = 0
        call standardir_lexical_reset(composite%lexical)
    end subroutine standardir_composite_reset

    subroutine standardir_composite_add(composite, node, ok, message)
        type(standardir_composite_t), intent(inout) :: composite
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: label

        ok = .false.; message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'composite record is not a list'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'composite record has no label'
            return
        end if
        label = trim(node%children(1)%atom)
        if (label == 'standardir') then
            ok = .true.
        else if (label == 'syntax') then
            if (composite%syntax_count >= size(composite%syntax)) then
                message = 'too many composite syntax records'
                return
            end if
            composite%syntax_count = composite%syntax_count + 1
            composite%syntax(composite%syntax_count) = node
            ok = .true.
        else if (label == 'lexical-fact') then
            call standardir_lexical_add(node, composite%lexical, ok, message)
        else
            message = 'unsupported composite record: '//trim(label)
            return
        end if
    end subroutine standardir_composite_add

    subroutine standardir_composite_validate(composite, ok, message)
        type(standardir_composite_t), intent(in) :: composite
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count

        call standardir_lexical_validate(composite%lexical, ok, message)
        if (.not. ok) return
        call standardir_group_syntax(composite%syntax, composite%syntax_count, groups, &
            group_count, ok, message)
    end subroutine standardir_composite_validate

    subroutine standardir_composite_emit_ebnf(unit, composite, ok, message)
        integer, intent(in) :: unit
        type(standardir_composite_t), intent(in) :: composite
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i

        call standardir_composite_validate(composite, ok, message)
        if (.not. ok) return
        call standardir_group_syntax(composite%syntax, composite%syntax_count, groups, &
            group_count, ok, message)
        if (.not. ok) return
        do i = 1, group_count
            call standardir_emit_ebnf_group(unit, composite%syntax, groups(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_lexical_emit_ebnf(unit, composite%lexical, ok, message)
    end subroutine standardir_composite_emit_ebnf

    subroutine standardir_composite_emit_antlr(unit, composite, ok, message)
        integer, intent(in) :: unit
        type(standardir_composite_t), intent(in) :: composite
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i

        write (unit, '(a)') 'grammar Fortran2023;'
        write (unit, '(a)')
        call standardir_composite_validate(composite, ok, message)
        if (.not. ok) return
        call standardir_group_syntax(composite%syntax, composite%syntax_count, groups, &
            group_count, ok, message)
        if (.not. ok) return
        do i = 1, group_count
            call standardir_emit_antlr_group(unit, composite%syntax, groups(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_lexical_emit_antlr(unit, composite%lexical, ok, message)
    end subroutine standardir_composite_emit_antlr

    subroutine standardir_composite_emit_bison(unit, composite, ok, message)
        integer, intent(in) :: unit
        type(standardir_composite_t), intent(in) :: composite
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i

        write (unit, '(a)') '/* Generated from composite StandardIR */'
        call standardir_composite_validate(composite, ok, message)
        if (.not. ok) return
        call standardir_lexical_emit_bison(unit, composite%lexical, ok, message)
        if (.not. ok) return
        write (unit, '(a)') '%%'
        call standardir_group_syntax(composite%syntax, composite%syntax_count, groups, &
            group_count, ok, message)
        if (.not. ok) return
        do i = 1, group_count
            call standardir_emit_bison_group(unit, composite%syntax, groups(i), ok, message)
            if (.not. ok) return
        end do
        write (unit, '(a)') '%%'
    end subroutine standardir_composite_emit_bison

    subroutine standardir_composite_emit_treesitter(unit, composite, ok, message)
        integer, intent(in) :: unit
        type(standardir_composite_t), intent(in) :: composite
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_group_t) :: groups(standardir_max_syntax_groups)
        integer :: group_count, i

        write (unit, '(a)') '// Generated from composite StandardIR'
        write (unit, '(a)') 'module.exports = grammar({'
        write (unit, '(a)') '  name: "fortran2023",'
        write (unit, '(a)') '  rules: {'
        call standardir_composite_validate(composite, ok, message)
        if (.not. ok) return
        call standardir_group_syntax(composite%syntax, composite%syntax_count, groups, &
            group_count, ok, message)
        if (.not. ok) return
        do i = 1, group_count
            call standardir_emit_treesitter_group(unit, composite%syntax, groups(i), ok, message)
            if (.not. ok) return
        end do
        call standardir_lexical_emit_treesitter(unit, composite%lexical, ok, message)
        if (.not. ok) return
        write (unit, '(a)') '  }'
        write (unit, '(a)') '});'
    end subroutine standardir_composite_emit_treesitter

end module standardir_composite

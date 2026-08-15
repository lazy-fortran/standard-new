module standardir_grammar_export_support
    !! Generic preparation checks for batch grammar export.

    use standardir_grammar_targetnorm, only: standardir_grammar_factor_role_family, &
        standardir_grammar_validate_role_family_witness, standardir_target_role_family_config_t, &
        standardir_target_role_family_witness_t, standardir_target_rule_t
    use standardir_grammar_producer, only: standardir_grammar_rule_t
    implicit none
    private

    public :: standardir_grammar_apply_role_family
    public :: standardir_grammar_validate_export_input

contains

    subroutine standardir_grammar_validate_export_input(rules, format, minimum_format, maximum_format, ok, &
            message)
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        integer, intent(in) :: format, minimum_format, maximum_format
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j

        ok = .false.
        message = ''
        if (format < minimum_format .or. format > maximum_format) then
            message = 'grammar export format is unsupported'
            return
        end if
        if (size(rules) < 1) then
            message = 'grammar export batch is empty'
            return
        end if
        do i = 2, size(rules)
            if (trim(rules(i)%lhs) /= trim(rules(i - 1)%lhs)) then
                do j = 1, i - 1
                    if (trim(rules(j)%lhs) == trim(rules(i)%lhs)) then
                        message = 'grammar export batch interleaves LHS groups'
                        return
                    end if
                end do
            end if
        end do
        ok = .true.
    end subroutine standardir_grammar_validate_export_input

    subroutine standardir_grammar_apply_role_family(normalized, selected_root, roots, reachability_mode, &
            role_family, role_family_mode, witness, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: normalized(:)
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        logical, intent(in) :: reachability_mode
        type(standardir_target_role_family_config_t), intent(in), optional :: role_family
        logical, intent(out) :: role_family_mode
        type(standardir_target_role_family_witness_t), allocatable, intent(out) :: witness(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: factored(:)
        character(len=128), allocatable :: protected_roots(:)

        role_family_mode = present(role_family)
        allocate (witness(0))
        ok = .false.
        message = ''
        if (.not. role_family_mode) then
            ok = .true.
            return
        end if
        if (reachability_mode) then
            if (present(selected_root)) then
                allocate (protected_roots(1))
                protected_roots(1) = trim(selected_root)
            else
                allocate (protected_roots(size(roots)))
                protected_roots = roots
            end if
            call standardir_grammar_factor_role_family(normalized, role_family, factored, witness, ok, &
                message, protected_lhs=protected_roots)
        else
            call standardir_grammar_factor_role_family(normalized, role_family, factored, witness, ok, &
                message)
        end if
        if (.not. ok) return
        call standardir_grammar_validate_role_family_witness(normalized, factored, witness, ok, message)
        if (.not. ok) return
        call move_alloc(factored, normalized)
    end subroutine standardir_grammar_apply_role_family

end module standardir_grammar_export_support

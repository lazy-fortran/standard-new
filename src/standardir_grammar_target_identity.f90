module standardir_grammar_target_identity
    !! Identity comparisons for generic target records.

    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_target_records, only: same_expression, standardir_target_provenance_t, &
        standardir_target_rule_t
    implicit none
    private

    public :: same_provenance, same_provenance_list, same_roles, same_target_rule

contains

    logical function same_provenance(left, right)
        type(standardir_target_provenance_t), intent(in) :: left, right
        type(standardir_source_ref_t) :: a, b

        a = left%source
        b = right%source
        same_provenance = left%alternative == right%alternative .and. &
            (left%source_expression_present .eqv. right%source_expression_present) .and. &
            trim(left%source_expression_sha256) == trim(right%source_expression_sha256) .and. &
            trim(a%document) == trim(b%document) .and. trim(a%clause) == trim(b%clause) .and. &
            trim(a%rule) == trim(b%rule) .and. a%page == b%page .and. &
            a%end_page == b%end_page .and. a%byte_start == b%byte_start .and. &
            a%byte_length == b%byte_length .and. trim(a%source_hash) == trim(b%source_hash)
    end function same_provenance

    logical function same_provenance_list(left, right)
        type(standardir_target_provenance_t), allocatable, intent(in) :: left(:), right(:)
        integer :: i

        same_provenance_list = allocated(left) .eqv. allocated(right)
        if (.not. same_provenance_list) return
        if (.not. allocated(left)) return
        if (size(left) /= size(right)) then
            same_provenance_list = .false.
            return
        end if
        do i = 1, size(left)
            if (.not. same_provenance(left(i), right(i))) then
                same_provenance_list = .false.
                return
            end if
        end do
    end function same_provenance_list

    logical function same_target_rule(left, right)
        type(standardir_target_rule_t), intent(in) :: left, right

        same_target_rule = trim(left%id) == trim(right%id) .and. left%alternative == right%alternative .and. &
            trim(left%lhs) == trim(right%lhs) .and. same_source_ref(left%source, right%source) .and. &
            same_expression(left%expression, right%expression) .and. &
            trim(left%target_expression_sha256) == trim(right%target_expression_sha256) .and. &
            same_provenance_list(left%provenance, right%provenance) .and. &
            same_roles(left%source_roles, right%source_roles) .and. left%origin == right%origin .and. &
            left%resolution == right%resolution
    end function same_target_rule

    logical function same_source_ref(left, right)
        type(standardir_source_ref_t), intent(in) :: left, right

        same_source_ref = trim(left%document) == trim(right%document) .and. &
            trim(left%clause) == trim(right%clause) .and. &
            trim(left%occurrence_clause) == trim(right%occurrence_clause) .and. &
            trim(left%rule) == trim(right%rule) .and. &
            left%page == right%page .and. left%end_page == right%end_page .and. &
            left%byte_start == right%byte_start .and. left%byte_length == right%byte_length .and. &
            left%occurrence == right%occurrence .and. trim(left%source_hash) == trim(right%source_hash)
    end function same_source_ref

    logical function same_roles(left, right)
        character(len=128), allocatable, intent(in) :: left(:), right(:)
        integer :: i

        same_roles = allocated(left) .eqv. allocated(right)
        if (.not. same_roles) return
        if (.not. allocated(left)) return
        if (size(left) /= size(right)) then
            same_roles = .false.
            return
        end if
        do i = 1, size(left)
            if (trim(left(i)) /= trim(right(i))) then
                same_roles = .false.
                return
            end if
        end do
    end function same_roles

end module standardir_grammar_target_identity

module standardir_grammar_target_records
    !! Generic target records and append/equality helpers.

    use standardir_export, only: standardir_source_ref_t
    implicit none
    private

    type, public :: standardir_target_expression_t
        integer :: kind = 0
        character(len=128) :: name = ''
        integer :: minimum = 0
        logical :: unbounded = .false.
        type(standardir_target_expression_t), allocatable :: children(:)
    end type standardir_target_expression_t

    type, public :: standardir_target_provenance_t
        type(standardir_source_ref_t) :: source
        integer :: alternative = 0
        logical :: source_expression_present = .true.
        character(len=64) :: source_expression_sha256 = ''
    end type standardir_target_provenance_t

    type, public :: standardir_target_rule_t
        character(len=128) :: id = ''
        integer :: alternative = 0
        character(len=128) :: lhs = ''
        type(standardir_target_expression_t) :: expression
        type(standardir_source_ref_t) :: source
        type(standardir_target_provenance_t), allocatable :: provenance(:)
        character(len=64) :: target_expression_sha256 = ''
        character(len=128), allocatable :: source_roles(:)
        integer :: origin = 0
        integer :: resolution = 0
    end type standardir_target_rule_t

    type, public :: standardir_target_role_family_config_t
        logical :: enabled = .false.
        character(len=128) :: representative = ''
    end type standardir_target_role_family_config_t

    integer, parameter, public :: standardir_target_role_family_factored = 1
    integer, parameter, public :: standardir_target_role_family_rejected = 2

    type, public :: standardir_target_role_family_witness_t
        character(len=128) :: alias_role = ''
        character(len=128) :: representative_role = ''
        integer :: disposition = 0
        character(len=256) :: reason = ''
        character(len=128), allocatable :: source_roles(:)
        type(standardir_target_provenance_t), allocatable :: alias_provenance(:)
        type(standardir_target_provenance_t), allocatable :: representative_provenance(:)
        character(len=64) :: alias_target_expression_sha256 = ''
        character(len=64) :: representative_target_expression_sha256 = ''
    end type standardir_target_role_family_witness_t

    public :: append_expression, append_target, contains_expression, same_expression

contains

    subroutine append_target(values, value)
        type(standardir_target_rule_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_rule_t), intent(in) :: value
        type(standardir_target_rule_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_target

    subroutine append_expression(values, value)
        type(standardir_target_expression_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_expression_t), intent(in) :: value
        type(standardir_target_expression_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_expression

    subroutine contains_expression(values, value, found)
        type(standardir_target_expression_t), intent(in) :: values(:), value
        logical, intent(out) :: found
        integer :: i

        found = .false.
        do i = 1, size(values)
            if (same_expression(values(i), value)) then
                found = .true.
                return
            end if
        end do
    end subroutine contains_expression

    recursive logical function same_expression(left, right) result(equal)
        type(standardir_target_expression_t), intent(in) :: left, right
        integer :: i

        equal = .false.
        if (left%kind /= right%kind .or. trim(left%name) /= trim(right%name)) return
        if (left%minimum /= right%minimum .or. left%unbounded .neqv. right%unbounded) return
        if (allocated(left%children) .neqv. allocated(right%children)) return
        if (.not. allocated(left%children)) then
            equal = .true.
            return
        end if
        if (size(left%children) /= size(right%children)) return
        do i = 1, size(left%children)
            if (.not. same_expression(left%children(i), right%children(i))) return
        end do
        equal = .true.
    end function same_expression

end module standardir_grammar_target_records

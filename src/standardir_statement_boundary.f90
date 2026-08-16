module standardir_statement_boundary
    !! Validate statement-sequence witnesses for a later target lowering.
    !!
    !! This is a lowering plan, not an insertion pass.  It deliberately keeps
    !! the source expression path because target normalization may change the
    !! target tree before a future lowering maps that path.

    use standardir_statement_sequence, only: standardir_sequence_compound_repeat_item, &
        standardir_sequence_first_plus_repeat, standardir_sequence_internal, &
        standardir_sequence_repeat_item, standardir_statement_sequence_candidate_t
    implicit none
    private

    character(len=18), parameter, public :: standardir_statement_boundary_marker = 'statement-boundary'
    character(len=18), parameter, public :: standardir_statement_boundary_separator = 'statement-boundary'

    type, public :: standardir_statement_boundary_evidence_t
        character(len=32) :: kind = ''
        character(len=128) :: item = ''
        character(len=128) :: derivation = ''
        character(len=32) :: status = ''
    end type standardir_statement_boundary_evidence_t

    type, public :: standardir_statement_boundary_site_t
        type(standardir_statement_sequence_candidate_t) :: candidate
        type(standardir_statement_boundary_evidence_t), allocatable :: evidence(:)
        character(len=18) :: marker = standardir_statement_boundary_marker
        character(len=18) :: separator = standardir_statement_boundary_separator
    end type standardir_statement_boundary_site_t

    type, public :: standardir_statement_boundary_plan_t
        type(standardir_statement_boundary_site_t), allocatable :: sites(:)
        logical :: insertion_supported = .false.
        character(len=128) :: integration_boundary = &
            'target expression mapping and token insertion remain downstream'
    end type standardir_statement_boundary_plan_t

    public :: standardir_statement_boundary_build_plan

contains

    subroutine standardir_statement_boundary_build_plan(candidates, plan, ok, message)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidates(:)
        type(standardir_statement_boundary_plan_t), intent(out) :: plan
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        plan = standardir_statement_boundary_plan_t()
        allocate (plan%sites(0))
        ok = .false.
        message = ''
        do i = 1, size(candidates)
            call validate_candidate(candidates(i), ok, message)
            if (.not. ok) return
            call append_site(plan%sites, candidates(i))
        end do
        call sort_sites(plan%sites)
        call coalesce_sites(plan%sites, ok, message)
        if (.not. ok) return
        ok = .true.
        message = ''
    end subroutine standardir_statement_boundary_build_plan

    subroutine validate_candidate(value, ok, message)
        type(standardir_statement_sequence_candidate_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (len_trim(value%source_rule) == 0 .or. len_trim(value%source_lhs) == 0) then
            message = 'statement boundary candidate lacks source rule or lhs'
            return
        end if
        if (len_trim(value%source_document) == 0 .or. len_trim(value%source_clause) == 0 .or. &
            len_trim(value%source_hash) == 0 .or. len_trim(value%source_page) == 0 .or. &
            len_trim(value%source_byte_start) == 0) then
            message = 'statement boundary candidate lacks source lineage'
            return
        end if
        if (.not. valid_source_hash(value%source_hash)) then
            message = 'statement boundary candidate has an invalid source hash'
            return
        end if
        if (.not. positive_decimal(value%source_page)) then
            message = 'statement boundary candidate has an invalid source page'
            return
        end if
        if (.not. nonnegative_decimal(value%source_byte_start)) then
            message = 'statement boundary candidate has an invalid source byte start'
            return
        end if
        if (len_trim(value%expression_path) == 0) then
            message = 'statement boundary candidate has an empty expression path'
            return
        end if
        if (.not. canonical_path(value%expression_path)) then
            message = 'statement boundary candidate has a malformed expression path'
            return
        end if
        if (len_trim(value%item) == 0 .or. len_trim(value%derivation) == 0) then
            message = 'statement boundary candidate lacks item derivation'
            return
        end if
        if (trim(value%status) /= 'candidate') then
            message = 'statement boundary candidate is not supported'
            return
        end if
        select case (trim(value%kind))
        case (standardir_sequence_repeat_item, standardir_sequence_first_plus_repeat, &
                standardir_sequence_compound_repeat_item, standardir_sequence_internal)
            continue
        case default
            message = 'statement boundary candidate kind is unsupported'
            return
        end select
        ok = .true.
    end subroutine validate_candidate

    logical function canonical_path(value)
        character(len=*), intent(in) :: value
        character(len=len_trim(value)) :: text
        integer :: i, first, length

        canonical_path = .false.
        if (len_trim(value) < 3) return
        text = trim(value)
        if (text(1:3) /= 'rhs') return
        length = len(text)
        if (length == 3) then
            canonical_path = .true.
            return
        end if
        if (text(4:4) /= '/') return
        i = 5
        do while (i <= length)
            first = i
            do while (i <= length)
                if (.not. is_digit(text(i:i))) exit
                i = i + 1
            end do
            if (i == first) return
            if (text(first:first) == '0') return
            if (i > length) then
                canonical_path = .true.
                return
            end if
            if (text(i:i) /= '/') return
            i = i + 1
            if (i > length) return
        end do
    end function canonical_path

    logical function positive_decimal(value)
        character(len=*), intent(in) :: value
        integer :: i

        positive_decimal = .false.
        if (len_trim(value) == 0) return
        do i = 1, len_trim(value)
            if (.not. is_digit(value(i:i))) return
            if (value(i:i) /= '0') positive_decimal = .true.
        end do
    end function positive_decimal

    logical function nonnegative_decimal(value)
        character(len=*), intent(in) :: value
        integer :: i

        nonnegative_decimal = len_trim(value) > 0
        if (.not. nonnegative_decimal) return
        do i = 1, len_trim(value)
            if (.not. is_digit(value(i:i))) then
                nonnegative_decimal = .false.
                return
            end if
        end do
    end function nonnegative_decimal

    logical function decimal_after(left, right)
        character(len=*), intent(in) :: left, right
        character(len=len_trim(left)) :: left_text
        character(len=len_trim(right)) :: right_text
        integer :: left_start, right_start, left_length, right_length

        left_text = trim(left)
        right_text = trim(right)
        left_start = 1
        right_start = 1
        left_length = len(left_text)
        right_length = len(right_text)
        do while (left_start < left_length)
            if (left_text(left_start:left_start) /= '0') exit
            left_start = left_start + 1
        end do
        do while (right_start < right_length)
            if (right_text(right_start:right_start) /= '0') exit
            right_start = right_start + 1
        end do
        left_length = left_length - left_start + 1
        right_length = right_length - right_start + 1
        if (left_length > right_length) then
            decimal_after = .true.
        else if (left_length < right_length) then
            decimal_after = .false.
        else
            decimal_after = left_text(left_start:) > right_text(right_start:)
        end if
    end function decimal_after

    logical function valid_source_hash(value)
        character(len=*), intent(in) :: value
        integer :: i

        valid_source_hash = len_trim(value) == 64
        if (.not. valid_source_hash) return
        do i = 1, 64
            if (.not. is_hex_digit(value(i:i))) then
                valid_source_hash = .false.
                return
            end if
        end do
    end function valid_source_hash

    logical function is_hex_digit(value)
        character(len=1), intent(in) :: value

        is_hex_digit = (value >= '0' .and. value <= '9') .or. &
            (value >= 'a' .and. value <= 'f') .or. (value >= 'A' .and. value <= 'F')
    end function is_hex_digit

    logical function is_digit(value)
        character(len=1), intent(in) :: value

        is_digit = value >= '0' .and. value <= '9'
    end function is_digit

    subroutine append_site(values, candidate)
        type(standardir_statement_boundary_site_t), allocatable, intent(inout) :: values(:)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        type(standardir_statement_boundary_site_t), allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1)%candidate = candidate
        allocate (expanded(old_size + 1)%evidence(1))
        expanded(old_size + 1)%evidence(1) = candidate_evidence(candidate)
        call move_alloc(expanded, values)
    end subroutine append_site

    logical function same_location(left, right)
        type(standardir_statement_boundary_site_t), intent(in) :: left, right

        same_location = trim(left%candidate%source_document) == trim(right%candidate%source_document) .and. &
            trim(left%candidate%source_clause) == trim(right%candidate%source_clause) .and. &
            trim(left%candidate%source_hash) == trim(right%candidate%source_hash) .and. &
            trim(left%candidate%source_page) == trim(right%candidate%source_page) .and. &
            trim(left%candidate%source_byte_start) == trim(right%candidate%source_byte_start) .and. &
            trim(left%candidate%source_rule) == trim(right%candidate%source_rule) .and. &
            trim(left%candidate%source_lhs) == trim(right%candidate%source_lhs) .and. &
            trim(left%candidate%expression_path) == trim(right%candidate%expression_path)
    end function same_location

    logical function same_evidence(left, right)
        type(standardir_statement_boundary_evidence_t), intent(in) :: left, right

        same_evidence = trim(left%kind) == trim(right%kind) .and. trim(left%item) == trim(right%item) .and. &
            trim(left%derivation) == trim(right%derivation) .and. trim(left%status) == trim(right%status)
    end function same_evidence

    function candidate_evidence(candidate) result(value)
        type(standardir_statement_sequence_candidate_t), intent(in) :: candidate
        type(standardir_statement_boundary_evidence_t) :: value

        value%kind = trim(candidate%kind)
        value%item = trim(candidate%item)
        value%derivation = trim(candidate%derivation)
        value%status = trim(candidate%status)
    end function candidate_evidence

    subroutine coalesce_sites(values, ok, message)
        type(standardir_statement_boundary_site_t), allocatable, intent(inout) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_statement_boundary_site_t), allocatable :: coalesced(:)
        integer :: i, last

        allocate (coalesced(0))
        ok = .false.
        message = ''
        do i = 1, size(values)
            if (size(coalesced) == 0) then
                call append_site_value(coalesced, values(i))
                cycle
            end if
            last = size(coalesced)
            if (.not. same_location(values(i), coalesced(last))) then
                call append_site_value(coalesced, values(i))
                cycle
            end if
            if (same_evidence(values(i)%evidence(1), coalesced(last)%evidence(1))) then
                message = 'statement boundary candidate evidence is duplicated or ambiguous'
                deallocate (coalesced)
                return
            end if
            call append_evidence(coalesced(last)%evidence, values(i)%evidence(1))
        end do
        call move_alloc(coalesced, values)
        ok = .true.
    end subroutine coalesce_sites

    subroutine append_site_value(values, value)
        type(standardir_statement_boundary_site_t), allocatable, intent(inout) :: values(:)
        type(standardir_statement_boundary_site_t), intent(in) :: value
        type(standardir_statement_boundary_site_t), allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_site_value

    subroutine append_evidence(values, value)
        type(standardir_statement_boundary_evidence_t), allocatable, intent(inout) :: values(:)
        type(standardir_statement_boundary_evidence_t), intent(in) :: value
        type(standardir_statement_boundary_evidence_t), allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_evidence

    subroutine sort_sites(values)
        type(standardir_statement_boundary_site_t), intent(inout) :: values(:)
        type(standardir_statement_boundary_site_t) :: temporary
        integer :: i, j

        do i = 2, size(values)
            temporary = values(i)
            j = i - 1
            do while (j >= 1)
                if (.not. site_after(values(j), temporary)) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = temporary
        end do
    end subroutine sort_sites

    logical function site_after(left, right)
        type(standardir_statement_boundary_site_t), intent(in) :: left, right

        site_after = .false.
        if (trim(left%candidate%source_document) > trim(right%candidate%source_document)) then
            site_after = .true.
        else if (trim(left%candidate%source_document) < trim(right%candidate%source_document)) then
            return
        else if (trim(left%candidate%source_clause) > trim(right%candidate%source_clause)) then
            site_after = .true.
        else if (trim(left%candidate%source_clause) < trim(right%candidate%source_clause)) then
            return
        else if (trim(left%candidate%source_hash) > trim(right%candidate%source_hash)) then
            site_after = .true.
        else if (trim(left%candidate%source_hash) < trim(right%candidate%source_hash)) then
            return
        else if (decimal_after(left%candidate%source_page, right%candidate%source_page)) then
            site_after = .true.
        else if (decimal_after(right%candidate%source_page, left%candidate%source_page)) then
            return
        else if (trim(left%candidate%source_page) > trim(right%candidate%source_page)) then
            site_after = .true.
        else if (trim(left%candidate%source_page) < trim(right%candidate%source_page)) then
            return
        else if (decimal_after(left%candidate%source_byte_start, right%candidate%source_byte_start)) then
            site_after = .true.
        else if (decimal_after(right%candidate%source_byte_start, left%candidate%source_byte_start)) then
            return
        else if (trim(left%candidate%source_byte_start) > trim(right%candidate%source_byte_start)) then
            site_after = .true.
        else if (trim(left%candidate%source_byte_start) < trim(right%candidate%source_byte_start)) then
            return
        else if (trim(left%candidate%source_rule) > trim(right%candidate%source_rule)) then
            site_after = .true.
        else if (trim(left%candidate%source_rule) < trim(right%candidate%source_rule)) then
            return
        else if (trim(left%candidate%source_lhs) > trim(right%candidate%source_lhs)) then
            site_after = .true.
        else if (trim(left%candidate%source_lhs) < trim(right%candidate%source_lhs)) then
            return
        else if (trim(left%candidate%expression_path) > trim(right%candidate%expression_path)) then
            site_after = .true.
        else if (trim(left%candidate%expression_path) < trim(right%candidate%expression_path)) then
            return
        else if (trim(left%candidate%kind) > trim(right%candidate%kind)) then
            site_after = .true.
        else if (trim(left%candidate%kind) < trim(right%candidate%kind)) then
            return
        else if (trim(left%candidate%item) > trim(right%candidate%item)) then
            site_after = .true.
        else if (trim(left%candidate%item) < trim(right%candidate%item)) then
            return
        else if (trim(left%candidate%derivation) > trim(right%candidate%derivation)) then
            site_after = .true.
        else if (trim(left%candidate%derivation) < trim(right%candidate%derivation)) then
            return
        else
            site_after = trim(left%candidate%status) > trim(right%candidate%status)
        end if
    end function site_after

end module standardir_statement_boundary

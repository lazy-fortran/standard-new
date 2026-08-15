module standardir_lexer_contract
    !! Source-backed, target-neutral lexer contract projection.
    !!
    !! The serialized form is JSON Lines.  The first line is a header:
    !! {"kind":"lexer-contract-header","format":1,"origin":"MECHANICAL"}
    !! Each following line is one lexer-token record.  `pattern` and
    !! `codepoint` retain the source expression; `ranges` makes its Unicode
    !! scalar intervals machine-readable.  This contract declares facts only:
    !! it contains no lexer actions, target syntax, or Fortran-specific cases.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir_lexical, only: standardir_lexical_facts_t, &
        standardir_lexical_resolve_spelling, &
        standardir_lexical_validate
    implicit none
    private

    integer, parameter, public :: standardir_lexer_contract_format = 1
    integer, parameter, public :: standardir_lexer_contract_max_ranges = 4
    integer, parameter, public :: standardir_lexer_contract_max_tokens = 16
    character(len=*), parameter, public :: standardir_lexer_contract_origin_mechanical = &
        'MECHANICAL'

    type, public :: standardir_lexer_contract_token_t
        character(len=128) :: token_name = ''
        character(len=256) :: source_term = ''
        character(len=256) :: canonical_spelling = ''
        character(len=64) :: lexical_class = ''
        character(len=256) :: pattern = ''
        character(len=64) :: codepoint = ''
        integer :: range_count = 0
        integer(int64) :: range_first(standardir_lexer_contract_max_ranges) = 0_int64
        integer(int64) :: range_last(standardir_lexer_contract_max_ranges) = 0_int64
        character(len=128) :: document = ''
        character(len=128) :: clause = ''
        character(len=64) :: source_rule = ''
        character(len=64) :: source_page = ''
        character(len=128) :: source_hash = ''
        character(len=16) :: origin = ''
    end type standardir_lexer_contract_token_t

    type, public :: standardir_lexer_contract_t
        integer :: count = 0
        character(len=16) :: origin = ''
        type(standardir_lexer_contract_token_t) :: tokens(standardir_lexer_contract_max_tokens)
    end type standardir_lexer_contract_t

    public :: standardir_lexer_contract_project
    public :: standardir_lexer_contract_reset
    public :: standardir_lexer_contract_validate
    public :: standardir_lexer_contract_write

contains

    subroutine standardir_lexer_contract_reset(contract)
        type(standardir_lexer_contract_t), intent(out) :: contract

        contract%count = 0
        contract%origin = ''
    end subroutine standardir_lexer_contract_reset

    subroutine standardir_lexer_contract_project(facts, origin, contract, ok, message)
        type(standardir_lexical_facts_t), intent(in) :: facts
        character(len=*), intent(in) :: origin
        type(standardir_lexer_contract_t), intent(out) :: contract
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: spelling
        integer :: i

        call standardir_lexer_contract_reset(contract)
        ok = .false.
        message = ''
        if (.not. valid_origin(origin)) then
            message = 'lexer contract origin is invalid'
            return
        end if
        contract%origin = trim(origin)
        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return

        if (facts%count > standardir_lexer_contract_max_tokens) then
            message = 'lexer contract has too many tokens'
            ok = .false.
            return
        end if
        contract%count = facts%count
        do i = 1, facts%count
            contract%tokens(i)%token_name = trim(facts%facts(i)%target_name)
            contract%tokens(i)%source_term = trim(facts%facts(i)%source_term)
            call standardir_lexical_resolve_spelling(facts%facts(i), spelling, ok, message)
            if (.not. ok) then
                call standardir_lexer_contract_reset(contract)
                return
            end if
            contract%tokens(i)%canonical_spelling = trim(spelling)
            contract%tokens(i)%lexical_class = trim(facts%facts(i)%class_name)
            contract%tokens(i)%pattern = trim(facts%facts(i)%codepoint)
            contract%tokens(i)%codepoint = trim(facts%facts(i)%codepoint)
            contract%tokens(i)%range_count = facts%facts(i)%range_count
            contract%tokens(i)%range_first = facts%facts(i)%range_first
            contract%tokens(i)%range_last = facts%facts(i)%range_last
            contract%tokens(i)%document = trim(facts%facts(i)%document)
            contract%tokens(i)%clause = trim(facts%facts(i)%clause)
            contract%tokens(i)%source_rule = trim(facts%facts(i)%source_rule)
            contract%tokens(i)%source_page = trim(facts%facts(i)%source_page)
            contract%tokens(i)%source_hash = trim(facts%facts(i)%source_hash)
            contract%tokens(i)%origin = trim(origin)
        end do
        call standardir_lexer_contract_validate(contract, ok, message)
        if (.not. ok) call standardir_lexer_contract_reset(contract)
    end subroutine standardir_lexer_contract_project

    subroutine standardir_lexer_contract_validate(contract, ok, message)
        type(standardir_lexer_contract_t), intent(in) :: contract
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, k, l

        ok = .false.
        message = ''
        if (contract%count < 0 .or. contract%count > size(contract%tokens)) then
            message = 'lexer contract token count is outside storage'
            return
        end if
        if (.not. valid_origin(contract%origin)) then
            message = 'lexer contract origin is invalid'
            return
        end if
        do i = 1, contract%count
            if (len_trim(contract%tokens(i)%token_name) == 0 .or. &
                len_trim(contract%tokens(i)%source_term) == 0 .or. &
                len_trim(contract%tokens(i)%lexical_class) == 0) then
                message = 'lexer contract token identity is incomplete'
                return
            end if
            if (len_trim(contract%tokens(i)%pattern) == 0 .or. &
                len_trim(contract%tokens(i)%codepoint) == 0) then
                message = 'lexer contract lexical pattern is incomplete'
                return
            end if
            if (len_trim(contract%tokens(i)%document) == 0 .or. &
                len_trim(contract%tokens(i)%clause) == 0 .or. &
                len_trim(contract%tokens(i)%source_rule) == 0 .or. &
                len_trim(contract%tokens(i)%source_page) == 0 .or. &
                len_trim(contract%tokens(i)%source_hash) == 0) then
                message = 'lexer contract provenance is incomplete'
                return
            end if
            if (.not. valid_origin(contract%tokens(i)%origin)) then
                message = 'lexer contract origin is invalid'
                return
            end if
            if (trim(contract%tokens(i)%origin) /= trim(contract%origin)) then
                message = 'lexer contract token origin differs from header'
                return
            end if
            if (contract%tokens(i)%range_count < 0 .or. &
                contract%tokens(i)%range_count > standardir_lexer_contract_max_ranges) then
                message = 'lexer contract range count is outside storage'
                return
            end if
            if (contract%tokens(i)%range_count == 0) then
                if (trim(contract%tokens(i)%codepoint) /= 'processor-defined') then
                    message = 'lexer contract has no source-defined scalar or range'
                    return
                end if
            else
                do j = 1, contract%tokens(i)%range_count
                    if (contract%tokens(i)%range_first(j) > contract%tokens(i)%range_last(j)) then
                        message = 'lexer contract range is reversed'
                        return
                    end if
                    do k = 1, j - 1
                        if (ranges_overlap(contract%tokens(i)%range_first(k), &
                            contract%tokens(i)%range_last(k), contract%tokens(i)%range_first(j), &
                            contract%tokens(i)%range_last(j))) then
                            message = 'lexer contract contains overlapping ranges'
                            return
                        end if
                    end do
                end do
            end if
            do j = 1, i - 1
                if (trim(contract%tokens(i)%token_name) == trim(contract%tokens(j)%token_name)) then
                    message = 'lexer contract has duplicate token names'
                    return
                end if
                if (trim(contract%tokens(i)%source_term) == trim(contract%tokens(j)%source_term)) then
                    message = 'lexer contract has duplicate source terms'
                    return
                end if
                if (len_trim(contract%tokens(i)%canonical_spelling) > 0 .and. &
                    trim(contract%tokens(i)%canonical_spelling) == &
                    trim(contract%tokens(j)%canonical_spelling)) then
                    message = 'lexer contract has ambiguous canonical spellings'
                    return
                end if
                do k = 1, contract%tokens(i)%range_count
                    do l = 1, contract%tokens(j)%range_count
                        if (ranges_overlap(contract%tokens(i)%range_first(k), &
                            contract%tokens(i)%range_last(k), contract%tokens(j)%range_first(l), &
                            contract%tokens(j)%range_last(l))) then
                            message = 'lexer contract has overlapping token ranges'
                            return
                        end if
                    end do
                end do
            end do
        end do
        ok = .true.
    end subroutine standardir_lexer_contract_validate

    subroutine standardir_lexer_contract_write(contract, unit, ok, message)
        type(standardir_lexer_contract_t), intent(in) :: contract
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: range_text
        integer :: i, j

        call standardir_lexer_contract_validate(contract, ok, message)
        if (.not. ok) return
        call write_text(unit, '{"kind":"lexer-contract-header","format":1,"origin":"'// &
            trim(contract%origin)//'"}', .false., ok)
        if (.not. ok) return
        do i = 1, contract%count
            call write_text(unit, '{"kind":"lexer-token","token_name":', .true., ok)
            call write_json_string(unit, contract%tokens(i)%token_name, ok)
            call write_field(unit, ',"source_term":', contract%tokens(i)%source_term, ok)
            call write_field(unit, ',"canonical_spelling":', &
                contract%tokens(i)%canonical_spelling, ok)
            call write_field(unit, ',"lexical_class":', contract%tokens(i)%lexical_class, ok)
            call write_field(unit, ',"pattern":', contract%tokens(i)%pattern, ok)
            call write_field(unit, ',"codepoint":', contract%tokens(i)%codepoint, ok)
            if (.not. ok) exit
            call write_text(unit, ',"ranges":[', .true., ok)
            do j = 1, contract%tokens(i)%range_count
                if (j > 1) call write_text(unit, ',', .true., ok)
                write (range_text, '(a,i0,a,i0,a)') &
                    '{"first":', contract%tokens(i)%range_first(j), &
                    ',"last":', contract%tokens(i)%range_last(j), '}'
                call write_text(unit, trim(range_text), .true., ok)
            end do
            call write_text(unit, '],"document":', .true., ok)
            call write_json_string(unit, contract%tokens(i)%document, ok)
            call write_field(unit, ',"clause":', contract%tokens(i)%clause, ok)
            call write_field(unit, ',"rule":', contract%tokens(i)%source_rule, ok)
            call write_field(unit, ',"page":', contract%tokens(i)%source_page, ok)
            call write_field(unit, ',"source_hash":', contract%tokens(i)%source_hash, ok)
            call write_field(unit, ',"origin":', contract%tokens(i)%origin, ok)
            if (.not. ok) exit
            call write_text(unit, '}', .false., ok)
        end do
        if (.not. ok) message = 'could not write lexer contract'
    end subroutine standardir_lexer_contract_write

    subroutine write_text(unit, value, advance, ok)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(in) :: advance
        logical, intent(inout) :: ok

        integer :: ios

        if (.not. ok) return
        if (advance) then
            write (unit, '(a)', advance='no', iostat=ios) value
        else
            write (unit, '(a)', iostat=ios) value
        end if
        if (ios /= 0) ok = .false.
    end subroutine write_text

    subroutine write_field(unit, prefix, value, ok)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: prefix, value
        logical, intent(inout) :: ok

        if (.not. ok) return
        call write_text(unit, prefix, .true., ok)
        call write_json_string(unit, value, ok)
    end subroutine write_field

    subroutine write_json_string(unit, value, ok)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(inout) :: ok

        character(len=8) :: escaped
        integer :: code, i

        if (.not. ok) return
        call write_text(unit, '"', .true., ok)
        do i = 1, len_trim(value)
            if (.not. ok) return
            code = iachar(value(i:i))
            select case (code)
            case (8)
                call write_text(unit, '\b', .true., ok)
            case (9)
                call write_text(unit, '\t', .true., ok)
            case (10)
                call write_text(unit, '\n', .true., ok)
            case (12)
                call write_text(unit, '\f', .true., ok)
            case (13)
                call write_text(unit, '\r', .true., ok)
            case (34, 92)
                call write_text(unit, achar(92), .true., ok)
                call write_text(unit, value(i:i), .true., ok)
            case default
                if (code < 32) then
                    write (escaped, '("\u00",z2.2)') code
                    call write_text(unit, trim(escaped), .true., ok)
                else
                    call write_text(unit, value(i:i), .true., ok)
                end if
            end select
        end do
        call write_text(unit, '"', .true., ok)
    end subroutine write_json_string

    logical function valid_origin(origin)
        character(len=*), intent(in) :: origin

        valid_origin = trim(origin) == standardir_lexer_contract_origin_mechanical
    end function valid_origin

    logical function ranges_overlap(first_a, last_a, first_b, last_b)
        integer(int64), intent(in) :: first_a, last_a, first_b, last_b

        ranges_overlap = first_a <= last_b .and. first_b <= last_a
    end function ranges_overlap

end module standardir_lexer_contract

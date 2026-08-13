module standardir_lexical
    !! Source-defined lexical facts and their target-specific terminal exports.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_list, sx_node_t
    use standardir_syntax_fields, only: standardir_atom_equals, standardir_read_pair, &
        standardir_read_source
    implicit none
    private

    integer, parameter, public :: standardir_max_lexical_facts = 16
    integer, parameter :: lexical_max_ranges = 4
    integer, parameter :: lexical_text_length = 256

    type, public :: standardir_lexical_fact_t
        character(len=lexical_text_length) :: source_term = ''
        character(len=64) :: class_name = ''
        character(len=128) :: target_name = ''
        character(len=64) :: source_rule = ''
        character(len=64) :: source_page = ''
        character(len=128) :: document = ''
        character(len=128) :: clause = ''
        character(len=128) :: source_hash = ''
        character(len=64) :: codepoint = ''
        integer :: range_count = 0
        integer(int64) :: range_first(lexical_max_ranges) = 0
        integer(int64) :: range_last(lexical_max_ranges) = 0
    end type standardir_lexical_fact_t

    type, public :: standardir_lexical_facts_t
        integer :: count = 0
        type(standardir_lexical_fact_t) :: facts(standardir_max_lexical_facts)
    end type standardir_lexical_facts_t

    public :: standardir_lexical_add
    public :: standardir_lexical_reset
    public :: standardir_lexical_validate

contains

    subroutine standardir_lexical_reset(facts)
        type(standardir_lexical_facts_t), intent(out) :: facts

        facts%count = 0
    end subroutine standardir_lexical_reset

    subroutine standardir_lexical_add(node, facts, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_lexical_facts_t), intent(inout) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_lexical_fact_t) :: fact
        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 2) then
            message = 'lexical fact has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), 'lexical-fact')) then
            message = 'record is not a lexical fact'
            return
        end if
        call read_fact(node, fact, ok, message)
        if (.not. ok) return
        if (facts%count >= size(facts%facts)) then
            message = 'too many lexical facts'
            return
        end if
        do i = 1, facts%count
            if (trim(facts%facts(i)%source_term) == trim(fact%source_term)) then
                message = 'duplicate lexical fact: '//trim(fact%source_term)
                return
            end if
        end do
        facts%count = facts%count + 1
        facts%facts(facts%count) = fact
        ok = .true.
    end subroutine standardir_lexical_add

    subroutine standardir_lexical_validate(facts, ok, message)
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j

        ok = .false.
        message = ''
        if (facts%count < 0 .or. facts%count > size(facts%facts)) then
            message = 'lexical fact count is outside storage'
            return
        end if
        do i = 1, facts%count
            if (len_trim(facts%facts(i)%source_term) == 0) then
                message = 'lexical fact lacks source term, class or target name'
                return
            end if
            if (len_trim(facts%facts(i)%target_name) == 0) then
                message = 'lexical fact lacks source term, class or target name'
                return
            end if
            if (len_trim(facts%facts(i)%class_name) == 0) then
                message = 'lexical fact lacks source term, class or target name'
                return
            end if
            if (len_trim(facts%facts(i)%document) == 0) then
                message = 'lexical fact lacks complete source provenance'
                return
            end if
            if (len_trim(facts%facts(i)%clause) == 0) then
                message = 'lexical fact lacks complete source provenance'
                return
            end if
            if (len_trim(facts%facts(i)%source_page) == 0) then
                message = 'lexical fact lacks complete source provenance'
                return
            end if
            if (.not. is_sha256(facts%facts(i)%source_hash)) then
                message = 'lexical fact lacks complete source provenance'
                return
            end if
            if (facts%facts(i)%range_count < 0 .or. &
                facts%facts(i)%range_count > lexical_max_ranges) then
                message = 'lexical fact has too many codepoint ranges'
                return
            end if
            if (facts%facts(i)%range_count == 0 .and. &
                trim(facts%facts(i)%codepoint) /= 'processor-defined') then
                message = 'lexical fact has no source-defined scalar or range'
                return
            end if
            do j = 1, i - 1
                if (trim(facts%facts(i)%target_name) == &
                    trim(facts%facts(j)%target_name)) then
                    message = 'duplicate lexical target: '// &
                        trim(facts%facts(i)%target_name)
                    return
                end if
            end do
            if (.not. source_scalar_is_exact(facts%facts(i))) then
                message = 'Unicode lexical fact does not preserve its UTF-8 scalar'
                return
            end if
        end do
        ok = .true.
    end subroutine standardir_lexical_validate

    logical function is_sha256(value)
        character(len=*), intent(in) :: value

        integer :: i

        is_sha256 = len_trim(value) == 64
        if (.not. is_sha256) return
        do i = 1, 64
            if (hex_digit(value(i:i)) < 0) then
                is_sha256 = .false.
                return
            end if
        end do
    end function is_sha256

    subroutine read_fact(node, fact, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_lexical_fact_t), intent(out) :: fact
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: source_term, class_name, target_name, codepoint, source_rule
        character(len=256) :: document, clause, page, source_hash
        integer :: i

        fact = standardir_lexical_fact_t()
        source_term = ''; class_name = ''; target_name = ''; codepoint = ''
        source_rule = ''; document = ''; clause = ''; page = ''; source_hash = ''
        ok = .false.; message = ''
        do i = 2, node%child_count
            if (node%children(i)%kind /= sx_list) then
                message = 'lexical fact field is not a pair'
                return
            end if
            if (standardir_atom_equals(node%children(i)%children(1), 'source-term')) then
                call standardir_read_pair(node%children(i), 'source-term', source_term, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'class')) then
                call standardir_read_pair(node%children(i), 'class', class_name, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'target')) then
                call standardir_read_pair(node%children(i), 'target', target_name, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'rule')) then
                call standardir_read_pair(node%children(i), 'rule', source_rule, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'codepoint')) then
                call standardir_read_pair(node%children(i), 'codepoint', codepoint, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'source')) then
                call standardir_read_source(node%children(i), document, clause, page, source_hash, &
                    ok, message)
            else
                message = 'unknown lexical fact field'
                return
            end if
            if (.not. ok) return
        end do
        if (len_trim(source_rule) == 0 .or. len_trim(codepoint) == 0) then
            message = 'lexical fact lacks rule or codepoint'
            return
        end if
        fact%source_term = trim(source_term)
        fact%class_name = trim(class_name)
        fact%target_name = trim(target_name)
        fact%source_rule = trim(source_rule)
        fact%source_page = trim(page)
        fact%document = trim(document)
        fact%clause = trim(clause)
        fact%source_hash = trim(source_hash)
        fact%codepoint = trim(codepoint)
        call parse_codepoint(fact%codepoint, fact%range_count, fact%range_first, &
            fact%range_last, ok, message)
    end subroutine read_fact

    subroutine parse_codepoint(text, count, first, last, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(out) :: count
        integer(int64), intent(out) :: first(:), last(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=64) :: part, left, right
        integer :: position, comma, dash

        count = 0; first = 0; last = 0; ok = .false.; message = ''
        if (trim(text) == 'processor-defined') then
            ok = .true.
            return
        end if
        position = 1
        do while (position <= len_trim(text))
            comma = index(text(position:), ',')
            if (comma == 0) then
                part = text(position:len_trim(text))
                position = len_trim(text) + 1
            else
                part = text(position:position + comma - 2)
                position = position + comma
            end if
            dash = index(part, '-')
            if (dash == 0) then
                left = trim(part); right = trim(part)
            else
                left = trim(part(:dash - 1)); right = trim(part(dash + 1:))
            end if
            if (count >= size(first)) then
                message = 'lexical codepoint range count exceeds storage'
                return
            end if
            call parse_scalar(left, first(count + 1), ok, message)
            if (.not. ok) return
            call parse_scalar(right, last(count + 1), ok, message)
            if (.not. ok) return
            count = count + 1
        end do
        ok = count > 0
        if (.not. ok) message = 'empty lexical codepoint expression'
    end subroutine parse_codepoint

    subroutine parse_scalar(text, scalar, ok, message)
        character(len=*), intent(in) :: text
        integer(int64), intent(out) :: scalar
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, digit

        scalar = 0; ok = .false.; message = ''
        if (len_trim(text) < 3) then
            message = 'codepoint is not a U+ scalar'
            return
        end if
        if (text(1:2) /= 'U+') then
            message = 'codepoint is not a U+ scalar'
            return
        end if
        do i = 3, len_trim(text)
            digit = hex_digit(text(i:i))
            if (digit < 0) then
                message = 'codepoint contains a non-hex scalar'
                return
            end if
            scalar = scalar * 16_int64 + int(digit, int64)
        end do
        ok = scalar <= int(z'10ffff', int64)
        if (ok) then
            if (scalar >= int(z'd800', int64) .and. scalar <= int(z'dfff', int64)) then
                ok = .false.
            end if
        end if
        if (.not. ok) message = 'codepoint exceeds Unicode scalar range'
    end subroutine parse_scalar

    integer function hex_digit(value)
        character(len=1), intent(in) :: value

        integer :: code

        code = iachar(value)
        if (code >= iachar('0') .and. code <= iachar('9')) then
            hex_digit = code - iachar('0')
        else if (code >= iachar('A') .and. code <= iachar('F')) then
            hex_digit = code - iachar('A') + 10
        else if (code >= iachar('a') .and. code <= iachar('f')) then
            hex_digit = code - iachar('a') + 10
        else
            hex_digit = -1
        end if
    end function hex_digit

    logical function source_scalar_is_exact(fact)
        type(standardir_lexical_fact_t), intent(in) :: fact

        integer :: n

        source_scalar_is_exact = .true.
        if (fact%range_count /= 1) return
        if (fact%range_first(1) /= int(z'2013', int64) .and. &
            fact%range_first(1) /= int(z'2019', int64)) return
        n = len_trim(fact%source_term)
        source_scalar_is_exact = n == 3
        if (.not. source_scalar_is_exact) return
        if (fact%range_first(1) == int(z'2013', int64)) then
            source_scalar_is_exact = iachar(fact%source_term(1:1)) == 226 .and. &
                iachar(fact%source_term(2:2)) == 128 .and. &
                iachar(fact%source_term(3:3)) == 147 .and. &
                trim(fact%target_name) == 'EN_DASH'
        else
            source_scalar_is_exact = iachar(fact%source_term(1:1)) == 226 .and. &
                iachar(fact%source_term(2:2)) == 128 .and. &
                iachar(fact%source_term(3:3)) == 153 .and. &
                trim(fact%target_name) == 'RIGHT_SINGLE_QUOTE'
        end if
    end function source_scalar_is_exact

end module standardir_lexical

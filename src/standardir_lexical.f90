module standardir_lexical
    !! Source-defined lexical facts and their target-specific terminal exports.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use byte_span, only: byte_span_from_array, byte_span_t
    use fortsx, only: sx_list, sx_node_t
    use standardir_syntax_fields, only: standardir_atom_equals, standardir_read_pair, &
        standardir_read_source
    use utf8_boundary, only: utf8_codepoint_t, utf8_decode_next, utf8_validate
    implicit none
    private

    integer, parameter, public :: standardir_max_lexical_facts = 16
    integer, parameter, public :: standardir_lexical_lookup_match = 0
    integer, parameter, public :: standardir_lexical_lookup_no_match = 1
    integer, parameter, public :: standardir_lexical_lookup_unsupported = 2
    integer, parameter, public :: standardir_lexical_lookup_ambiguous = 3
    integer, parameter, public :: standardir_lexical_lookup_invalid_scalar = 4
    integer, parameter, public :: standardir_lexical_lookup_invalid_facts = 5
    integer, parameter :: lexical_max_ranges = 4
    integer, parameter :: lexical_text_length = 256

    type, public :: standardir_lexical_fact_t
        character(len=lexical_text_length) :: source_term = ''
        character(len=lexical_text_length) :: canonical_spelling = ''
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
    public :: standardir_lexical_lookup
    public :: standardir_lexical_resolve_spelling
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

    subroutine standardir_lexical_lookup(facts, scalar, result, status, message)
        type(standardir_lexical_facts_t), intent(in) :: facts
        integer(int64), intent(in) :: scalar
        type(standardir_lexical_fact_t), intent(out) :: result
        integer, intent(out) :: status
        character(len=*), intent(out) :: message

        logical :: ok
        integer :: i, j, match_count
        logical :: processor_defined

        result = standardir_lexical_fact_t()
        status = standardir_lexical_lookup_no_match
        message = ''
        if (.not. is_unicode_scalar(scalar)) then
            status = standardir_lexical_lookup_invalid_scalar
            message = 'lookup value is not a Unicode scalar'
            return
        end if
        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) then
            status = standardir_lexical_lookup_invalid_facts
            return
        end if

        match_count = 0
        processor_defined = .false.
        do i = 1, facts%count
            if (facts%facts(i)%range_count == 0) then
                processor_defined = .true.
                if (.not. processor_defined_fact_is_set(result)) result = facts%facts(i)
            else
                do j = 1, facts%facts(i)%range_count
                    if (scalar >= facts%facts(i)%range_first(j) .and. &
                        scalar <= facts%facts(i)%range_last(j)) then
                        match_count = match_count + 1
                        result = facts%facts(i)
                        exit
                    end if
                end do
            end if
        end do
        if (match_count > 1) then
            result = standardir_lexical_fact_t()
            status = standardir_lexical_lookup_ambiguous
            message = 'lookup value matches overlapping lexical facts'
        else if (match_count == 1) then
            status = standardir_lexical_lookup_match
        else if (processor_defined) then
            status = standardir_lexical_lookup_unsupported
            message = 'lookup requires a processor-defined lexical fact'
        else
            result = standardir_lexical_fact_t()
            status = standardir_lexical_lookup_no_match
            message = 'lookup value matches no lexical fact'
        end if
    end subroutine standardir_lexical_lookup

    subroutine standardir_lexical_validate(facts, ok, message)
        type(standardir_lexical_facts_t), intent(in) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, k, l

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
            do j = 1, facts%facts(i)%range_count
                do k = 1, j - 1
                    if (ranges_overlap(facts%facts(i)%range_first(k), &
                        facts%facts(i)%range_last(k), facts%facts(i)%range_first(j), &
                        facts%facts(i)%range_last(j))) then
                        message = 'overlapping lexical codepoint ranges'
                        return
                    end if
                end do
            end do
            do j = 1, i - 1
                if (trim(facts%facts(i)%source_term) == &
                    trim(facts%facts(j)%source_term)) then
                    message = 'duplicate lexical source term: '// &
                        trim(facts%facts(i)%source_term)
                    return
                end if
                if (trim(facts%facts(i)%target_name) == &
                    trim(facts%facts(j)%target_name)) then
                    message = 'duplicate lexical target: '// &
                        trim(facts%facts(i)%target_name)
                    return
                end if
                do k = 1, facts%facts(i)%range_count
                    do l = 1, facts%facts(j)%range_count
                        if (ranges_overlap(facts%facts(i)%range_first(k), &
                            facts%facts(i)%range_last(k), facts%facts(j)%range_first(l), &
                            facts%facts(j)%range_last(l))) then
                            message = 'overlapping lexical codepoint ranges'
                            return
                        end if
                    end do
                end do
            end do
            do j = 1, facts%facts(i)%range_count
                if (.not. is_unicode_scalar(facts%facts(i)%range_first(j)) .or. &
                    .not. is_unicode_scalar(facts%facts(i)%range_last(j))) then
                    message = 'lexical codepoint range is outside Unicode scalar range'
                    return
                end if
                if (facts%facts(i)%range_first(j) > facts%facts(i)%range_last(j)) then
                    message = 'lexical codepoint range is reversed'
                    return
                end if
            end do
            if (.not. source_scalar_is_exact(facts%facts(i))) then
                message = 'Unicode lexical fact does not preserve its UTF-8 scalar'
                return
            end if
            if (.not. canonical_spelling_is_valid(facts%facts(i)%canonical_spelling)) then
                message = 'lexical fact has an unrepresentable canonical spelling'
                return
            end if
        end do
        ok = .true.
    end subroutine standardir_lexical_validate

    subroutine standardir_lexical_resolve_spelling(fact, spelling, ok, message)
        type(standardir_lexical_fact_t), intent(in) :: fact
        character(len=*), intent(out) :: spelling
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        logical :: exact
        integer :: width
        integer(int64) :: scalar

        spelling = ''
        ok = .false.
        message = ''
        if (.not. canonical_spelling_is_valid(fact%canonical_spelling)) then
            message = 'lexical fact has an unrepresentable canonical spelling'
            return
        end if
        if (len_trim(fact%canonical_spelling) > 0) then
            spelling = trim(fact%canonical_spelling)
            ok = .true.
            return
        end if
        call source_term_scalar(fact%source_term, scalar, width, exact, ok, message)
        if (.not. ok) return
        if (exact .and. width > 1) then
            message = 'lexical fact lacks a canonical spelling for a Unicode scalar'
            ok = .false.
            return
        end if
        ok = .true.
    end subroutine standardir_lexical_resolve_spelling

    logical function processor_defined_fact_is_set(fact)
        type(standardir_lexical_fact_t), intent(in) :: fact

        processor_defined_fact_is_set = len_trim(fact%target_name) > 0
    end function processor_defined_fact_is_set

    logical function ranges_overlap(first_a, last_a, first_b, last_b)
        integer(int64), intent(in) :: first_a, last_a, first_b, last_b

        ranges_overlap = first_a <= last_b .and. first_b <= last_a
    end function ranges_overlap

    logical function is_unicode_scalar(value)
        integer(int64), intent(in) :: value

        is_unicode_scalar = value >= 0_int64
        if (.not. is_unicode_scalar) return
        is_unicode_scalar = value <= int(z'10ffff', int64)
        if (.not. is_unicode_scalar) return
        if (value >= int(z'd800', int64)) then
            if (value <= int(z'dfff', int64)) is_unicode_scalar = .false.
        end if
    end function is_unicode_scalar

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

        character(len=256) :: source_term, canonical_spelling, class_name, target_name, codepoint
        character(len=256) :: source_rule
        character(len=256) :: document, clause, page, source_hash
        integer :: i

        fact = standardir_lexical_fact_t()
        source_term = ''; canonical_spelling = ''; class_name = ''; target_name = ''; codepoint = ''
        source_rule = ''; document = ''; clause = ''; page = ''; source_hash = ''
        ok = .false.; message = ''
        do i = 2, node%child_count
            if (node%children(i)%kind /= sx_list) then
                message = 'lexical fact field is not a pair'
                return
            end if
            if (standardir_atom_equals(node%children(i)%children(1), 'source-term')) then
                call standardir_read_pair(node%children(i), 'source-term', source_term, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'canonical-spelling')) then
                call standardir_read_pair(node%children(i), 'canonical-spelling', canonical_spelling, &
                    ok, message)
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
                    ok, message, reject_unknown=.true.)
            else
                ok = .false.
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
        fact%canonical_spelling = trim(canonical_spelling)
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

        logical :: exact, ok
        integer :: width
        integer(int64) :: scalar
        character(len=256) :: message

        call source_term_scalar(fact%source_term, scalar, width, exact, ok, message)
        source_scalar_is_exact = ok
        if (.not. ok .or. .not. exact) return
        if (fact%range_count /= 1) return
        if (fact%range_first(1) /= fact%range_last(1)) return
        source_scalar_is_exact = scalar == fact%range_first(1)
    end function source_scalar_is_exact

    subroutine source_term_scalar(source_term, scalar, width, exact, ok, message)
        character(len=*), intent(in) :: source_term
        integer(int64), intent(out) :: scalar
        integer, intent(out) :: width
        logical, intent(out) :: exact, ok
        character(len=*), intent(out) :: message

        integer(int8), target :: bytes(lexical_text_length)
        type(byte_span_t) :: source
        type(utf8_codepoint_t) :: codepoint
        integer :: length, bad_offset

        scalar = 0_int64
        width = 0
        exact = .false.
        ok = .false.
        message = ''
        call source_term_bytes(source_term, bytes, length, ok, message)
        if (.not. ok) return
        call byte_span_from_array(bytes, 1, length, source, ok, message)
        if (.not. ok) return
        call utf8_validate(source, ok, bad_offset, message)
        if (.not. ok) then
            message = 'lexical fact source term is not valid UTF-8'
            return
        end if
        if (length == 0) then
            ok = .true.
            return
        end if
        call utf8_decode_next(source, 1, codepoint, ok, message)
        if (.not. ok) return
        if (codepoint%width == length) then
            exact = .true.
            scalar = int(codepoint%value, int64)
            width = codepoint%width
        end if
    end subroutine source_term_scalar

    subroutine source_term_bytes(source_term, bytes, length, ok, message)
        character(len=*), intent(in) :: source_term
        integer(int8), intent(out) :: bytes(:)
        integer, intent(out) :: length
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, code

        bytes = 0_int8
        length = len_trim(source_term)
        ok = .false.
        message = ''
        if (length > size(bytes)) then
            message = 'lexical fact source term exceeds UTF-8 storage'
            return
        end if
        do i = 1, length
            code = iachar(source_term(i:i))
            if (code > 127) code = code - 256
            bytes(i) = int(code, int8)
        end do
        ok = .true.
    end subroutine source_term_bytes

    logical function canonical_spelling_is_valid(value)
        character(len=*), intent(in) :: value

        integer :: i, code

        canonical_spelling_is_valid = .true.
        do i = 1, len_trim(value)
            code = iachar(value(i:i))
            if (code < 33 .or. code > 126) then
                canonical_spelling_is_valid = .false.
                return
            end if
        end do
    end function canonical_spelling_is_valid

end module standardir_lexical

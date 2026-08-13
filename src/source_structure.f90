module source_structure
    !! Index document structure without assigning semantic meaning.
    !!
    !! The numeric heading and R... rule recognizers are deliberately bounded
    !! to the canonical J3/Fortran page shape. Cross-reference records retain
    !! only a line-start "See" block; they do not parse its targets.

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    use sha256, only: sha256_context_t, sha256_final, sha256_init, sha256_update
    implicit none
    private

    integer, parameter :: max_page_bytes = 65536
    integer, parameter :: max_line_bytes = 4096
    integer, parameter :: hash_bytes = 32
    integer, parameter :: hash_text_length = 64

    type :: index_state_t
        logical :: rule_active = .false.
        character(len=16) :: rule = ''
    end type index_state_t

    public :: source_structure_index

contains

    subroutine source_structure_index(text_unit, index_unit, output_unit, records, ok, &
            message, expected_hash)
        integer, intent(in) :: text_unit, index_unit, output_unit
        integer, intent(out) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: expected_hash

        character(len=hash_text_length) :: source_hash
        character(len=256) :: line, key1
        character(len=max_page_bytes) :: page_text
        type(index_state_t) :: state
        integer(int64) :: page_start, page_length, expected_start, file_size, total_bytes
        integer :: ios, page, page_count, page_seen
        integer :: page_records
        logical :: valid

        records = 0
        ok = .false.
        message = ''
        call canonical_hash(text_unit, source_hash, valid, message)
        if (.not. valid) return
        if (present(expected_hash)) then
            if (.not. same_hash(source_hash, expected_hash)) then
                message = 'canonical text hash does not match expected source hash'
                return
            end if
        end if

        call read_index_header(index_unit, page_count, valid, message)
        if (.not. valid) return
        inquire (unit=text_unit, size=file_size, iostat=ios)
        if (ios /= 0) then
            message = 'cannot determine canonical text size'
            return
        end if

        write (output_unit, '(a)') &
            '{"format":1,"origin":"MECHANICAL","source":"canonical-text",'// &
            '"source_sha256":"'//source_hash//'"}'
        expected_start = 0_int64
        page_seen = 0
        state%rule_active = .false.
        do page = 1, page_count
            read (index_unit, '(a)', iostat=ios) line
            if (ios /= 0) then
                message = 'page index ended before all pages were read'
                return
            end if
            call parse_page_line(line, page_seen, page_start, page_length, valid, message)
            if (.not. valid) return
            if (page_seen /= page) then
                message = 'page index is not in one-based page order'
                return
            end if
            if (page_start /= expected_start) then
                message = 'page index has a non-contiguous byte start'
                return
            end if
            if (page_length > int(max_page_bytes, int64)) then
                message = 'canonical page exceeds bounded indexer buffer'
                return
            end if
            if (page_start > file_size) then
                message = 'page starts beyond canonical text'
                return
            end if
            if (page_length > file_size - page_start) then
                message = 'page extends beyond canonical text'
                return
            end if
            if (page_length > 0_int64) then
                read (text_unit, pos=page_start + 1, iostat=ios) &
                    page_text(1:int(page_length))
                if (ios /= 0) then
                    message = 'cannot read canonical page'
                    return
                end if
            end if
            call index_page(page, page_start, page_text, int(page_length), state, &
                output_unit, source_hash, page_records, valid, message)
            if (.not. valid) return
            records = records + page_records
            expected_start = expected_start + page_length
            if (page < page_count) expected_start = expected_start + 1_int64
        end do

        read (index_unit, '(a)', iostat=ios) line
        if (ios /= 0) then
            message = 'page index has no bytes record'
            return
        end if
        read (line, *, iostat=ios) key1, total_bytes
        if (ios /= 0) then
            message = 'malformed page-index bytes record'
            return
        end if
        if (trim(key1) /= 'bytes') then
            message = 'page index bytes record is missing'
            return
        end if
        if (total_bytes /= expected_start) then
            message = 'page-index bytes total differs from page spans'
            return
        end if
        read (index_unit, '(a)', iostat=ios) line
        if (ios == 0) then
            message = 'page index contains records after bytes total'
            return
        end if
        if (expected_start /= file_size) then
            message = 'page index total differs from canonical text size'
            return
        end if
        ok = .true.
    end subroutine source_structure_index

    subroutine canonical_hash(unit, hash, ok, message)
        integer, intent(in) :: unit
        character(len=hash_text_length), intent(out) :: hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sha256_context_t) :: context
        integer(int8) :: bytes(65536), digest(hash_bytes)
        integer(int64) :: file_size, position, remaining, chunk
        integer :: ios

        hash = ''
        ok = .false.
        message = ''
        inquire (unit=unit, size=file_size, iostat=ios)
        if (ios /= 0) then
            message = 'cannot determine canonical text size for hashing'
            return
        end if
        call sha256_init(context)
        position = 1_int64
        remaining = file_size
        do while (remaining > 0_int64)
            chunk = min(remaining, int(size(bytes), int64))
            read (unit, pos=position, iostat=ios) bytes(1:int(chunk))
            if (ios /= 0) then
                message = 'cannot read canonical text for hashing'
                return
            end if
            call sha256_update(context, bytes(1:int(chunk)))
            position = position + chunk
            remaining = remaining - chunk
        end do
        call sha256_final(context, digest)
        call digest_hex(digest, hash)
        ok = .true.
    end subroutine canonical_hash

    subroutine digest_hex(digest, text)
        integer(int8), intent(in) :: digest(:)
        character(len=hash_text_length), intent(out) :: text

        character(len=16), parameter :: digits = '0123456789abcdef'
        integer(int32) :: value, high, low
        integer :: i

        text = ''
        do i = 1, size(digest)
            value = int(digest(i), int32)
            if (value < 0) value = value + 256
            high = shiftr(value, 4)
            low = iand(value, 15_int32)
            text(2 * i - 1:2 * i - 1) = digits(high + 1:high + 1)
            text(2 * i:2 * i) = digits(low + 1:low + 1)
        end do
    end subroutine digest_hex

    logical function same_hash(actual, expected)
        character(len=*), intent(in) :: actual, expected

        same_hash = .false.
        if (.not. valid_hash(expected)) return
        if (actual /= expected(1:hash_text_length)) return
        same_hash = .true.
    end function same_hash

    logical function valid_hash(value)
        character(len=*), intent(in) :: value
        integer :: i, code

        valid_hash = .false.
        if (len_trim(value) /= hash_text_length) return
        do i = 1, hash_text_length
            code = iachar(value(i:i))
            if (code >= iachar('0')) then
                if (code <= iachar('9')) cycle
            end if
            if (code >= iachar('a')) then
                if (code <= iachar('f')) cycle
            end if
            if (code >= iachar('A')) then
                if (code <= iachar('F')) cycle
            end if
            return
        end do
        valid_hash = .true.
    end function valid_hash

    subroutine read_index_header(unit, page_count, ok, message)
        integer, intent(in) :: unit
        integer, intent(out) :: page_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: line, key
        integer :: ios

        ok = .false.
        message = ''
        page_count = 0
        call required_index_line(unit, 'canonical-format 1', ok, message)
        if (.not. ok) return
        call required_index_line(unit, 'origin MECHANICAL', ok, message)
        if (.not. ok) return
        call required_index_line(unit, 'encoding UTF-8', ok, message)
        if (.not. ok) return
        call required_index_line(unit, 'separator FORM-FEED', ok, message)
        if (.not. ok) return
        read (unit, '(a)', iostat=ios) line
        if (ios /= 0) then
            message = 'page index has no pages record'
            return
        end if
        read (line, *, iostat=ios) key, page_count
        if (ios /= 0) then
            message = 'malformed page-index pages record'
            return
        end if
        if (trim(key) /= 'pages') then
            message = 'page index pages record is missing'
            return
        end if
        if (page_count < 0) then
            message = 'page index has a negative page count'
            return
        end if
        ok = .true.
    end subroutine read_index_header

    subroutine required_index_line(unit, expected, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: expected
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: line
        integer :: ios

        read (unit, '(a)', iostat=ios) line
        ok = .false.
        message = ''
        if (ios /= 0) then
            message = 'page index ended in its header'
            return
        end if
        if (trim(line) /= trim(expected)) then
            message = 'unexpected page-index header'
            return
        end if
        ok = .true.
    end subroutine required_index_line

    subroutine parse_page_line(line, page, page_start, page_length, ok, message)
        character(len=*), intent(in) :: line
        integer, intent(out) :: page
        integer(int64), intent(out) :: page_start, page_length
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=32) :: key1, key2, key3
        integer :: ios

        page = 0
        page_start = 0_int64
        page_length = 0_int64
        read (line, *, iostat=ios) key1, page, key2, page_start, key3, page_length
        ok = .false.
        message = ''
        if (ios /= 0) then
            message = 'malformed page-index page record'
            return
        end if
        if (trim(key1) /= 'page') then
            message = 'page-index page record is missing page key'
            return
        end if
        if (trim(key2) /= 'start') then
            message = 'page-index page record is missing start key'
            return
        end if
        if (trim(key3) /= 'length') then
            message = 'page-index page record is missing length key'
            return
        end if
        if (page < 1) then
            message = 'page-index page number is below one'
            return
        end if
        if (page_start < 0_int64) then
            message = 'page-index byte start is negative'
            return
        end if
        if (page_length < 0_int64) then
            message = 'page-index byte length is negative'
            return
        end if
        ok = .true.
    end subroutine parse_page_line

    subroutine index_page(page, page_start, page_text, page_length, state, output_unit, &
            source_hash, records, ok, message)
        integer, intent(in) :: page, page_length, output_unit
        integer(int64), intent(in) :: page_start
        character(len=*), intent(in) :: page_text
        type(index_state_t), intent(inout) :: state
        character(len=*), intent(in) :: source_hash
        integer, intent(out) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=max_line_bytes) :: line
        integer :: cursor, line_end, next_line, newline_pos
        integer :: current_length
        integer(int64) :: source_start

        records = 0
        ok = .false.
        message = ''
        if (page_length < 0) then
            message = 'negative page length passed to structure indexer'
            return
        end if
        cursor = 1
        do while (cursor <= page_length)
            newline_pos = index(page_text(cursor:page_length), achar(10))
            if (newline_pos == 0) then
                line_end = page_length
                next_line = page_length + 1
            else
                line_end = cursor + newline_pos - 2
                next_line = cursor + newline_pos
            end if
            current_length = line_end - cursor + 1
            if (current_length > max_line_bytes) then
                message = 'canonical line exceeds bounded indexer buffer'
                return
            end if
            line = ''
            if (current_length > 0) line(1:current_length) = &
                page_text(cursor:line_end)
            source_start = page_start + int(cursor - 1, int64)
            call index_line(page, source_start, line, current_length, state, output_unit, &
                source_hash, records, ok, message)
            if (.not. ok) return
            cursor = next_line
        end do
        ok = .true.
    end subroutine index_page

    subroutine index_line(page, source_start, line, line_length, state, output_unit, &
            source_hash, records, ok, message)
        integer, intent(in) :: page, line_length, output_unit
        integer(int64), intent(in) :: source_start
        character(len=*), intent(in) :: line
        type(index_state_t), intent(inout) :: state
        character(len=*), intent(in) :: source_hash
        integer, intent(inout) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=max_line_bytes) :: clean
        character(len=64) :: heading_number
        character(len=16) :: rule
        integer :: clean_length, line_number, level
        logical :: has_line_number
        logical :: is_rule, is_heading, is_cross_reference

        ok = .false.
        message = ''
        rule = ''
        heading_number = ''
        line_number = -1
        level = 0
        call strip_printed_number(line, line_length, clean, line_number, has_line_number)
        clean_length = len_trim(clean)
        call parse_rule_start(clean, clean_length, is_rule, rule)
        if (is_rule) then
            state%rule_active = .true.
            state%rule = rule
            call emit_rule_start(output_unit, source_hash, rule, line_number, line, &
                line_length, page, source_start)
            records = records + 1
            ok = .true.
            return
        end if
        if (state%rule_active) then
            if (is_rule_continuation(clean, clean_length, has_line_number)) then
                call emit_rule_continuation(output_unit, source_hash, state%rule, &
                    line_number, line, line_length, page, source_start)
                records = records + 1
                ok = .true.
                return
            end if
            state%rule_active = .false.
            state%rule = ''
        end if
        call parse_heading(clean, clean_length, is_heading, heading_number, level)
        if (is_heading) then
            call emit_heading(output_unit, source_hash, heading_number, level, line, &
                line_length, page, source_start)
            records = records + 1
            ok = .true.
            return
        end if
        call parse_cross_reference(clean, clean_length, is_cross_reference)
        if (is_cross_reference) then
            call emit_cross_reference(output_unit, source_hash, line_number, line, &
                line_length, page, source_start)
            records = records + 1
        end if
        ok = .true.
    end subroutine index_line

    subroutine strip_printed_number(line, line_length, clean, line_number, found)
        character(len=*), intent(in) :: line
        integer, intent(in) :: line_length
        character(len=*), intent(out) :: clean
        integer, intent(out) :: line_number
        logical, intent(out) :: found

        integer :: i, ios, start
        character(len=max_line_bytes) :: remainder

        clean = ''
        line_number = -1
        found = .false.
        if (line_length == 0) return
        clean(1:line_length) = adjustl(line(1:line_length))
        start = 1
        do while (start <= line_length)
            if (clean(start:start) < '0') exit
            if (clean(start:start) > '9') exit
            start = start + 1
        end do
        if (start == 1) return
        if (start > line_length) return
        if (clean(start:start) /= ' ') return
        read (clean(1:start - 1), *, iostat=ios) line_number
        if (ios /= 0) return
        i = start
        do while (i <= line_length)
            if (clean(i:i) /= ' ') exit
            i = i + 1
        end do
        remainder = clean
        clean = ''
        if (i <= line_length) clean(1:line_length - i + 1) = remainder(i:line_length)
        found = .true.
    end subroutine strip_printed_number

    subroutine parse_rule_start(clean, line_length, found, rule)
        character(len=*), intent(in) :: clean
        integer, intent(in) :: line_length
        logical, intent(out) :: found
        character(len=*), intent(out) :: rule

        integer :: i, rule_end, is_position

        found = .false.
        rule = ''
        if (line_length < 4) return
        if (clean(1:1) /= 'R') return
        i = 2
        do while (i <= line_length)
            if (clean(i:i) < '0') exit
            if (clean(i:i) > '9') exit
            i = i + 1
        end do
        if (i == 2) return
        rule_end = i - 1
        if (i > line_length) return
        if (clean(i:i) /= ' ') return
        is_position = index(clean(i:line_length), ' is ')
        if (is_position == 0) return
        if (i + is_position + 3 > line_length) return
        rule = clean(1:rule_end)
        found = .true.
    end subroutine parse_rule_start

    logical function is_rule_continuation(clean, line_length, has_line_number)
        character(len=*), intent(in) :: clean
        integer, intent(in) :: line_length
        logical, intent(in) :: has_line_number
        integer :: i

        is_rule_continuation = .false.
        if (.not. has_line_number) return
        if (line_length == 0) return
        if (line_length >= 3) then
            if (clean(1:3) == 'or ') then
                is_rule_continuation = .true.
                return
            end if
        end if
        select case (clean(1:1))
        case ('[', '(', '.', '+', '-', '*', '/', '=', '_', '''', '"')
            is_rule_continuation = .true.
            return
        case default
        end select
        if (clean(1:1) >= 'a') then
            if (clean(1:1) <= 'z') then
                do i = 1, line_length
                    if (clean(i:i) == ' ') return
                end do
                is_rule_continuation = .true.
            end if
        end if
    end function is_rule_continuation

    subroutine parse_heading(clean, line_length, found, number, level)
        character(len=*), intent(in) :: clean
        integer, intent(in) :: line_length
        logical, intent(out) :: found
        character(len=*), intent(out) :: number
        integer, intent(out) :: level

        integer :: i, number_end

        found = .false.
        number = ''
        level = 0
        if (line_length == 0) return
        i = 1
        call consume_digits(clean, line_length, i)
        if (i == 1) return
        level = 1
        do while (i <= line_length)
            if (clean(i:i) /= '.') exit
            i = i + 1
            if (i > line_length) return
            if (.not. is_digit(clean(i:i))) return
            call consume_digits(clean, line_length, i)
            level = level + 1
        end do
        if (i > line_length) return
        if (.not. is_space(clean(i:i))) return
        number_end = i - 1
        do while (i <= line_length)
            if (.not. is_space(clean(i:i))) exit
            i = i + 1
        end do
        if (i > line_length) return
        if (index(clean(1:line_length), '. . .') > 0) return
        number = clean(1:number_end)
        found = .true.
    end subroutine parse_heading

    subroutine consume_digits(text, text_length, position)
        character(len=*), intent(in) :: text
        integer, intent(in) :: text_length
        integer, intent(inout) :: position

        do while (position <= text_length)
            if (.not. is_digit(text(position:position))) exit
            position = position + 1
        end do
    end subroutine consume_digits

    logical function is_digit(value)
        character(len=1), intent(in) :: value

        is_digit = .false.
        if (value < '0') return
        if (value > '9') return
        is_digit = .true.
    end function is_digit

    logical function is_space(value)
        character(len=1), intent(in) :: value

        is_space = .false.
        if (value == ' ') is_space = .true.
        if (value == achar(9)) is_space = .true.
    end function is_space

    subroutine parse_cross_reference(clean, line_length, found)
        character(len=*), intent(in) :: clean
        integer, intent(in) :: line_length
        logical, intent(out) :: found

        found = .false.
        if (line_length < 5) return
        if (clean(1:4) /= 'See ') return
        if (len_trim(clean(5:line_length)) == 0) return
        found = .true.
    end subroutine parse_cross_reference

    subroutine emit_heading(unit, source_hash, number, level, text, text_length, page, start)
        integer, intent(in) :: unit, level, text_length, page
        character(len=*), intent(in) :: source_hash, number, text
        integer(int64), intent(in) :: start

        write (unit, '(a)', advance='no') '{"kind":"section-heading","number":'
        call json_string(unit, number, len_trim(number))
        write (unit, '(a,i0,a)', advance='no') ',"level":', level, ',"text":'
        call json_string(unit, text, text_length)
        call emit_provenance(unit, source_hash, page, start, text_length)
        write (unit, '(a)') ''
    end subroutine emit_heading

    subroutine emit_rule_start(unit, source_hash, rule, line_number, text, text_length, &
            page, start)
        integer, intent(in) :: unit, line_number, text_length, page
        character(len=*), intent(in) :: source_hash, rule, text
        integer(int64), intent(in) :: start

        write (unit, '(a)', advance='no') '{"kind":"rule-block-start","rule":'
        call json_string(unit, rule, len_trim(rule))
        write (unit, '(a,i0,a)', advance='no') ',"line_number":', line_number, ',"text":'
        call json_string(unit, text, text_length)
        call emit_provenance(unit, source_hash, page, start, text_length)
        write (unit, '(a)') ''
    end subroutine emit_rule_start

    subroutine emit_rule_continuation(unit, source_hash, rule, line_number, text, &
            text_length, page, start)
        integer, intent(in) :: unit, line_number, text_length, page
        character(len=*), intent(in) :: source_hash, rule, text
        integer(int64), intent(in) :: start

        write (unit, '(a)', advance='no') '{"kind":"rule-continuation","owner":'
        call json_string(unit, rule, len_trim(rule))
        write (unit, '(a,i0,a)', advance='no') ',"line_number":', line_number, ',"text":'
        call json_string(unit, text, text_length)
        call emit_provenance(unit, source_hash, page, start, text_length)
        write (unit, '(a)') ''
    end subroutine emit_rule_continuation

    subroutine emit_cross_reference(unit, source_hash, line_number, text, text_length, &
            page, start)
        integer, intent(in) :: unit, line_number, text_length, page
        character(len=*), intent(in) :: source_hash, text
        integer(int64), intent(in) :: start

        write (unit, '(a,i0,a)', advance='no') &
            '{"kind":"cross-reference-block","line_number":', line_number, ',"text":'
        call json_string(unit, text, text_length)
        call emit_provenance(unit, source_hash, page, start, text_length)
        write (unit, '(a)') ''
    end subroutine emit_cross_reference

    subroutine emit_provenance(unit, source_hash, page, start, length)
        integer, intent(in) :: unit, page, length
        character(len=*), intent(in) :: source_hash
        integer(int64), intent(in) :: start

        write (unit, '(a,i0)', advance='no') ',"page":', page
        write (unit, '(a,i0)', advance='no') ',"byte_start":', start
        write (unit, '(a,i0)', advance='no') ',"byte_length":', length
        write (unit, '(a)', advance='no') ',"source_sha256":"'//source_hash//'"'
        write (unit, '(a)', advance='no') ',"origin":"MECHANICAL"}'
    end subroutine emit_provenance

    subroutine json_string(unit, value, value_length)
        integer, intent(in) :: unit, value_length
        character(len=*), intent(in) :: value
        integer :: i, code

        write (unit, '(a)', advance='no') '"'
        do i = 1, value_length
            code = iachar(value(i:i))
            select case (code)
            case (8)
                write (unit, '(a)', advance='no') '\b'
            case (9)
                write (unit, '(a)', advance='no') '\t'
            case (10)
                write (unit, '(a)', advance='no') '\n'
            case (12)
                write (unit, '(a)', advance='no') '\f'
            case (13)
                write (unit, '(a)', advance='no') '\r'
            case (32:126)
                if (value(i:i) == '"') then
                    write (unit, '(a)', advance='no') '\"'
                else if (value(i:i) == '\') then
                    write (unit, '(a)', advance='no') '\\'
                else
                    write (unit, '(a)', advance='no') value(i:i)
                end if
            case default
                write (unit, '(a)', advance='no') value(i:i)
            end select
        end do
        write (unit, '(a)', advance='no') '"'
    end subroutine json_string

end module source_structure

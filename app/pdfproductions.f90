program pdfproductions
    !! Parse canonical clause-5 lines into provenance-bearing JSONL records.
    !!
    !! The output is a lossless line representation: a production start has
    !! `is`, an `or` continuation has `or`, and bracketed continuation lines
    !! have `sequence`. Later StandardIR assembly can therefore distinguish a
    !! sequence from an alternative without reparsing the PDF.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    type :: parser_state_t
        logical :: active = .false.
        character(len=16) :: rule = ''
        character(len=256) :: lhs = ''
        character(len=128) :: occurrence_clause = ''
        integer :: sequence_index = 0
        integer :: occurrence = 0
    end type parser_state_t

    character(len=4096) :: canonical_path, index_path, output_path, argument
    character(len=256) :: index_line, key1, key2, key3
    character(len=:), allocatable :: page_text ! text-policy: C string boundary
    character(len=256) :: message
    type(parser_state_t) :: state
    integer(int64) :: page_start, page_length
    integer :: argc, first_page, last_page, page, text_unit, index_unit
    integer :: output_unit, ios, page_count, records
    logical :: ok

    argc = command_argument_count()
    if (argc /= 5) then
        call get_command_argument(0, canonical_path)
        print '(a)', 'usage: '//trim(canonical_path)// &
            ' <canonical.text> <pages.index> <output.jsonl> <first-page> <last-page>'
        stop 2
    end if

    call get_command_argument(1, canonical_path)
    call get_command_argument(2, index_path)
    call get_command_argument(3, output_path)
    call get_command_argument(4, argument)
    read (argument, *, iostat=ios) first_page
    if (ios /= 0) then
        print '(a)', 'error: first page must be an integer'
        stop 2
    end if
    call get_command_argument(5, argument)
    read (argument, *, iostat=ios) last_page
    if (ios /= 0) then
        print '(a)', 'error: last page must be an integer'
        stop 2
    end if

    open (newunit=text_unit, file=trim(canonical_path), access='stream', &
        form='unformatted', action='read', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open canonical text'
        stop 1
    end if
    open (newunit=index_unit, file=trim(index_path), action='read', iostat=ios)
    if (ios /= 0) then
        close (text_unit)
        print '(a)', 'error: cannot open page index'
        stop 1
    end if
    open (newunit=output_unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (text_unit)
        close (index_unit)
        print '(a)', 'error: cannot open production output'
        stop 1
    end if

    write (output_unit, '(a)') &
        '{"format":1,"origin":"MECHANICAL","source":"canonical-text"}'
    state%active = .false.
    records = 0
    page_count = 0
    allocate (character(len=65536) :: page_text)

    do
        read (index_unit, '(a)', iostat=ios) index_line
        if (ios /= 0) exit
        if (index_line(1:5) /= 'page ') cycle
        read (index_line, *, iostat=ios) key1, page, key2, page_start, &
            key3, page_length
        if (ios /= 0) then
            close (output_unit, status='delete')
            close (index_unit)
            close (text_unit)
            print '(a)', 'error: malformed page index'
            stop 1
        end if
        if (page < first_page) cycle
        if (page > last_page) exit
        if (page_length > int(len(page_text), int64)) then
            close (output_unit, status='delete')
            close (index_unit)
            close (text_unit)
            print '(a)', 'error: page exceeds parser buffer'
            stop 1
        end if
        if (page_length > 0_int64) then
            read (text_unit, pos=page_start + 1, iostat=ios) &
                page_text(1:int(page_length))
            if (ios /= 0) then
                close (output_unit, status='delete')
                close (index_unit)
                close (text_unit)
                print '(a)', 'error: cannot read canonical page'
                stop 1
            end if
        end if
        call process_page(page, page_start, page_text, int(page_length), state, &
            output_unit, records, ok, message)
        if (.not. ok) then
            close (output_unit, status='delete')
            close (index_unit)
            close (text_unit)
            print '(a)', 'error: '//trim(message)
            stop 1
        end if
        page_count = page_count + 1
    end do

    close (output_unit)
    close (index_unit)
    close (text_unit)
    print '(a,i0,a,i0,a)', 'extracted ', records, ' production lines from ', &
        page_count, ' pages'

contains

    subroutine process_page(page, page_start, page_text, page_length, state, &
            output_unit, records, ok, message)
        integer, intent(in) :: page, page_length, output_unit
        integer(int64), intent(in) :: page_start
        character(len=*), intent(in) :: page_text
        type(parser_state_t), intent(inout) :: state
        integer, intent(inout) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=4096) :: line
        integer :: cursor, line_end, line_length, next_line, newline_pos
        integer(int64) :: source_start

        ok = .false.
        message = ''
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
            line_length = line_end - cursor + 1
            if (line_length > len(line)) then
                message = 'canonical line exceeds parser buffer'
                return
            end if
            line = ''
            if (line_length > 0) line(1:line_length) = page_text(cursor:line_end)
            source_start = page_start + int(cursor - 1, int64)
            if (line_length > 0) then
                call process_line(line(1:line_length), source_start, page, state, &
                    output_unit, records, ok, message)
            else
                call process_line('', source_start, page, state, output_unit, &
                    records, ok, message)
            end if
            if (.not. ok) return
            cursor = next_line
        end do
        ok = .true.
    end subroutine process_page

    subroutine process_line(line, source_start, page, state, output_unit, records, &
            ok, message)
        character(len=*), intent(in) :: line
        integer(int64), intent(in) :: source_start
        integer, intent(in) :: page, output_unit
        type(parser_state_t), intent(inout) :: state
        integer, intent(inout) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=4096) :: clean, rhs, operator
        character(len=16) :: rule
        character(len=256) :: lhs
        character(len=128) :: heading_clause
        integer :: rule_number
        logical :: found, heading_found, boundary, layout_only

        ok = .false.
        message = ''
        call strip_line_number(line, clean)
        call parse_occurrence_heading(clean, heading_clause, heading_found)
        if (heading_found) then
            state%occurrence_clause = trim(heading_clause)
            state%active = .false.
            ok = .true.
            return
        end if
        call parse_start(clean, found, rule_number, lhs, rhs)
        if (found) then
            write (rule, '("R",i0)') rule_number
            state%active = .true.
            state%rule = adjustl(rule)
            state%lhs = lhs
            state%sequence_index = 0
            state%occurrence = state%occurrence + 1
            call emit_record('production-start', state%rule, lhs, 'is', rhs, &
                page, source_start, len(line), line, state%occurrence, output_unit, &
                records, ok, message, state%occurrence_clause)
            return
        end if

        if (.not. state%active) then
            ok = .true.
            return
        end if
        call continuation(clean, boundary, layout_only, operator, rhs)
        if (boundary) then
            if (.not. layout_only) state%active = .false.
            ok = .true.
            return
        end if
        state%sequence_index = state%sequence_index + 1
        call emit_record('production-continuation', state%rule, state%lhs, &
            operator, rhs, page, source_start, len(line), line, state%occurrence, &
            output_unit, records, ok, message)
    end subroutine process_line

    subroutine strip_line_number(line, clean)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: clean
        integer :: i, n

        clean = line
        call remove_layout_controls(clean)
        clean = adjustl(clean)
        n = len_trim(clean)
        i = 1
        do while (i <= n)
            if (clean(i:i) < '0' .or. clean(i:i) > '9') exit
            i = i + 1
        end do
        if (i > 1 .and. i <= n) then
            if (clean(i:i) == ' ') then
                do while (i <= n)
                    if (clean(i:i) /= ' ') exit
                    i = i + 1
                end do
                if (i <= n) clean = adjustl(clean(i:))
            end if
        end if
    end subroutine strip_line_number

    subroutine parse_start(clean, found, rule_number, lhs, rhs)
        character(len=*), intent(in) :: clean
        logical, intent(out) :: found
        integer, intent(out) :: rule_number
        character(len=*), intent(out) :: lhs, rhs
        character(len=4096) :: after
        integer :: i, n, ios, is_position

        found = .false.
        rule_number = 0
        lhs = ''
        rhs = ''
        after = adjustl(clean)
        n = len_trim(after)
        if (n < 5) return
        if (after(1:1) /= 'R') return
        i = 2
        do while (i <= n)
            if (after(i:i) < '0' .or. after(i:i) > '9') exit
            i = i + 1
        end do
        if (i == 2) return
        read (after(2:i - 1), *, iostat=ios) rule_number
        if (ios /= 0) return
        if (i > n) return
        after = adjustl(after(i:))
        is_position = index(after, ' is ')
        if (is_position <= 1) return
        lhs = trim(after(1:is_position - 1))
        if (is_position + 4 <= len_trim(after)) rhs = trim(after(is_position + 4:))
        found = len_trim(lhs) > 0 .and. len_trim(rhs) > 0
    end subroutine parse_start

    subroutine parse_occurrence_heading(clean, clause, found)
        character(len=*), intent(in) :: clean
        character(len=*), intent(out) :: clause
        logical, intent(out) :: found

        character(len=4096) :: text
        integer :: i, n, first_end, components

        clause = ''
        found = .false.
        text = adjustl(clean)
        n = len_trim(text)
        if (n == 0) return
        i = 1
        call consume_digits(text, n, i)
        if (i == 1) return
        first_end = i - 1
        components = 1
        do while (i <= n)
            if (text(i:i) /= '.') exit
            i = i + 1
            if (i > n) return
            if (.not. is_digit(text(i:i))) return
            call consume_digits(text, n, i)
            components = components + 1
        end do
        if (components < 2) return
        if (i > n) return
        if (.not. is_space(text(i:i))) return
        if (index(text(1:n), '. . .') > 0) return
        do while (i <= n)
            if (.not. is_space(text(i:i))) exit
            i = i + 1
        end do
        if (i > n) return
        if (.not. has_letter(text(i:n))) return
        clause = text(1:first_end)
        found = .true.
    end subroutine parse_occurrence_heading

    subroutine consume_digits(text, n, position)
        character(len=*), intent(in) :: text
        integer, intent(in) :: n
        integer, intent(inout) :: position

        do while (position <= n)
            if (.not. is_digit(text(position:position))) exit
            position = position + 1
        end do
    end subroutine consume_digits

    logical function is_digit(value)
        character(len=1), intent(in) :: value

        is_digit = value >= '0' .and. value <= '9'
    end function is_digit

    logical function is_space(value)
        character(len=1), intent(in) :: value

        is_space = value == ' ' .or. value == achar(9)
    end function is_space

    logical function has_letter(text)
        character(len=*), intent(in) :: text
        integer :: i

        has_letter = .false.
        do i = 1, len_trim(text)
            if (text(i:i) >= 'A' .and. text(i:i) <= 'Z') then
                has_letter = .true.
                return
            end if
            if (text(i:i) >= 'a' .and. text(i:i) <= 'z') then
                has_letter = .true.
                return
            end if
        end do
    end function has_letter

    subroutine continuation(clean, boundary, layout_only, operator, rhs)
        character(len=*), intent(in) :: clean
        logical, intent(out) :: boundary, layout_only
        character(len=*), intent(out) :: operator, rhs
        character(len=4096) :: text
        integer :: n

        text = adjustl(clean)
        n = len_trim(text)
        boundary = n == 0
        layout_only = boundary
        operator = ''
        rhs = ''
        if (boundary) return
        if (is_layout_line(text)) then
            boundary = .true.
            layout_only = .true.
            return
        end if
        if (index(text, '5.2 ') == 1 .or. index(text, '5.3 ') == 1) then
            boundary = .true.
            layout_only = .false.
            return
        end if
        if (index(text, '2023-') == 1 .or. index(text, 'WD ') == 1) then
            boundary = .true.
            layout_only = .false.
            return
        end if
        if (text(1:1) >= '0' .and. text(1:1) <= '9') then
            boundary = .true.
            layout_only = .false.
            return
        end if
        if (is_prose_boundary(text)) then
            boundary = .true.
            layout_only = .false.
            return
        end if
        if (.not. looks_like_grammar(text)) then
            boundary = .true.
            layout_only = .false.
            return
        end if
        if (n >= 3) then
            if (text(1:3) == 'or ') then
                operator = 'or'
                if (n > 3) rhs = trim(text(4:))
            end if
        end if
        if (len_trim(operator) == 0) then
            operator = 'sequence'
            rhs = trim(text)
        end if
    end subroutine continuation

    logical function is_layout_line(text)
        character(len=*), intent(in) :: text

        is_layout_line = index(text, 'J3/') == 1 .or. index(text, '2023-') == 1 .or. &
            index(text, '2024-') == 1 .or. index(text, '2025-') == 1 .or. &
            index(text, 'WD ') == 1
        if (.not. is_layout_line) then
            is_layout_line = all_digits(text)
        end if
    end function is_layout_line

    logical function all_digits(text)
        character(len=*), intent(in) :: text
        integer :: i

        all_digits = len_trim(text) > 0
        do i = 1, len_trim(text)
            if (text(i:i) < '0' .or. text(i:i) > '9') all_digits = .false.
        end do
    end function all_digits

    subroutine remove_layout_controls(text)
        character(len=*), intent(inout) :: text
        integer :: i

        do i = 1, len(text)
            if (text(i:i) == achar(12) .or. text(i:i) == achar(13)) text(i:i) = ' '
        end do
    end subroutine remove_layout_controls

    logical function is_prose_boundary(text)
        character(len=*), intent(in) :: text
        integer :: n

        is_prose_boundary = .false.
        n = len_trim(text)
        if (has_prefix(text, 'NOTE') .or. has_prefix(text, 'Table') .or. &
            has_prefix(text, 'Examples') .or. has_prefix(text, 'See ')) then
            is_prose_boundary = .true.
            return
        end if
        if (text(1:1) == 'C') then
            if (n >= 2) then
                if (text(2:2) >= '0' .and. text(2:2) <= '9') then
                    is_prose_boundary = .true.
                end if
            end if
        end if
    end function is_prose_boundary

    logical function looks_like_grammar(text)
        character(len=*), intent(in) :: text
        character(len=16), parameter :: prose_prefixes(15) = [ character(len=16) :: &
            'The', 'A ', 'An ', 'Each ', 'For ', 'This ', 'Examples', 'is ', &
            'shall ', 'must ', 'means ', 'where ', 'which ', 'that ', 'if ' ]
        integer :: i, n

        looks_like_grammar = .false.
        n = len_trim(text)
        if (n == 0) return
        if (index(text, 'or ') == 1) then
            looks_like_grammar = .true.
            return
        end if
        select case (text(1:1))
        case ('[', '(', '.', '+', '-', '*', '/', '=', '_', '''', '"')
            looks_like_grammar = .true.
            return
        case default
        end select
        if (index(text(1:n), ' ') == 0 .and. index(text(1:n), achar(9)) == 0) then
            looks_like_grammar = .true.
            return
        end if
        if (index(text(1:n), '...') > 0 .or. index(text(1:n), ']') > 0) then
            looks_like_grammar = .true.
            return
        end if
        do i = 1, size(prose_prefixes)
            if (has_prefix(text, prose_prefixes(i))) return
        end do
        if (text(1:1) >= 'A' .and. text(1:1) <= 'Z') then
            looks_like_grammar = .true.
        else if (text(1:1) >= 'a' .and. text(1:1) <= 'z') then
            looks_like_grammar = .true.
        end if
    end function looks_like_grammar

    logical function has_prefix(text, prefix)
        character(len=*), intent(in) :: text, prefix
        integer :: n

        has_prefix = .false.
        n = len_trim(text)
        if (n < len_trim(prefix)) return
        if (text(1:len_trim(prefix)) == trim(prefix)) has_prefix = .true.
    end function has_prefix

    subroutine emit_record(kind, rule, lhs, operator, rhs, page, source_start, &
            source_length, source_line, occurrence, output_unit, records, ok, message, &
            occurrence_clause)
        character(len=*), intent(in) :: kind, rule, lhs, operator, rhs
        character(len=*), intent(in) :: source_line
        integer, intent(in) :: page, output_unit, source_length
        integer, intent(in) :: occurrence
        integer(int64), intent(in) :: source_start
        integer, intent(inout) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: occurrence_clause

        write (output_unit, '(a)', advance='no') '{"kind":'
        call json_string(output_unit, kind)
        write (output_unit, '(a)', advance='no') ',"rule":'
        call json_string(output_unit, rule)
        write (output_unit, '(a)', advance='no') ',"lhs":'
        call json_string(output_unit, lhs)
        write (output_unit, '(a)', advance='no') ',"operator":'
        call json_string(output_unit, operator)
        write (output_unit, '(a)', advance='no') ',"text":'
        call json_string(output_unit, rhs)
        write (output_unit, '(a,i0)', advance='no') ',"page":', page
        write (output_unit, '(a,i0)', advance='no') ',"byte_start":', source_start
        write (output_unit, '(a,i0)', advance='no') ',"byte_length":', source_length
        write (output_unit, '(a,i0)', advance='no') ',"occurrence":', occurrence
        if (present(occurrence_clause)) then
            if (len_trim(occurrence_clause) > 0) then
                write (output_unit, '(a)', advance='no') ',"occurrence_clause":'
                call json_string(output_unit, occurrence_clause)
            end if
        end if
        write (output_unit, '(a)', advance='no') ',"source_line":'
        call json_string_exact(output_unit, source_line)
        write (output_unit, '(a)', advance='no') ',"origin":"MECHANICAL"}'
        write (output_unit, '(a)') ''
        records = records + 1
        ok = .true.
        message = ''
    end subroutine emit_record

    subroutine json_string(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        integer :: i

        write (unit, '(a)', advance='no') '"'
        do i = 1, len_trim(value)
            select case (value(i:i))
            case ('"')
                write (unit, '(a)', advance='no') '\"'
            case ('\')
                write (unit, '(a)', advance='no') '\\'
            case default
                write (unit, '(a)', advance='no') value(i:i)
            end select
        end do
        write (unit, '(a)', advance='no') '"'
    end subroutine json_string

    subroutine json_string_exact(unit, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        integer :: i

        write (unit, '(a)', advance='no') '"'
        do i = 1, len(value)
            select case (value(i:i))
            case ('"')
                write (unit, '(a)', advance='no') '\"'
            case ('\')
                write (unit, '(a)', advance='no') '\\'
            case (achar(9))
                write (unit, '(a)', advance='no') '\t'
            case (achar(12))
                write (unit, '(a)', advance='no') '\f'
            case (achar(13))
                write (unit, '(a)', advance='no') '\r'
            case default
                write (unit, '(a)', advance='no') value(i:i)
            end select
        end do
        write (unit, '(a)', advance='no') '"'
    end subroutine json_string_exact

end program pdfproductions

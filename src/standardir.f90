module standardir
    !! The first machine-readable StandardIR syntax slice.
    !!
    !! This module deliberately knows only the grammar vocabulary recovered from
    !! the clause-5 production slice. It does not copy a comparison grammar and
    !! it does not make provenance optional.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    integer, parameter, public :: standardir_max_items = 64
    integer, parameter, public :: standardir_max_alternatives = 64

    type, public :: standardir_item_t
        integer :: kind = 0
        integer :: base_kind = 1
        character(len=256) :: name = ''
        integer :: minimum = 1
        logical :: unbounded = .false.
    end type standardir_item_t

    type, public :: standardir_alternative_t
        integer :: item_count = 0
        type(standardir_item_t) :: items(standardir_max_items)
    end type standardir_alternative_t

    type, public :: standardir_syntax_t
        character(len=16) :: rule = ''
        character(len=256) :: lhs = ''
        integer :: alternative_count = 0
        type(standardir_alternative_t) :: alternatives(standardir_max_alternatives)
        integer :: first_page = 0
        integer :: last_page = 0
        integer(int64) :: first_byte = 0
        integer(int64) :: last_byte = 0
        character(len=16384) :: pending = ''
        logical :: incomplete = .false.
    end type standardir_syntax_t

    public :: standardir_reset, standardir_start, standardir_add
    public :: standardir_emit

contains

    subroutine standardir_reset(production)
        type(standardir_syntax_t), intent(out) :: production
        production%rule = ''
        production%lhs = ''
        production%alternative_count = 0
        production%first_page = 0
        production%last_page = 0
        production%first_byte = 0_int64
        production%last_byte = 0_int64
        production%pending = ''
        production%incomplete = .false.
    end subroutine standardir_reset

    subroutine standardir_start(production, rule, lhs, page, byte_start, byte_length, &
            ok, message)
        type(standardir_syntax_t), intent(out) :: production
        character(len=*), intent(in) :: rule, lhs
        integer, intent(in) :: page
        integer(int64), intent(in) :: byte_start, byte_length
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_reset(production)
        production%rule = trim(rule)
        production%lhs = trim(lhs)
        production%alternative_count = 1
        production%first_page = page
        production%last_page = page
        production%first_byte = byte_start
        production%last_byte = byte_start + byte_length
        production%alternatives(1)%item_count = 0
        ok = .true.
        message = ''
        if (len_trim(production%rule) == 0 .or. len_trim(production%lhs) == 0) then
            ok = .false.
            message = 'production has an empty rule or left-hand side'
        end if
    end subroutine standardir_start

    subroutine standardir_add(production, operator, text, page, byte_start, byte_length, &
            ok, message)
        type(standardir_syntax_t), intent(inout) :: production
        character(len=*), intent(in) :: operator, text
        integer, intent(in) :: page
        integer(int64), intent(in) :: byte_start, byte_length
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=16384) :: candidate
        integer :: alternative, pending_length, text_length

        ok = .false.
        message = ''
        if (trim(operator) == 'or') then
            if (production%alternative_count >= standardir_max_alternatives) then
                message = 'too many alternatives in production'
                return
            end if
            production%alternative_count = production%alternative_count + 1
            production%alternatives(production%alternative_count)%item_count = 0
            production%pending = ''
            production%incomplete = .false.
        else if (trim(operator) /= 'sequence') then
            message = 'unknown production operator'
            return
        end if
        alternative = production%alternative_count
        if (len_trim(text) == 0 .and. trim(operator) == 'sequence') then
            ok = .true.
            return
        end if
        pending_length = len_trim(production%pending)
        text_length = len_trim(text)
        if (pending_length > 0) then
            if (pending_length + 1 + text_length > len(candidate)) then
                message = 'grammar RHS exceeds StandardIR buffer'
                return
            end if
            candidate = trim(production%pending)//' '//trim(text)
        else
            if (text_length > len(candidate)) then
                message = 'grammar RHS exceeds StandardIR buffer'
                return
            end if
            candidate = trim(text)
        end if
        call parse_notation(production%alternatives(alternative), candidate, ok, message)
        if (.not. ok .or. len_trim(message) > 0) then
            if (trim(message) == 'unclosed optional grammar group') then
                production%pending = candidate
                production%incomplete = .true.
                ok = .true.
            else
                return
            end if
        else
            production%pending = candidate
            production%incomplete = .false.
        end if
        call update_span(production, page, byte_start, byte_length)
    end subroutine standardir_add

    recursive subroutine parse_notation(alternative, text, ok, message)
        type(standardir_alternative_t), intent(inout) :: alternative
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_alternative_t) :: inner
        type(standardir_item_t) :: item
        character(len=1024) :: clean, inner_text, token
        integer :: n, position, close, inner_count, saved_position
        logical :: repeated, dots

        clean = adjustl(text)
        n = len_trim(clean)
        if (n == 0) then
            ok = .false.
            message = 'empty grammar notation'
            return
        end if
        alternative%item_count = 0

        ok = .false.
        message = ''
        position = 1
        do while (position <= n)
            do while (position <= n)
                if (.not. is_space(clean(position:position))) exit
                position = position + 1
            end do
            if (position > n) exit

            if (clean(position:position) == '[' .and. position == n) then
                item%kind = 6
                item%base_kind = 6
                item%name = '['
                item%minimum = 1
                item%unbounded = .false.
                call append_item(alternative, item, ok, message)
                if (.not. ok) return
                position = position + 1
            else if (clean(position:position) == '[') then
                close = matching_bracket(clean, position, n)
                if (close == 0) then
                    message = 'unclosed optional grammar group'
                    return
                end if
                inner_text = ''
                if (close > position + 1) then
                    inner_text = clean(position + 1:close - 1)
                end if
                inner%item_count = 0
                call parse_notation(inner, inner_text, ok, message)
                if (.not. ok) return
                inner_count = inner%item_count
                if (inner_count == 0) then
                    ok = .false.
                    message = 'empty optional grammar group'
                    return
                end if
                item = inner%items(1)
                if (inner_count == 1) then
                    item%kind = 2
                    if (item%base_kind == 0) item%base_kind = 1
                else
                    item%kind = 4
                    item%base_kind = 0
                    item%name = trim(inner_text)
                end if
                item%minimum = 0
                item%unbounded = .false.
                position = close + 1
                do while (position <= n)
                    if (.not. is_space(clean(position:position))) exit
                    position = position + 1
                end do
                repeated = .false.
                if (position + 2 <= n) then
                    if (clean(position:position + 2) == '...') then
                        repeated = .true.
                        position = position + 3
                    end if
                end if
                if (repeated) then
                    if (item%kind == 2) item%kind = 3
                    if (item%kind == 4) item%kind = 5
                    item%minimum = 0
                    item%unbounded = .true.
                end if
                call append_item(alternative, item, ok, message)
                if (.not. ok) return
            else if (clean(position:position) == ']') then
                if (position /= n) then
                    message = 'unexpected closing grammar group'
                    return
                end if
                item%kind = 6
                item%base_kind = 6
                item%name = ']'
                item%minimum = 1
                item%unbounded = .false.
                call append_item(alternative, item, ok, message)
                if (.not. ok) return
                position = position + 1
            else
                call read_token(clean, position, n, token)
                if (len_trim(token) == 0) then
                    message = 'empty grammar token'
                    return
                end if
                if (trim(token) == '.') then
                    saved_position = position
                    call consume_ellipsis(clean, position, n, dots)
                    if (dots) then
                        token = '...'
                    else
                        position = saved_position
                    end if
                end if
                if (trim(token) == '...') then
                    if (alternative%item_count == 0) then
                        message = 'repetition has no preceding grammar item'
                        return
                    end if
                    item = alternative%items(alternative%item_count)
                    select case (item%kind)
                    case (1, 6)
                        item%kind = 3
                        item%minimum = 1
                    case (2)
                        item%kind = 3
                        item%minimum = 0
                    case (4)
                        item%kind = 5
                        item%minimum = 0
                    case default
                        message = 'grammar item is already repeated'
                        return
                    end select
                    item%unbounded = .true.
                    alternative%items(alternative%item_count) = item
                else
                    item%kind = token_kind(token)
                    item%base_kind = item%kind
                    item%name = trim(token)
                    item%minimum = 1
                    item%unbounded = .false.
                    call append_item(alternative, item, ok, message)
                    if (.not. ok) return
                end if
            end if
        end do

        if (alternative%item_count == 0) then
            message = 'empty grammar notation'
            return
        end if
        ok = .true.
    end subroutine parse_notation

    subroutine append_item(alternative, item, ok, message)
        type(standardir_alternative_t), intent(inout) :: alternative
        type(standardir_item_t), intent(in) :: item
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (alternative%item_count >= standardir_max_items) then
            message = 'too many items in production alternative'
            return
        end if
        alternative%item_count = alternative%item_count + 1
        alternative%items(alternative%item_count) = item
        ok = .true.
    end subroutine append_item

    integer function matching_bracket(text, start, n)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start, n
        integer :: depth, position

        matching_bracket = 0
        depth = 0
        do position = start, n
            if (text(position:position) == '[') depth = depth + 1
            if (text(position:position) == ']') then
                depth = depth - 1
                if (depth == 0) then
                    matching_bracket = position
                    return
                end if
            end if
        end do
    end function matching_bracket

    subroutine read_token(text, position, n, token)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(in) :: n
        character(len=*), intent(out) :: token
        integer :: first

        token = ''
        first = position
        do while (position <= n)
            if (is_space(text(position:position))) exit
            if (text(position:position) == '[' .or. text(position:position) == ']') exit
            position = position + 1
        end do
        if (position > first) token = text(first:position - 1)
    end subroutine read_token

    subroutine consume_ellipsis(text, position, n, found)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(in) :: n
        logical, intent(out) :: found
        integer :: cursor

        found = .false.
        cursor = position
        do while (cursor <= n)
            if (.not. is_space(text(cursor:cursor))) exit
            cursor = cursor + 1
        end do
        if (cursor > n) return
        if (text(cursor:cursor) /= '.') return
        cursor = cursor + 1
        do while (cursor <= n)
            if (.not. is_space(text(cursor:cursor))) exit
            cursor = cursor + 1
        end do
        if (cursor > n) return
        if (text(cursor:cursor) /= '.') return
        cursor = cursor + 1
        position = cursor
        found = .true.
    end subroutine consume_ellipsis

    logical function is_space(character)
        character(len=1), intent(in) :: character
        is_space = character == ' ' .or. character == char(9)
    end function is_space

    integer function token_kind(token)
        character(len=*), intent(in) :: token
        character(len=1) :: first

        token_kind = 1
        if (len_trim(token) == 0) return
        first = token(1:1)
        if ((first >= 'A' .and. first <= 'Z') .or. index('(),=*+-/:?@_''"', first) > 0) then
            token_kind = 6
        end if
    end function token_kind

    subroutine update_span(production, page, byte_start, byte_length)
        type(standardir_syntax_t), intent(inout) :: production
        integer, intent(in) :: page
        integer(int64), intent(in) :: byte_start, byte_length
        production%last_page = max(production%last_page, page)
        production%last_byte = max(production%last_byte, byte_start + byte_length)
    end subroutine update_span

    subroutine standardir_emit(unit, production, source_hash, clause, ok, message)
        integer, intent(in) :: unit
        type(standardir_syntax_t), intent(in) :: production
        character(len=*), intent(in) :: source_hash, clause
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (production%incomplete) then
            message = 'production has an unclosed grammar group'
            return
        end if
        call piece(unit, '(syntax ', ok, message)
        call piece(unit, trim(production%rule)//' (lhs '//trim(production%lhs)//') (rhs ', &
            ok, message)
        if (.not. ok) return
        if (production%alternative_count == 1) then
            call emit_alternative(unit, production%alternatives(1), ok, message)
        else
            call piece(unit, '(alt ', ok, message)
            do i = 1, production%alternative_count
                call emit_alternative(unit, production%alternatives(i), ok, message)
                if (.not. ok) return
                if (i < production%alternative_count) call piece(unit, ' ', ok, message)
            end do
            call piece(unit, ')', ok, message)
        end if
        if (.not. ok) return
        call piece(unit, ')', ok, message)
        if (.not. ok) return
        call emit_source(unit, production, source_hash, clause, ok, message)
        if (.not. ok) return
        call piece(unit, ')', ok, message)
        if (.not. ok) return
        call finish_line(unit, ok, message)
    end subroutine standardir_emit

    subroutine emit_alternative(unit, alternative, ok, message)
        integer, intent(in) :: unit
        type(standardir_alternative_t), intent(in) :: alternative
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: i

        call piece(unit, '(seq', ok, message)
        do i = 1, alternative%item_count
            call piece(unit, ' ', ok, message)
            call emit_item(unit, alternative%items(i), ok, message)
            if (.not. ok) return
        end do
        call piece(unit, ')', ok, message)
    end subroutine emit_alternative

    subroutine emit_item(unit, item, ok, message)
        integer, intent(in) :: unit
        type(standardir_item_t), intent(in) :: item
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message

        character(len=32) :: minimum
        type(standardir_alternative_t) :: group
        character(len=4096) :: parse_message
        logical :: parse_ok

        select case (item%kind)
        case (1, 6)
            if (item%kind == 6) then
                call emit_named(unit, 'token', item%name, ok, message)
            else
                call emit_named(unit, 'ref', item%name, ok, message)
            end if
        case (2)
            call piece(unit, '(optional ', ok, message)
            call emit_base(unit, item, ok, message)
            call piece(unit, ')', ok, message)
        case (3)
            write (minimum, '(i0)') item%minimum
            call piece(unit, '(repeat ', ok, message)
            call emit_base(unit, item, ok, message)
            call piece(unit, ' '//trim(minimum)//' unbounded)', ok, message)
        case (4, 5)
            group%item_count = 0
            call parse_notation(group, trim(item%name), parse_ok, parse_message)
            if (.not. parse_ok) then
                ok = .false.
                message = trim(parse_message)
                return
            end if
            if (item%kind == 4) then
                call piece(unit, '(optional ', ok, message)
            else
                write (minimum, '(i0)') item%minimum
                call piece(unit, '(repeat ', ok, message)
            end if
            call emit_alternative(unit, group, ok, message)
            if (item%kind == 4) then
                call piece(unit, ')', ok, message)
            else
                call piece(unit, ' '//trim(minimum)//' unbounded)', ok, message)
            end if
        case default
            ok = .false.
            message = 'unknown StandardIR item kind'
        end select
    end subroutine emit_item

    subroutine emit_base(unit, item, ok, message)
        integer, intent(in) :: unit
        type(standardir_item_t), intent(in) :: item
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message

        if (item%base_kind == 6) then
            call emit_named(unit, 'token', item%name, ok, message)
        else
            call emit_named(unit, 'ref', item%name, ok, message)
        end if
    end subroutine emit_base

    subroutine emit_named(unit, kind, name, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: kind, name
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message

        call piece(unit, '('//trim(kind)//' ', ok, message)
        call emit_atom(unit, name, ok, message)
        call piece(unit, ')', ok, message)
    end subroutine emit_named

    subroutine emit_atom(unit, atom, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: atom
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: i, n
        logical :: quoted

        n = len_trim(atom)
        quoted = n == 0
        do i = 1, n
            if (atom(i:i) == ' ' .or. atom(i:i) == char(9) .or. &
                atom(i:i) == '(' .or. atom(i:i) == ')' .or. &
                atom(i:i) == '"' .or. atom(i:i) == char(92)) then
                quoted = .true.
            end if
        end do
        if (.not. quoted) then
            call piece(unit, trim(atom), ok, message)
            return
        end if
        call piece(unit, '"', ok, message)
        do i = 1, n
            if (atom(i:i) == '"' .or. atom(i:i) == char(92)) then
                call piece(unit, char(92), ok, message)
            end if
            call piece(unit, atom(i:i), ok, message)
            if (.not. ok) return
        end do
        call piece(unit, '"', ok, message)
    end subroutine emit_atom

    subroutine emit_source(unit, production, source_hash, clause, ok, message)
        integer, intent(in) :: unit
        type(standardir_syntax_t), intent(in) :: production
        character(len=*), intent(in) :: source_hash, clause
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        character(len=32) :: page, last_page, start, length

        write (page, '(i0)') production%first_page
        write (last_page, '(i0)') production%last_page
        write (start, '(i0)') production%first_byte
        write (length, '(i0)') production%last_byte - production%first_byte
        call piece(unit, ' (source (document J3-24-007) (clause '//trim(clause)//') (rule '// &
            trim(production%rule)//') (page '//trim(page)//') (end-page '// &
            trim(last_page)//') (byte-start '//trim(start)//') (byte-length '// &
            trim(length)//') (source-sha256 '//trim(source_hash)//'))', ok, message)
    end subroutine emit_source

    subroutine piece(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: ios

        if (.not. ok .and. len_trim(message) > 0) return
        write (unit, '(a)', advance='no', iostat=ios) text
        if (ios /= 0) then
            ok = .false.
            message = 'cannot write StandardIR'
        else
            ok = .true.
        end if
    end subroutine piece

    subroutine finish_line(unit, ok, message)
        integer, intent(in) :: unit
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        integer :: ios

        write (unit, '(a)', iostat=ios) ''
        if (ios /= 0) then
            ok = .false.
            message = 'cannot finish StandardIR record'
        end if
    end subroutine finish_line

end module standardir

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

        integer :: alternative

        ok = .false.
        message = ''
        if (trim(operator) == 'or') then
            if (production%alternative_count >= standardir_max_alternatives) then
                message = 'too many alternatives in production'
                return
            end if
            production%alternative_count = production%alternative_count + 1
        else if (trim(operator) /= 'sequence') then
            message = 'unknown production operator'
            return
        end if
        alternative = production%alternative_count
        call parse_item(production%alternatives(alternative), operator, text, ok, message)
        if (.not. ok) return
        call update_span(production, page, byte_start, byte_length)
    end subroutine standardir_add

    subroutine parse_item(alternative, operator, text, ok, message)
        type(standardir_alternative_t), intent(inout) :: alternative
        character(len=*), intent(in) :: operator, text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_item_t) :: item

        ok = .false.
        message = ''
        if (len_trim(text) == 0 .and. trim(operator) == 'sequence') then
            ok = .true.
            return
        end if
        if (trim(operator) == 'sequence' .and. len_trim(text) == 0) then
            message = 'empty sequence item'
            return
        end if
        if (alternative%item_count >= standardir_max_items) then
            message = 'too many items in production alternative'
            return
        end if
        call parse_notation(text, item, ok, message)
        if (.not. ok) return
        alternative%item_count = alternative%item_count + 1
        alternative%items(alternative%item_count) = item
    end subroutine parse_item

    subroutine parse_notation(text, item, ok, message)
        character(len=*), intent(in) :: text
        type(standardir_item_t), intent(out) :: item
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=512) :: clean, name
        integer :: n

        item%kind = 1
        item%name = ''
        item%minimum = 1
        item%unbounded = .false.
        clean = adjustl(text)
        n = len_trim(clean)
        if (n == 0) then
            ok = .false.
            message = 'empty grammar notation'
            return
        end if
        if (n >= 7) then
            if (clean(1:1) == '[' .and. clean(n - 2:n) == '...') then
                if (clean(n - 3:n - 3) == ']') then
                    name = adjustl(trim(clean(2:n - 4)))
                else if (n >= 8 .and. clean(n - 4:n - 4) == ']') then
                    name = adjustl(trim(clean(2:n - 5)))
                else
                    ok = .false.
                    message = 'malformed repeated optional notation'
                    return
                end if
                item%kind = 3
                item%minimum = 0
                item%unbounded = .true.
            else if (clean(1:1) == '[' .and. clean(n:n) == ']') then
                name = adjustl(trim(clean(2:n - 1)))
                item%kind = 2
            else
                name = trim(clean(1:n))
            end if
        else if (n >= 3) then
            if (clean(1:1) == '[' .and. clean(n:n) == ']') then
                name = adjustl(trim(clean(2:n - 1)))
                item%kind = 2
            else
                name = trim(clean(1:n))
            end if
        else
            name = trim(clean(1:n))
        end if
        if (len_trim(name) == 0) then
            ok = .false.
            message = 'empty grammar item'
            return
        end if
        item%name = trim(name)
        ok = .true.
        message = ''
    end subroutine parse_notation

    subroutine update_span(production, page, byte_start, byte_length)
        type(standardir_syntax_t), intent(inout) :: production
        integer, intent(in) :: page
        integer(int64), intent(in) :: byte_start, byte_length
        production%last_page = max(production%last_page, page)
        production%last_byte = max(production%last_byte, byte_start + byte_length)
    end subroutine update_span

    subroutine standardir_emit(unit, production, source_hash, ok, message)
        integer, intent(in) :: unit
        type(standardir_syntax_t), intent(in) :: production
        character(len=*), intent(in) :: source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
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
        call emit_source(unit, production, source_hash, ok, message)
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

        select case (item%kind)
        case (1)
            call piece(unit, '(ref '//trim(item%name)//')', ok, message)
        case (2)
            call piece(unit, '(optional (ref '//trim(item%name)//'))', ok, message)
        case (3)
            write (minimum, '(i0)') item%minimum
            call piece(unit, '(repeat (ref '//trim(item%name)//') '//trim(minimum)// &
                ' unbounded)', ok, message)
        case default
            ok = .false.
            message = 'unknown StandardIR item kind'
        end select
    end subroutine emit_item

    subroutine emit_source(unit, production, source_hash, ok, message)
        integer, intent(in) :: unit
        type(standardir_syntax_t), intent(in) :: production
        character(len=*), intent(in) :: source_hash
        logical, intent(inout) :: ok
        character(len=*), intent(inout) :: message
        character(len=32) :: page, last_page, start, length

        write (page, '(i0)') production%first_page
        write (last_page, '(i0)') production%last_page
        write (start, '(i0)') production%first_byte
        write (length, '(i0)') production%last_byte - production%first_byte
        call piece(unit, ' (source (document J3-24-007) (clause 5) (rule '// &
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

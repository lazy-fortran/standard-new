module standardir_lexical_layout
    !! Source-backed, target-neutral projection of lexical layout facts.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    integer, parameter, public :: standardir_layout_max_records = 256
    integer, parameter, public :: standardir_layout_max_text = 256

    type, public :: standardir_layout_source_t
        character(len=standardir_layout_max_text) :: document = ''
        character(len=standardir_layout_max_text) :: clause = ''
        character(len=standardir_layout_max_text) :: rule = ''
        integer :: page = 0
        character(len=64) :: source_hash = ''
    end type standardir_layout_source_t

    type, public :: standardir_layout_record_t
        character(len=32) :: kind = ''
        character(len=32) :: source_form = ''
        character(len=32) :: terminator = ''
        character(len=32) :: signal = ''
        character(len=32) :: policy = ''
        type(standardir_layout_source_t) :: source
        character(len=32) :: origin = ''
    end type standardir_layout_record_t

    type, public :: standardir_layout_t
        integer :: count = 0
        type(standardir_layout_record_t) :: records(standardir_layout_max_records)
    end type standardir_layout_t

    public :: standardir_layout_add
    public :: standardir_layout_reset
    public :: standardir_layout_validate
    public :: standardir_layout_write

contains

    subroutine standardir_layout_reset(layout)
        type(standardir_layout_t), intent(out) :: layout
        layout%count = 0
    end subroutine standardir_layout_reset

    subroutine standardir_layout_add(node, layout, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_layout_t), intent(inout) :: layout
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        type(standardir_layout_record_t) :: record
        integer :: i

        ok = .false.; message = ''
        call read_record(node, record, ok, message)
        if (.not. ok) return
        do i = 1, layout%count
            if (same_fact(layout%records(i), record)) then
                message = 'duplicate lexical layout fact'
                return
            end if
        end do
        if (layout%count >= size(layout%records)) then
            message = 'too many lexical layout facts'
            return
        end if
        layout%count = layout%count + 1
        layout%records(layout%count) = record
        ok = .true.
    end subroutine standardir_layout_add

    subroutine standardir_layout_validate(layout, ok, message)
        type(standardir_layout_t), intent(in) :: layout
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j

        ok = .false.; message = ''
        if (layout%count < 0 .or. layout%count > size(layout%records)) then
            message = 'lexical layout count is outside storage'; return
        end if
        do i = 1, layout%count
            call validate_record(layout%records(i), ok, message)
            if (.not. ok) return
            do j = 1, i - 1
                if (same_fact(layout%records(i), layout%records(j))) then
                    message = 'duplicate lexical layout fact'; return
                end if
            end do
        end do
        ok = .true.
    end subroutine standardir_layout_validate

    subroutine standardir_layout_write(layout, unit, ok, message)
        type(standardir_layout_t), intent(in) :: layout
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, ios

        call standardir_layout_validate(layout, ok, message)
        if (.not. ok) return
        write (unit, '(a)', iostat=ios) '{"kind":"lexical-layout-header","format":1}'
        ok = ios == 0
        do i = 1, layout%count
            if (.not. ok) exit
            call write_record(layout%records(i), unit, ok)
        end do
        if (.not. ok) message = 'could not write lexical layout output'
    end subroutine standardir_layout_write

    subroutine read_record(node, record, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_layout_record_t), intent(out) :: record
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: labels(7)
        integer :: i, j, slot, label_count
        character(len=standardir_layout_max_text) :: label, value

        record = standardir_layout_record_t(); labels = ''; label_count = 0
        ok = node%kind == sx_list .and. node%child_count >= 2
        if (.not. ok) then; message = 'lexical layout record is malformed'; return; end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'lexical layout record has no kind'; return
        end if
        record%kind = trim(node%children(1)%atom)
        if (.not. valid_kind(record%kind)) then
            message = 'unknown lexical layout record kind'; return
        end if
        do i = 2, node%child_count
            if (node%children(i)%kind /= sx_list .or. node%children(i)%child_count /= 2) then
                message = 'lexical layout field is malformed'; return
            end if
            if (node%children(i)%children(1)%kind /= sx_atom .or. &
                (trim(node%children(i)%children(1)%atom) /= 'source' .and. &
                node%children(i)%children(2)%kind /= sx_atom)) then
                message = 'lexical layout field is not an atom pair'; return
            end if
            label = trim(node%children(i)%children(1)%atom)
            value = ''
            if (node%children(i)%children(2)%kind == sx_atom) then
                value = trim(node%children(i)%children(2)%atom)
            end if
            call field_index(record%kind, label, slot, ok)
            if (.not. ok) then; message = 'unknown lexical layout field'; return; end if
            do j = 2, i - 1
                if (trim(node%children(j)%children(1)%atom) == label) then
                    message = 'duplicate lexical layout field'; return
                end if
            end do
            label_count = label_count + 1
            labels(label_count) = label
            select case (label)
            case ('source-form'); record%source_form = value
            case ('terminator'); record%terminator = value
            case ('signal'); record%signal = value
            case ('policy'); record%policy = value
            case ('origin'); record%origin = value
            case ('source')
                if (node%children(i)%children(2)%kind /= sx_list) then
                    message = 'source must be a source-ref record'; return
                end if
            end select
        end do
        do i = 2, node%child_count
            if (node%children(i)%children(1)%kind == sx_atom .and. &
                trim(node%children(i)%children(1)%atom) == 'source') then
                call read_source(node%children(i)%children(2), record%source, ok, message)
                if (.not. ok) return
            end if
        end do
        call validate_record(record, ok, message)
    end subroutine read_record

    subroutine read_source(node, source, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_layout_source_t), intent(out) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        logical :: seen(5)
        integer :: i, value, ios
        character(len=64) :: label, text

        source = standardir_layout_source_t(); seen = .false.; ok = .false.
        if (node%kind /= sx_list .or. node%child_count /= 6) then
            message = 'source-ref has the wrong shape'; return
        end if
        if (node%children(1)%kind /= sx_atom .or. trim(node%children(1)%atom) /= 'source-ref') then
            message = 'source field is not a source-ref'; return
        end if
        do i = 2, 6
            if (node%children(i)%kind /= sx_list .or. node%children(i)%child_count /= 2 .or. &
                node%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(i)%children(2)%kind /= sx_atom) then
                message = 'source-ref field is malformed'; return
            end if
            label = trim(node%children(i)%children(1)%atom)
            text = trim(node%children(i)%children(2)%atom)
            select case (label)
            case ('document'); value = 1
            case ('clause'); value = 2
            case ('rule'); value = 3
            case ('page'); value = 4
            case ('source-hash'); value = 5
            case default; message = 'unknown source-ref field'; return
            end select
            if (seen(value)) then; message = 'duplicate source-ref field'; return; end if
            seen(value) = .true.
            select case (value)
            case (1); source%document = text
            case (2); source%clause = text
            case (3); source%rule = text
            case (4); read (text, *, iostat=ios) source%page
            case (5); source%source_hash = text
            end select
            if (value == 4 .and. ios /= 0) then; message = 'source-ref page is not an integer'; return; end if
        end do
        ok = .true.
    end subroutine read_source

    subroutine validate_record(record, ok, message)
        type(standardir_layout_record_t), intent(in) :: record
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        ok = .false.; message = ''
        if (.not. valid_kind(record%kind)) then; message = 'unknown lexical layout record kind'; return; end if
        if (.not. valid_enum(record%source_form, 'source-form')) then; message = 'invalid source-form'; return; end if
        if (.not. valid_origin(record%origin)) then; message = 'invalid lexical layout origin'; return; end if
        if (.not. valid_source(record%source)) then; message = 'invalid lexical layout provenance'; return; end if
        select case (trim(record%kind))
        case ('statement-boundary')
            if (.not. valid_enum(record%terminator, 'terminator')) then; message = 'invalid terminator'; return; end if
        case ('continuation')
            if (.not. valid_enum(record%signal, 'continuation-signal')) then; message = 'invalid continuation signal'; return; end if
        case ('keyword-name-policy')
            if (.not. valid_enum(record%policy, 'keyword-policy')) then; message = 'invalid keyword policy'; return; end if
        end select
        ok = .true.
    end subroutine validate_record

    logical function valid_source(source)
        type(standardir_layout_source_t), intent(in) :: source
        integer :: i, code
        valid_source = len_trim(source%document) > 0 .and. len_trim(source%clause) > 0 .and. &
            len_trim(source%rule) > 0 .and. source%page > 0 .and. len_trim(source%source_hash) == 64
        if (.not. valid_source) return
        do i = 1, 64
            code = iachar(source%source_hash(i:i))
            if (.not. ((code >= iachar('0') .and. code <= iachar('9')) .or. &
                (code >= iachar('a') .and. code <= iachar('f')) .or. &
                (code >= iachar('A') .and. code <= iachar('F')))) valid_source = .false.
        end do
    end function valid_source

    logical function valid_kind(value)
        character(len=*), intent(in) :: value
        valid_kind = trim(value) == 'statement-boundary' .or. trim(value) == 'continuation' .or. &
            trim(value) == 'keyword-name-policy'
    end function valid_kind

    logical function valid_origin(value)
        character(len=*), intent(in) :: value
        valid_origin = any([character(len=32) :: 'mechanical','search','smt','llm','llm-repair','human', &
            'imported','differential'] == trim(value))
    end function valid_origin

    logical function valid_enum(value, name)
        character(len=*), intent(in) :: value, name
        select case (trim(name))
        case ('source-form'); valid_enum = trim(value) == 'free-form' .or. trim(value) == 'fixed-form'
        case ('terminator'); valid_enum = trim(value) == 'end-of-line' .or. trim(value) == 'semicolon' .or. trim(value) == 'comment'
        case ('continuation-signal'); valid_enum = trim(value) == 'trailing-ampersand' .or. trim(value) == 'leading-ampersand' .or. trim(value) == 'fixed-form-marker'
        case ('keyword-policy'); valid_enum = trim(value) == 'not-reserved'
        case default; valid_enum = .false.
        end select
    end function valid_enum

    subroutine field_index(kind, label, index, ok)
        character(len=*), intent(in) :: kind, label
        integer, intent(out) :: index
        logical, intent(out) :: ok
        index = 0; ok = .true.
        select case (trim(label))
        case ('source-form'); index = 1
        case ('terminator'); index = 2
        case ('signal'); index = 3
        case ('policy'); index = 4
        case ('source'); index = 5
        case ('origin'); index = 6
        case default; ok = .false.
        end select
        if (trim(kind) == 'statement-boundary' .and. index == 3) ok = .false.
        if (trim(kind) == 'statement-boundary' .and. index == 4) ok = .false.
        if (trim(kind) == 'continuation' .and. index == 2) ok = .false.
        if (trim(kind) == 'continuation' .and. index == 4) ok = .false.
        if (trim(kind) == 'keyword-name-policy' .and. index == 2) ok = .false.
        if (trim(kind) == 'keyword-name-policy' .and. index == 3) ok = .false.
    end subroutine field_index

    logical function same_fact(a, b)
        type(standardir_layout_record_t), intent(in) :: a, b
        same_fact = .false.
        if (trim(a%kind) /= trim(b%kind)) return
        if (trim(a%source_form) /= trim(b%source_form)) return
        if (trim(a%terminator) /= trim(b%terminator)) return
        if (trim(a%signal) /= trim(b%signal)) return
        if (trim(a%policy) /= trim(b%policy)) return
        if (trim(a%source%document) /= trim(b%source%document)) return
        if (trim(a%source%clause) /= trim(b%source%clause)) return
        if (trim(a%source%rule) /= trim(b%source%rule)) return
        if (a%source%page /= b%source%page) return
        if (trim(a%source%source_hash) /= trim(b%source%source_hash)) return
        if (trim(a%origin) /= trim(b%origin)) return
        same_fact = .true.
    end function same_fact

    subroutine write_record(record, unit, ok)
        type(standardir_layout_record_t), intent(in) :: record
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        integer :: ios
        character(len=2048) :: line
        select case (trim(record%kind))
        case ('statement-boundary')
            write (line, '(a)') '{"kind":"statement-boundary","source_form":"'//json_escape(record%source_form)//'","terminator":"'//json_escape(record%terminator)//'","source":{"document":"'//json_escape(record%source%document)//'","clause":"'//json_escape(record%source%clause)//'","rule":"'//json_escape(record%source%rule)//'","page":'//trim(itoa(record%source%page))//',"source_hash":"'//json_escape(record%source%source_hash)//'"},"origin":"'//json_escape(record%origin)//'"}'
        case ('continuation')
            write (line, '(a)') '{"kind":"continuation","source_form":"'//json_escape(record%source_form)//'","signal":"'//json_escape(record%signal)//'","source":{"document":"'//json_escape(record%source%document)//'","clause":"'//json_escape(record%source%clause)//'","rule":"'//json_escape(record%source%rule)//'","page":'//trim(itoa(record%source%page))//',"source_hash":"'//json_escape(record%source%source_hash)//'"},"origin":"'//json_escape(record%origin)//'"}'
        case ('keyword-name-policy')
            write (line, '(a)') '{"kind":"keyword-name-policy","source_form":"'//json_escape(record%source_form)//'","policy":"'//json_escape(record%policy)//'","source":{"document":"'//json_escape(record%source%document)//'","clause":"'//json_escape(record%source%clause)//'","rule":"'//json_escape(record%source%rule)//'","page":'//trim(itoa(record%source%page))//',"source_hash":"'//json_escape(record%source%source_hash)//'"},"origin":"'//json_escape(record%origin)//'"}'
        end select
        write (unit, '(a)', iostat=ios) trim(line)
        ok = ios == 0
    end subroutine write_record

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(i0)') value
    end function itoa

    function json_escape(value) result(text)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=standardir_layout_max_text * 2) :: escaped
        integer :: i, n, output
        escaped = ''; output = 0; n = len_trim(value)
        do i = 1, n
            select case (value(i:i))
            case ('"', '\')
                output = output + 1; escaped(output:output) = '\'
            case (achar(8)); output = output + 2; escaped(output-1:output) = '\b'
            case (achar(9)); output = output + 2; escaped(output-1:output) = '\t'
            case (achar(10)); output = output + 2; escaped(output-1:output) = '\n'
            case (achar(13)); output = output + 2; escaped(output-1:output) = '\r'
            case default; output = output + 1; escaped(output:output) = value(i:i)
            end select
            if (output >= len(escaped)) exit
        end do
        text = escaped(:output)
    end function json_escape

end module standardir_lexical_layout

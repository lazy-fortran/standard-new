module production_json
    !! Read the lossless production JSONL interchange records.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    type, public :: production_record_t
        character(len=32) :: kind = ''
        character(len=32) :: rule = ''
        character(len=256) :: lhs = ''
        character(len=32) :: operator = ''
        character(len=16384) :: text = ''
        character(len=4096) :: source_line = ''
        integer :: page = 0
        integer(int64) :: byte_start = 0
        integer(int64) :: byte_length = 0
        integer :: occurrence = 0
        logical :: has_source_line = .false.
        logical :: has_occurrence = .false.
    end type production_record_t

    public :: production_json_parse

contains

    subroutine production_json_parse(line, record, found, ok, message)
        character(len=*), intent(in) :: line
        type(production_record_t), intent(out) :: record
        logical, intent(out) :: found, ok
        character(len=*), intent(out) :: message
        character(len=256) :: raw
        integer :: read_status
        logical :: present

        record = production_record_t()
        found = .false.
        ok = .false.
        message = ''
        call json_field(line, 'kind', record%kind, present)
        if (.not. present) then
            found = .false.
            ok = .true.
            return
        end if
        found = .true.
        if (trim(record%kind) /= 'production-start' .and. &
            trim(record%kind) /= 'production-continuation') then
            ok = .true.
            return
        end if
        call required_string(line, 'rule', record%rule, ok, message)
        if (.not. ok) return
        call required_string(line, 'lhs', record%lhs, ok, message)
        if (.not. ok) return
        call required_string(line, 'operator', record%operator, ok, message)
        if (.not. ok) return
        call required_string(line, 'text', record%text, ok, message)
        if (.not. ok) return
        call required_integer(line, 'page', record%page, ok, message)
        if (.not. ok) return
        call required_int64(line, 'byte_start', record%byte_start, ok, message)
        if (.not. ok) return
        call required_int64(line, 'byte_length', record%byte_length, ok, message)
        if (.not. ok) return
        call json_field(line, 'occurrence', raw, record%has_occurrence)
        if (record%has_occurrence) then
            read (raw, *, iostat=read_status) record%occurrence
            if (read_status /= 0) then
                ok = .false.
                message = 'invalid JSON integer field occurrence'
                return
            end if
        end if
        call json_field(line, 'source_line', record%source_line, record%has_source_line)
        ok = .true.
    end subroutine production_json_parse

    subroutine required_string(line, key, value, ok, message)
        character(len=*), intent(in) :: line, key
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        logical :: found

        call json_field(line, key, value, found)
        ok = found .and. len_trim(value) > 0
        message = ''
        if (.not. ok) message = 'missing JSON string field '//trim(key)
    end subroutine required_string

    subroutine required_integer(line, key, value, ok, message)
        character(len=*), intent(in) :: line, key
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: raw
        integer :: read_status

        call json_field(line, key, raw, ok)
        if (.not. ok) then
            message = 'missing JSON integer field '//trim(key)
            return
        end if
        read (raw, *, iostat=read_status) value
        ok = read_status == 0
        message = ''
        if (.not. ok) message = 'invalid JSON integer field '//trim(key)
    end subroutine required_integer

    subroutine required_int64(line, key, value, ok, message)
        character(len=*), intent(in) :: line, key
        integer(int64), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=256) :: raw
        integer :: read_status

        call json_field(line, key, raw, ok)
        if (.not. ok) then
            message = 'missing JSON integer field '//trim(key)
            return
        end if
        read (raw, *, iostat=read_status) value
        ok = read_status == 0
        message = ''
        if (.not. ok) message = 'invalid JSON integer field '//trim(key)
    end subroutine required_int64

    subroutine json_field(line, key, value, found)
        character(len=*), intent(in) :: line, key
        character(len=*), intent(out) :: value
        logical, intent(out) :: found
        character(len=256) :: needle
        character(len=16384) :: raw
        integer :: begin, finish, n, position, raw_length, slash_count
        logical :: escaped, decoded

        value = ''
        found = .false.
        needle = '"'//trim(key)//'":'
        position = index(line, trim(needle))
        if (position == 0) return
        begin = position + len_trim(needle)
        n = len_trim(line)
        if (begin > n) return
        if (line(begin:begin) == '"') then
            begin = begin + 1
            finish = begin
            do while (finish <= n)
                if (line(finish:finish) == '"') then
                    slash_count = 0
                    position = finish - 1
                    do while (position >= begin)
                        if (line(position:position) /= '\') exit
                        slash_count = slash_count + 1
                        position = position - 1
                    end do
                    escaped = mod(slash_count, 2) == 1
                    if (.not. escaped) exit
                end if
                finish = finish + 1
            end do
            if (finish > n .or. finish - begin > len(value)) return
            raw = ''
            raw_length = finish - begin
            if (raw_length > 0) raw(1:raw_length) = line(begin:finish - 1)
            if (raw_length > 0) then
                call unescape_json(raw(1:raw_length), value, decoded)
            else
                call unescape_json('', value, decoded)
            end if
            if (.not. decoded) return
        else
            finish = begin
            do while (finish <= n)
                if (line(finish:finish) == ',' .or. line(finish:finish) == '}') exit
                finish = finish + 1
            end do
            if (finish - begin > len(value)) return
            if (finish > begin) value(1:finish - begin) = line(begin:finish - 1)
        end if
        found = .true.
    end subroutine json_field

    subroutine unescape_json(raw, value, ok)
        character(len=*), intent(in) :: raw
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        integer :: i, n, output
        character(len=1) :: ch

        value = ''
        ok = .false.
        n = len(raw)
        output = 0
        i = 1
        do while (i <= n)
            if (raw(i:i) == '\') then
                i = i + 1
                if (i > n) return
                select case (raw(i:i))
                case ('"', '\', '/')
                    ch = raw(i:i)
                case ('b')
                    ch = achar(8)
                case ('f')
                    ch = achar(12)
                case ('n')
                    ch = achar(10)
                case ('r')
                    ch = achar(13)
                case ('t')
                    ch = achar(9)
                case default
                    return
                end select
            else
                ch = raw(i:i)
            end if
            if (output >= len(value)) return
            output = output + 1
            value(output:output) = ch
            i = i + 1
        end do
        ok = .true.
    end subroutine unescape_json

end module production_json

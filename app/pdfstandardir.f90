program pdfstandardir
    !! Convert lossless production JSONL into canonical StandardIR SX.
    !!
    !! JSONL is an interchange artifact. This executable assembles its
    !! alternatives and sequences, parses the small clause-5 notation subset,
    !! and writes one provenance-bearing syntax object per production.

    use, intrinsic :: iso_fortran_env, only: int64
    use standardir, only: standardir_syntax_t, standardir_start, standardir_add, &
        standardir_emit
    implicit none

    character(len=4096) :: input_path, output_path, source_hash
    character(len=64) :: clause
    character(len=16384) :: line
    character(len=256) :: value, kind, rule, lhs, operator, text, message
    type(standardir_syntax_t) :: production
    integer(int64) :: byte_start, byte_length
    integer :: argc, input_unit, output_unit, ios, page, records
    logical :: found, active, ok

    argc = command_argument_count()
    if (argc /= 4) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)// &
            ' <productions.jsonl> <output.sx> <source-sha256> <clause>'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)
    call get_command_argument(3, source_hash)
    call get_command_argument(4, clause)

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open production JSONL'
        stop 1
    end if
    open (newunit=output_unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (input_unit)
        print '(a)', 'error: cannot open StandardIR output'
        stop 1
    end if

    write (output_unit, '(a)') '(standardir (format 1) (origin MECHANICAL) '// &
        '(source (document J3-24-007) (clause '//trim(clause)//') (source-sha256 '// &
        trim(source_hash)//')))'
    active = .false.
    records = 0

    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        call json_field(line, 'kind', kind, found)
        if (.not. found) cycle
        if (trim(kind) == 'production-start') then
            if (active) then
                call standardir_emit(output_unit, production, source_hash, clause, ok, message)
                if (.not. ok) call fail_output(input_unit, output_unit, message)
                records = records + 1
            end if
            call required_string(line, 'rule', rule, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_string(line, 'lhs', lhs, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_string(line, 'text', text, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_integer(line, 'page', page, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_int64(line, 'byte_start', byte_start, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_int64(line, 'byte_length', byte_length, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call standardir_start(production, rule, lhs, page, byte_start, &
                byte_length, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call standardir_add(production, 'sequence', text, page, byte_start, &
                byte_length, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            active = .true.
        else if (trim(kind) == 'production-continuation' .and. active) then
            call required_string(line, 'operator', operator, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_string(line, 'text', text, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_integer(line, 'page', page, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_int64(line, 'byte_start', byte_start, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call required_int64(line, 'byte_length', byte_length, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
            call standardir_add(production, operator, text, page, byte_start, &
                byte_length, ok, message)
            if (.not. ok) call fail_input(input_unit, output_unit, message)
        end if
    end do

    if (active) then
        call standardir_emit(output_unit, production, source_hash, clause, ok, message)
        if (.not. ok) call fail_output(input_unit, output_unit, message)
        records = records + 1
    end if
    close (input_unit)
    close (output_unit)
    print '(a,i0,a)', 'wrote ', records, ' StandardIR syntax objects'

contains

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
        integer :: begin, finish, n, position, slash_count
        character(len=256) :: raw
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
            if (finish > n) return
            if (finish - begin > len(value)) return
            raw = ''
            if (finish > begin) raw(1:finish - begin) = line(begin:finish - 1)
            call unescape_json(raw, value, decoded)
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
        n = len_trim(raw)
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

    subroutine fail_input(input_unit, output_unit, message)
        integer, intent(in) :: input_unit, output_unit
        character(len=*), intent(in) :: message
        close (input_unit)
        close (output_unit, status='delete')
        print '(a)', 'error: '//trim(message)
        stop 1
    end subroutine fail_input

    subroutine fail_output(input_unit, output_unit, message)
        integer, intent(in) :: input_unit, output_unit
        character(len=*), intent(in) :: message
        close (input_unit)
        close (output_unit, status='delete')
        print '(a)', 'error: '//trim(message)
        stop 1
    end subroutine fail_output

end program pdfstandardir

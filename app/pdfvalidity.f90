program pdfvalidity
    !! Independently audit production spans against canonical source bytes.

    use, intrinsic :: iso_fortran_env, only: int64
    use production_json, only: production_json_parse, production_record_t
    implicit none

    character(len=4096) :: canonical_path, production_path, standardir_path, report_path
    character(len=65536) :: line, source_bytes
    character(len=256) :: message
    type(production_record_t) :: record
    integer(int64) :: source_size, previous_end
    integer(int64) :: starts(10000), lengths(10000)
    integer :: occurrences(10000)
    integer :: argc, canonical_unit, production_unit, standardir_unit, report_unit
    integer :: ios, line_number, start_count, continuation_count, record_count
    integer :: syntax_count, errors, source_mismatches, occurrence_errors
    integer :: order_errors, duplicate_errors, active_occurrence, previous_occurrence
    integer :: i, j, span_length
    logical :: found, ok, active, report_ok

    argc = command_argument_count()
    if (argc /= 4) then
        call get_command_argument(0, report_path)
        print '(a)', 'usage: '//trim(report_path)// &
            ' <canonical.text> <productions.jsonl> <standardir.sx> <report.txt>'
        stop 2
    end if
    call get_command_argument(1, canonical_path)
    call get_command_argument(2, production_path)
    call get_command_argument(3, standardir_path)
    call get_command_argument(4, report_path)

    inquire (file=trim(canonical_path), size=source_size, iostat=ios)
    if (ios /= 0) call fail('cannot stat canonical text')
    open (newunit=canonical_unit, file=trim(canonical_path), access='stream', &
        form='unformatted', action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open canonical text')
    open (newunit=production_unit, file=trim(production_path), action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open production JSONL')
    open (newunit=standardir_unit, file=trim(standardir_path), action='read', iostat=ios)
    if (ios /= 0) call fail('cannot open StandardIR SX')
    open (newunit=report_unit, file=trim(report_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open audit report')

    record_count = 0
    start_count = 0
    continuation_count = 0
    syntax_count = 0
    errors = 0
    source_mismatches = 0
    occurrence_errors = 0
    order_errors = 0
    duplicate_errors = 0
    previous_end = -1_int64
    previous_occurrence = 0
    active_occurrence = 0
    active = .false.
    line_number = 0

    do
        read (production_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        line_number = line_number + 1
        call production_json_parse(line, record, found, ok, message)
        if (.not. ok) then
            errors = errors + 1
            cycle
        end if
        if (.not. found) cycle
        if (trim(record%kind) /= 'production-start' .and. &
            trim(record%kind) /= 'production-continuation') cycle
        record_count = record_count + 1
        if (.not. record%has_occurrence .or. .not. record%has_source_line) then
            errors = errors + 1
        end if
        if (record%byte_start < 0_int64 .or. record%byte_length < 0_int64 .or. &
            record%byte_start + record%byte_length > source_size) then
            errors = errors + 1
        else
            if (record%byte_length > int(len(source_bytes), int64)) then
                errors = errors + 1
            else if (record%byte_length > 0_int64) then
                span_length = int(record%byte_length)
                read (canonical_unit, pos=record%byte_start + 1, iostat=ios) &
                    source_bytes(1:span_length)
                if (ios /= 0) then
                    errors = errors + 1
                else if (.not. record%has_source_line .or. &
                        source_bytes(1:span_length) /= record%source_line(1:span_length)) then
                    source_mismatches = source_mismatches + 1
                end if
            end if
            if (record%byte_start < previous_end) order_errors = order_errors + 1
            previous_end = record%byte_start + record%byte_length
        end if

        if (trim(record%kind) == 'production-start') then
            start_count = start_count + 1
            active = .true.
            active_occurrence = record%occurrence
            if (record%occurrence <= previous_occurrence) occurrence_errors = occurrence_errors + 1
            previous_occurrence = record%occurrence
            if (start_count <= size(starts)) then
                starts(start_count) = record%byte_start
                lengths(start_count) = record%byte_length
                occurrences(start_count) = record%occurrence
            else
                errors = errors + 1
            end if
        else
            continuation_count = continuation_count + 1
            if (.not. active .or. record%occurrence /= active_occurrence) then
                occurrence_errors = occurrence_errors + 1
            end if
        end if
    end do
    close (production_unit)

    do i = 1, min(start_count, size(starts))
        do j = 1, i - 1
            if (starts(i) == starts(j) .and. lengths(i) == lengths(j)) then
                duplicate_errors = duplicate_errors + 1
            end if
        end do
    end do

    do
        read (standardir_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (index(adjustl(line), '(syntax ') == 1) syntax_count = syntax_count + 1
    end do
    close (standardir_unit)
    close (canonical_unit)

    write (report_unit, '(a)') 'E0147 source-backed StandardIR validity audit'
    write (report_unit, '(a,i0)') 'production-records: ', record_count
    write (report_unit, '(a,i0)') 'production-starts: ', start_count
    write (report_unit, '(a,i0)') 'production-continuations: ', continuation_count
    write (report_unit, '(a,i0)') 'standardir-syntax-objects: ', syntax_count
    write (report_unit, '(a,i0)') 'source-span-mismatches: ', source_mismatches
    write (report_unit, '(a,i0)') 'occurrence-errors: ', occurrence_errors
    write (report_unit, '(a,i0)') 'ordering-errors: ', order_errors
    write (report_unit, '(a,i0)') 'duplicate-spans: ', duplicate_errors
    if (start_count /= syntax_count) errors = errors + 1
    errors = errors + source_mismatches + occurrence_errors + order_errors + duplicate_errors
    report_ok = errors == 0 .and. start_count > 0
    if (report_ok) then
        write (report_unit, '(a)') 'status: PASS'
    else
        write (report_unit, '(a)') 'status: FAIL'
    end if
    close (report_unit)
    if (.not. report_ok) stop 1
    print '(a,i0,a)', 'validity audit passed for ', start_count, ' production starts'

contains

    subroutine fail(reason)
        character(len=*), intent(in) :: reason
        print '(a)', 'error: '//trim(reason)
        error stop 1
    end subroutine fail

end program pdfvalidity

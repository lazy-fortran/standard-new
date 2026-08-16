program test_pdfstandardir
    !! Fixed CLI outputs are the behavioral oracle for both projection modes.

    implicit none

    character(len=*), parameter :: input_path = 'build/test_pdfstandardir.jsonl'
    character(len=*), parameter :: legacy_path = 'build/test_pdfstandardir_legacy.sx'
    character(len=*), parameter :: records_path = 'build/test_pdfstandardir_records.sx'
    character(len=*), parameter :: hash = 'fixture-hash'
    character(len=*), parameter :: legacy_expected = &
        '(standardir (format 1) (origin MECHANICAL) (source (document J3-24-007) '// &
        '(clause 5.1) (source-sha256 fixture-hash)))'
    character(len=*), parameter :: first_expected = &
        '(syntax R501 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (occurrence-clause 5.1) (rule R501) '// &
        '(page 45) (end-page 45) (byte-start 100) (byte-length 20) '// &
        '(source-sha256 fixture-hash) (occurrence 1)))'
    character(len=*), parameter :: repeated_expected = &
        '(syntax R501 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (occurrence-clause 5.1) (rule R501) '// &
        '(page 46) (end-page 46) (byte-start 300) (byte-length 30) '// &
        '(source-sha256 fixture-hash) (occurrence 2)))'
    character(len=*), parameter :: assumed_expected = &
        '(syntax R1 (lhs assumed) (rhs (seq (token ASSUMED))) (source '// &
        '(document J3-24-007) (clause 5.1) (rule R1) (page 47) '// &
        '(end-page 47) (byte-start 500) (byte-length 10) '// &
        '(source-sha256 fixture-hash)))'
    character(len=*), parameter :: first_record_expected = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document caller-document) (clause 5) (rule R501) (page 45) '// &
        '(source-hash caller-hash) (occurrence-clause 5.1) (end-page 45) '// &
        '(byte-start 100) (byte-length 20) (occurrence 1))) '// &
        '(origin human) (resolution resolved))'
    character(len=*), parameter :: repeated_record_expected = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document caller-document) (clause 5) (rule R501) (page 46) '// &
        '(source-hash caller-hash) (occurrence-clause 5.1) (end-page 46) '// &
        '(byte-start 300) (byte-length 30) (occurrence 2))) '// &
        '(origin human) (resolution resolved))'
    character(len=*), parameter :: assumed_record_expected = &
        '(syntax-item (id R1) (lhs assumed) (source (source-ref '// &
        '(document caller-document) (clause 5.1) (rule R1) (page 47) '// &
        '(source-hash caller-hash) (end-page 47) (byte-start 500) '// &
        '(byte-length 10))) (origin human) (resolution resolved))'
    character(len=4096) :: command, actual
    integer :: unit, ios, exit_status

    open (newunit=unit, file=input_path, status='replace', action='write', iostat=ios)
    call require(ios == 0, 'could not write JSONL fixture')
    write (unit, '(a)') '{"kind":"production-start","rule":"R501",'// &
        '"lhs":"program","text":"PROGRAM","page":45,'// &
        '"byte_start":100,"byte_length":20,"occurrence":1}'
    write (unit, '(a)') '{"kind":"production-start","rule":"R501",'// &
        '"lhs":"program","text":"PROGRAM","page":46,'// &
        '"byte_start":300,"byte_length":30,"occurrence":2}'
    write (unit, '(a)') '{"kind":"production-start","rule":"R1",'// &
        '"lhs":"assumed","text":"ASSUMED","page":47,'// &
        '"byte_start":500,"byte_length":10}'
    close (unit)

    command = 'fo exec --no-build pdfstandardir '//input_path//' '//legacy_path//' '//hash//' 5.1'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'legacy projection command failed')
    call read_line(legacy_path, 1, actual)
    call require(trim(actual) == legacy_expected, 'legacy header changed')
    call read_line(legacy_path, 2, actual)
    call require(trim(actual) == first_expected, 'first occurrence output differs')
    call read_line(legacy_path, 3, actual)
    call require(trim(actual) == repeated_expected, 'repeated occurrence output differs')
    call read_line(legacy_path, 4, actual)
    call require(trim(actual) == assumed_expected, 'assumed syntax output differs')

    command = 'fo exec --no-build pdfstandardir '//input_path//' '//records_path//' '// &
        'caller-hash 5.1 --syntax-items caller-document human resolved'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'source-backed projection command failed')
    call read_line(records_path, 1, actual)
    call require(trim(actual) == first_record_expected, 'first source-backed record differs')
    call read_line(records_path, 2, actual)
    call require(trim(actual) == repeated_record_expected, 'repeated source-backed record differs')
    call read_line(records_path, 3, actual)
    call require(trim(actual) == assumed_record_expected, 'assumed source-backed record differs')

    open (newunit=unit, file=input_path, status='replace', action='write', iostat=ios)
    call require(ios == 0, 'could not write negative JSONL fixture')
    write (unit, '(a)') '{"kind":"production-start","rule":"R501",'// &
        '"lhs":"program","text":"PROGRAM","page":45,'// &
        '"byte_start":100,"byte_length":20,"occurrence":-1}'
    close (unit)
    command = 'fo exec --no-build pdfstandardir '//input_path//' '//legacy_path//' '//hash//' 5.1'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status /= 0, 'negative occurrence was accepted')

    print '(a)', 'pdfstandardir projection test passed'

contains

    subroutine read_line(path, line_number, value)
        character(len=*), intent(in) :: path
        integer, intent(in) :: line_number
        character(len=*), intent(out) :: value
        integer :: line

        open (newunit=unit, file=path, action='read', iostat=ios)
        call require(ios == 0, 'could not read projection output')
        do line = 1, line_number
            read (unit, '(a)', iostat=ios) value
        end do
        close (unit)
        call require(ios == 0, 'projection output is empty')
    end subroutine read_line

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_pdfstandardir

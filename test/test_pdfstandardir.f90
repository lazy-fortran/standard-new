program test_pdfstandardir
    !! Fixed CLI outputs are the behavioral oracle for both projection modes.

    implicit none

    character(len=*), parameter :: input_path = 'build/test_pdfstandardir.jsonl'
    character(len=*), parameter :: legacy_path = 'build/test_pdfstandardir_legacy.sx'
    character(len=*), parameter :: records_path = 'build/test_pdfstandardir_records.sx'
    character(len=*), parameter :: heading_canonical = &
        '1 4.1.3 Syntax heading'//achar(10)// &
        '2 R501 first is PROGRAM'//achar(10)// &
        '3 5.1 Contents . . . 4'//achar(10)// &
        '4 5.1 Syntax section'//achar(10)// &
        '5 R502 second is SECOND'//achar(10)// &
        '6 17 paragraph number'//achar(10)// &
        '7 14..2 malformed'//achar(10)//achar(12)// &
        '1 R503 later is LATER'//achar(10)// &
        '2 14.2.3 Advanced syntax'//achar(10)// &
        '3 R504 final is FINAL'//achar(10)// &
        '4 See 14.2.3 in prose'//achar(10)// &
        '5 R505 retained is RETAINED'//achar(10)
    character(len=*), parameter :: heading_json_path = 'build/test_pdfstandardir_heading.jsonl'
    character(len=*), parameter :: heading_sx_path = 'build/test_pdfstandardir_heading.sx'
    character(len=*), parameter :: hash = 'fixture-hash'
    character(len=*), parameter :: legacy_expected = &
        '(standardir (format 1) (origin MECHANICAL) (source (document J3-24-007) '// &
        '(clause 5.1) (source-sha256 fixture-hash)))'
    character(len=*), parameter :: first_expected = &
        '(syntax R501 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (occurrence-clause 7.2) (rule R501) '// &
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
        '(source-hash caller-hash) (occurrence-clause 7.2) (end-page 45) '// &
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
        '"byte_start":100,"byte_length":20,"occurrence":1,'// &
        '"occurrence_clause":"7.2"}'
    write (unit, '(a)') '{"kind":"production-start","rule":"R501",'// &
        '"lhs":"program","text":"PROGRAM","page":46,'// &
        '"byte_start":300,"byte_length":30,"occurrence":2}'
    write (unit, '(a)') '{"kind":"production-start","rule":"R1",'// &
        '"lhs":"assumed","text":"ASSUMED","page":47,'// &
        '"byte_start":500,"byte_length":10}'
    close (unit)

    command = 'fo exec pdfstandardir '//input_path//' '//legacy_path//' '//hash//' 5.1'
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

    call write_bytes('build/test_pdfstandardir_heading.canonical', heading_canonical)
    call write_index('build/test_pdfstandardir_heading.index')
    command = 'fo exec pdfproductions '// &
        'build/test_pdfstandardir_heading.canonical '// &
        'build/test_pdfstandardir_heading.index '//trim(heading_json_path)//' 1 2'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'heading extraction command failed')
    call read_line(heading_json_path, 2, actual)
    call require(index(actual, '"occurrence_clause":"4"') > 0 .and. &
        index(actual, '"page":1,"byte_start":23,"byte_length":23') > 0, &
        'first heading or byte provenance differs')
    call read_line(heading_json_path, 3, actual)
    call require(index(actual, '"occurrence_clause":"5"') > 0, &
        'heading state did not change to clause 5')
    call read_line(heading_json_path, 4, actual)
    call require(index(actual, '"occurrence_clause":"5"') > 0 .and. &
        index(actual, '"page":2,"byte_start":156') > 0, &
        'later-page production did not retain clause 5')
    call read_line(heading_json_path, 5, actual)
    call require(index(actual, '"occurrence_clause":"14"') > 0, &
        'heading 14.2.3 did not set broad clause 14')
    call read_line(heading_json_path, 6, actual)
    call require(index(actual, '"occurrence_clause":"14"') > 0, &
        'malformed or prose numeric text changed heading state')

    command = 'fo exec --no-build pdfstandardir '//trim(heading_json_path)//' '// &
        trim(heading_sx_path)//' cli-clause 99'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'heading StandardIR command failed')
    call read_line(heading_sx_path, 2, actual)
    call require(trim(actual) == &
        '(syntax R501 (lhs first) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (occurrence-clause 4) (rule R501) '// &
        '(page 1) (end-page 1) (byte-start 23) (byte-length 23) '// &
        '(source-sha256 cli-clause) (occurrence 1)))', &
        'StandardIR did not consume occurrence clause 4')
    call read_line(heading_sx_path, 4, actual)
    call require(trim(actual) == &
        '(syntax R503 (lhs later) (rhs (seq (token LATER))) (source '// &
        '(document J3-24-007) (clause 5) (rule R503) '// &
        '(page 2) (end-page 2) (byte-start 156) (byte-length 21) '// &
        '(source-sha256 cli-clause) (occurrence 3)))', &
        'StandardIR did not preserve later-page provenance')
    call read_line(heading_sx_path, 6, actual)
    call require(trim(actual) == &
        '(syntax R505 (lhs retained) (rhs (seq (token RETAINED))) (source '// &
        '(document J3-24-007) (clause 5) (occurrence-clause 14) (rule R505) '// &
        '(page 2) (end-page 2) (byte-start 247) (byte-length 27) '// &
        '(source-sha256 cli-clause) (occurrence 5)))', &
        'StandardIR silently replaced occurrence clause 14')

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

    subroutine write_bytes(path, bytes)
        character(len=*), intent(in) :: path, bytes
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
        call require(ios == 0, 'could not create heading canonical fixture')
        write (unit, iostat=ios) bytes
        close (unit)
        call require(ios == 0, 'could not write heading canonical fixture')
    end subroutine write_bytes

    subroutine write_index(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create heading index fixture')
        write (unit, '(a)') 'page 1 start 0 length 155'
        write (unit, '(a)') 'page 2 start 156 length 119'
        close (unit)
    end subroutine write_index

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_pdfstandardir

program test_pdfstandardir
    !! Fixed CLI outputs are the behavioral oracle for both projection modes.

    implicit none

    character(len=*), parameter :: input_path = 'build/test_pdfstandardir.jsonl'
    character(len=*), parameter :: legacy_path = 'build/test_pdfstandardir_legacy.sx'
    character(len=*), parameter :: records_path = 'build/test_pdfstandardir_records.sx'
    character(len=*), parameter :: hash = 'fixture-hash'
    character(len=*), parameter :: legacy_expected = &
        '(standardir (format 1) (origin MECHANICAL) (source (document J3-24-007) '// &
        '(clause 5) (source-sha256 fixture-hash)))'
    character(len=*), parameter :: syntax_expected = &
        '(syntax R501 (lhs program) (rhs (seq (token PROGRAM))) (source '// &
        '(document J3-24-007) (clause 5) (rule R501) (page 45) (end-page 45) '// &
        '(byte-start 100) (byte-length 20) (source-sha256 fixture-hash)))'
    character(len=*), parameter :: record_expected = &
        '(syntax-item (id R501) (lhs program) (source (source-ref '// &
        '(document caller-document) (clause caller-clause) (rule R501) (page 45) '// &
        '(source-hash caller-hash))) (origin human) (resolution resolved))'
    character(len=4096) :: command, actual
    integer :: unit, ios, exit_status

    open (newunit=unit, file=input_path, status='replace', action='write', iostat=ios)
    call require(ios == 0, 'could not write JSONL fixture')
    write (unit, '(a)') '{"kind":"production-start","rule":"R501",'// &
        '"lhs":"program","text":"PROGRAM","page":45,'// &
        '"byte_start":100,"byte_length":20}'
    close (unit)

    command = 'build/fo/bin/pdfstandardir '//input_path//' '//legacy_path//' '//hash//' 5'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'legacy projection command failed')
    call read_line(legacy_path, 1, actual)
    call require(trim(actual) == legacy_expected, 'legacy header changed')
    call read_line(legacy_path, 2, actual)
    call require(trim(actual) == syntax_expected, 'legacy syntax output changed')

    command = 'build/fo/bin/pdfstandardir '//input_path//' '//records_path//' '// &
        'caller-hash caller-clause --syntax-items caller-document human resolved'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'source-backed projection command failed')
    call read_line(records_path, 1, actual)
    call require(trim(actual) == record_expected, 'source-backed record differs')

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

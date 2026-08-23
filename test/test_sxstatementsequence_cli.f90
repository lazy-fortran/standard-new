program test_sxstatementsequence_cli
    !! Independent behavioral checks for the statement-sequence CLI.

    implicit none

    character(len=*), parameter :: standardir_path = 'build/sxstatementsequence-standardir.sx'
    character(len=*), parameter :: layout_path = 'build/sxstatementsequence-layout.sx'
    character(len=*), parameter :: output_path = 'build/sxstatementsequence-output.tsv'
    character(len=*), parameter :: hash = &
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
    character(len=*), parameter :: expected_header = &
        'rule'//achar(9)//'container'//achar(9)//'source_document'//achar(9)// &
        'source_clause'//achar(9)//'page'//achar(9)//'byte_start'//achar(9)// &
        'source_sha256'//achar(9)//'kind'//achar(9)//'path'//achar(9)//'item'//achar(9)// &
        'derivation'//achar(9)//'status'
    character(len=4096) :: command, line
    integer :: unit, ios, exit_status

    call remove_if_exists(output_path)
    call write_fixtures()
    command = 'fo exec sxstatementsequence '//standardir_path//' '// &
        layout_path//' '//output_path
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'CLI rejected valid line-oriented SX inputs')
    call read_line(output_path, 1, line)
    call require(trim(line) == expected_header, 'TSV header is not stable')
    call read_line(output_path, 2, line)
    call require(index(trim(line), 'R3'//achar(9)//'execution-part'//achar(9)//'DOC') == 1 .and. &
        index(trim(line), 'first-plus-repeat'//achar(9)//'rhs/2'//achar(9)) > 0, &
        'CLI did not retain the canonical root expression path')
    call read_line(output_path, 3, line)
    call require(index(trim(line), 'repeat-item'//achar(9)//'rhs/2'//achar(9)) > 0, &
        'CLI did not retain the canonical repeated root path')
    call read_line(output_path, 4, line)
    call require(index(trim(line), 'R4'//achar(9)//'wrapper'//achar(9)//'DOC') == 1 .and. &
        index(trim(line), 'sequence-internal'//achar(9)//'rhs/1'//achar(9)) > 0, &
        'CLI did not retain the canonical nested sequence path')
    call read_line(output_path, 5, line)
    call require(index(trim(line), 'repeat-item'//achar(9)//'rhs/2/1'//achar(9)) > 0, &
        'CLI did not retain the canonical nested repeat path')

    command = 'fo exec --no-build sxstatementsequence '//standardir_path//' '// &
        layout_path//' '//output_path
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status /= 0, 'CLI overwrote an existing output')
    call read_line(output_path, 1, line)
    call require(trim(line) == expected_header, 'overwrite refusal changed the output')
    print '(a)', 'statement-sequence CLI test passed'

contains

    subroutine write_fixtures()
        open (newunit=unit, file=standardir_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create StandardIR fixture')
        write (unit, '(a)') '(standardir (format 1) (origin MECHANICAL))'
        write (unit, '(a)') ''
        write (unit, '(a)') syntax('R1', 'save-stmt', '(seq (token SAVE))', '1', '10')
        write (unit, '(a)') syntax('R2', 'execution-part-construct', &
            '(alt (seq (ref save-stmt)))', '2', '20')
        write (unit, '(a)') syntax('R3', 'execution-part', &
            '(seq (ref execution-part-construct) '// &
            '(repeat (ref execution-part-construct) 0 unbounded))', '3', '30')
        write (unit, '(a)') syntax('R4', 'wrapper', &
            '(seq (ref save-stmt) (optional (repeat (ref execution-part-construct) '// &
            '0 unbounded)))', '4', '40')
        close (unit)

        open (newunit=unit, file=layout_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create lexical layout fixture')
        write (unit, '(a)') '(statement-class-suffix (source-form all) (suffix -stmt) '// &
            '(source (source-ref (document DOC) (clause 4.1.4) '// &
            '(locator statement-class) (page 45) (source-hash '//hash//'))) '// &
            '(origin mechanical))'
        close (unit)
    end subroutine write_fixtures

    function syntax(rule, lhs, rhs, page, byte_start) result(text)
        character(len=*), intent(in) :: rule, lhs, rhs, page, byte_start
        character(len=4096) :: text

        write (text, '(a)') '(syntax '//trim(rule)//' (lhs '//trim(lhs)//') (rhs '//trim(rhs)//') '// &
            '(source (document DOC) (clause 5) (page '//trim(page)//') '// &
            '(byte-start '//trim(byte_start)//') (source-sha256 '//hash//')))'
    end function syntax

    subroutine read_line(path, line_number, value)
        character(len=*), intent(in) :: path
        integer, intent(in) :: line_number
        character(len=*), intent(out) :: value
        integer :: line_index

        open (newunit=unit, file=path, action='read', iostat=ios)
        call require(ios == 0, 'could not read CLI output')
        do line_index = 1, line_number
            read (unit, '(a)', iostat=ios) value
        end do
        close (unit)
        call require(ios == 0, 'CLI output is missing expected lines')
    end subroutine read_line

    subroutine remove_if_exists(path)
        character(len=*), intent(in) :: path
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=path, status='old', iostat=ios)
        call require(ios == 0, 'could not remove stale CLI output')
        close (unit, status='delete', iostat=ios)
        call require(ios == 0, 'could not remove stale CLI output')
    end subroutine remove_if_exists

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop trim(message)
    end subroutine require

end program test_sxstatementsequence_cli

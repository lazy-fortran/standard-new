program test_sxstatementboundarymap_cli
    !! Independent behavioral checks for raw alternative-path mapping.

    implicit none

    character(len=*), parameter :: sx_path = 'build/sxstatementboundarymap.sx'
    character(len=*), parameter :: candidate_path = 'build/sxstatementboundarymap.tsv'
    character(len=*), parameter :: output_path = 'build/sxstatementboundarymap-output.tsv'
    character(len=*), parameter :: missing_path = 'build/no-such-candidates.tsv'
    character(len=*), parameter :: malformed_path = 'build/sxstatementboundarymap-malformed.tsv'
    character(len=*), parameter :: hash = repeat('a', 64)
    character(len=4096) :: command, line
    integer :: unit, ios, exit_status

    call remove_if_exists(output_path)
    call write_fixtures()
    command = 'fo exec --no-build sxstatementboundarymap '//sx_path//' '//candidate_path//' '//output_path
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'CLI rejected valid raw SX and candidate fixtures')
    call require(count_lines_containing(output_path, achar(9)//'mapped'//achar(9)) == 2, &
        'raw alternative paths were not mapped')
    call require(count_lines_containing(output_path, achar(9)//'ambiguous'//achar(9)) == 1, &
        'duplicate raw source occurrence was not ambiguous')
    call require(count_lines_containing(output_path, achar(9)//'unsupported'//achar(9)) == 1, &
        'missing raw path was not retained as unsupported')
    call require(count_lines_containing(output_path, achar(9)//'suppressed'//achar(9)) == 1, &
        'suppressed candidate was not retained')
    call require(count_lines(output_path) == 6, 'output row count changed')
    call read_matching(output_path, 'foo-stmt', line)
    call require(index(line, achar(9)//'mapped'//achar(9)//'3'//achar(9)//'1'//achar(9)// &
        'foo-stmt'//achar(9)//'1'//achar(9)//'1') > 0, &
        'R1505 alternative 1 raw provenance was lost')
    call read_matching(output_path, 'bar-stmt', line)
    call require(index(line, achar(9)//'mapped'//achar(9)//'5'//achar(9)//'1'//achar(9)// &
        'bar-stmt'//achar(9)//'2'//achar(9)//'2') > 0, &
        'R1505 alternative 2 raw provenance was lost')
    call read_matching(output_path, 'suppressed-item', line)
    call require(index(line, achar(9)//'DOC'//achar(9)) > 0, 'candidate provenance was not preserved')

    call write_malformed_fixture()
    command = 'fo exec --no-build sxstatementboundarymap '//sx_path//' '//malformed_path//' '// &
        'build/sxstatementboundarymap-negative.tsv'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status /= 0, 'CLI accepted a malformed expression path')
    command = 'fo exec --no-build sxstatementboundarymap '//sx_path//' '//missing_path//' '// &
        'build/sxstatementboundarymap-missing.tsv'
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status /= 0, 'CLI accepted a missing candidate path')
    print '(a)', 'statement-boundary mapping CLI test passed'

contains

    subroutine write_fixtures()
        open (newunit=unit, file=sx_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create SX fixture')
        write (unit, '(a)') '(standardir (format 1) (origin MECHANICAL))'
        write (unit, '(a)') syntax('R1505', 'execution-part', &
            '(alt (seq (ref foo-stmt)) (seq (ref bar-stmt)))', '1', '10')
        write (unit, '(a)') syntax('R1506', 'duplicate-part', '(seq (ref duplicate-stmt))', '2', '20')
        write (unit, '(a)') syntax('R1506', 'duplicate-part', '(seq (ref duplicate-stmt))', '2', '20')
        write (unit, '(a)') syntax('R1507', 'missing-part', '(seq (ref present-stmt))', '3', '30')
        close (unit)

        open (newunit=unit, file=candidate_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create candidate fixture')
        write (unit, '(a)') header()
        write (unit, '(a)') candidate('R1505', 'execution-part', '1', '10', 'rhs/1/1', 'foo-stmt', 'candidate')
        write (unit, '(a)') candidate('R1505', 'execution-part', '1', '10', 'rhs/2/1', 'bar-stmt', 'candidate')
        write (unit, '(a)') candidate('R1506', 'duplicate-part', '2', '20', 'rhs/1', 'duplicate-stmt', 'candidate')
        write (unit, '(a)') candidate('R1507', 'missing-part', '3', '30', 'rhs/9', 'missing-stmt', 'candidate')
        write (unit, '(a)') candidate('R1505', 'execution-part', '1', '10', 'rhs/1/1', 'suppressed-item', 'suppressed')
        close (unit)
    end subroutine write_fixtures

    subroutine write_malformed_fixture()
        open (newunit=unit, file=malformed_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create malformed fixture')
        write (unit, '(a)') header()
        write (unit, '(a)') candidate('R1505', 'execution-part', '1', '10', 'rhs/0', 'bad-path', 'candidate')
        close (unit)
    end subroutine write_malformed_fixture

    function syntax(rule, lhs, rhs, page, byte_start) result(text)
        character(len=*), intent(in) :: rule, lhs, rhs, page, byte_start
        character(len=4096) :: text

        write (text, '(a)') '(syntax '//trim(rule)//' (lhs '//trim(lhs)//') (rhs '//trim(rhs)//') '// &
            '(source (document DOC) (clause 5) (rule '//trim(rule)//') (page '//trim(page)//') '// &
            '(byte-start '//trim(byte_start)//') '// &
            '(source-sha256 '//hash//')))'
    end function syntax

    function candidate(rule, lhs, page, byte_start, path, item, status) result(text)
        character(len=*), intent(in) :: rule, lhs, page, byte_start, path, item, status
        character(len=4096) :: text
        character(len=1) :: tab

        tab = achar(9)
        text = trim(rule)//tab//trim(lhs)//tab//'DOC'//tab//'5'//tab//trim(page)//tab//trim(byte_start)//tab//hash//tab// &
            'repeat-item'//tab//trim(path)//tab//trim(item)//tab//'fixture'//tab//trim(status)
    end function candidate

    function header() result(text)
        character(len=4096) :: text
        text = 'rule'//achar(9)//'container'//achar(9)//'source_document'//achar(9)//'source_clause'//achar(9)// &
            'page'//achar(9)//'byte_start'//achar(9)//'source_sha256'//achar(9)//'kind'//achar(9)//'path'//achar(9)// &
            'item'//achar(9)//'derivation'//achar(9)//'status'
    end function header

    integer function count_lines_containing(path, needle) result(count)
        character(len=*), intent(in) :: path, needle
        character(len=16384) :: value
        integer :: line_status

        count = 0
        open (newunit=unit, file=path, action='read', iostat=ios)
        call require(ios == 0, 'could not read CLI output')
        do
            read (unit, '(a)', iostat=line_status) value
            if (line_status < 0) exit
            call require(line_status == 0, 'could not read CLI output line')
            if (index(value, needle) > 0) count = count + 1
        end do
        close (unit)
    end function count_lines_containing

    integer function count_lines(path) result(count)
        character(len=*), intent(in) :: path
        character(len=16384) :: value
        integer :: line_status

        count = 0
        open (newunit=unit, file=path, action='read', iostat=ios)
        call require(ios == 0, 'could not read CLI output')
        do
            read (unit, '(a)', iostat=line_status) value
            if (line_status < 0) exit
            call require(line_status == 0, 'could not read CLI output line')
            count = count + 1
        end do
        close (unit)
    end function count_lines

    subroutine read_matching(path, needle, value)
        character(len=*), intent(in) :: path, needle
        character(len=*), intent(out) :: value
        integer :: line_status

        value = ''
        open (newunit=unit, file=path, action='read', iostat=ios)
        call require(ios == 0, 'could not read CLI output')
        do
            read (unit, '(a)', iostat=line_status) value
            if (line_status < 0) exit
            call require(line_status == 0, 'could not read CLI output line')
            if (index(value, needle) > 0) exit
        end do
        close (unit)
        call require(index(value, needle) > 0, 'expected output row was missing')
    end subroutine read_matching

    subroutine remove_if_exists(path)
        character(len=*), intent(in) :: path
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=path, status='old', iostat=ios)
        call require(ios == 0, 'could not remove stale output')
        close (unit, status='delete', iostat=ios)
        call require(ios == 0, 'could not remove stale output')
    end subroutine remove_if_exists

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) error stop trim(message)
    end subroutine require

end program test_sxstatementboundarymap_cli

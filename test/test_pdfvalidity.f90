program test_pdfvalidity
    !! The audit has an independent positive and negative source-span oracle.

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len=*), parameter :: canonical_path = 'build/pdfvalidity.canonical'
    character(len=*), parameter :: production_path = 'build/pdfvalidity.jsonl'
    character(len=*), parameter :: bad_production_path = 'build/pdfvalidity-bad.jsonl'
    character(len=*), parameter :: standardir_path = 'build/pdfvalidity.sx'
    character(len=*), parameter :: report_path = 'build/pdfvalidity.report'
    character(len=*), parameter :: bad_report_path = 'build/pdfvalidity-bad.report'
    character(len=*), parameter :: line1 = '1 R1 thing is TOKEN'
    character(len=*), parameter :: line2 = '2 or NEXT'
    character(len=4096) :: command
    integer :: unit, ios, exit_status
    integer(int64) :: second_start

    second_start = len(line1) + 1_int64
    open (newunit=unit, file=canonical_path, access='stream', form='unformatted', &
        status='replace', action='write', iostat=ios)
    call require(ios == 0, 'could not create canonical fixture')
    write (unit) line1
    write (unit) achar(10)
    write (unit) line2
    write (unit) achar(10)
    close (unit)

    call write_productions(production_path, line2)
    call write_productions(bad_production_path, 'BROKEN')
    open (newunit=unit, file=standardir_path, status='replace', action='write', iostat=ios)
    call require(ios == 0, 'could not create StandardIR fixture')
    write (unit, '(a)') '(standardir (format 1))'
    write (unit, '(a)') '(syntax R1 (lhs thing) (rhs (seq (token TOKEN))))'
    close (unit)

    command = 'fo exec --no-build pdfvalidity '//canonical_path//' '//production_path// &
        ' '//standardir_path//' '//report_path
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status == 0, 'positive audit control failed')
    command = 'fo exec --no-build pdfvalidity '//canonical_path//' '// &
        bad_production_path//' '//standardir_path//' '//bad_report_path
    call execute_command_line(trim(command), wait=.true., exitstat=exit_status)
    call require(exit_status /= 0, 'negative audit control was accepted')
    print '(a)', 'pdfvalidity audit test passed'

contains

    subroutine write_productions(path, continuation_source)
        character(len=*), intent(in) :: path, continuation_source
        character(len=4096) :: record_line

        open (newunit=unit, file=path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create production fixture')
        write (unit, '(a)') '{"kind":"production-start","rule":"R1",'// &
            '"lhs":"thing","operator":"is","text":"TOKEN","page":45,'// &
            '"byte_start":0,"byte_length":'//trim(integer_text(int(len(line1), int64)))// &
            ',"occurrence":1,"source_line":"'//line1//'"}'
        record_line = '{"kind":"production-continuation","rule":"R1",'// &
            '"lhs":"thing","operator":"or","text":"NEXT","page":45,'// &
            '"byte_start":'//trim(integer_text(second_start))// &
            ',"byte_length":'//trim(integer_text(int(len(line2), int64)))// &
            ',"occurrence":1,"source_line":"'//trim(continuation_source)//'"}'
        write (unit, '(a)') trim(record_line)
        close (unit)
    end subroutine write_productions

    function integer_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=32) :: text

        write (text, '(i0)') value
    end function integer_text

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_pdfvalidity

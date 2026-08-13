program test_source_structure
    !! Fixed JSONL and byte spans are the independent structure oracle.

    use, intrinsic :: iso_fortran_env, only: int64
    use source_structure, only: source_structure_index
    implicit none

    character(len=*), parameter :: page_one = &
        '1 5 Sample clause'//achar(10)// &
        '2 R501 program is program-unit'//achar(10)// &
        '3 [ program-unit ] ...'//achar(10)
    character(len=*), parameter :: page_two = &
        '1 or external-program'//achar(10)// &
        '2 See 6.1 for details.'//achar(10)// &
        '3 5.1 Sample section'//achar(10)// &
        '4 5.1 Contents . . . 4'//achar(10)// &
        '5 R502 appears in prose'//achar(10)
    character(len=*), parameter :: canonical = page_one//achar(12)//page_two
    character(len=*), parameter :: source_hash = &
        'bc814197395a043c4f05dcc8557befc1a519a6ab004826bc68dea03f6f8da970'
    character(len=*), parameter :: negative_text = &
        '1 ordinary prose'//achar(10)// &
        '2 R501 appears only here'//achar(10)// &
        '3 References are prose'//achar(10)// &
        '4 5.1 Contents . . . 4'//achar(10)
    character(len=*), parameter :: negative_hash = &
        'd9ce533f7e39fb7f7d1f89c8729d3caa8ebecb1737d6cf53316e8cab076a81ff'
    character(len=*), parameter :: header = &
        '{"format":1,"origin":"MECHANICAL","source":"canonical-text",'// &
        '"source_sha256":"'//source_hash//'"}'//achar(10)
    character(len=*), parameter :: negative_header = &
        '{"format":1,"origin":"MECHANICAL","source":"canonical-text",'// &
        '"source_sha256":"'//negative_hash//'"}'//achar(10)
    character(len=*), parameter :: expected = header// &
        '{"kind":"section-heading","number":"5","level":1,'// &
        '"text":"1 5 Sample clause","page":1,"byte_start":0,'// &
        '"byte_length":17,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)// &
        '{"kind":"rule-block-start","rule":"R501","line_number":2,'// &
        '"text":"2 R501 program is program-unit","page":1,"byte_start":18,'// &
        '"byte_length":30,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)// &
        '{"kind":"rule-continuation","owner":"R501","line_number":3,'// &
        '"text":"3 [ program-unit ] ...","page":1,"byte_start":49,'// &
        '"byte_length":22,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)// &
        '{"kind":"rule-continuation","owner":"R501","line_number":1,'// &
        '"text":"1 or external-program","page":2,"byte_start":73,'// &
        '"byte_length":21,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)// &
        '{"kind":"cross-reference-block","line_number":2,'// &
        '"text":"2 See 6.1 for details.","page":2,"byte_start":95,'// &
        '"byte_length":22,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)// &
        '{"kind":"section-heading","number":"5.1","level":2,'// &
        '"text":"3 5.1 Sample section","page":2,"byte_start":118,'// &
        '"byte_length":20,"source_sha256":"'//source_hash//'",'// &
        '"origin":"MECHANICAL"}'//achar(10)
    character(len=*), parameter :: positive_index = &
        'canonical-format 1'//achar(10)// &
        'origin MECHANICAL'//achar(10)// &
        'encoding UTF-8'//achar(10)// &
        'separator FORM-FEED'//achar(10)// &
        'pages 2'//achar(10)// &
        'page 1 start 0 length 72'//achar(10)// &
        'page 2 start 73 length 113'//achar(10)// &
        'bytes 186'//achar(10)
    character(len=*), parameter :: negative_index = &
        'canonical-format 1'//achar(10)// &
        'origin MECHANICAL'//achar(10)// &
        'encoding UTF-8'//achar(10)// &
        'separator FORM-FEED'//achar(10)// &
        'pages 1'//achar(10)// &
        'page 1 start 0 length 88'//achar(10)// &
        'bytes 88'//achar(10)
    character(len=*), parameter :: malformed_index = &
        'canonical-format 1'//achar(10)// &
        'origin MECHANICAL'//achar(10)// &
        'encoding UTF-8'//achar(10)// &
        'separator FORM-FEED'//achar(10)// &
        'pages 2'//achar(10)// &
        'page 1 start 0'//achar(10)
    character(len=*), parameter :: tampered = &
        '1 A Sample clause'//achar(10)// &
        '2 R501 program xx program-unit'//achar(10)// &
        '3 [ program-unit ] ...'//achar(10)// &
        achar(12)//page_two

    call write_bytes('build/source-structure.canonical', canonical)
    call write_text('build/source-structure.index', positive_index)
    call check_success('build/source-structure.canonical', 'build/source-structure.index', &
        'build/source-structure.jsonl', source_hash, expected, 6)

    call write_bytes('build/source-structure-negative.canonical', negative_text)
    call write_text('build/source-structure-negative.index', negative_index)
    call check_success('build/source-structure-negative.canonical', &
        'build/source-structure-negative.index', 'build/source-structure-negative.jsonl', &
        negative_hash, negative_header, 0)

    call write_text('build/source-structure-malformed.index', malformed_index)
    call check_failure('build/source-structure.canonical', &
        'build/source-structure-malformed.index', 'malformed page-index')

    call write_bytes('build/source-structure-tampered.canonical', tampered)
    call check_failure('build/source-structure-tampered.canonical', &
        'build/source-structure.index', 'canonical text hash')

    print *, 'all source structure tests passed'

contains

    subroutine check_success(text_path, index_path, output_path, expected_hash, expected_text, &
            expected_records)
        character(len=*), intent(in) :: text_path, index_path, output_path
        character(len=*), intent(in) :: expected_hash, expected_text
        integer, intent(in) :: expected_records
        character(len=256) :: message
        character(len=:), allocatable :: actual
        integer(int64) :: file_size
        integer :: text_unit, index_unit, output_unit, ios, records
        logical :: ok

        call run_index(text_path, index_path, output_path, expected_hash, records, ok, message)
        call require(ok, 'indexer rejected valid fixture: '//trim(message))
        if (records /= expected_records) then
            print '(a,i0)', 'record count was ', records
            call require(.false., 'fixture record count differs')
        end if
        inquire (file=output_path, size=file_size)
        allocate (character(len=file_size) :: actual)
        open (newunit=output_unit, file=output_path, access='stream', &
            form='unformatted', action='read', iostat=ios)
        call require(ios == 0, 'structure output could not be reopened')
        read (output_unit, iostat=ios) actual
        close (output_unit)
        call require(ios == 0, 'structure output could not be read')
        call require(actual == expected_text, 'structure output differs from independent oracle')
    end subroutine check_success

    subroutine check_failure(text_path, index_path, expected_message)
        character(len=*), intent(in) :: text_path, index_path, expected_message
        character(len=256) :: message
        integer :: text_unit, index_unit, output_unit, ios, records
        logical :: ok

        open (newunit=text_unit, file=text_path, access='stream', form='unformatted', &
            action='read', iostat=ios)
        call require(ios == 0, 'failure fixture text could not be opened')
        open (newunit=index_unit, file=index_path, action='read', iostat=ios)
        call require(ios == 0, 'failure fixture index could not be opened')
        open (newunit=output_unit, status='scratch', action='readwrite', iostat=ios)
        call require(ios == 0, 'failure fixture output could not be opened')
        call source_structure_index(text_unit, index_unit, output_unit, records, ok, message, &
            source_hash)
        close (output_unit)
        close (index_unit)
        close (text_unit)
        call require(.not. ok, 'malformed or tampered input was accepted')
        call require(index(message, expected_message) > 0, &
            'failure message lacks expected cause')
    end subroutine check_failure

    subroutine run_index(text_path, index_path, output_path, expected_hash, records, ok, message)
        character(len=*), intent(in) :: text_path, index_path, output_path, expected_hash
        integer, intent(out) :: records
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: text_unit, index_unit, output_unit, ios

        open (newunit=text_unit, file=text_path, access='stream', form='unformatted', &
            action='read', iostat=ios)
        call require(ios == 0, 'fixture text could not be opened')
        open (newunit=index_unit, file=index_path, action='read', iostat=ios)
        call require(ios == 0, 'fixture index could not be opened')
        open (newunit=output_unit, file=output_path, status='replace', action='write', &
            iostat=ios)
        call require(ios == 0, 'fixture output could not be opened')
        call source_structure_index(text_unit, index_unit, output_unit, records, ok, message, &
            expected_hash)
        close (output_unit)
        close (index_unit)
        close (text_unit)
    end subroutine run_index

    subroutine write_bytes(path, bytes)
        character(len=*), intent(in) :: path, bytes
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
        call require(ios == 0, 'could not create byte fixture')
        write (unit, iostat=ios) bytes
        close (unit)
        call require(ios == 0, 'could not write byte fixture')
    end subroutine write_bytes

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        character(len=4096) :: line
        integer :: cursor, line_end, line_length, newline_pos, unit, ios

        open (newunit=unit, file=path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not create text fixture')
        cursor = 1
        do while (cursor <= len(text))
            newline_pos = index(text(cursor:), achar(10))
            if (newline_pos == 0) then
                line_end = len(text)
            else
                line_end = cursor + newline_pos - 2
            end if
            line_length = line_end - cursor + 1
            line = ''
            if (line_length > 0) line(1:line_length) = text(cursor:line_end)
            write (unit, '(a)', iostat=ios) line(1:line_length)
            call require(ios == 0, 'could not write text fixture')
            if (newline_pos == 0) exit
            cursor = cursor + newline_pos
        end do
        close (unit)
    end subroutine write_text

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(message)
            stop 1
        end if
    end subroutine require

end program test_source_structure

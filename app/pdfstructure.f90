program pdfstructure
    !! Emit a bounded, provenance-bearing index of canonical document structure.

    use source_structure, only: source_structure_index
    implicit none

    character(len=4096) :: canonical_path, index_path, output_path, expected_hash
    character(len=256) :: message
    integer :: argc, text_unit, index_unit, output_unit, ios, records
    logical :: ok

    argc = command_argument_count()
    if (argc /= 3) then
        if (argc /= 4) then
            call get_command_argument(0, output_path)
            print '(a)', 'usage: '//trim(output_path)// &
                ' <canonical.text> <pages.index> <output.jsonl> [source-sha256]'
            stop 2
        end if
    end if
    call get_command_argument(1, canonical_path)
    call get_command_argument(2, index_path)
    call get_command_argument(3, output_path)
    expected_hash = ''
    if (argc == 4) call get_command_argument(4, expected_hash)

    open (newunit=text_unit, file=trim(canonical_path), access='stream', &
        form='unformatted', action='read', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open canonical text'
        stop 1
    end if
    open (newunit=index_unit, file=trim(index_path), action='read', iostat=ios)
    if (ios /= 0) then
        close (text_unit)
        print '(a)', 'error: cannot open page index'
        stop 1
    end if
    open (newunit=output_unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (text_unit)
        close (index_unit)
        print '(a)', 'error: cannot open structure output'
        stop 1
    end if

    if (argc == 4) then
        call source_structure_index(text_unit, index_unit, output_unit, records, ok, &
            message, expected_hash)
    else
        call source_structure_index(text_unit, index_unit, output_unit, records, ok, &
            message)
    end if
    if (.not. ok) then
        close (output_unit, status='delete')
        close (index_unit)
        close (text_unit)
        print '(a)', 'error: '//trim(message)
        stop 1
    end if

    close (output_unit)
    close (index_unit)
    close (text_unit)
    print '(a,i0,a)', 'indexed ', records, ' structural records'
end program pdfstructure

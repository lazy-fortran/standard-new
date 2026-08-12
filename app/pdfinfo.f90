program pdfinfo
    !! Report what fortpdf can currently see in a document.
    !!
    !! This exists so the binding can be checked against an independent
    !! extractor on a real file rather than only on constructed fixtures:
    !!
    !!     fo exec pdfinfo -- ../lazy-fortran-new/.cache/j3-24-007.pdf
    !!
    !! and compare with any other PDF tool. Disagreement on a document this
    !! size is worth investigating before a single grammar rule is extracted
    !! from it.

    use fortpdf, only: pdf_document_t, pdf_open, pdf_close, pdf_page_count, &
        pdf_message_len
    implicit none

    type(pdf_document_t) :: doc
    character(len=pdf_message_len) :: message
    character(len=4096) :: path
    integer :: argc, length, status
    logical :: ok

    argc = command_argument_count()
    if (argc /= 1) then
        call get_command_argument(0, path)
        print '(a)', 'usage: '//trim(path)//' <file.pdf>'
        stop 2
    end if

    call get_command_argument(1, path, length, status)
    if (status /= 0) then
        print '(a)', 'error: cannot read the file argument'
        stop 2
    end if

    call pdf_open(trim(path), doc, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        stop 1
    end if

    print '(a,a)', 'file  ', trim(path)
    print '(a,i0)', 'pages ', pdf_page_count(doc)

    call pdf_close(doc)
end program pdfinfo

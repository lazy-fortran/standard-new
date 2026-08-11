program pdftext
    !! Emit one machine-readable text/layout record per line.
    !!
    !! Text is emitted as UTF-8 byte values so embedded newlines cannot change
    !! the record structure. The rectangle index is the index in Poppler's
    !! returned text-layout array.

    use fortpdf, only: pdf_document_t, pdf_glyph_t, pdf_open, pdf_close, &
        pdf_page_text_layout, pdf_message_len
    implicit none

    type(pdf_document_t) :: doc
    type(pdf_glyph_t), allocatable :: glyphs(:)
    character(len=:), allocatable :: text ! text-policy: C string boundary
    character(len=4096) :: path, argument
    character(len=pdf_message_len) :: message
    integer :: argc, page_number, ios, i
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, path)
        print '(a)', 'usage: '//trim(path)//' <file.pdf> <one-based-page>'
        stop 2
    end if

    call get_command_argument(1, path)
    call get_command_argument(2, argument)
    read (argument, *, iostat=ios) page_number
    if (ios /= 0) then
        print '(a)', 'error: page must be an integer'
        stop 2
    end if

    call pdf_open(trim(path), doc, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        stop 1
    end if

    call pdf_page_text_layout(doc, page_number, text, glyphs, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        call pdf_close(doc)
        stop 1
    end if

    print '(a,i0)', 'text-length ', len(text)
    do i = 1, len(text)
        print '(a,1x,i0,1x,i0)', 'text-byte', i - 1, iachar(text(i:i))
    end do
    print '(a,i0)', 'glyph-count ', size(glyphs)
    do i = 1, size(glyphs)
        print '(a,1x,i0,1x,i0,4(1x,f12.4))', 'glyph', i - 1, &
            glyphs(i)%text_index, glyphs(i)%x1, glyphs(i)%y1, &
            glyphs(i)%x2, glyphs(i)%y2
    end do

    call pdf_close(doc)
end program pdftext

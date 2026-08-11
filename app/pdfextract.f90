program pdfextract
    !! Extract every page as stable text-byte and rectangle records.
    !!
    !! The output is deliberately line-oriented and contains no decoded text:
    !! UTF-8 bytes remain authoritative, while each glyph record carries its
    !! Poppler text index and PDF-point rectangle.

    use fortpdf, only: pdf_document_t, pdf_glyph_t, pdf_open, pdf_close, &
        pdf_page_count, pdf_page_text_layout, pdf_message_len
    implicit none

    type(pdf_document_t) :: doc
    type(pdf_glyph_t), allocatable :: glyphs(:)
    character(len=:), allocatable :: text ! text-policy: C string boundary
    character(len=4096) :: input_path, output_path
    character(len=pdf_message_len) :: message
    integer :: argc, page, unit, i, ios, pages
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <file.pdf> <output.layout>'
        stop 2
    end if

    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)
    call pdf_open(trim(input_path), doc, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        stop 1
    end if

    open (newunit=unit, file=trim(output_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open output file '
        call pdf_close(doc)
        stop 1
    end if

    pages = pdf_page_count(doc)
    write (unit, '(a)') 'format 1'
    write (unit, '(a)') 'origin MECHANICAL'
    write (unit, '(a)') 'encoding UTF-8-bytes'
    do page = 1, pages
        call pdf_page_text_layout(doc, page, text, glyphs, ok, message)
        if (.not. ok) then
            close (unit, status='delete')
            print '(a,i0,a)', 'error: page ', page, ': '//trim(message)
            call pdf_close(doc)
            stop 1
        end if

        write (unit, '(a,1x,i0)') 'page', page
        write (unit, '(a,1x,i0)') 'text-length', len(text)
        do i = 1, len(text)
            write (unit, '(a,1x,i0,1x,i0)') 'text-byte', i - 1, iachar(text(i:i))
        end do
        write (unit, '(a,1x,i0)') 'glyph-count', size(glyphs)
        do i = 1, size(glyphs)
            write (unit, '(a,1x,i0,1x,i0,4(1x,f12.4))') 'glyph', i - 1, &
                glyphs(i)%text_index, glyphs(i)%x1, glyphs(i)%y1, &
                glyphs(i)%x2, glyphs(i)%y2
        end do
    end do

    close (unit)
    call pdf_close(doc)
    print '(a,i0,a)', 'extracted ', pages, ' pages'
end program pdfextract

program pdfcanonical
    !! Write canonical UTF-8 bytes and a page-span index for a PDF.
    !!
    !! The text file contains only the canonical byte projection. The index
    !! records each page's zero-based byte start and length; a form-feed
    !! separates adjacent pages and is not part of either page span.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortpdf, only: pdf_document_t, pdf_glyph_t, pdf_open, pdf_close, &
        pdf_page_count, pdf_page_text_layout, pdf_message_len
    use fortpdf_text, only: pdf_write_canonical_page
    implicit none

    type(pdf_document_t) :: doc
    type(pdf_glyph_t), allocatable :: glyphs(:)
    character(len=:), allocatable :: text ! text-policy: C string boundary
    character(len=4096) :: input_path, text_path, index_path
    character(len=pdf_message_len) :: message
    integer(int64) :: page_start, page_length, total_bytes
    integer :: argc, page, pages, text_unit, index_unit, ios
    logical :: ok

    argc = command_argument_count()
    if (argc /= 3) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)// &
            ' <file.pdf> <output.text> <output.index>'
        stop 2
    end if

    call get_command_argument(1, input_path)
    call get_command_argument(2, text_path)
    call get_command_argument(3, index_path)
    call pdf_open(trim(input_path), doc, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        stop 1
    end if

    open (newunit=text_unit, file=trim(text_path), status='replace', &
        access='stream', form='unformatted', action='write', iostat=ios)
    if (ios /= 0) then
        print '(a)', 'error: cannot open canonical text output'
        call pdf_close(doc)
        stop 1
    end if
    open (newunit=index_unit, file=trim(index_path), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        close (text_unit, status='delete')
        print '(a)', 'error: cannot open canonical index output'
        call pdf_close(doc)
        stop 1
    end if

    pages = pdf_page_count(doc)
    write (index_unit, '(a)') 'canonical-format 1'
    write (index_unit, '(a)') 'origin MECHANICAL'
    write (index_unit, '(a)') 'encoding UTF-8'
    write (index_unit, '(a)') 'separator FORM-FEED'
    write (index_unit, '(a,1x,i0)') 'pages', pages

    total_bytes = 0_int64
    do page = 1, pages
        call pdf_page_text_layout(doc, page, text, glyphs, ok, message)
        if (.not. ok) then
            close (text_unit, status='delete')
            close (index_unit, status='delete')
            print '(a,i0,a)', 'error: page ', page, ': '//trim(message)
            call pdf_close(doc)
            stop 1
        end if

        page_start = total_bytes
        call pdf_write_canonical_page(text_unit, text, glyphs, page_length, ok, message)
        if (.not. ok) then
            close (text_unit, status='delete')
            close (index_unit, status='delete')
            print '(a)', 'error: '//trim(message)
            call pdf_close(doc)
            stop 1
        end if
        write (index_unit, '(a,1x,i0,1x,a,1x,i0,1x,a,1x,i0)') &
            'page', page, 'start', page_start, 'length', page_length
        total_bytes = total_bytes + page_length

        if (page < pages) then
            write (text_unit, iostat=ios) achar(12)
            if (ios /= 0) then
                close (text_unit, status='delete')
                close (index_unit, status='delete')
                print '(a)', 'error: cannot write page separator'
                call pdf_close(doc)
                stop 1
            end if
            total_bytes = total_bytes + 1_int64
        end if
    end do

    write (index_unit, '(a,1x,i0)') 'bytes', total_bytes
    close (text_unit)
    close (index_unit)
    call pdf_close(doc)
    print '(a,i0,a)', 'canonicalized ', pages, ' pages'
end program pdfcanonical

program test_fortpdf
    !! Behavioural tests for the poppler binding.
    !!
    !! The oracle is independent of the code under test: each fixture PDF is
    !! constructed here with a known page count, so a correct answer cannot be
    !! produced by echoing anything the library was told. The two-page and
    !! four-page cases exist so that an implementation returning a constant
    !! fails, which a single one-page case would not detect.

    use fortpdf, only: pdf_document_t, pdf_glyph_t, pdf_open, pdf_close, &
        pdf_page_count, pdf_page_text_layout, pdf_is_open, &
        pdf_message_len
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_page_count(1, all_passed)
    call check_page_count(2, all_passed)
    call check_page_count(4, all_passed)
    call check_empty_layout(all_passed)
    call check_text_layout(all_passed)
    call check_missing_file(all_passed)
    call check_not_a_pdf(all_passed)

    if (.not. all_passed) then
        print *, 'FAILED'
        stop 1
    end if
    print *, 'all tests passed'

contains

    subroutine check_page_count(npages, ok_all)
        integer, intent(in) :: npages
        logical, intent(inout) :: ok_all

        type(pdf_document_t) :: doc
        character(len=pdf_message_len) :: message
        character(len=64) :: path
        logical :: ok
        integer :: got

        write (path, '(a,i0,a)') 'build/fixture_', npages, 'p.pdf'
        call write_fixture_pdf(trim(path), npages)

        call pdf_open(trim(path), doc, ok, message)
        if (.not. ok) then
            print *, 'FAIL: could not open fixture: ', trim(message)
            ok_all = .false.
            return
        end if

        got = pdf_page_count(doc)
        if (got /= npages) then
            print *, 'FAIL: expected', npages, 'pages, got', got
            ok_all = .false.
        end if

        call pdf_close(doc)
        if (pdf_is_open(doc)) then
            print *, 'FAIL: document still open after pdf_close'
            ok_all = .false.
        end if
    end subroutine check_page_count

    subroutine check_empty_layout(ok_all)
        !! A page without text is a valid empty extraction.
        logical, intent(inout) :: ok_all

        type(pdf_document_t) :: doc
        type(pdf_glyph_t), allocatable :: glyphs(:)
        character(len=:), allocatable :: text
        character(len=pdf_message_len) :: message
        logical :: ok

        call write_fixture_pdf('build/empty_layout_fixture.pdf', 1)
        call pdf_open('build/empty_layout_fixture.pdf', doc, ok, message)
        if (.not. ok) then
            print *, 'FAIL: could not open empty fixture: ', trim(message)
            ok_all = .false.
            return
        end if

        call pdf_page_text_layout(doc, 1, text, glyphs, ok, message)
        if (.not. ok .or. len(text) /= 0 .or. size(glyphs) /= 0) then
            print *, 'FAIL: empty page did not produce an empty layout'
            ok_all = .false.
        end if
        call pdf_close(doc)

        call pdf_page_text_layout(doc, 1, text, glyphs, ok, message)
        if (ok) then
            print *, 'FAIL: closed document accepted a layout request'
            ok_all = .false.
        end if
    end subroutine check_empty_layout

    subroutine check_text_layout(ok_all)
        !! The fixture places `R501` at a known PDF coordinate. The expected
        !! text and position come from the constructed content stream, not
        !! from a second call to the binding.
        logical, intent(inout) :: ok_all

        type(pdf_document_t) :: doc
        type(pdf_glyph_t), allocatable :: glyphs(:)
        character(len=:), allocatable :: text
        character(len=pdf_message_len) :: message
        logical :: ok
        real :: tolerance

        call write_text_fixture_pdf('build/text_fixture.pdf')
        call pdf_open('build/text_fixture.pdf', doc, ok, message)
        if (.not. ok) then
            print *, 'FAIL: could not open text fixture: ', trim(message)
            ok_all = .false.
            return
        end if

        call pdf_page_text_layout(doc, 1, text, glyphs, ok, message)
        if (.not. ok) then
            print *, 'FAIL: text layout failed: ', trim(message)
            ok_all = .false.
        else if (len(text) < 4 .or. text(1:4) /= 'R501') then
            print *, 'FAIL: text layout returned unexpected text: ', text
            ok_all = .false.
        else if (size(glyphs) < 4) then
            print *, 'FAIL: expected at least four text rectangles, got', size(glyphs)
            ok_all = .false.
        else
            tolerance = 0.01
            if (abs(real(glyphs(1)%x1) - 72.0) > tolerance) then
                print *, 'FAIL: first glyph does not start at x=72:', glyphs(1)%x1
                ok_all = .false.
            end if
            if (glyphs(1)%y2 <= glyphs(1)%y1) then
                print *, 'FAIL: first glyph has no positive height'
                ok_all = .false.
            end if
            if (glyphs(4)%x1 <= glyphs(1)%x1) then
                print *, 'FAIL: glyph rectangles are not in text order'
                ok_all = .false.
            end if
        end if

        call pdf_close(doc)
    end subroutine check_text_layout

    subroutine check_missing_file(ok_all)
        !! Absent input must be reported, not survived silently.
        logical, intent(inout) :: ok_all
        type(pdf_document_t) :: doc
        character(len=pdf_message_len) :: message
        logical :: ok

        call pdf_open('build/definitely_not_here.pdf', doc, ok, message)
        if (ok) then
            print *, 'FAIL: opening a missing file reported success'
            ok_all = .false.
            call pdf_close(doc)
            return
        end if
        if (len_trim(message) == 0) then
            print *, 'FAIL: failure reported with an empty message'
            ok_all = .false.
        end if
        if (pdf_page_count(doc) /= -1) then
            print *, 'FAIL: page count of an unopened document must be -1'
            ok_all = .false.
        end if
    end subroutine check_missing_file

    subroutine check_not_a_pdf(ok_all)
        !! A file that exists but is not a PDF is a different failure path.
        logical, intent(inout) :: ok_all
        type(pdf_document_t) :: doc
        character(len=pdf_message_len) :: message
        logical :: ok
        integer :: u

        open (newunit=u, file='build/not_a_pdf.txt', status='replace', action='write')
        write (u, '(a)') 'This is not a PDF.'
        close (u)

        call pdf_open('build/not_a_pdf.txt', doc, ok, message)
        if (ok) then
            print *, 'FAIL: a text file was accepted as a PDF'
            ok_all = .false.
            call pdf_close(doc)
        end if
    end subroutine check_not_a_pdf

    subroutine write_fixture_pdf(path, npages)
        !! Write a minimal but structurally valid PDF with `npages` empty
        !! pages, including a correct cross-reference table. Built here rather
        !! than committed so that the expected page count is established by
        !! construction.
        character(len=*), intent(in) :: path
        integer, intent(in) :: npages

        character(len=1), parameter :: nl = achar(10)
        character(len=:), allocatable :: buf, kids, body
        integer, allocatable :: offset(:)
        integer :: nobj, i, u, xref_at

        nobj = 2 + npages ! catalog, pages node, one per page
        allocate (offset(nobj))

        buf = '%PDF-1.4'//nl

        offset(1) = len(buf)
        buf = buf//'1 0 obj'//nl//'<</Type/Catalog/Pages 2 0 R>>'//nl//'endobj'//nl

        kids = ''
        do i = 1, npages
            kids = kids//itoa(2 + i)//' 0 R'
            if (i < npages) kids = kids//' '
        end do

        offset(2) = len(buf)
        buf = buf//'2 0 obj'//nl// &
            '<</Type/Pages/Kids['//kids//']/Count '//itoa(npages)//'>>'//nl// &
            'endobj'//nl

        do i = 1, npages
            offset(2 + i) = len(buf)
            buf = buf//itoa(2 + i)//' 0 obj'//nl// &
                '<</Type/Page/Parent 2 0 R/MediaBox[0 0 612 792]>>'//nl// &
                'endobj'//nl
        end do

        xref_at = len(buf)
        buf = buf//'xref'//nl//'0 '//itoa(nobj + 1)//nl
        buf = buf//'0000000000 65535 f '//nl ! the free-list head
        do i = 1, nobj
            buf = buf//xref_entry(offset(i))//nl
        end do

        buf = buf//'trailer'//nl// &
            '<</Size '//itoa(nobj + 1)//'/Root 1 0 R>>'//nl// &
            'startxref'//nl//itoa(xref_at)//nl//'%%EOF'//nl

        open (newunit=u, file=path, status='replace', action='write', &
            access='stream', form='unformatted')
        write (u) buf
        close (u)
    end subroutine write_fixture_pdf

    subroutine write_text_fixture_pdf(path)
        !! One Helvetica text run at (72,720), independent of Poppler.
        character(len=*), intent(in) :: path

        character(len=1), parameter :: nl = achar(10)
        character(len=:), allocatable :: buf, content
        integer, allocatable :: offset(:)
        integer :: i, u, xref_at

        content = 'BT /F1 12 Tf 72 720 Td (R501) Tj ET'//nl
        allocate (offset(5))
        buf = '%PDF-1.4'//nl

        offset(1) = len(buf)
        buf = buf//'1 0 obj'//nl//'<</Type/Catalog/Pages 2 0 R>>'//nl//'endobj'//nl
        offset(2) = len(buf)
        buf = buf//'2 0 obj'//nl//'<</Type/Pages/Kids[3 0 R]/Count 1>>'//nl//'endobj'//nl
        offset(3) = len(buf)
        buf = buf//'3 0 obj'//nl// &
            '<</Type/Page/Parent 2 0 R/MediaBox[0 0 612 792]'// &
            '/Resources<</Font<</F1 4 0 R>>>>/Contents 5 0 R>>'//nl// &
            'endobj'//nl
        offset(4) = len(buf)
        buf = buf//'4 0 obj'//nl// &
            '<</Type/Font/Subtype/Type1/BaseFont/Helvetica>>'//nl// &
            'endobj'//nl
        offset(5) = len(buf)
        buf = buf//'5 0 obj'//nl//'<</Length '//itoa(len(content))//'>>'//nl// &
            'stream'//nl//content//'endstream'//nl//'endobj'//nl

        xref_at = len(buf)
        buf = buf//'xref'//nl//'0 6'//nl// &
            '0000000000 65535 f '//nl
        do i = 1, 5
            buf = buf//xref_entry(offset(i))//nl
        end do
        buf = buf//'trailer'//nl//'<</Size 6/Root 1 0 R>>'//nl// &
            'startxref'//nl//itoa(xref_at)//nl//'%%EOF'//nl

        open (newunit=u, file=path, status='replace', action='write', &
            access='stream', form='unformatted')
        write (u) buf
        close (u)
    end subroutine write_text_fixture_pdf

    function xref_entry(off) result(s)
        !! A cross-reference entry is exactly 20 bytes including the newline
        !! the caller appends: ten digits, space, five digits, space, type,
        !! space.
        integer, intent(in) :: off
        character(len=19) :: s
        write (s, '(i10.10,1x,i5.5,1x,a1,1x)') off, 0, 'n'
    end function xref_entry

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=32) :: tmp
        write (tmp, '(i0)') n
        s = trim(tmp)
    end function itoa

end program test_fortpdf

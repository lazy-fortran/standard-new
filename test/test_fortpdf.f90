program test_fortpdf
    !! Behavioural tests for the poppler binding.
    !!
    !! The oracle is independent of the code under test: each fixture PDF is
    !! constructed here with a known page count, so a correct answer cannot be
    !! produced by echoing anything the library was told. The two-page and
    !! four-page cases exist so that an implementation returning a constant
    !! fails, which a single one-page case would not detect.

    use fortpdf, only: pdf_document_t, pdf_open, pdf_close, pdf_page_count, &
                       pdf_is_open, pdf_message_len
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_page_count(1, all_passed)
    call check_page_count(2, all_passed)
    call check_page_count(4, all_passed)
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

        nobj = 2 + npages                      ! catalog, pages node, one per page
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
        buf = buf//'0000000000 65535 f '//nl          ! the free-list head
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

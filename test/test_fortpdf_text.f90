program test_fortpdf_text
    !! The oracle is an independently written expected canonical page. The
    !! input omits three word spaces but supplies their rectangle gaps.

    use, intrinsic :: iso_c_binding, only: c_double
    use, intrinsic :: iso_fortran_env, only: int64
    use fortpdf, only: pdf_glyph_t, pdf_message_len
    use fortpdf_text, only: pdf_write_canonical_page
    implicit none

    character(len=:), allocatable :: text, expected, actual
    character(len=pdf_message_len) :: message
    type(pdf_glyph_t), allocatable :: glyphs(:)
    integer(int64) :: bytes_written, file_size
    integer :: i, unit, ios
    logical :: ok
    real(c_double) :: x, y

    text = 'R501programisprogram-unit'//achar(10)//'[ program-unit ] ...'
    expected = 'R501 program is program-unit'//achar(10)// &
        '[ program-unit ] ...'//achar(10)
    allocate (glyphs(len(text)))

    x = 0.0_c_double
    y = 100.0_c_double
    do i = 1, len(text)
        glyphs(i)%text_index = i - 1
        glyphs(i)%byte_offset = i - 1
        glyphs(i)%byte_length = 1
        if (iachar(text(i:i)) == 10) then
            glyphs(i)%x1 = x
            glyphs(i)%x2 = x
            glyphs(i)%y1 = y
            glyphs(i)%y2 = y
            x = 0.0_c_double
            y = 110.0_c_double
        else
            if (i == 5 .or. i == 12 .or. i == 14) x = x + 10.0_c_double
            glyphs(i)%x1 = x
            glyphs(i)%x2 = x + 5.0_c_double
            glyphs(i)%y1 = y
            glyphs(i)%y2 = y + 8.0_c_double
            x = x + 5.0_c_double
        end if
    end do

    open (newunit=unit, file='build/canonical_fixture.txt', status='replace', &
        access='stream', form='unformatted', action='write')
    call pdf_write_canonical_page(unit, text, glyphs, bytes_written, ok, message)
    close (unit)
    if (.not. ok) then
        print *, 'FAIL: canonical writer failed: ', trim(message)
        stop 1
    end if

    inquire (file='build/canonical_fixture.txt', size=file_size)
    if (file_size /= int(len(expected), int64) .or. bytes_written /= file_size) then
        print *, 'FAIL: canonical byte count is incorrect'
        stop 1
    end if

    allocate (character(len=file_size) :: actual)
    open (newunit=unit, file='build/canonical_fixture.txt', access='stream', &
        form='unformatted', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: canonical fixture could not be reopened'
        stop 1
    end if
    read (unit, iostat=ios) actual
    close (unit)
    if (ios /= 0 .or. actual /= expected) then
        print *, 'FAIL: canonical bytes differ from the independent oracle'
        stop 1
    end if

    print *, 'all canonical text tests passed'
end program test_fortpdf_text

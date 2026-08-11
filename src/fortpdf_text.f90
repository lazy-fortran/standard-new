module fortpdf_text
    !! Canonical text projection for layout-aware PDF extraction.
    !!
    !! The byte sequence returned by Poppler remains authoritative. This
    !! module writes a second, normalized projection in geometric reading
    !! order and inserts one ASCII space where a same-line rectangle gap is
    !! larger than a glyph width.
    !! It never accumulates the page in a Fortran character value.

    use, intrinsic :: iso_c_binding, only: c_double
    use, intrinsic :: iso_fortran_env, only: int64
    use fortpdf, only: pdf_glyph_t
    implicit none
    private

    public :: pdf_write_canonical_page

    real(c_double), parameter :: line_tolerance = 3.0_c_double
    real(c_double), parameter :: minimum_gap = 1.0_c_double
    real(c_double), parameter :: gap_width_fraction = 0.5_c_double

contains

    subroutine pdf_write_canonical_page(unit, text, glyphs, bytes_written, ok, message)
        !! Write one page's canonical text projection to an unformatted stream.
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text ! text-policy: C string boundary
        type(pdf_glyph_t), intent(in) :: glyphs(:)
        integer(int64), intent(out) :: bytes_written
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer, allocatable :: order(:)
        integer :: i, j, k, n_order, start, width, temporary
        logical :: current_space, previous_space, have_previous
        real(c_double) :: previous_x2, previous_y1, previous_width
        real(c_double) :: current_width, gap, threshold

        bytes_written = 0_int64
        ok = .false.
        message = ''
        have_previous = .false.
        previous_space = .true.
        previous_x2 = 0.0_c_double
        previous_y1 = 0.0_c_double
        previous_width = 0.0_c_double

        allocate (order(size(glyphs)))
        n_order = 0
        do i = 1, size(glyphs)
            if (glyph_is_line_break(text, glyphs(i))) cycle
            n_order = n_order + 1
            order(n_order) = i
            j = n_order
            do while (j > 1)
                if (.not. glyph_before(glyphs(order(j)), glyphs(order(j - 1)))) exit
                temporary = order(j)
                order(j) = order(j - 1)
                order(j - 1) = temporary
                j = j - 1
            end do
        end do

        do k = 1, n_order
            i = order(k)
            start = int(glyphs(i)%byte_offset) + 1
            width = int(glyphs(i)%byte_length)
            if (width <= 0) then
                message = 'non-positive UTF-8 span length'
                return
            end if
            if (start < 1 .or. start + width - 1 > len(text)) then
                message = 'glyph UTF-8 span is outside page text'
                return
            end if

            current_space = ascii_space(text(start:start), width)
            current_width = glyphs(i)%x2 - glyphs(i)%x1
            if (have_previous) then
                if (abs(glyphs(i)%y1 - previous_y1) > line_tolerance) then
                    call write_piece(unit, achar(10), bytes_written, ok, message)
                    if (.not. ok) return
                else
                    if (.not. current_space) then
                        if (.not. previous_space) then
                            gap = glyphs(i)%x1 - previous_x2
                            threshold = max(minimum_gap, &
                                gap_width_fraction * max(previous_width, current_width))
                            if (gap > threshold) then
                                call write_piece(unit, ' ', bytes_written, ok, message)
                                if (.not. ok) return
                            end if
                        end if
                    end if
                end if
            end if

            call write_piece(unit, text(start:start + width - 1), bytes_written, ok, message)
            if (.not. ok) return

            have_previous = .true.
            previous_space = current_space
            previous_x2 = glyphs(i)%x2
            previous_y1 = glyphs(i)%y1
            previous_width = current_width
        end do

        if (have_previous) then
            call write_piece(unit, achar(10), bytes_written, ok, message)
            if (.not. ok) return
        end if

        ok = .true.
    end subroutine pdf_write_canonical_page

    logical function glyph_before(left, right)
        type(pdf_glyph_t), intent(in) :: left, right

        if (abs(left%y1 - right%y1) > line_tolerance) then
            glyph_before = left%y1 < right%y1
        else if (abs(left%x1 - right%x1) > minimum_gap) then
            glyph_before = left%x1 < right%x1
        else
            glyph_before = left%text_index < right%text_index
        end if
    end function glyph_before

    logical function glyph_is_line_break(text, glyph)
        character(len=*), intent(in) :: text
        type(pdf_glyph_t), intent(in) :: glyph
        integer :: start, width, byte

        glyph_is_line_break = .false.
        start = int(glyph%byte_offset) + 1
        width = int(glyph%byte_length)
        if (width /= 1) return
        if (start < 1 .or. start > len(text)) return
        byte = iachar(text(start:start))
        select case (byte)
        case (10, 12, 13)
            glyph_is_line_break = .true.
        end select
    end function glyph_is_line_break

    logical function ascii_space(first_byte, width)
        character(len=1), intent(in) :: first_byte
        integer, intent(in) :: width
        integer :: byte

        ascii_space = .false.
        if (width /= 1) return
        byte = iachar(first_byte)
        select case (byte)
        case (9, 10, 12, 13, 32)
            ascii_space = .true.
        end select
    end function ascii_space

    subroutine write_piece(unit, piece, bytes_written, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: piece
        integer(int64), intent(inout) :: bytes_written
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        write (unit, iostat=ios) piece
        if (ios /= 0) then
            ok = .false.
            message = 'cannot write canonical text'
            return
        end if

        bytes_written = bytes_written + int(len(piece), int64)
        ok = .true.
        message = ''
    end subroutine write_piece

end module fortpdf_text

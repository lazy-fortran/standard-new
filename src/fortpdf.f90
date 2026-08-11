module fortpdf
    !! Layout-aware PDF access for Fortran, over the poppler-glib C API.
    !!
    !! The extraction pipeline needs character positions, not just a text dump:
    !! the standard's grammar productions are recognized by their typographic
    !! structure as much as by their content. poppler_page_get_text_layout
    !! returns a rectangle per character, which is the layer this module
    !! exposes alongside the text returned by poppler_page_get_text.
    !!
    !! Only the document handle is owned here. Callers must call pdf_close.

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_size_t, &
        c_null_ptr, c_null_char, c_associated, &
        c_f_pointer, c_int32_t, c_double
    implicit none
    private

    public :: pdf_document_t, pdf_glyph_t, pdf_open, pdf_close, pdf_page_count, &
        pdf_page_text_layout, pdf_is_open

    integer, parameter, public :: pdf_message_len = 512

    type :: pdf_document_t
        private
        type(c_ptr) :: handle = c_null_ptr
    end type pdf_document_t

    type, public :: pdf_glyph_t
        !! One rectangle in Poppler's text-layout array. `text_index` is the
        !! zero-based position assigned by Poppler in the returned text;
        !! byte_offset and byte_length identify its UTF-8 span.
        integer(c_int32_t) :: text_index = 0
        integer(c_int32_t) :: byte_offset = 0
        integer(c_int32_t) :: byte_length = 1
        real(c_double) :: x1 = 0.0_c_double
        real(c_double) :: y1 = 0.0_c_double
        real(c_double) :: x2 = 0.0_c_double
        real(c_double) :: y2 = 0.0_c_double
    end type pdf_glyph_t

    type, bind(c) :: poppler_rectangle_t
        real(c_double) :: x1
        real(c_double) :: y1
        real(c_double) :: x2
        real(c_double) :: y2
    end type poppler_rectangle_t

    ! GError layout: { GQuark domain; gint code; gchar *message; }
    type, bind(c) :: gerror_t
        integer(c_int32_t) :: domain
        integer(c_int)     :: code
        type(c_ptr)        :: message
    end type gerror_t

    interface
        function c_poppler_document_new_from_file(uri, password, error) &
                bind(c, name='poppler_document_new_from_file') result(doc)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: uri(*)
            type(c_ptr), value, intent(in) :: password
            type(c_ptr), intent(inout) :: error
            type(c_ptr) :: doc
        end function c_poppler_document_new_from_file

        function c_poppler_document_get_n_pages(doc) &
                bind(c, name='poppler_document_get_n_pages') result(n)
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: doc
            integer(c_int) :: n
        end function c_poppler_document_get_n_pages

        function c_poppler_document_get_page(doc, index) &
                bind(c, name='poppler_document_get_page') result(page)
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: doc
            integer(c_int), value, intent(in) :: index
            type(c_ptr) :: page
        end function c_poppler_document_get_page

        function c_poppler_page_get_text(page) &
                bind(c, name='poppler_page_get_text') result(text)
            import :: c_ptr
            type(c_ptr), value, intent(in) :: page
            type(c_ptr) :: text
        end function c_poppler_page_get_text

        function c_poppler_page_get_text_layout(page, rectangles, n_rectangles) &
                bind(c, name='poppler_page_get_text_layout') result(has_text)
            import :: c_ptr, c_int, c_int32_t
            type(c_ptr), value, intent(in) :: page
            type(c_ptr), intent(out) :: rectangles
            integer(c_int32_t), intent(out) :: n_rectangles
            integer(c_int) :: has_text
        end function c_poppler_page_get_text_layout

        function c_g_canonicalize_filename(filename, relative_to) &
                bind(c, name='g_canonicalize_filename') result(abs_path)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: filename(*)
            type(c_ptr), value, intent(in) :: relative_to
            type(c_ptr) :: abs_path
        end function c_g_canonicalize_filename

        function c_g_filename_to_uri(filename, hostname, error) &
                bind(c, name='g_filename_to_uri') result(uri)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: filename(*)
            type(c_ptr), value, intent(in) :: hostname
            type(c_ptr), intent(inout) :: error
            type(c_ptr) :: uri
        end function c_g_filename_to_uri

        subroutine c_g_object_unref(obj) bind(c, name='g_object_unref')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: obj
        end subroutine c_g_object_unref

        subroutine c_g_free(mem) bind(c, name='g_free')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: mem
        end subroutine c_g_free

        subroutine c_g_error_free(err) bind(c, name='g_error_free')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: err
        end subroutine c_g_error_free

        function c_strlen(s) bind(c, name='strlen') result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value, intent(in) :: s
            integer(c_size_t) :: n
        end function c_strlen
    end interface

contains

    logical function pdf_is_open(doc)
        type(pdf_document_t), intent(in) :: doc
        pdf_is_open = c_associated(doc%handle)
    end function pdf_is_open

    subroutine pdf_open(path, doc, ok, message)
        !! Open a PDF. On failure `ok` is false and `message` says why; the
        !! document is left closed. Errors are reported, never printed here.
        character(len=*), intent(in) :: path
        type(pdf_document_t), intent(out) :: doc
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(c_ptr) :: err, uri_ptr, abs_ptr
        character(len=:), allocatable :: uri, abs_path ! text-policy: C string boundary

        ok = .false.
        message = ''
        doc%handle = c_null_ptr

        ! g_filename_to_uri rejects relative paths, so canonicalize first
        ! against the current directory rather than making callers pass
        ! absolute paths.
        abs_ptr = c_g_canonicalize_filename(trim(path)//c_null_char, c_null_ptr)
        if (.not. c_associated(abs_ptr)) then
            message = 'cannot canonicalize path '//trim(path)
            return
        end if
        abs_path = c_string_to_fortran(abs_ptr)
        call c_g_free(abs_ptr)

        err = c_null_ptr
        uri_ptr = c_g_filename_to_uri(abs_path//c_null_char, c_null_ptr, err)
        if (.not. c_associated(uri_ptr)) then
            message = 'cannot form a file URI for '//trim(path)//gerror_text(err)
            call clear_error(err)
            return
        end if
        uri = c_string_to_fortran(uri_ptr)
        call c_g_free(uri_ptr)

        err = c_null_ptr
        doc%handle = c_poppler_document_new_from_file(uri//c_null_char, c_null_ptr, err)
        if (.not. c_associated(doc%handle)) then
            message = 'cannot open '//trim(path)//gerror_text(err)
            call clear_error(err)
            return
        end if
        call clear_error(err)

        ok = .true.
    end subroutine pdf_open

    subroutine pdf_close(doc)
        type(pdf_document_t), intent(inout) :: doc
        if (c_associated(doc%handle)) call c_g_object_unref(doc%handle)
        doc%handle = c_null_ptr
    end subroutine pdf_close

    integer function pdf_page_count(doc)
        !! Number of pages, or -1 if the document is not open.
        type(pdf_document_t), intent(in) :: doc
        if (.not. c_associated(doc%handle)) then
            pdf_page_count = -1
            return
        end if
        pdf_page_count = int(c_poppler_document_get_n_pages(doc%handle))
    end function pdf_page_count

    subroutine pdf_page_text_layout(doc, page_number, text, glyphs, ok, message)
        !! Return Poppler's UTF-8 text and one rectangle for each layout entry.
        !! Page numbers are one-based. Empty pages are successful empty
        !! results; invalid pages and closed documents are errors.
        type(pdf_document_t), intent(in) :: doc
        integer, intent(in) :: page_number
        character(len=:), allocatable, intent(out) :: text ! text-policy: C string boundary
        type(pdf_glyph_t), allocatable, intent(out) :: glyphs(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(c_ptr) :: page, c_text, rectangles
        type(poppler_rectangle_t), pointer :: rect(:)
        integer(c_int32_t) :: n_rectangles
        integer(c_int) :: has_text
        integer :: i, n, dims(1), position, width

        ok = .false.
        message = ''
        text = ''
        allocate (glyphs(0))

        if (.not. c_associated(doc%handle)) then
            message = 'document is not open'
            return
        end if
        if (page_number < 1 .or. page_number > pdf_page_count(doc)) then
            message = 'page number is out of range'
            return
        end if

        page = c_poppler_document_get_page(doc%handle, int(page_number - 1, c_int))
        if (.not. c_associated(page)) then
            message = 'cannot obtain page'
            return
        end if

        c_text = c_poppler_page_get_text(page)
        if (c_associated(c_text)) then
            text = c_string_to_fortran(c_text)
            call c_g_free(c_text)
        end if

        rectangles = c_null_ptr
        n_rectangles = 0
        has_text = c_poppler_page_get_text_layout(page, rectangles, n_rectangles)
        n = max(0, int(n_rectangles))
        if (n > 0 .and. c_associated(rectangles)) then
            deallocate (glyphs)
            allocate (glyphs(n))
            dims(1) = n
            call c_f_pointer(rectangles, rect, dims)
            position = 1
            do i = 1, n
                if (position > len(text)) then
                    message = 'text/layout length mismatch'
                    exit
                end if
                width = utf8_width(text, position)
                if (position + width - 1 > len(text)) then
                    message = 'truncated UTF-8 text/layout entry'
                    exit
                end if
                glyphs(i)%text_index = int(i - 1, c_int32_t)
                glyphs(i)%byte_offset = int(position - 1, c_int32_t)
                glyphs(i)%byte_length = int(width, c_int32_t)
                glyphs(i)%x1 = rect(i)%x1
                glyphs(i)%y1 = rect(i)%y1
                glyphs(i)%x2 = rect(i)%x2
                glyphs(i)%y2 = rect(i)%y2
                position = position + width
            end do
            if (len_trim(message) == 0 .and. position - 1 /= len(text)) then
                message = 'text/layout UTF-8 coverage mismatch'
            end if
        end if
        if (c_associated(rectangles)) call c_g_free(rectangles)
        call c_g_object_unref(page)

        ! Poppler reports FALSE for a page without text. That is a valid empty
        ! extraction, not a binding error; retain the text and empty layout.
        if (len_trim(message) == 0) then
            if (has_text /= 0 .or. n == 0) ok = .true.
        end if
    end subroutine pdf_page_text_layout

    ! -- helpers ------------------------------------------------------------

    function gerror_text(err) result(text)
        !! GLib's message for an error, as ': <message>', or empty.
        type(c_ptr), intent(in) :: err
        character(len=:), allocatable :: text ! text-policy: C string boundary
        type(gerror_t), pointer :: e

        text = ''
        if (.not. c_associated(err)) return
        call c_f_pointer(err, e)
        if (.not. c_associated(e%message)) return
        text = ': '//c_string_to_fortran(e%message)
    end function gerror_text

    subroutine clear_error(err)
        type(c_ptr), intent(inout) :: err
        if (c_associated(err)) call c_g_error_free(err)
        err = c_null_ptr
    end subroutine clear_error

    function c_string_to_fortran(ptr) result(s)
        !! Copy a NUL-terminated C string into a Fortran string.
        type(c_ptr), intent(in) :: ptr
        character(len=:), allocatable :: s ! text-policy: C string boundary
        character(kind=c_char), pointer :: buf(:)
        integer :: n, i, dims(1)

        if (.not. c_associated(ptr)) then
            s = ''
            return
        end if
        n = int(c_strlen(ptr))
        if (n <= 0) then
            s = ''
            return
        end if
        dims(1) = n
        call c_f_pointer(ptr, buf, dims)
        allocate (character(len=n) :: s)
        do i = 1, n
            s(i:i) = buf(i)
        end do
    end function c_string_to_fortran

    integer function utf8_width(text, position)
        !! Width in bytes of the UTF-8 scalar beginning at POSITION.
        character(len=*), intent(in) :: text
        integer, intent(in) :: position
        integer :: first

        first = iachar(text(position:position))
        select case (first)
        case (0:127)
            utf8_width = 1
        case (192:223)
            utf8_width = 2
        case (224:239)
            utf8_width = 3
        case (240:247)
            utf8_width = 4
        case default
            utf8_width = 1
        end select
    end function utf8_width

end module fortpdf

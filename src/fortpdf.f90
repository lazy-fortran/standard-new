module fortpdf
    !! Layout-aware PDF access for Fortran, over the poppler-glib C API.
    !!
    !! The extraction pipeline needs character positions, not just a text dump:
    !! the standard's grammar productions are recognized by their typographic
    !! structure as much as by their content. poppler_page_get_text_layout
    !! returns a rectangle per character, which is the layer this module will
    !! expose. Version 0 opens a document and reports its page count, which is
    !! enough to prove the binding and the build.
    !!
    !! Only the document handle is owned here. Callers must call pdf_close.

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_size_t, &
                                           c_null_ptr, c_null_char, c_associated, &
                                           c_f_pointer, c_int32_t, c_loc
    implicit none
    private

    public :: pdf_document_t, pdf_open, pdf_close, pdf_page_count, pdf_is_open

    integer, parameter, public :: pdf_message_len = 512

    type :: pdf_document_t
        private
        type(c_ptr) :: handle = c_null_ptr
    end type pdf_document_t

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
        character(len=:), allocatable :: uri, abs_path  ! text-policy: C string boundary

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

    ! -- helpers ------------------------------------------------------------

    function gerror_text(err) result(text)
        !! GLib's message for an error, as ': <message>', or empty.
        type(c_ptr), intent(in) :: err
        character(len=:), allocatable :: text  ! text-policy: C string boundary
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
        character(len=:), allocatable :: s  ! text-policy: C string boundary
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

end module fortpdf

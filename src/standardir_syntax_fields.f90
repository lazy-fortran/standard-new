module standardir_syntax_fields
    !! Shared validation and field readers for StandardIR syntax objects.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    implicit none
    private

    public :: standardir_atom_equals
    public :: standardir_read_atom
    public :: standardir_read_label
    public :: standardir_read_pair
    public :: standardir_read_source
    public :: standardir_read_syntax_header

contains

    subroutine standardir_read_syntax_header(node, rule, lhs, document, clause, page, source_hash, &
            ok, message, source_lineage, source_byte_start, source_byte_length)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: rule, lhs, document, clause, page, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(out), optional :: source_lineage, source_byte_start, source_byte_length

        rule = ''
        lhs = ''
        document = ''
        clause = ''
        page = ''
        source_hash = ''
        if (present(source_lineage)) source_lineage = ''
        if (present(source_byte_start)) source_byte_start = ''
        if (present(source_byte_length)) source_byte_length = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 5) then
            message = 'syntax object has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), 'syntax')) then
            message = 'syntax object has no syntax label'
            return
        end if
        call standardir_read_atom(node%children(2), rule, ok, message)
        if (.not. ok) return
        call standardir_read_pair(node%children(3), 'lhs', lhs, ok, message)
        if (.not. ok) return
        call standardir_read_source(node%children(5), document, clause, page, source_hash, ok, message, &
            source_lineage, source_byte_start, source_byte_length)
    end subroutine standardir_read_syntax_header

    subroutine standardir_read_source(node, document, clause, page, source_hash, ok, message, &
            source_lineage, source_byte_start, source_byte_length, reject_unknown)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: document, clause, page, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(out), optional :: source_lineage, source_byte_start, source_byte_length
        logical, intent(in), optional :: reject_unknown
        integer :: i

        document = ''
        clause = ''
        page = ''
        source_hash = ''
        if (present(source_lineage)) source_lineage = ''
        if (present(source_byte_start)) source_byte_start = ''
        if (present(source_byte_length)) source_byte_length = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'source field has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), 'source')) then
            message = 'syntax object has no source field'
            return
        end if
        do i = 2, node%child_count
            if (node%children(i)%kind /= sx_list) then
                message = 'source child is not an SX list'
                return
            end if
            if (node%children(i)%child_count < 1) then
                message = 'source child is empty'
                return
            end if
            if (standardir_atom_equals(node%children(i)%children(1), 'document')) then
                call standardir_read_pair(node%children(i), 'document', document, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'clause')) then
                call standardir_read_pair(node%children(i), 'clause', clause, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'page')) then
                call standardir_read_pair(node%children(i), 'page', page, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'source-sha256')) then
                call standardir_read_pair(node%children(i), 'source-sha256', source_hash, ok, message)
            else if (standardir_atom_equals(node%children(i)%children(1), 'source-lineage')) then
                if (present(source_lineage)) then
                    call standardir_read_pair(node%children(i), 'source-lineage', source_lineage, ok, message)
                else if (present(reject_unknown)) then
                    if (reject_unknown) then
                        ok = .false.
                        message = 'source field has an unsupported child'
                        return
                    end if
                end if
            else if (standardir_atom_equals(node%children(i)%children(1), 'byte-start')) then
                if (present(source_byte_start)) then
                    call standardir_read_pair(node%children(i), 'byte-start', source_byte_start, ok, message)
                else if (present(reject_unknown)) then
                    if (reject_unknown) then
                        ok = .false.
                        message = 'source field has an unsupported child'
                        return
                    end if
                end if
            else if (standardir_atom_equals(node%children(i)%children(1), 'byte-length')) then
                if (present(source_byte_length)) then
                    call standardir_read_pair(node%children(i), 'byte-length', source_byte_length, ok, message)
                else if (present(reject_unknown)) then
                    if (reject_unknown) then
                        ok = .false.
                        message = 'source field has an unsupported child'
                        return
                    end if
                end if
            else
                if (present(reject_unknown)) then
                    if (reject_unknown) then
                        ok = .false.
                        message = 'source field has an unknown child'
                        return
                    end if
                end if
            end if
            if (.not. ok .and. len_trim(message) /= 0) return
        end do
        if (len_trim(document) == 0 .or. len_trim(clause) == 0 .or. &
            len_trim(page) == 0 .or. len_trim(source_hash) == 0) then
            message = 'source field lacks document, clause, page or source hash'
            return
        end if
        ok = .true.
    end subroutine standardir_read_source

    subroutine standardir_read_pair(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 2) then
            message = 'source pair has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), label)) then
            message = 'source pair label differs'
            return
        end if
        call standardir_read_atom(node%children(2), value, ok, message)
    end subroutine standardir_read_pair

    subroutine standardir_read_label(node, label, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        label = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = 'grammar expression has the wrong shape'
            return
        end if
        call standardir_read_atom(node%children(1), label, ok, message)
    end subroutine standardir_read_label

    subroutine standardir_read_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'expected an SX atom'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = 'SX atom exceeds syntax field buffer'
            return
        end if
        value = trim(node%atom)
        ok = len_trim(value) > 0
        if (.not. ok) message = 'SX atom is empty'
    end subroutine standardir_read_atom

    logical function standardir_atom_equals(node, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: expected

        standardir_atom_equals = node%kind == sx_atom .and. trim(node%atom) == expected
    end function standardir_atom_equals

end module standardir_syntax_fields

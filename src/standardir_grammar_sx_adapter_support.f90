module standardir_grammar_sx_adapter_support
    !! Raw syntax header, source, and SX field helpers for the adapter.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_export, only: standardir_source_ref_t, standardir_validate_source_ref
    use standardir_grammar_producer, only: standardir_grammar_origin_differential, &
        standardir_grammar_origin_mechanical, standardir_grammar_resolution_disputed, &
        standardir_grammar_resolution_resolved, standardir_grammar_resolution_unresolved
    implicit none
    private

    public :: validate_metadata, read_syntax, trim_expression

contains

    subroutine validate_metadata(origin, resolution, ok, message)
        integer, intent(in) :: origin, resolution
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (origin < standardir_grammar_origin_mechanical .or. &
            origin > standardir_grammar_origin_differential) then
            message = 'grammar origin is invalid'
            return
        end if
        if (resolution /= standardir_grammar_resolution_resolved .and. &
            resolution /= standardir_grammar_resolution_unresolved .and. &
            resolution /= standardir_grammar_resolution_disputed) then
            message = 'grammar resolution is invalid'
            return
        end if
        ok = .true.
    end subroutine validate_metadata

    subroutine read_syntax(node, rule, lhs, expression, source, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: rule, lhs
        type(sx_node_t), intent(out) :: expression
        type(standardir_source_ref_t), intent(out) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        rule = ''
        lhs = ''
        call clear_source(source)
        ok = .false.
        message = ''
        call expect_list(node, 'syntax', 5, ok, message)
        if (.not. ok) return
        call read_atom(node%children(2), 'syntax rule', rule, ok, message)
        if (.not. ok) return
        call read_pair_atom(node%children(3), 'lhs', lhs, ok, message)
        if (.not. ok) return
        if (.not. is_pair(node%children(4), 'rhs')) then
            message = 'syntax rhs field is malformed'
            return
        end if
        expression = node%children(4)%children(2)
        call read_source_record(node%children(5), source, ok, message)
        if (.not. ok) return
        call standardir_validate_source_ref(source, ok, message)
        if (.not. ok) message = 'syntax source provenance is incomplete'
    end subroutine read_syntax

    subroutine read_source_record(node, source, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_source_ref_t), intent(out) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, value
        logical :: have_document, have_clause, have_rule, have_page, have_hash
        character(len=128) :: label, text

        call clear_source(source)
        have_document = .false.
        have_clause = .false.
        have_rule = .false.
        have_page = .false.
        have_hash = .false.
        ok = .false.
        message = ''
        call expect_list(node, 'source', -1, ok, message)
        if (.not. ok) return
        if (node%child_count < 6) then
            message = 'source record is malformed'
            return
        end if
        do i = 2, node%child_count
            if (.not. pair_label(node%children(i), label)) then
                message = 'source field is malformed'
                return
            end if
            select case (trim(label))
            case ('document', 'clause', 'rule', 'source-sha256')
                call pair_text(node%children(i), text, ok, message)
                if (.not. ok) return
                select case (trim(label))
                case ('document')
                    source%document = text
                    have_document = .true.
                case ('clause')
                    source%clause = text
                    have_clause = .true.
                case ('rule')
                    source%rule = text
                    have_rule = .true.
                case ('source-sha256')
                    source%source_hash = text
                    have_hash = .true.
                end select
            case ('page', 'end-page', 'byte-start', 'byte-length')
                call pair_integer(node%children(i), value, ok, message)
                if (.not. ok) return
                if (trim(label) == 'page') then
                    source%page = value
                    have_page = .true.
                end if
            case default
                message = 'unsupported source field: '//trim(label)
                return
            end select
        end do
        ok = have_document .and. have_clause .and. have_rule .and. have_page .and. have_hash
        if (.not. ok) message = 'source record lacks required provenance'
    end subroutine read_source_record

    subroutine expect_list(node, label, expected_children, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        integer, intent(in) :: expected_children
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count < 1) then
            message = trim(label)//' is not a list'
            return
        end if
        if (node%children(1)%kind /= sx_atom .or. &
            trim(node%children(1)%atom) /= trim(label)) then
            message = trim(label)//' label differs'
            return
        end if
        if (expected_children >= 0 .and. node%child_count /= expected_children) then
            message = trim(label)//' has the wrong field count'
            return
        end if
        ok = .true.
    end subroutine expect_list

    subroutine read_atom(node, description, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: description
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom .or. len_trim(node%atom) == 0) then
            message = trim(description)//' is not a nonempty atom'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = trim(description)//' exceeds adapter storage'
            return
        end if
        value = trim(node%atom)
        ok = .true.
    end subroutine read_atom

    subroutine read_pair_atom(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        if (.not. is_pair(node, label)) then
            ok = .false.
            message = trim(label)//' field is malformed'
            return
        end if
        call read_atom(node%children(2), trim(label)//' value', value, ok, message)
    end subroutine read_pair_atom

    subroutine pair_text(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%child_count /= 2 .or. node%children(2)%kind /= sx_atom) then
            message = 'source text field is malformed'
            return
        end if
        if (len_trim(node%children(2)%atom) > len(value)) then
            message = 'source text field exceeds adapter storage'
            return
        end if
        value = trim(node%children(2)%atom)
        ok = len_trim(value) > 0
    end subroutine pair_text

    subroutine pair_integer(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        value = 0
        ok = .false.
        message = ''
        if (node%child_count /= 2 .or. node%children(2)%kind /= sx_atom) then
            message = 'source integer field is malformed'
            return
        end if
        read (node%children(2)%atom, *, iostat=ios) value
        if (ios /= 0) then
            message = 'source integer field is not an integer'
            return
        end if
        ok = .true.
    end subroutine pair_integer

    logical function is_pair(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label

        is_pair = .false.
        if (node%kind /= sx_list .or. node%child_count /= 2) return
        if (node%children(1)%kind /= sx_atom) return
        is_pair = trim(node%children(1)%atom) == trim(label)
    end function is_pair

    logical function pair_label(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: label

        label = ''
        pair_label = .false.
        if (node%kind /= sx_list .or. node%child_count /= 2) return
        if (node%children(1)%kind /= sx_atom) return
        label = trim(node%children(1)%atom)
        pair_label = len_trim(label) > 0
    end function pair_label

    function trim_expression(node) result(label)
        type(sx_node_t), intent(in) :: node
        character(len=32) :: label

        label = ''
        if (node%kind == sx_list .and. node%child_count >= 1) then
            if (node%children(1)%kind == sx_atom) label = trim(node%children(1)%atom)
        end if
    end function trim_expression

    subroutine clear_source(source)
        type(standardir_source_ref_t), intent(out) :: source

        source = standardir_source_ref_t()
    end subroutine clear_source

end module standardir_grammar_sx_adapter_support


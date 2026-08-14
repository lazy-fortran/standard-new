module standardir_reference_closure_io
    !! SX readers for the source-backed closure sidecars.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_export, only: standardir_source_ref_t
    use standardir_grammar_sx_adapter_support, only: read_source_record
    use standardir_reference_closure, only: closure_kind_alias, closure_kind_erratum, &
        closure_kind_lexical, closure_kind_list, closure_kind_production, &
        closure_kind_scalar, closure_kind_semantic_only, closure_kind_unresolved, &
        closure_classification_t
    implicit none
    private

    public :: closure_read_classification
    public :: closure_read_root

contains

    subroutine closure_read_classification(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(closure_classification_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: label, text
        integer :: i

        value = closure_classification_t()
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'classification is not an SX list'
            return
        end if
        if (node%child_count < 2) then
            message = 'classification has no fields'
            return
        end if
        if (.not. atom_is(node%children(1), 'classification')) then
            message = 'classification label differs'
            return
        end if

        do i = 2, node%child_count
            if (.not. field_label(node%children(i), label)) then
                message = 'classification field is malformed'
                return
            end if
            select case (trim(label))
            case ('source')
                call read_source_record(node%children(i), value%source, ok, &
                    message)
                if (.not. ok) return
            case ('name', 'target', 'separator', 'terminal', 'family', 'suffix', 'prefix')
                call pair_atom(node%children(i), text, ok, message)
                if (.not. ok) return
                select case (trim(label))
                case ('name')
                    value%name = text
                case ('target')
                    value%target = text
                case ('separator')
                    value%separator = text
                case ('terminal')
                    value%terminal = text
                case ('family')
                    value%family = text
                case ('suffix')
                    value%suffix = text
                case ('prefix')
                    value%prefix = text
                end select
            case ('kind')
                call pair_atom(node%children(i), text, ok, message)
                if (.not. ok) return
                call parse_kind(text, value%kind, ok, message)
                if (.not. ok) return
            case default
                message = 'unsupported classification field: '//trim(label)
                return
            end select
        end do

        if (len_trim(value%name) == 0) then
            message = 'classification name is empty'
            return
        end if
        if (value%kind == 0) then
            message = 'classification kind is missing'
            return
        end if
        call require_source(value%source, ok, message)
    end subroutine closure_read_classification

    subroutine closure_read_root(node, name, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: name
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        name = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'root is not an SX list'
            return
        end if
        if (node%child_count /= 2) then
            message = 'root has the wrong field count'
            return
        end if
        if (.not. atom_is(node%children(1), 'root')) then
            message = 'root label differs'
            return
        end if
        call atom_value(node%children(2), name, ok, message)
    end subroutine closure_read_root

    subroutine parse_kind(text, kind, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(out) :: kind
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        kind = 0
        ok = .true.
        message = ''
        select case (trim(text))
        case ('production')
            kind = closure_kind_production
        case ('alias')
            kind = closure_kind_alias
        case ('list')
            kind = closure_kind_list
        case ('scalar')
            kind = closure_kind_scalar
        case ('lexical')
            kind = closure_kind_lexical
        case ('erratum')
            kind = closure_kind_erratum
        case ('semantic-only')
            kind = closure_kind_semantic_only
        case ('unresolved')
            kind = closure_kind_unresolved
        case default
            ok = .false.
            message = 'unknown classification kind: '//trim(text)
        end select
    end subroutine parse_kind

    subroutine require_source(source, ok, message)
        type(standardir_source_ref_t), intent(in) :: source
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = len_trim(source%document) > 0
        if (.not. ok) then
            message = 'classification source document is empty'
            return
        end if
        ok = len_trim(source%clause) > 0
        if (.not. ok) then
            message = 'classification source clause is empty'
            return
        end if
        ok = len_trim(source%rule) > 0
        if (.not. ok) then
            message = 'classification source rule is empty'
            return
        end if
        ok = source%page > 0
        if (.not. ok) then
            message = 'classification source page is invalid'
            return
        end if
        ok = len_trim(source%source_hash) > 0
        if (.not. ok) message = 'classification source hash is empty'
    end subroutine require_source

    subroutine pair_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'field is not a pair'
            return
        end if
        if (node%child_count /= 2) then
            message = 'field pair has the wrong field count'
            return
        end if
        call atom_value(node%children(2), value, ok, message)
    end subroutine pair_atom

    subroutine atom_value(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'field value is not an atom'
            return
        end if
        if (len_trim(node%atom) == 0) then
            message = 'field value is empty'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = 'field value exceeds closure storage'
            return
        end if
        value = trim(node%atom)
        ok = .true.
    end subroutine atom_value

    logical function atom_is(node, expected)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: expected

        atom_is = .false.
        if (node%kind /= sx_atom) return
        atom_is = trim(node%atom) == trim(expected)
    end function atom_is

    logical function pair_label(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: label

        label = ''
        pair_label = .false.
        if (node%kind /= sx_list) return
        if (node%child_count /= 2) return
        if (node%children(1)%kind /= sx_atom) return
        label = trim(node%children(1)%atom)
        pair_label = len_trim(label) > 0
    end function pair_label

    logical function field_label(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: label

        label = ''
        field_label = .false.
        if (node%kind /= sx_list) return
        if (node%child_count < 2) return
        if (node%children(1)%kind /= sx_atom) return
        label = trim(node%children(1)%atom)
        field_label = len_trim(label) > 0
    end function field_label

end module standardir_reference_closure_io

module standardir_export
    !! Typed, source-backed StandardIR records for frontend consumers.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use schema_value_runtime, only: schema_runtime_close_list, schema_runtime_finish, &
        schema_runtime_open_list, schema_runtime_read_atom, schema_runtime_read_int, &
        schema_runtime_write_atom, schema_runtime_write_int, schema_runtime_write_space
    implicit none
    private

    integer, parameter, public :: standardir_origin_mechanical = 1
    integer, parameter, public :: standardir_origin_search = 2
    integer, parameter, public :: standardir_origin_smt = 3
    integer, parameter, public :: standardir_origin_llm = 4
    integer, parameter, public :: standardir_origin_llm_repair = 5
    integer, parameter, public :: standardir_origin_human = 6
    integer, parameter, public :: standardir_origin_imported = 7
    integer, parameter, public :: standardir_origin_differential = 8

    integer, parameter, public :: standardir_resolution_resolved = 1
    integer, parameter, public :: standardir_resolution_unresolved = 2
    integer, parameter, public :: standardir_resolution_disputed = 3

    type, public :: standardir_source_ref_t
        character(len=128) :: document = ''
        character(len=128) :: clause = ''
        character(len=128) :: rule = ''
        integer :: page = 0
        character(len=128) :: source_hash = ''
    end type standardir_source_ref_t

    type, public :: standardir_syntax_item_t
        character(len=128) :: id = ''
        character(len=128) :: lhs = ''
        type(standardir_source_ref_t) :: source
        integer :: origin = 0
        integer :: resolution = 0
    end type standardir_syntax_item_t

    type, public :: standardir_semantic_item_t
        character(len=128) :: id = ''
        character(len=128) :: subject = ''
        type(standardir_source_ref_t) :: source
        integer :: origin = 0
        integer :: resolution = 0
    end type standardir_semantic_item_t

    public :: standardir_read_source_ref, standardir_write_source_ref
    public :: standardir_read_syntax_item, standardir_write_syntax_item
    public :: standardir_validate_source_ref, standardir_validate_syntax_item
    public :: standardir_read_semantic_item, standardir_write_semantic_item
    public :: standardir_validate_semantic_item

contains

    subroutine standardir_read_source_ref(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_source_ref_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call read_record_start(node, 'source-ref', 5, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(2), 'document', value%document, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(3), 'clause', value%clause, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(4), 'rule', value%rule, ok, message)
        if (.not. ok) return
        call read_int_pair(node%children(5), 'page', value%page, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(6), 'source-hash', value%source_hash, ok, message)
        if (.not. ok) return
        call standardir_validate_source_ref(value, ok, message)
    end subroutine standardir_read_source_ref

    subroutine standardir_read_syntax_item(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_syntax_item_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call read_record_start(node, 'syntax-item', 5, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(2), 'id', value%id, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(3), 'lhs', value%lhs, ok, message)
        if (.not. ok) return
        if (.not. is_pair(node%children(4), 'source')) then
            ok = .false.
            message = 'syntax-item source field is malformed'
            return
        end if
        call standardir_read_source_ref(node%children(4)%children(2), value%source, ok, message)
        if (.not. ok) return
        call read_enum_pair(node%children(5), 'origin', value%origin, .true., ok, message)
        if (.not. ok) return
        call read_enum_pair(node%children(6), 'resolution', value%resolution, .false., ok, message)
    end subroutine standardir_read_syntax_item

    subroutine standardir_read_semantic_item(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_semantic_item_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call read_record_start(node, 'semantic-item', 5, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(2), 'id', value%id, ok, message)
        if (.not. ok) return
        call read_name_pair(node%children(3), 'subject', value%subject, ok, message)
        if (.not. ok) return
        if (.not. is_pair(node%children(4), 'source')) then
            ok = .false.
            message = 'semantic-item source field is malformed'
            return
        end if
        call standardir_read_source_ref(node%children(4)%children(2), value%source, ok, message)
        if (.not. ok) return
        call read_enum_pair(node%children(5), 'origin', value%origin, .true., ok, message)
        if (.not. ok) return
        call read_enum_pair(node%children(6), 'resolution', value%resolution, .false., ok, message)
    end subroutine standardir_read_semantic_item

    subroutine standardir_write_source_ref(value, unit, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_validate_source_ref(value, ok, message)
        if (.not. ok) return
        call write_record_start(unit, 'source-ref', ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'document', value%document, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'clause', value%clause, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'rule', value%rule, ok, message)
        if (.not. ok) return
        call write_int_pair(unit, 'page', value%page, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'source-hash', value%source_hash, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
        if (ok) call schema_runtime_finish(unit, ok, message)
    end subroutine standardir_write_source_ref

    subroutine standardir_write_syntax_item(value, unit, ok, message)
        type(standardir_syntax_item_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_validate_syntax_item(value, ok, message)
        if (.not. ok) return
        call write_record_start(unit, 'syntax-item', ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'id', value%id, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'lhs', value%lhs, ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        call standardir_write_source_ref_inner(value%source, unit, ok, message)
        if (.not. ok) return
        call write_enum_pair(unit, 'origin', value%origin, .true., ok, message)
        if (.not. ok) return
        call write_enum_pair(unit, 'resolution', value%resolution, .false., ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
        if (ok) call schema_runtime_finish(unit, ok, message)
    end subroutine standardir_write_syntax_item

    subroutine standardir_write_semantic_item(value, unit, ok, message)
        type(standardir_semantic_item_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_validate_semantic_item(value, ok, message)
        if (.not. ok) return
        call write_record_start(unit, 'semantic-item', ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'id', value%id, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'subject', value%subject, ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        if (.not. ok) return
        call standardir_write_source_ref_inner(value%source, unit, ok, message)
        if (.not. ok) return
        call write_enum_pair(unit, 'origin', value%origin, .true., ok, message)
        if (.not. ok) return
        call write_enum_pair(unit, 'resolution', value%resolution, .false., ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
        if (ok) call schema_runtime_finish(unit, ok, message)
    end subroutine standardir_write_semantic_item

    subroutine standardir_validate_source_ref(value, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = len_trim(value%document) > 0 .and. len_trim(value%clause) > 0 .and. &
            len_trim(value%rule) > 0 .and. value%page > 0 .and. &
            len_trim(value%source_hash) > 0
        message = ''
        if (.not. ok) message = 'source-ref has incomplete provenance'
    end subroutine standardir_validate_source_ref

    subroutine standardir_validate_syntax_item(value, ok, message)
        type(standardir_syntax_item_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = len_trim(value%id) > 0 .and. len_trim(value%lhs) > 0
        message = ''
        if (.not. ok) then
            message = 'syntax-item has an empty id or lhs'
            return
        end if
        call standardir_validate_source_ref(value%source, ok, message)
        if (.not. ok) return
        call validate_enum(value%origin, .true., ok, message)
        if (.not. ok) return
        call validate_enum(value%resolution, .false., ok, message)
    end subroutine standardir_validate_syntax_item

    subroutine standardir_validate_semantic_item(value, ok, message)
        type(standardir_semantic_item_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = len_trim(value%id) > 0 .and. len_trim(value%subject) > 0
        message = ''
        if (.not. ok) then
            message = 'semantic-item has an empty id or subject'
            return
        end if
        call standardir_validate_source_ref(value%source, ok, message)
        if (.not. ok) return
        call validate_enum(value%origin, .true., ok, message)
        if (.not. ok) return
        call validate_enum(value%resolution, .false., ok, message)
    end subroutine standardir_validate_semantic_item

    subroutine read_record_start(node, tag, field_count, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        integer, intent(in) :: field_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        message = ''
        ok = node%kind == sx_list
        if (.not. ok) then
            message = trim(tag)//' is not an SX list'
            return
        end if
        ok = node%child_count == field_count + 1
        if (.not. ok) then
            message = trim(tag)//' has the wrong field count'
            return
        end if
        ok = node%children(1)%kind == sx_atom
        if (ok) ok = trim(node%children(1)%atom) == tag
        if (.not. ok) message = trim(tag)//' has the wrong tag'
    end subroutine read_record_start

    logical function is_pair(node, tag)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        is_pair = .false.
        if (node%kind /= sx_list) return
        if (node%child_count /= 2) return
        if (node%children(1)%kind /= sx_atom) return
        is_pair = trim(node%children(1)%atom) == tag
    end function is_pair

    subroutine read_name_pair(node, tag, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        if (.not. is_pair(node, tag)) then
            ok = .false.; message = 'record field is malformed'; value = ''; return
        end if
        call schema_runtime_read_atom(node%children(2), value, ok, message)
        if (ok .and. len_trim(value) == 0) then
            ok = .false.; message = 'record name field is empty'
        end if
    end subroutine read_name_pair

    subroutine read_int_pair(node, tag, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        if (.not. is_pair(node, tag)) then
            ok = .false.; message = 'record integer field is malformed'; return
        end if
        call schema_runtime_read_int(node%children(2), value, ok, message)
    end subroutine read_int_pair

    subroutine read_enum_pair(node, tag, value, origin, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: tag
        integer, intent(out) :: value
        logical, intent(in) :: origin
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        if (.not. is_pair(node, tag)) then
            ok = .false.; message = 'record enum field is malformed'; return
        end if
        call schema_runtime_read_atom(node%children(2), atom, ok, message)
        if (.not. ok) return
        call enum_value(atom, origin, value, ok, message)
    end subroutine read_enum_pair

    subroutine enum_value(atom, origin, value, ok, message)
        character(len=*), intent(in) :: atom
        logical, intent(in) :: origin
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128), parameter :: origins(8) = [ character(len=128) :: &
            'mechanical', 'search', 'smt', 'llm', 'llm-repair', 'human', &
            'imported', 'differential' ]
        character(len=128), parameter :: resolutions(3) = [ character(len=128) :: &
            'resolved', 'unresolved', 'disputed' ]
        integer :: i
        value = 0; ok = .false.; message = ''
        if (origin) then
            do i = 1, size(origins)
                if (trim(atom) == origins(i)) then
                    value = i
                    ok = .true.
                    return
                end if
            end do
            message = 'unknown StandardIR origin'
        else
            do i = 1, size(resolutions)
                if (trim(atom) == resolutions(i)) then
                    value = i
                    ok = .true.
                    return
                end if
            end do
            message = 'unknown StandardIR resolution'
        end if
    end subroutine enum_value

    subroutine validate_enum(value, origin, ok, message)
        integer, intent(in) :: value
        logical, intent(in) :: origin
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        ok = (origin .and. value >= 1 .and. value <= 8) .or. &
            ((.not. origin) .and. value >= 1 .and. value <= 3)
        message = ''
        if (.not. ok) message = 'StandardIR enum value is outside its contract'
    end subroutine validate_enum

    subroutine write_record_start(unit, tag, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: tag
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_open_list(unit, tag, ok, message)
    end subroutine write_record_start

    subroutine write_name_pair(unit, tag, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: tag, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message); if (.not. ok) return
        call schema_runtime_open_list(unit, tag, ok, message); if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message); if (.not. ok) return
        call schema_runtime_write_atom(unit, value, ok, message); if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_name_pair

    subroutine write_int_pair(unit, tag, value, ok, message)
        integer, intent(in) :: unit, value
        character(len=*), intent(in) :: tag
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message); if (.not. ok) return
        call schema_runtime_open_list(unit, tag, ok, message); if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message); if (.not. ok) return
        call schema_runtime_write_int(unit, value, ok, message); if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_int_pair

    subroutine write_enum_pair(unit, tag, value, origin, ok, message)
        integer, intent(in) :: unit, value
        character(len=*), intent(in) :: tag
        logical, intent(in) :: origin
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        if (origin) then
            select case (value)
            case (1); atom = 'mechanical'
            case (2); atom = 'search'
            case (3); atom = 'smt'
            case (4); atom = 'llm'
            case (5); atom = 'llm-repair'
            case (6); atom = 'human'
            case (7); atom = 'imported'
            case (8); atom = 'differential'
            end select
        else
            select case (value)
            case (1); atom = 'resolved'
            case (2); atom = 'unresolved'
            case (3); atom = 'disputed'
            end select
        end if
        call write_name_pair(unit, tag, atom, ok, message)
    end subroutine write_enum_pair

    subroutine standardir_write_source_ref_inner(value, unit, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_open_list(unit, 'source', ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        if (.not. ok) return
        call write_source_ref_body(value, unit, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine standardir_write_source_ref_inner

    subroutine write_source_ref_body(value, unit, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_record_start(unit, 'source-ref', ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'document', value%document, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'clause', value%clause, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'rule', value%rule, ok, message)
        if (.not. ok) return
        call write_int_pair(unit, 'page', value%page, ok, message)
        if (.not. ok) return
        call write_name_pair(unit, 'source-hash', value%source_hash, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_source_ref_body

end module standardir_export

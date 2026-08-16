module standardir_export
    !! Typed, source-backed StandardIR records for frontend consumers.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir, only: standardir_syntax_t
    use standardir_source_provenance, only: standardir_normative_clause
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
        character(len=128) :: occurrence_clause = ''
        character(len=128) :: rule = ''
        integer :: page = 0
        integer :: end_page = 0
        integer(int64) :: byte_start = 0
        integer(int64) :: byte_length = 0
        integer :: occurrence = 0
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
    public :: standardir_write_syntax_item_from_production
    public :: standardir_validate_source_ref, standardir_validate_syntax_item
    public :: standardir_read_semantic_item, standardir_write_semantic_item
    public :: standardir_write_semantic_item_from_fields
    public :: standardir_validate_semantic_item

contains

    subroutine standardir_read_source_ref(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_source_ref_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = standardir_source_ref_t()
        ok = node%kind == sx_list .and. node%child_count >= 6
        if (ok) ok = node%children(1)%kind == sx_atom
        if (ok) ok = trim(node%children(1)%atom) == 'source-ref'
        if (.not. ok) then
            message = 'source-ref has the wrong shape'
            return
        end if
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
        call read_optional_source_fields(node, value, ok, message)
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
        call write_optional_source_fields(value, unit, ok, message)
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

    subroutine standardir_write_syntax_item_from_production(unit, production, document, &
            clause, source_hash, origin, resolution, ok, message)
        integer, intent(in) :: unit
        type(standardir_syntax_t), intent(in) :: production
        character(len=*), intent(in) :: document, clause, source_hash
        integer, intent(in) :: origin, resolution
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_syntax_item_t) :: value
        character(len=128) :: normative_clause, occurrence_clause
        logical :: found

        ok = .false.
        message = ''
        if (production%incomplete) then
            message = 'production has an unclosed grammar group'
            return
        end if
        if (production%alternative_count < 1 .or. &
            production%alternative_count > size(production%alternatives)) then
            message = 'production has an invalid alternative count'
            return
        end if
        if (production%first_page <= 0 .or. production%last_page < production%first_page) then
            message = 'production has an invalid page span'
            return
        end if

        value%id = trim(production%rule)
        value%lhs = trim(production%lhs)
        value%source%document = trim(document)
        call standardir_normative_clause(production%rule, normative_clause, found)
        occurrence_clause = trim(production%occurrence_clause)
        if (len_trim(occurrence_clause) == 0) occurrence_clause = trim(clause)
        if (found) then
            value%source%clause = trim(normative_clause)
            if (trim(occurrence_clause) /= trim(normative_clause)) then
                value%source%occurrence_clause = trim(occurrence_clause)
            end if
        else
            value%source%clause = trim(clause)
            if (trim(occurrence_clause) /= trim(clause)) then
                value%source%occurrence_clause = trim(occurrence_clause)
            end if
        end if
        value%source%rule = trim(production%rule)
        value%source%page = production%first_page
        value%source%end_page = production%last_page
        value%source%byte_start = production%first_byte
        value%source%byte_length = production%last_byte - production%first_byte
        value%source%source_hash = trim(source_hash)
        value%source%occurrence = production%occurrence
        value%origin = origin
        value%resolution = resolution
        call standardir_write_syntax_item(value, unit, ok, message)
    end subroutine standardir_write_syntax_item_from_production

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

    subroutine standardir_write_semantic_item_from_fields(unit, id, subject, document, &
            clause, rule, page, source_hash, origin, resolution, ok, message)
        integer, intent(in) :: unit, page, origin, resolution
        character(len=*), intent(in) :: id, subject, document, clause, rule, source_hash
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_semantic_item_t) :: value

        value%id = id
        value%subject = subject
        value%source%document = document
        value%source%clause = clause
        value%source%rule = rule
        value%source%page = page
        value%source%source_hash = source_hash
        value%origin = origin
        value%resolution = resolution
        call standardir_write_semantic_item(value, unit, ok, message)
    end subroutine standardir_write_semantic_item_from_fields

    subroutine standardir_validate_source_ref(value, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = len_trim(value%document) > 0 .and. len_trim(value%clause) > 0 .and. &
            len_trim(value%rule) > 0 .and. value%page > 0 .and. &
            len_trim(value%source_hash) > 0
        message = ''
        if (.not. ok) then
            message = 'source-ref has incomplete provenance'
            return
        end if
        if (value%end_page > 0 .and. value%end_page < value%page) then
            ok = .false.
            message = 'source-ref has an invalid page span'
            return
        end if
        if (value%byte_start < 0_int64 .or. value%byte_length < 0_int64) then
            ok = .false.
            message = 'source-ref has an invalid byte span'
            return
        end if
        if (value%occurrence < 0) then
            ok = .false.
            message = 'source-ref has an invalid occurrence'
        end if
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
        call write_optional_source_fields(value, unit, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_source_ref_body

    subroutine read_optional_source_fields(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_source_ref_t), intent(inout) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: label
        integer :: i

        ok = .true.
        message = ''
        do i = 7, node%child_count
            if (.not. is_pair(node%children(i), 'occurrence-clause') .and. &
                .not. is_pair(node%children(i), 'end-page') .and. &
                .not. is_pair(node%children(i), 'byte-start') .and. &
                .not. is_pair(node%children(i), 'byte-length') .and. &
                .not. is_pair(node%children(i), 'occurrence')) then
                ok = .false.
                message = 'source-ref has an unknown optional field'
                return
            end if
            label = trim(node%children(i)%children(1)%atom)
            select case (trim(label))
            case ('occurrence-clause')
                call read_name_pair(node%children(i), label, value%occurrence_clause, ok, message)
            case ('end-page')
                call read_int_pair(node%children(i), label, value%end_page, ok, message)
            case ('byte-start', 'byte-length')
                call read_int64_pair(node%children(i), label, value, ok, message)
            case ('occurrence')
                call read_int_pair(node%children(i), label, value%occurrence, ok, message)
            end select
            if (.not. ok) return
        end do
    end subroutine read_optional_source_fields

    subroutine read_int64_pair(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        type(standardir_source_ref_t), intent(inout) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        integer :: ios

        if (.not. is_pair(node, label)) then
            ok = .false.
            message = 'source-ref integer field is malformed'
            return
        end if
        call schema_runtime_read_atom(node%children(2), atom, ok, message)
        if (.not. ok) return
        if (trim(label) == 'byte-start') then
            read (atom, *, iostat=ios) value%byte_start
        else
            read (atom, *, iostat=ios) value%byte_length
        end if
        ok = ios == 0
        message = ''
        if (.not. ok) message = 'source-ref integer field is invalid'
    end subroutine read_int64_pair

    subroutine write_int64_pair(unit, label, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label
        integer(int64), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=64) :: text

        write (text, '(i0)') value
        call write_name_pair(unit, label, text, ok, message)
    end subroutine write_int64_pair

    subroutine write_optional_source_fields(value, unit, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        if (len_trim(value%occurrence_clause) > 0) then
            call write_name_pair(unit, 'occurrence-clause', value%occurrence_clause, ok, message)
            if (.not. ok) return
        end if
        if (value%end_page > 0) then
            call write_int_pair(unit, 'end-page', value%end_page, ok, message)
            if (.not. ok) return
        end if
        if (value%byte_start /= 0_int64 .or. value%byte_length /= 0_int64) then
            call write_int64_pair(unit, 'byte-start', value%byte_start, ok, message)
            if (.not. ok) return
            call write_int64_pair(unit, 'byte-length', value%byte_length, ok, message)
            if (.not. ok) return
        end if
        if (value%occurrence > 0) then
            call write_int_pair(unit, 'occurrence', value%occurrence, ok, message)
            if (.not. ok) return
        end if
        ok = .true.
        message = ''
    end subroutine write_optional_source_fields

end module standardir_export

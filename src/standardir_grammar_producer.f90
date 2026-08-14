module standardir_grammar_producer
    !! Typed producer for the versioned StandardIR grammar handoff.
    !!
    !! This module owns only the normalized, source-backed node table.  It does
    !! not interpret PDF text, choose parser algorithms, or dispatch Fortran
    !! tokens.  The flat table is deliberately a serialization boundary.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use schema_value_runtime, only: schema_runtime_close_list, schema_runtime_finish, &
        schema_runtime_open_list, schema_runtime_read_atom, schema_runtime_read_bool, &
        schema_runtime_read_int, schema_runtime_write_atom, schema_runtime_write_bool, &
        schema_runtime_write_int, schema_runtime_write_name, schema_runtime_write_space
    use standardir_export, only: standardir_source_ref_t
    implicit none
    private

    integer, parameter, public :: standardir_grammar_reference = 1
    integer, parameter, public :: standardir_grammar_token = 2
    integer, parameter, public :: standardir_grammar_sequence = 3
    integer, parameter, public :: standardir_grammar_choice = 4
    integer, parameter, public :: standardir_grammar_optional = 5
    integer, parameter, public :: standardir_grammar_repeat = 6

    integer, parameter, public :: standardir_grammar_origin_mechanical = 1
    integer, parameter, public :: standardir_grammar_origin_search = 2
    integer, parameter, public :: standardir_grammar_origin_smt = 3
    integer, parameter, public :: standardir_grammar_origin_llm = 4
    integer, parameter, public :: standardir_grammar_origin_llm_repair = 5
    integer, parameter, public :: standardir_grammar_origin_human = 6
    integer, parameter, public :: standardir_grammar_origin_imported = 7
    integer, parameter, public :: standardir_grammar_origin_differential = 8

    integer, parameter, public :: standardir_grammar_resolution_resolved = 1
    integer, parameter, public :: standardir_grammar_resolution_unresolved = 2
    integer, parameter, public :: standardir_grammar_resolution_disputed = 3

    type, public :: standardir_grammar_node_t
        integer :: kind = 0
        character(len=128) :: name = ''
        integer :: minimum = 0
        logical :: unbounded = .false.
        integer :: first_child = 0
        integer :: child_count = 0
    end type standardir_grammar_node_t

    type, public :: standardir_grammar_nodes_t
        type(standardir_grammar_node_t), allocatable :: values(:)
    end type standardir_grammar_nodes_t

    type, public :: standardir_grammar_rule_t
        character(len=128) :: id = ''
        integer :: alternative = 0
        character(len=128) :: lhs = ''
        integer :: root = 0
        type(standardir_grammar_nodes_t) :: nodes
        type(standardir_source_ref_t) :: source
        integer :: origin = 0
        integer :: resolution = 0
    end type standardir_grammar_rule_t

    public :: standardir_grammar_read
    public :: standardir_grammar_validate
    public :: standardir_grammar_write

contains

    subroutine standardir_grammar_validate(value, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, last_child

        ok = .false.
        message = ''
        call require_name(value%id, 'rule id', ok, message)
        if (.not. ok) return
        if (value%alternative < 1) then
            message = 'grammar alternative must be positive'
            return
        end if
        call require_name(value%lhs, 'left-hand side', ok, message)
        if (.not. ok) return
        if (.not. allocated(value%nodes%values)) then
            message = 'grammar node list is not allocated'
            return
        end if
        if (size(value%nodes%values) < 1) then
            message = 'grammar node list is empty'
            return
        end if
        if (value%root < 1 .or. value%root > size(value%nodes%values)) then
            message = 'grammar root is outside the node list'
            return
        end if
        if (len_trim(value%source%document) == 0 .or. len_trim(value%source%clause) == 0 .or. &
            len_trim(value%source%rule) == 0 .or. value%source%page < 1 .or. &
            len_trim(value%source%source_hash) == 0) then
            message = 'grammar source provenance is incomplete'
            return
        end if
        if (value%origin < standardir_grammar_origin_mechanical .or. &
            value%origin > standardir_grammar_origin_differential) then
            message = 'grammar origin is invalid'
            return
        end if
        if (value%resolution < standardir_grammar_resolution_resolved .or. &
            value%resolution > standardir_grammar_resolution_disputed) then
            message = 'grammar resolution is invalid'
            return
        end if

        do i = 1, size(value%nodes%values)
            call validate_node(value%nodes%values(i), size(value%nodes%values), ok, message)
            if (.not. ok) return
            if (value%nodes%values(i)%child_count > 0) then
                last_child = value%nodes%values(i)%first_child + &
                    value%nodes%values(i)%child_count - 1
                if (last_child > size(value%nodes%values)) then
                    message = 'grammar node child range exceeds node list'
                    return
                end if
            end if
        end do
        ok = .true.
        message = ''
    end subroutine standardir_grammar_validate

    subroutine standardir_grammar_write(value, unit, ok, message)
        type(standardir_grammar_rule_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call standardir_grammar_validate(value, ok, message)
        if (.not. ok) return
        call schema_runtime_open_list(unit, 'syntax-rule', ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'id', value%id, ok, message)
        if (.not. ok) return
        call write_pair_int(unit, 'alternative', value%alternative, ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'lhs', value%lhs, ok, message)
        if (.not. ok) return
        call write_pair_int(unit, 'root', value%root, ok, message)
        if (.not. ok) return
        call write_nodes(value%nodes%values, unit, ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        call standardir_write_source_inner(value%source, unit, ok, message)
        if (.not. ok) return
        call write_pair_origin(unit, value%origin, ok, message)
        if (.not. ok) return
        call write_pair_resolution(unit, value%resolution, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
        if (ok) call schema_runtime_finish(unit, ok, message)
    end subroutine standardir_grammar_write

    subroutine standardir_grammar_read(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_grammar_rule_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call clear_rule(value)
        call expect_list(node, 'syntax-rule', 9, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(2), 'id', value%id, ok, message)
        if (.not. ok) return
        call read_pair_int(node%children(3), 'alternative', value%alternative, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(4), 'lhs', value%lhs, ok, message)
        if (.not. ok) return
        call read_pair_int(node%children(5), 'root', value%root, ok, message)
        if (.not. ok) return
        call read_nodes(node%children(6), value%nodes%values, ok, message)
        if (.not. ok) return
        if (.not. is_pair(node%children(7), 'source')) then
            message = 'grammar source field is malformed'
            return
        end if
        call read_source(node%children(7)%children(2), value%source, ok, message)
        if (.not. ok) return
        call read_pair_origin(node%children(8), value%origin, ok, message)
        if (.not. ok) return
        call read_pair_resolution(node%children(9), value%resolution, ok, message)
        if (.not. ok) return
        call standardir_grammar_validate(value, ok, message)
    end subroutine standardir_grammar_read

    subroutine clear_rule(value)
        type(standardir_grammar_rule_t), intent(out) :: value
        value%id = ''
        value%alternative = 0
        value%lhs = ''
        value%root = 0
        if (allocated(value%nodes%values)) deallocate (value%nodes%values)
        value%origin = 0
        value%resolution = 0
    end subroutine clear_rule

    subroutine validate_node(value, node_count, ok, message)
        type(standardir_grammar_node_t), intent(in) :: value
        integer, intent(in) :: node_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call require_name(value%name, 'grammar node name', ok, message)
        if (.not. ok) return
        if (value%kind < standardir_grammar_reference .or. &
            value%kind > standardir_grammar_repeat) then
            message = 'grammar node kind is invalid'
            return
        end if
        if (value%minimum < 0) then
            message = 'grammar node minimum is negative'
            return
        end if
        if (value%child_count < 0 .or. value%first_child < 0) then
            message = 'grammar node child range is negative'
            return
        end if
        if (value%child_count == 0 .and. value%first_child /= 0) then
            message = 'grammar leaf has a nonzero first child'
            return
        end if
        if (value%child_count > 0 .and. value%first_child < 1) then
            message = 'grammar child range has no first child'
            return
        end if
        select case (value%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            if (value%child_count /= 0 .or. value%minimum /= 1 .or. value%unbounded) then
                message = 'grammar leaf repetition metadata is invalid'
                return
            end if
        case (standardir_grammar_sequence, standardir_grammar_choice)
            if (value%child_count < 1 .or. value%minimum /= 1 .or. value%unbounded) then
                message = 'grammar group metadata is invalid'
                return
            end if
        case (standardir_grammar_optional)
            if (value%child_count /= 1 .or. value%minimum /= 0 .or. value%unbounded) then
                message = 'grammar optional metadata is invalid'
                return
            end if
        case (standardir_grammar_repeat)
            if (value%child_count /= 1 .or. value%minimum > 1 .or. .not. value%unbounded) then
                message = 'grammar repeat metadata is invalid'
                return
            end if
        end select
        if (value%child_count > 0 .and. value%first_child + value%child_count - 1 > node_count) then
            ok = .false.
            message = 'grammar node child range exceeds node list'
            return
        end if
        ok = .true.
        message = ''
    end subroutine validate_node

    subroutine write_nodes(values, unit, ok, message)
        type(standardir_grammar_node_t), intent(in) :: values(:)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        call open_list(unit, 'nodes', ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        call schema_runtime_open_list(unit, 'grammar-nodes', ok, message)
        if (.not. ok) return
        do i = 1, size(values)
            call schema_runtime_write_space(unit, ok, message)
            if (.not. ok) return
            call write_node(values(i), unit, ok, message)
            if (.not. ok) return
        end do
        call schema_runtime_close_list(unit, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_nodes

    subroutine write_node(value, unit, ok, message)
        type(standardir_grammar_node_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_open_list(unit, 'grammar-node', ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        if (.not. ok) return
        call write_atom_kind(unit, value%kind, ok, message)
        if (.not. ok) return
        call write_atom(unit, value%name, ok, message)
        if (.not. ok) return
        call write_space_int(unit, value%minimum, ok, message)
        if (.not. ok) return
        call write_space_bool(unit, value%unbounded, ok, message)
        if (.not. ok) return
        call write_space_int(unit, value%first_child, ok, message)
        if (.not. ok) return
        call write_space_int(unit, value%child_count, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine write_node

    subroutine read_nodes(node, values, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_grammar_node_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        call expect_list(node, 'nodes', 2, ok, message)
        if (.not. ok) return
        call expect_list(node%children(2), 'grammar-nodes', -1, ok, message)
        if (.not. ok) return
        if (node%children(2)%child_count < 2) then
            message = 'grammar node list is empty'
            return
        end if
        allocate (values(node%children(2)%child_count - 1))
        do i = 1, size(values)
            call read_node(node%children(2)%children(i + 1), values(i), ok, message)
            if (.not. ok) return
        end do
    end subroutine read_nodes

    subroutine read_node(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_grammar_node_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call expect_list(node, 'grammar-node', 7, ok, message)
        if (.not. ok) return
        call read_kind(node%children(2), value%kind, ok, message)
        if (.not. ok) return
        call schema_runtime_read_atom(node%children(3), value%name, ok, message)
        if (.not. ok) return
        call schema_runtime_read_int(node%children(4), value%minimum, ok, message)
        if (.not. ok) return
        call schema_runtime_read_bool(node%children(5), value%unbounded, ok, message)
        if (.not. ok) return
        call schema_runtime_read_int(node%children(6), value%first_child, ok, message)
        if (.not. ok) return
        call schema_runtime_read_int(node%children(7), value%child_count, ok, message)
    end subroutine read_node

    subroutine standardir_write_source_inner(value, unit, ok, message)
        type(standardir_source_ref_t), intent(in) :: value
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_open_list(unit, 'source', ok, message)
        if (.not. ok) return
        call schema_runtime_write_space(unit, ok, message)
        call schema_runtime_open_list(unit, 'source-ref', ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'document', value%document, ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'clause', value%clause, ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'rule', value%rule, ok, message)
        if (.not. ok) return
        call write_pair_int(unit, 'page', value%page, ok, message)
        if (.not. ok) return
        call write_pair_name(unit, 'source-hash', value%source_hash, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
        if (.not. ok) return
        call schema_runtime_close_list(unit, ok, message)
    end subroutine standardir_write_source_inner

    subroutine read_source(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(standardir_source_ref_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call expect_list(node, 'source-ref', 6, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(2), 'document', value%document, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(3), 'clause', value%clause, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(4), 'rule', value%rule, ok, message)
        if (.not. ok) return
        call read_pair_int(node%children(5), 'page', value%page, ok, message)
        if (.not. ok) return
        call read_pair_name(node%children(6), 'source-hash', value%source_hash, ok, message)
    end subroutine read_source

    subroutine write_pair_name(unit, label, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_pair_start(unit, label, ok, message)
        if (.not. ok) return
        call schema_runtime_write_name(unit, value, ok, message)
        if (ok) call schema_runtime_close_list(unit, ok, message)
    end subroutine write_pair_name

    subroutine write_pair_int(unit, label, value, ok, message)
        integer, intent(in) :: unit, value
        character(len=*), intent(in) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_pair_start(unit, label, ok, message)
        if (.not. ok) return
        call schema_runtime_write_int(unit, value, ok, message)
        if (ok) call schema_runtime_close_list(unit, ok, message)
    end subroutine write_pair_int

    subroutine write_pair_origin(unit, value, ok, message)
        integer, intent(in) :: unit, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_pair_enum(unit, 'origin', value, ok, message)
    end subroutine write_pair_origin

    subroutine write_pair_resolution(unit, value, ok, message)
        integer, intent(in) :: unit, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_pair_enum(unit, 'resolution', value, ok, message)
    end subroutine write_pair_resolution

    subroutine write_pair_enum(unit, label, value, ok, message)
        integer, intent(in) :: unit, value
        character(len=*), intent(in) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call write_pair_start(unit, label, ok, message)
        if (.not. ok) return
        if (label == 'origin') then
            call write_origin(value, unit, ok, message)
        else
            call write_resolution(value, unit, ok, message)
        end if
        if (ok) call schema_runtime_close_list(unit, ok, message)
    end subroutine write_pair_enum

    subroutine write_pair_start(unit, label, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message)
        if (.not. ok) return
        call schema_runtime_open_list(unit, label, ok, message)
        if (ok) call schema_runtime_write_space(unit, ok, message)
    end subroutine write_pair_start

    subroutine write_space_int(unit, value, ok, message)
        integer, intent(in) :: unit, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message)
        if (ok) call schema_runtime_write_int(unit, value, ok, message)
    end subroutine write_space_int

    subroutine write_space_bool(unit, value, ok, message)
        integer, intent(in) :: unit
        logical, intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message)
        if (ok) call schema_runtime_write_bool(unit, value, ok, message)
    end subroutine write_space_bool

    subroutine write_atom(unit, value, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message)
        if (ok) call schema_runtime_write_name(unit, value, ok, message)
    end subroutine write_atom

    subroutine open_list(unit, label, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call schema_runtime_write_space(unit, ok, message)
        if (ok) call schema_runtime_open_list(unit, label, ok, message)
    end subroutine open_list

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
        if (trim(node%children(1)%atom) /= label .or. node%children(1)%kind /= sx_atom) then
            message = trim(label)//' label differs'
            return
        end if
        if (expected_children >= 0 .and. node%child_count /= expected_children) then
            message = trim(label)//' has the wrong field count'
            return
        end if
        ok = .true.
    end subroutine expect_list

    logical function is_pair(node, label)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        is_pair = .false.
        if (node%kind /= sx_list) return
        if (node%child_count /= 2) return
        if (node%children(1)%kind /= sx_atom) return
        if (trim(node%children(1)%atom) == label) is_pair = .true.
    end function is_pair

    subroutine read_pair_name(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call require_pair(node, label, ok, message)
        if (ok) call schema_runtime_read_atom(node%children(2), value, ok, message)
    end subroutine read_pair_name

    subroutine read_pair_int(node, label, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        call require_pair(node, label, ok, message)
        if (ok) call schema_runtime_read_int(node%children(2), value, ok, message)
    end subroutine read_pair_int

    subroutine require_pair(node, label, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        ok = is_pair(node, label)
        message = ''
        if (.not. ok) message = trim(label)//' pair is malformed'
    end subroutine require_pair

    subroutine read_pair_origin(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        call require_pair(node, 'origin', ok, message)
        if (.not. ok) return
        call schema_runtime_read_atom(node%children(2), atom, ok, message)
        if (ok) call parse_origin(atom, value, ok, message)
    end subroutine read_pair_origin

    subroutine read_pair_resolution(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        call require_pair(node, 'resolution', ok, message)
        if (.not. ok) return
        call schema_runtime_read_atom(node%children(2), atom, ok, message)
        if (ok) call parse_resolution(atom, value, ok, message)
    end subroutine read_pair_resolution

    subroutine read_kind(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=128) :: atom
        call schema_runtime_read_atom(node, atom, ok, message)
        if (ok) call parse_kind(atom, value, ok, message)
    end subroutine read_kind

    subroutine write_atom_kind(unit, value, ok, message)
        integer, intent(in) :: unit, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: atom
        call kind_name(value, atom, ok, message)
        if (ok) call schema_runtime_write_atom(unit, atom, ok, message)
    end subroutine write_atom_kind

    subroutine write_origin(value, unit, ok, message)
        integer, intent(in) :: value, unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: atom
        call origin_name(value, atom, ok, message)
        if (ok) call schema_runtime_write_atom(unit, atom, ok, message)
    end subroutine write_origin

    subroutine write_resolution(value, unit, ok, message)
        integer, intent(in) :: value, unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: atom
        call resolution_name(value, atom, ok, message)
        if (ok) call schema_runtime_write_atom(unit, atom, ok, message)
    end subroutine write_resolution

    subroutine kind_name(value, name, ok, message)
        integer, intent(in) :: value
        character(len=*), intent(out) :: name
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), parameter :: names(6) = [character(len=9) :: 'reference', &
            'token', 'sequence', 'choice', 'optional', 'repeat']
        name = ''
        ok = value >= 1 .and. value <= 6
        message = ''
        if (ok) then
            name = names(value)
        else
            message = 'grammar node kind is invalid'
        end if
    end subroutine kind_name

    subroutine parse_kind(atom, value, ok, message)
        character(len=*), intent(in) :: atom
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: name
        name = trim(atom)
        select case (name)
        case ('reference'); value = standardir_grammar_reference
        case ('token'); value = standardir_grammar_token
        case ('sequence'); value = standardir_grammar_sequence
        case ('choice'); value = standardir_grammar_choice
        case ('optional'); value = standardir_grammar_optional
        case ('repeat'); value = standardir_grammar_repeat
        case default
            value = 0
            ok = .false.
            message = 'grammar node kind is invalid'
            return
        end select
        ok = .true.
        message = ''
    end subroutine parse_kind

    subroutine origin_name(value, name, ok, message)
        integer, intent(in) :: value
        character(len=*), intent(out) :: name
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), parameter :: names(8) = [character(len=12) :: 'mechanical', &
            'search', 'smt', 'llm', 'llm-repair', 'human', 'imported', 'differential']
        name = ''
        ok = value >= 1 .and. value <= 8
        message = ''
        if (ok) then
            name = names(value)
        else
            message = 'grammar origin is invalid'
        end if
    end subroutine origin_name

    subroutine parse_origin(atom, value, ok, message)
        character(len=*), intent(in) :: atom
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: name
        name = trim(atom)
        select case (name)
        case ('mechanical'); value = 1
        case ('search'); value = 2
        case ('smt'); value = 3
        case ('llm'); value = 4
        case ('llm-repair'); value = 5
        case ('human'); value = 6
        case ('imported'); value = 7
        case ('differential'); value = 8
        case default
            value = 0
            ok = .false.
            message = 'grammar origin is invalid'
            return
        end select
        ok = .true.
        message = ''
    end subroutine parse_origin

    subroutine resolution_name(value, name, ok, message)
        integer, intent(in) :: value
        character(len=*), intent(out) :: name
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), parameter :: names(3) = [character(len=10) :: 'resolved', &
            'unresolved', 'disputed']
        name = ''
        ok = value >= 1 .and. value <= 3
        message = ''
        if (ok) then
            name = names(value)
        else
            message = 'grammar resolution is invalid'
        end if
    end subroutine resolution_name

    subroutine parse_resolution(atom, value, ok, message)
        character(len=*), intent(in) :: atom
        integer, intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: name
        name = trim(atom)
        select case (name)
        case ('resolved'); value = 1
        case ('unresolved'); value = 2
        case ('disputed'); value = 3
        case default
            value = 0
            ok = .false.
            message = 'grammar resolution is invalid'
            return
        end select
        ok = .true.
        message = ''
    end subroutine parse_resolution

    subroutine require_name(value, description, ok, message)
        character(len=*), intent(in) :: value, description
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        ok = len_trim(value) > 0
        message = ''
        if (.not. ok) then
            message = trim(description)//' is empty'
            return
        end if
        do i = 1, len_trim(value)
            if (value(i:i) == ' ' .or. value(i:i) == '(' .or. value(i:i) == ')') then
                ok = .false.
                message = trim(description)//' is not a canonical atom'
                return
            end if
        end do
    end subroutine require_name

end module standardir_grammar_producer

module schema_ir
    !! Typed representation and validator for the first .sxs schema slice.
    !!
    !! A schema is an SX list whose declarations describe primitive types,
    !! records, sums, lists, optionals and enums.  This module only parses and
    !! validates the schema; source-tree wiring remains a later generator.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    implicit none
    private

    integer, parameter, public :: schema_primitive = 1
    integer, parameter, public :: schema_record = 2
    integer, parameter, public :: schema_sum = 3
    integer, parameter, public :: schema_list = 4
    integer, parameter, public :: schema_optional = 5
    integer, parameter, public :: schema_enum = 6

    integer, parameter, public :: schema_max_declarations = 64
    integer, parameter, public :: schema_max_members = 64
    integer, parameter :: schema_name_length = 128

    type, public :: schema_member_t
        character(len=schema_name_length) :: name = ''
        character(len=schema_name_length) :: type_name = ''
    end type schema_member_t

    type, public :: schema_declaration_t
        integer :: kind = 0
        character(len=schema_name_length) :: name = ''
        character(len=schema_name_length) :: target_type = ''
        integer :: member_count = 0
        type(schema_member_t) :: members(schema_max_members)
    end type schema_declaration_t

    type, public :: schema_t
        character(len=schema_name_length) :: name = ''
        integer :: declaration_count = 0
        type(schema_declaration_t) :: declarations(schema_max_declarations)
    end type schema_t

    public :: schema_parse_text
    public :: schema_parse_node
    public :: schema_validate

contains

    subroutine schema_parse_text(text, schema, ok, message)
        character(len=*), intent(in) :: text
        type(schema_t), intent(out) :: schema
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t) :: root

        call sx_parse(text, root, ok, message)
        if (.not. ok) return
        call schema_parse_node(root, schema, ok, message)
    end subroutine schema_parse_text

    subroutine schema_parse_node(root, schema, ok, message)
        type(sx_node_t), intent(in) :: root
        type(schema_t), intent(out) :: schema
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=schema_name_length) :: tag
        integer :: i

        call clear_schema(schema)
        ok = .false.
        message = ''
        if (root%kind /= sx_list) then
            message = 'schema root is not a list'
            return
        end if
        if (root%child_count < 2) then
            message = 'schema root needs a name'
            return
        end if
        call read_atom(root%children(1), tag, ok, message)
        if (.not. ok) return
        if (trim(tag) /= 'schema') then
            message = 'schema root does not start with schema'
            return
        end if
        call read_atom(root%children(2), schema%name, ok, message)
        if (.not. ok) return
        if (root%child_count - 2 > schema_max_declarations) then
            message = 'schema has too many declarations'
            return
        end if

        do i = 3, root%child_count
            schema%declaration_count = schema%declaration_count + 1
            call parse_declaration(root%children(i), schema%declarations(i - 2), ok, message)
            if (.not. ok) return
        end do
        call schema_validate(schema, ok, message)
    end subroutine schema_parse_node

    subroutine schema_validate(schema, ok, message)
        type(schema_t), intent(in) :: schema
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i, j, k

        ok = .false.
        message = ''
        if (len_trim(schema%name) == 0) then
            message = 'schema has an empty name'
            return
        end if
        if (schema%declaration_count < 0) then
            message = 'schema declaration count is negative'
            return
        end if
        if (schema%declaration_count > size(schema%declarations)) then
            message = 'schema declaration count exceeds storage'
            return
        end if
        do i = 1, schema%declaration_count
            if (len_trim(schema%declarations(i)%name) == 0) then
                message = 'schema declaration has an empty name'
                return
            end if
            do j = 1, i - 1
                if (trim(schema%declarations(i)%name) == &
                    trim(schema%declarations(j)%name)) then
                    message = 'duplicate schema declaration: '// &
                        trim(schema%declarations(i)%name)
                    return
                end if
            end do
            if (schema%declarations(i)%member_count < 0) then
                message = 'schema declaration has a negative member count'
                return
            end if
            if (schema%declarations(i)%member_count > schema_max_members) then
                message = 'schema declaration has too many members'
                return
            end if
            do j = 1, schema%declarations(i)%member_count
                if (len_trim(schema%declarations(i)%members(j)%name) == 0) then
                    message = 'schema member has an empty name'
                    return
                end if
                do k = 1, j - 1
                    if (trim(schema%declarations(i)%members(j)%name) == &
                        trim(schema%declarations(i)%members(k)%name)) then
                        message = 'duplicate member in '// &
                            trim(schema%declarations(i)%name)//': '// &
                            trim(schema%declarations(i)%members(j)%name)
                        return
                    end if
                end do
                if (schema%declarations(i)%kind == schema_record) then
                    if (.not. known_type(schema, &
                        schema%declarations(i)%members(j)%type_name)) then
                        message = 'unknown member type: '// &
                            trim(schema%declarations(i)%members(j)%type_name)
                        return
                    end if
                end if
            end do
            if (schema%declarations(i)%kind == schema_list .or. &
                schema%declarations(i)%kind == schema_optional) then
                if (.not. known_type(schema, schema%declarations(i)%target_type)) then
                    message = 'unknown target type: '// &
                        trim(schema%declarations(i)%target_type)
                    return
                end if
            end if
        end do
        ok = .true.
    end subroutine schema_validate

    subroutine parse_declaration(node, declaration, ok, message)
        type(sx_node_t), intent(in) :: node
        type(schema_declaration_t), intent(out) :: declaration
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=schema_name_length) :: kind_name
        integer :: i

        call clear_declaration(declaration)
        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'schema declaration is not a list'
            return
        end if
        if (node%child_count < 2) then
            message = 'schema declaration needs a kind and name'
            return
        end if
        call read_atom(node%children(1), kind_name, ok, message)
        if (.not. ok) return
        call read_atom(node%children(2), declaration%name, ok, message)
        if (.not. ok) return

        select case (trim(kind_name))
        case ('primitive')
            declaration%kind = schema_primitive
            if (node%child_count /= 2) then
                message = 'primitive declaration has the wrong field count'
                return
            end if
        case ('record')
            declaration%kind = schema_record
            do i = 3, node%child_count
                call parse_record_member(node%children(i), declaration, ok, message)
                if (.not. ok) return
            end do
        case ('sum')
            declaration%kind = schema_sum
            call parse_named_members(node, declaration, ok, message)
            if (.not. ok) return
        case ('list')
            declaration%kind = schema_list
            if (node%child_count /= 3) then
                message = 'list declaration has the wrong field count'
                return
            end if
            call read_atom(node%children(3), declaration%target_type, ok, message)
            if (.not. ok) return
        case ('optional')
            declaration%kind = schema_optional
            if (node%child_count /= 3) then
                message = 'optional declaration has the wrong field count'
                return
            end if
            call read_atom(node%children(3), declaration%target_type, ok, message)
            if (.not. ok) return
        case ('enum')
            declaration%kind = schema_enum
            call parse_named_members(node, declaration, ok, message)
            if (.not. ok) return
        case default
            message = 'unknown schema declaration kind: '//trim(kind_name)
            return
        end select
        ok = .true.
        message = ''
    end subroutine parse_declaration

    subroutine parse_record_member(node, declaration, ok, message)
        type(sx_node_t), intent(in) :: node
        type(schema_declaration_t), intent(inout) :: declaration
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'record member is not a list'
            return
        end if
        if (node%child_count /= 2) then
            message = 'record member needs a name and type'
            return
        end if
        call append_member(declaration, node%children(1), node%children(2), ok, message)
    end subroutine parse_record_member

    subroutine parse_named_members(node, declaration, ok, message)
        type(sx_node_t), intent(in) :: node
        type(schema_declaration_t), intent(inout) :: declaration
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=schema_name_length) :: member_name
        integer :: i

        ok = .false.
        message = ''
        if (node%child_count < 3) then
            message = 'sum or enum declaration has no members'
            return
        end if
        do i = 3, node%child_count
            call read_atom(node%children(i), member_name, ok, message)
            if (.not. ok) return
            call append_named_member(declaration, member_name, ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine parse_named_members

    subroutine append_member(declaration, name_node, type_node, ok, message)
        type(schema_declaration_t), intent(inout) :: declaration
        type(sx_node_t), intent(in) :: name_node, type_node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: member

        ok = .false.
        message = ''
        if (declaration%member_count >= schema_max_members) then
            message = 'schema declaration has too many members'
            return
        end if
        member = declaration%member_count + 1
        call read_atom(name_node, declaration%members(member)%name, ok, message)
        if (.not. ok) return
        call read_atom(type_node, declaration%members(member)%type_name, ok, message)
        if (.not. ok) return
        declaration%member_count = member
        ok = .true.
    end subroutine append_member

    subroutine append_named_member(declaration, name, ok, message)
        type(schema_declaration_t), intent(inout) :: declaration
        character(len=*), intent(in) :: name
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: member

        ok = .false.
        message = ''
        if (declaration%member_count >= schema_max_members) then
            message = 'schema declaration has too many members'
            return
        end if
        member = declaration%member_count + 1
        declaration%members(member)%name = trim(name)
        declaration%member_count = member
        ok = .true.
    end subroutine append_named_member

    subroutine read_atom(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        value = ''
        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'schema name is not an atom'
            return
        end if
        if (len_trim(node%atom) == 0) then
            message = 'schema name is empty'
            return
        end if
        if (len_trim(node%atom) > len(value)) then
            message = 'schema name exceeds storage'
            return
        end if
        value = trim(node%atom)
        ok = .true.
    end subroutine read_atom

    logical function known_type(schema, name)
        type(schema_t), intent(in) :: schema
        character(len=*), intent(in) :: name

        integer :: i

        known_type = .false.
        if (len_trim(name) == 0) return
        select case (trim(name))
        case ('bool', 'int', 'status', 'node', 'symbol', 'type', 'scope', 'name', &
                'string')
            known_type = .true.
            return
        end select
        do i = 1, schema%declaration_count
            if (trim(schema%declarations(i)%name) == trim(name)) then
                known_type = .true.
                return
            end if
        end do
    end function known_type

    subroutine clear_schema(schema)
        type(schema_t), intent(out) :: schema
        integer :: i, j

        schema%name = ''
        schema%declaration_count = 0
        schema%declarations%kind = 0
        schema%declarations%name = ''
        schema%declarations%target_type = ''
        schema%declarations%member_count = 0
        do i = 1, schema_max_declarations
            do j = 1, schema_max_members
                schema%declarations(i)%members(j)%name = ''
                schema%declarations(i)%members(j)%type_name = ''
            end do
        end do
    end subroutine clear_schema

    subroutine clear_declaration(declaration)
        type(schema_declaration_t), intent(out) :: declaration
        integer :: i

        declaration%kind = 0
        declaration%name = ''
        declaration%target_type = ''
        declaration%member_count = 0
        do i = 1, schema_max_members
            declaration%members(i)%name = ''
            declaration%members(i)%type_name = ''
        end do
    end subroutine clear_declaration

end module schema_ir

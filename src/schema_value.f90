module schema_value
    !! Validate and write the canonical SX value of a schema declaration.
    !!
    !! This is the generic reference path for the value contract.  Generated
    !! readers and writers may specialize it later, but their accepted tree and
    !! byte order must remain identical to this path.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    use schema_ir, only: schema_declaration_t, schema_enum, schema_list, &
        schema_optional, schema_primitive, schema_record, schema_sum, schema_t, &
        schema_validate
    implicit none
    private

    public :: schema_value_canonicalize

contains

    subroutine schema_value_canonicalize(schema, root_type, text, unit, ok, message)
        type(schema_t), intent(in) :: schema
        character(len=*), intent(in) :: root_type, text
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t) :: node

        call schema_validate(schema, ok, message)
        if (.not. ok) return
        call sx_parse(text, node, ok, message)
        if (.not. ok) return
        call write_value(schema, trim(root_type), node, unit, ok, message)
        if (.not. ok) return
        call emit(unit, '', ok, message)
    end subroutine schema_value_canonicalize

    recursive subroutine write_value(schema, type_name, node, unit, ok, message)
        type(schema_t), intent(in) :: schema
        character(len=*), intent(in) :: type_name
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(schema_declaration_t) :: declaration
        integer :: declaration_number

        ok = .false.
        message = ''
        declaration_number = declaration_index(schema, type_name)
        if (declaration_number > 0) then
            declaration = schema%declarations(declaration_number)
            select case (declaration%kind)
            case (schema_primitive)
                call write_primitive(type_name, node, unit, ok, message)
            case (schema_record)
                call write_record(schema, declaration, node, unit, ok, message)
            case (schema_sum)
                call write_sum(schema, declaration, node, unit, ok, message)
            case (schema_list)
                call write_list(schema, declaration, node, unit, ok, message)
            case (schema_optional)
                call write_optional(schema, declaration, node, unit, ok, message)
            case (schema_enum)
                call write_enum(declaration, node, unit, ok, message)
            case default
                message = 'schema declaration kind is invalid'
            end select
            return
        end if
        if (is_builtin_primitive(type_name)) then
            call write_primitive(type_name, node, unit, ok, message)
        else
            message = 'unknown schema value type: '//trim(type_name)
        end if
    end subroutine write_value

    subroutine write_primitive(type_name, node, unit, ok, message)
        character(len=*), intent(in) :: type_name
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: atom

        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'primitive schema value is not an atom'
            return
        end if
        atom = trim(node%atom)
        select case (trim(type_name))
        case ('bool')
            if (atom /= 'true' .and. atom /= 'false') then
                message = 'boolean schema value is not true or false'
                return
            end if
            call emit_trimmed(unit, atom, ok, message)
        case ('int')
            if (.not. is_decimal_integer(atom)) then
                message = 'integer schema value is not canonical decimal'
                return
            end if
            call emit_trimmed(unit, atom, ok, message)
        case ('string')
            call emit_quoted(unit, atom, ok, message)
        case ('name', 'status')
            if (.not. is_plain_atom(atom)) then
                message = 'name schema value is not a canonical atom'
                return
            end if
            call emit_trimmed(unit, atom, ok, message)
        case default
            message = 'primitive schema value type is unsupported: '//trim(type_name)
        end select
    end subroutine write_primitive

    subroutine write_record(schema, declaration, node, unit, ok, message)
        type(schema_t), intent(in) :: schema
        type(schema_declaration_t), intent(in) :: declaration
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'record schema value is not a list'
            return
        end if
        if (node%child_count /= declaration%member_count + 1) then
            message = 'record schema value has the wrong field count'
            return
        end if
        call emit(unit, '(', ok, message)
        if (.not. ok) return
        call emit(unit, trim(declaration%name), ok, message)
        if (.not. ok) return
        do i = 1, declaration%member_count
            if (node%children(i + 1)%kind /= sx_list) then
                ok = .false.
                message = 'record field is not a named pair'
                return
            end if
            if (node%children(i + 1)%child_count /= 2) then
                ok = .false.
                message = 'record field pair has the wrong length'
                return
            end if
            if (node%children(i + 1)%children(1)%kind /= sx_atom) then
                ok = .false.
                message = 'record field name is not an atom'
                return
            end if
            if (trim(node%children(i + 1)%children(1)%atom) /= &
                trim(declaration%members(i)%name)) then
                ok = .false.
                message = 'record fields are not in schema order'
                return
            end if
            call emit(unit, ' (', ok, message)
            if (.not. ok) return
            call emit(unit, trim(declaration%members(i)%name), ok, message)
            if (.not. ok) return
            call emit(unit, ' ', ok, message)
            if (.not. ok) return
            call write_value(schema, declaration%members(i)%type_name, &
                node%children(i + 1)%children(2), unit, ok, message)
            if (.not. ok) return
            call emit(unit, ')', ok, message)
            if (.not. ok) return
        end do
        call emit(unit, ')', ok, message)
    end subroutine write_record

    subroutine write_sum(schema, declaration, node, unit, ok, message)
        type(schema_t), intent(in) :: schema
        type(schema_declaration_t), intent(in) :: declaration
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: variant

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'sum schema value is not a variant list'
            return
        end if
        if (node%child_count < 1 .or. node%child_count > 2) then
            message = 'sum schema value has the wrong field count'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'sum variant name is not an atom'
            return
        end if
        variant = member_index(declaration, node%children(1)%atom)
        if (variant == 0) then
            message = 'unknown sum variant: '//trim(node%children(1)%atom)
            return
        end if
        if (len_trim(declaration%members(variant)%type_name) == 0) then
            if (node%child_count /= 1) then
                message = 'payload-less sum variant has a payload'
                return
            end if
        else
            if (node%child_count /= 2) then
                message = 'sum variant payload is missing'
                return
            end if
        end if
        call emit(unit, '(', ok, message)
        if (.not. ok) return
        call emit(unit, trim(declaration%members(variant)%name), ok, message)
        if (.not. ok) return
        if (node%child_count == 2) then
            call emit(unit, ' ', ok, message)
            if (.not. ok) return
            call write_value(schema, declaration%members(variant)%type_name, &
                node%children(2), unit, ok, message)
            if (.not. ok) return
        end if
        call emit(unit, ')', ok, message)
    end subroutine write_sum

    subroutine write_list(schema, declaration, node, unit, ok, message)
        type(schema_t), intent(in) :: schema
        type(schema_declaration_t), intent(in) :: declaration
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_list) then
            message = 'list schema value is not a list'
            return
        end if
        if (node%child_count < 1) then
            message = 'list schema value has no declaration tag'
            return
        end if
        if (node%children(1)%kind /= sx_atom .or. &
            trim(node%children(1)%atom) /= trim(declaration%name)) then
            message = 'list schema value has the wrong declaration tag'
            return
        end if
        call emit(unit, '(', ok, message)
        if (.not. ok) return
        call emit(unit, trim(declaration%name), ok, message)
        if (.not. ok) return
        do i = 2, node%child_count
            call emit(unit, ' ', ok, message)
            if (.not. ok) return
            call write_value(schema, declaration%target_type, node%children(i), unit, ok, &
                message)
            if (.not. ok) return
        end do
        call emit(unit, ')', ok, message)
    end subroutine write_list

    subroutine write_optional(schema, declaration, node, unit, ok, message)
        type(schema_t), intent(in) :: schema
        type(schema_declaration_t), intent(in) :: declaration
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        ok = .false.
        message = ''
        if (node%kind == sx_atom) then
            if (trim(node%atom) /= 'none') then
                message = 'optional absence must be none'
                return
            end if
            call emit(unit, 'none', ok, message)
            return
        end if
        if (node%kind /= sx_list) then
            message = 'optional schema value is not none or some'
            return
        end if
        if (node%child_count /= 2) then
            message = 'optional presence must be (some value)'
            return
        end if
        if (node%children(1)%kind /= sx_atom) then
            message = 'optional presence must be (some value)'
            return
        end if
        if (trim(node%children(1)%atom) /= 'some') then
            message = 'optional presence must be (some value)'
            return
        end if
        call emit(unit, '(some ', ok, message)
        if (.not. ok) return
        call write_value(schema, declaration%target_type, node%children(2), unit, ok, message)
        if (.not. ok) return
        call emit(unit, ')', ok, message)
    end subroutine write_optional

    subroutine write_enum(declaration, node, unit, ok, message)
        type(schema_declaration_t), intent(in) :: declaration
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        if (node%kind /= sx_atom) then
            message = 'enum schema value is not an atom'
            return
        end if
        do i = 1, declaration%member_count
            if (trim(node%atom) == trim(declaration%members(i)%name)) then
                call emit(unit, trim(declaration%members(i)%name), ok, message)
                return
            end if
        end do
        message = 'unknown enum value: '//trim(node%atom)
    end subroutine write_enum

    integer function declaration_index(schema, name) result(index)
        type(schema_t), intent(in) :: schema
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, schema%declaration_count
            if (trim(schema%declarations(i)%name) == trim(name)) then
                index = i
                return
            end if
        end do
    end function declaration_index

    integer function member_index(declaration, name) result(index)
        type(schema_declaration_t), intent(in) :: declaration
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, declaration%member_count
            if (trim(declaration%members(i)%name) == trim(name)) then
                index = i
                return
            end if
        end do
    end function member_index

    logical function is_builtin_primitive(name)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ('bool', 'int', 'status', 'name', 'string')
            is_builtin_primitive = .true.
        case default
            is_builtin_primitive = .false.
        end select
    end function is_builtin_primitive

    logical function is_plain_atom(atom)
        character(len=*), intent(in) :: atom
        integer :: i

        is_plain_atom = len_trim(atom) > 0
        if (.not. is_plain_atom) return
        do i = 1, len_trim(atom)
            if (atom(i:i) == ' ' .or. atom(i:i) == achar(9) .or. &
                atom(i:i) == '(' .or. atom(i:i) == ')' .or. &
                atom(i:i) == '"' .or. atom(i:i) == achar(92)) then
                is_plain_atom = .false.
                return
            end if
        end do
    end function is_plain_atom

    logical function is_decimal_integer(atom)
        character(len=*), intent(in) :: atom
        integer :: i, first, n, code

        n = len_trim(atom)
        is_decimal_integer = n > 0
        if (.not. is_decimal_integer) return
        first = 1
        if (atom(1:1) == '-') then
            first = 2
            if (first > n) then
                is_decimal_integer = .false.
                return
            end if
        end if
        if (n - first + 1 > 1 .and. atom(first:first) == '0') then
            is_decimal_integer = .false.
            return
        end if
        do i = first, n
            code = iachar(atom(i:i))
            if (code < iachar('0') .or. code > iachar('9')) then
                is_decimal_integer = .false.
                return
            end if
        end do
    end function is_decimal_integer

    subroutine emit(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        write (unit, '(a)', advance='no', iostat=ios) text
        ok = ios == 0
        message = ''
        if (.not. ok) message = 'cannot write canonical schema value'
    end subroutine emit

    subroutine emit_trimmed(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: ios

        write (unit, '(a)', advance='no', iostat=ios) trim(text)
        ok = ios == 0
        message = ''
        if (.not. ok) message = 'cannot write canonical schema value'
    end subroutine emit_trimmed

    subroutine emit_quoted(unit, text, ok, message)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i

        call emit(unit, '"', ok, message)
        if (.not. ok) return
        do i = 1, len_trim(text)
            if (text(i:i) == '"' .or. text(i:i) == achar(92)) then
                call emit(unit, achar(92), ok, message)
                if (.not. ok) return
            end if
            call emit(unit, text(i:i), ok, message)
            if (.not. ok) return
        end do
        call emit(unit, '"', ok, message)
    end subroutine emit_quoted

end module schema_value

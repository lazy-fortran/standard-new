module standardir_grammar_fact_codegen
    !! Generate bounded type-spec grammar-fact consumers from SX sources.

    use fortsx, only: sx_list, sx_node_t
    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, RESOLUTION_RESOLVED, &
        schema_validate_grammar_fact
    use standardir_syntax_fields, only: standardir_atom_equals, &
        standardir_read_pair, standardir_read_source
    implicit none
    private

    public :: standardir_generate_integer_type_spec_fact
    public :: standardir_generate_real_type_spec_fact
    public :: standardir_generate_double_precision_type_spec_fact
    public :: standardir_generate_complex_type_spec_fact
    public :: standardir_generate_logical_type_spec_fact
    public :: standardir_generate_character_type_spec_fact
    public :: standardir_generate_program_grammar_fact
    public :: standardir_generate_execution_part_grammar_fact
    public :: standardir_generate_stop_stmt_grammar_fact
    public :: standardir_generate_stop_code_grammar_fact
    public :: standardir_generate_print_stmt_grammar_fact
    public :: standardir_generate_format_grammar_fact
    public :: standardir_generate_output_item_grammar_fact
    public :: standardir_generate_int_literal_constant_grammar_fact
    public :: standardir_generate_assignment_stmt_grammar_fact
    public :: standardir_generate_level_2_expr_grammar_fact
    public :: standardir_generate_add_operand_grammar_fact
    public :: standardir_generate_mult_op_grammar_fact
    public :: standardir_generate_div_op_grammar_fact
    public :: standardir_generate_add_op_grammar_fact
    public :: standardir_generate_add_op_en_dash_grammar_fact
    public :: standardir_generate_expression_fact_table
    public :: standardir_generate_intrinsic_type_spec_lookup
    public :: standardir_generate_designator_grammar_fact
    public :: standardir_generate_variable_grammar_fact
    public :: standardir_generate_variable_name_grammar_fact

contains

    subroutine standardir_generate_integer_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R705', 'standardir_grammar_fact', &
            'integer_type_spec', 'integer', ok, message)
    end subroutine standardir_generate_integer_type_spec_fact

    subroutine standardir_generate_real_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R706', 'standardir_real_type_spec_fact', &
            'real_type_spec', 'real', ok, message)
    end subroutine standardir_generate_real_type_spec_fact

    subroutine standardir_generate_double_precision_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R707', &
            'standardir_double_precision_type_spec_fact', 'double_precision_type_spec', &
            'double precision', ok, message)
    end subroutine standardir_generate_double_precision_type_spec_fact

    subroutine standardir_generate_complex_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R704', 'standardir_complex_type_spec_fact', &
            'complex_type_spec', 'complex', ok, message)
    end subroutine standardir_generate_complex_type_spec_fact

    subroutine standardir_generate_logical_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R704', 'standardir_logical_type_spec_fact', &
            'logical_type_spec', 'logical', ok, message)
    end subroutine standardir_generate_logical_type_spec_fact

    subroutine standardir_generate_character_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R704', 'standardir_character_type_spec_fact', &
            'character_type_spec', 'character', ok, message)
    end subroutine standardir_generate_character_type_spec_fact

    subroutine standardir_generate_program_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R501', 'standardir_program_grammar_fact', &
            'program_grammar', 'program', ok, message)
    end subroutine standardir_generate_program_grammar_fact

    subroutine standardir_generate_execution_part_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R509', 'standardir_execution_part_grammar_fact', &
            'execution_part_grammar', 'execution-part', ok, message)
    end subroutine standardir_generate_execution_part_grammar_fact

    subroutine standardir_generate_stop_stmt_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1162', 'standardir_stop_stmt_grammar_fact', &
            'stop_stmt_grammar', 'stop-stmt', ok, message)
    end subroutine standardir_generate_stop_stmt_grammar_fact

    subroutine standardir_generate_stop_code_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1164', 'standardir_stop_code_grammar_fact', &
            'stop_code_grammar', 'stop-code', ok, message)
    end subroutine standardir_generate_stop_code_grammar_fact

    subroutine standardir_generate_print_stmt_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1212', 'standardir_print_stmt_grammar_fact', &
            'print_stmt_grammar', 'print-stmt', ok, message)
    end subroutine standardir_generate_print_stmt_grammar_fact

    subroutine standardir_generate_format_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1215', 'standardir_format_grammar_fact', &
            'format_grammar', 'format', ok, message)
    end subroutine standardir_generate_format_grammar_fact

    subroutine standardir_generate_output_item_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1217', 'standardir_output_item_grammar_fact', &
            'output_item_grammar', 'output-item', ok, message)
    end subroutine standardir_generate_output_item_grammar_fact

    subroutine standardir_generate_int_literal_constant_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R708', &
            'standardir_int_literal_constant_grammar_fact', 'int_literal_constant_grammar', &
            'int-literal-constant', ok, message)
    end subroutine standardir_generate_int_literal_constant_grammar_fact

    subroutine standardir_generate_assignment_stmt_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1033', &
            'standardir_assignment_stmt_grammar_fact', 'assignment_stmt_grammar', &
            'assignment-stmt', ok, message)
    end subroutine standardir_generate_assignment_stmt_grammar_fact

    subroutine standardir_generate_level_2_expr_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1007', &
            'standardir_level_2_expr_grammar_fact', 'level_2_expr_grammar', &
            'level-2-expr', ok, message)
    end subroutine standardir_generate_level_2_expr_grammar_fact

    subroutine standardir_generate_add_operand_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1006', &
            'standardir_add_operand_grammar_fact', 'add_operand_grammar', &
            'add-operand', ok, message)
    end subroutine standardir_generate_add_operand_grammar_fact

    subroutine standardir_generate_mult_op_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1009', 'standardir_mult_op_grammar_fact', &
            'mult_op_grammar', 'mult-op', ok, message)
    end subroutine standardir_generate_mult_op_grammar_fact

    subroutine standardir_generate_div_op_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1009', 'standardir_div_op_grammar_fact', &
            'div_op_grammar', 'div-op', ok, message)
    end subroutine standardir_generate_div_op_grammar_fact

    subroutine standardir_generate_add_op_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1010', &
            'standardir_add_op_grammar_fact', 'add_op_grammar', 'add-op', ok, message)
    end subroutine standardir_generate_add_op_grammar_fact

    subroutine standardir_generate_add_op_en_dash_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R1010', &
            'standardir_add_op_en_dash_grammar_fact', 'add_op_en_dash_grammar', 'add-op', ok, message)
    end subroutine standardir_generate_add_op_en_dash_grammar_fact

    subroutine standardir_generate_expression_fact_table(nodes, unit, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(grammar_fact_t), allocatable :: facts(:)
        type(grammar_fact_t) :: value
        integer :: i, count

        ok = .false.
        message = ''
        allocate (facts(size(nodes)))
        count = 0
        do i = 1, size(nodes)
            call read_grammar_fact_value(nodes(i), value, ok, message)
            if (.not. ok) return
            if (.not. expression_fact_rule(value)) cycle
            count = count + 1
            facts(count) = value
        end do
        if (count == 0) then
            message = 'expression-fact table has no bounded grammar facts'
            return
        end if

        call emit_expression_fact_table(unit, facts(:count))
        ok = .true.

    contains

        logical function expression_fact_rule(value)
            type(grammar_fact_t), intent(in) :: value

            expression_fact_rule = trim(value%source%clause) == '10' .and. &
                trim(value%id) == trim(value%source%rule) .and. &
                (trim(value%id) == 'R1006' .or. trim(value%id) == 'R1009' .or. &
                trim(value%id) == 'R1010')
        end function expression_fact_rule

    end subroutine standardir_generate_expression_fact_table

    subroutine standardir_generate_designator_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R901', 'standardir_designator_grammar_fact', &
            'designator_grammar', 'designator', ok, message)
    end subroutine standardir_generate_designator_grammar_fact

    subroutine standardir_generate_variable_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R902', 'standardir_variable_grammar_fact', &
            'variable_grammar', 'variable', ok, message)
    end subroutine standardir_generate_variable_grammar_fact

    subroutine standardir_generate_variable_name_grammar_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R903', 'standardir_variable_name_grammar_fact', &
            'variable_name_grammar', 'variable-name', ok, message)
    end subroutine standardir_generate_variable_name_grammar_fact

    subroutine emit_expression_fact_table(unit, facts)
        integer, intent(in) :: unit
        type(grammar_fact_t), intent(in) :: facts(:)

        integer :: i
        character(len=32) :: count_text

        write (count_text, '(i0)') size(facts)
        call emit_line(unit, 'module standardir_expression_fact_generated')
        call emit_line(unit, '    !! Generated from specs/grammar-facts-v0.sx; do not edit.')
        call emit_line(unit, '')
        call emit_line(unit, '    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, &')
        call emit_line(unit, '        RESOLUTION_RESOLVED')
        call emit_line(unit, '    implicit none')
        call emit_line(unit, '    private')
        call emit_line(unit, '')
        call emit_line(unit, '    type, public :: standardir_expression_fact_t')
        call emit_line(unit, '        type(grammar_fact_t) :: fact')
        call emit_line(unit, '    end type standardir_expression_fact_t')
        call emit_line(unit, '')
        call emit_line(unit, '    integer, parameter, public :: standardir_expression_fact_count = '// &
            trim(count_text))
        call emit_line(unit, '    public :: standardir_make_expression_fact_table')
        call emit_line(unit, '    public :: standardir_lookup_expression_fact')
        call emit_line(unit, '')
        call emit_line(unit, 'contains')
        call emit_line(unit, '')
        call emit_line(unit, '    subroutine standardir_make_expression_fact_table(values)')
        call emit_line(unit, '        type(standardir_expression_fact_t), intent(out) :: values('// &
            trim(count_text)//')')
        call emit_line(unit, '')
        do i = 1, size(facts)
            call emit_fact_assignment(unit, i, facts(i))
        end do
        call emit_line(unit, '    end subroutine standardir_make_expression_fact_table')
        call emit_line(unit, '')
        call emit_line(unit, '    subroutine standardir_lookup_expression_fact(id, expression, value, found)')
        call emit_line(unit, '        character(len=*), intent(in) :: id, expression')
        call emit_line(unit, '        type(standardir_expression_fact_t), intent(out) :: value')
        call emit_line(unit, '        logical, intent(out) :: found')
        call emit_line(unit, '')
        call emit_line(unit, '        type(standardir_expression_fact_t) :: values('//trim(count_text)//')')
        call emit_line(unit, '        integer :: i')
        call emit_line(unit, '')
        call emit_line(unit, '        call standardir_make_expression_fact_table(values)')
        call emit_line(unit, '        value%fact%id = ''''')
        call emit_line(unit, '        value%fact%expression = ''''')
        call emit_line(unit, '        value%fact%source%document = ''''')
        call emit_line(unit, '        value%fact%source%clause = ''''')
        call emit_line(unit, '        value%fact%source%rule = ''''')
        call emit_line(unit, '        value%fact%source%page = 0')
        call emit_line(unit, '        value%fact%source%source_hash = ''''')
        call emit_line(unit, '        value%fact%origin = 0')
        call emit_line(unit, '        value%fact%resolution = 0')
        call emit_line(unit, '        found = .false.')
        call emit_line(unit, '        do i = 1, size(values)')
        call emit_line(unit, '            if (trim(values(i)%fact%id) == trim(id) .and. &')
        call emit_line(unit, '                trim(values(i)%fact%expression) == trim(expression)) then')
        call emit_line(unit, '                value = values(i)')
        call emit_line(unit, '                found = .true.')
        call emit_line(unit, '                return')
        call emit_line(unit, '            end if')
        call emit_line(unit, '        end do')
        call emit_line(unit, '    end subroutine standardir_lookup_expression_fact')
        call emit_line(unit, '')
        call emit_line(unit, 'end module standardir_expression_fact_generated')
    end subroutine emit_expression_fact_table

    subroutine emit_fact_assignment(unit, index, fact)
        integer, intent(in) :: unit, index
        type(grammar_fact_t), intent(in) :: fact
        call emit_fact_text(unit, index, 'id', fact%id)
        call emit_fact_text(unit, index, 'expression', fact%expression)
        call emit_fact_text(unit, index, 'source%document', fact%source%document)
        call emit_fact_text(unit, index, 'source%clause', fact%source%clause)
        call emit_fact_text(unit, index, 'source%rule', fact%source%rule)
        call emit_fact_integer(unit, index, 'source%page', fact%source%page)
        call emit_fact_text(unit, index, 'source%source_hash', fact%source%source_hash)
        call emit_fact_constant(unit, index, 'origin', origin_name(fact%origin))
        call emit_fact_constant(unit, index, 'resolution', resolution_name(fact%resolution))
    end subroutine emit_fact_assignment

    subroutine emit_fact_text(unit, index, field, value)
        integer, intent(in) :: unit, index
        character(len=*), intent(in) :: field, value
        character(len=2048) :: line

        write (line, '(a,i0,a,a,a,a,a)') '        values(', index, ')%fact%', trim(field), &
            ' = ''', trim(value), ''''
        call emit_line(unit, trim(line))
    end subroutine emit_fact_text

    subroutine emit_fact_integer(unit, index, field, value)
        integer, intent(in) :: unit, index, value
        character(len=*), intent(in) :: field
        character(len=256) :: line

        write (line, '(a,i0,a,a,a,i0)') '        values(', index, ')%fact%', trim(field), &
            ' = ', value
        call emit_line(unit, trim(line))
    end subroutine emit_fact_integer

    subroutine emit_fact_constant(unit, index, field, value)
        integer, intent(in) :: unit, index
        character(len=*), intent(in) :: field, value
        character(len=256) :: line

        write (line, '(a,i0,a,a,a,a)') '        values(', index, ')%fact%', trim(field), ' = ', trim(value)
        call emit_line(unit, trim(line))
    end subroutine emit_fact_constant

    character(len=32) function origin_name(origin)
        integer, intent(in) :: origin

        if (origin == ORIGIN_MECHANICAL) then
            origin_name = 'ORIGIN_MECHANICAL'
        else
            origin_name = '0'
        end if
    end function origin_name

    character(len=32) function resolution_name(resolution)
        integer, intent(in) :: resolution

        if (resolution == RESOLUTION_RESOLVED) then
            resolution_name = 'RESOLUTION_RESOLVED'
        else
            resolution_name = '0'
        end if
    end function resolution_name

    subroutine standardir_generate_intrinsic_type_spec_lookup(nodes, unit, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(grammar_fact_t) :: facts(6)
        logical :: found(6)
        integer :: i

        ok = .false.
        message = ''
        found = .false.
        if (size(nodes) /= 6) then
            message = 'intrinsic type-spec lookup requires R704 COMPLEX, R704 LOGICAL, R704 CHARACTER, R705, R706 and R707'
            return
        end if
        do i = 1, size(nodes)
            call read_grammar_fact_value(nodes(i), facts(i), ok, message)
            if (.not. ok) return
            call collect_fact(facts(i), ok, message)
            if (.not. ok) return
        end do
        if (.not. all(found)) then
            message = 'intrinsic type-spec lookup is missing a bounded grammar fact'
            return
        end if

        call emit_lookup(unit, facts)
        ok = .true.

    contains

        subroutine collect_fact(value, callback_ok, callback_message)
            type(grammar_fact_t), intent(in) :: value
            logical, intent(out) :: callback_ok
            character(len=*), intent(out) :: callback_message

            integer :: index

            index = intrinsic_type_spec_index(value)
            callback_ok = index > 0
            if (callback_ok) callback_ok = .not. found(index)
            if (callback_ok .and. index == 4) callback_ok = trim(value%expression) == 'or COMPLEX [ kind-selector ]'
            if (callback_ok .and. index == 5) callback_ok = trim(value%expression) == 'or LOGICAL [ kind-selector ]'
            if (callback_ok .and. index == 6) callback_ok = trim(value%expression) == 'or CHARACTER [ char-selector ]'
            if (callback_ok) callback_ok = intrinsic_source_matches(value)
            callback_message = ''
            if (.not. callback_ok) then
                callback_message = 'intrinsic type-spec lookup has an unknown or duplicate rule'
                return
            end if
            facts(index) = value
            found(index) = .true.
        end subroutine collect_fact

        logical function intrinsic_source_matches(value)
            type(grammar_fact_t), intent(in) :: value

            intrinsic_source_matches = trim(value%source%document) == 'J3-24-007' .and. &
                trim(value%source%clause) == '7' .and. trim(value%source%source_hash) == &
                '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
            if (.not. intrinsic_source_matches) return
            select case (trim(value%id))
            case ('R705', 'R706', 'R707')
                intrinsic_source_matches = value%source%page == 67 .and. &
                    trim(value%source%rule) == trim(value%id)
            case ('R704')
                intrinsic_source_matches = value%source%page == 80 .and. &
                    trim(value%source%rule) == 'R704'
            case default
                intrinsic_source_matches = .false.
            end select
        end function intrinsic_source_matches

        integer function intrinsic_type_spec_index(value)
            type(grammar_fact_t), intent(in) :: value

            intrinsic_type_spec_index = 0
            select case (trim(value%id))
            case ('R705')
                intrinsic_type_spec_index = 1
            case ('R706')
                intrinsic_type_spec_index = 2
            case ('R707')
                intrinsic_type_spec_index = 3
            case ('R704')
                if (trim(value%expression) == 'or COMPLEX [ kind-selector ]') then
                    intrinsic_type_spec_index = 4
                else if (trim(value%expression) == 'or LOGICAL [ kind-selector ]') then
                    intrinsic_type_spec_index = 5
                else if (trim(value%expression) == 'or CHARACTER [ char-selector ]') then
                    intrinsic_type_spec_index = 6
                end if
            end select
        end function intrinsic_type_spec_index

    end subroutine standardir_generate_intrinsic_type_spec_lookup

    subroutine emit_lookup(unit, facts)
        integer, intent(in) :: unit
        type(grammar_fact_t), intent(in) :: facts(:)

        integer :: i
        character(len=32) :: canonical_name

        call emit_line(unit, 'module standardir_intrinsic_type_spec_generated')
        call emit_line(unit, '    !! Generated from specs/grammar-facts-v0.sx; do not edit.')
        call emit_line(unit, '')
        call emit_line(unit, '    implicit none')
        call emit_line(unit, '    private')
        call emit_line(unit, '')
        call emit_line(unit, '    type, public :: standardir_intrinsic_type_spec_t')
        call emit_line(unit, '        character(len=32) :: canonical_name = ''''')
        call emit_line(unit, '        character(len=128) :: source_spelling = ''''')
        call emit_line(unit, '        character(len=64) :: source_rule = ''''')
        call emit_line(unit, '        character(len=128) :: document = ''''')
        call emit_line(unit, '        character(len=64) :: clause = ''''')
        call emit_line(unit, '        integer :: page = 0')
        call emit_line(unit, '        character(len=64) :: source_hash = ''''')
        call emit_line(unit, '    end type standardir_intrinsic_type_spec_t')
        call emit_line(unit, '')
        call emit_line(unit, '    integer, parameter, public :: standardir_intrinsic_type_spec_count = 6')
        call emit_line(unit, '    public :: standardir_make_intrinsic_type_spec_lookup')
        call emit_line(unit, '    public :: standardir_lookup_intrinsic_type_spec')
        call emit_line(unit, '')
        call emit_line(unit, 'contains')
        call emit_line(unit, '')
        call emit_line(unit, '    subroutine standardir_make_intrinsic_type_spec_lookup(values)')
        call emit_line(unit, '        type(standardir_intrinsic_type_spec_t), intent(out) :: values(6)')
        call emit_line(unit, '')
        do i = 1, size(facts)
            canonical_name = intrinsic_canonical_name(facts(i))
            call emit_assignment(unit, i, 'canonical_name', canonical_name)
            call emit_assignment(unit, i, 'source_spelling', intrinsic_source_spelling(facts(i)))
            call emit_assignment(unit, i, 'source_rule', facts(i)%source%rule)
            call emit_assignment(unit, i, 'document', facts(i)%source%document)
            call emit_assignment(unit, i, 'clause', facts(i)%source%clause)
            call emit_integer_assignment(unit, i, 'page', facts(i)%source%page)
            call emit_assignment(unit, i, 'source_hash', facts(i)%source%source_hash)
        end do
        call emit_line(unit, '    end subroutine standardir_make_intrinsic_type_spec_lookup')
        call emit_line(unit, '')
        call emit_line(unit, '    subroutine standardir_lookup_intrinsic_type_spec(source_spelling, value, found)')
        call emit_line(unit, '        character(len=*), intent(in) :: source_spelling')
        call emit_line(unit, '        type(standardir_intrinsic_type_spec_t), intent(out) :: value')
        call emit_line(unit, '        logical, intent(out) :: found')
        call emit_line(unit, '')
        call emit_line(unit, '        type(standardir_intrinsic_type_spec_t) :: values(6)')
        call emit_line(unit, '        integer :: i')
        call emit_line(unit, '')
        call emit_line(unit, '        call standardir_make_intrinsic_type_spec_lookup(values)')
        call emit_line(unit, '        value = standardir_intrinsic_type_spec_t()')
        call emit_line(unit, '        found = .false.')
        call emit_line(unit, '        do i = 1, size(values)')
        call emit_line(unit, '            if (trim(values(i)%source_spelling) == trim(source_spelling)) then')
        call emit_line(unit, '                value = values(i)')
        call emit_line(unit, '                found = .true.')
        call emit_line(unit, '                return')
        call emit_line(unit, '            end if')
        call emit_line(unit, '        end do')
        call emit_line(unit, '    end subroutine standardir_lookup_intrinsic_type_spec')
        call emit_line(unit, '')
        call emit_line(unit, 'end module standardir_intrinsic_type_spec_generated')
    end subroutine emit_lookup

    character(len=32) function intrinsic_canonical_name(value)
        type(grammar_fact_t), intent(in) :: value

        select case (trim(value%id))
        case ('R705')
            intrinsic_canonical_name = 'integer'
        case ('R706')
            intrinsic_canonical_name = 'real'
        case ('R707')
            intrinsic_canonical_name = 'double_precision'
        case ('R704')
            if (index(trim(value%expression), 'LOGICAL') > 0) then
                intrinsic_canonical_name = 'logical'
            else if (index(trim(value%expression), 'CHARACTER') > 0) then
                intrinsic_canonical_name = 'character'
            else
                intrinsic_canonical_name = 'complex'
            end if
        case default
            intrinsic_canonical_name = ''
        end select
    end function intrinsic_canonical_name

    character(len=128) function intrinsic_source_spelling(value)
        type(grammar_fact_t), intent(in) :: value

        select case (trim(value%id))
        case ('R704')
            if (index(trim(value%expression), 'LOGICAL') > 0) then
                intrinsic_source_spelling = 'LOGICAL [ kind-selector ]'
            else if (index(trim(value%expression), 'CHARACTER') > 0) then
                intrinsic_source_spelling = 'CHARACTER [ char-selector ]'
            else
                intrinsic_source_spelling = 'COMPLEX'
            end if
        case default
            intrinsic_source_spelling = value%expression
        end select
    end function intrinsic_source_spelling

    subroutine emit_assignment(unit, index, field, value)
        integer, intent(in) :: unit, index
        character(len=*), intent(in) :: field, value
        character(len=1024) :: line

        write (line, '(a,i0,a,a,a)') '        values(', index, ')%', trim(field), ' = '''//trim(value)//''''
        call emit_line(unit, trim(line))
    end subroutine emit_assignment

    subroutine emit_integer_assignment(unit, index, field, value)
        integer, intent(in) :: unit, index, value
        character(len=*), intent(in) :: field
        character(len=256) :: line

        write (line, '(a,i0,a,a,a,i0)') '        values(', index, ')%', trim(field), ' = ', value
        call emit_line(unit, trim(line))
    end subroutine emit_integer_assignment

    subroutine emit_line(unit, line)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: line

        write (unit, '(a)') line
    end subroutine emit_line

    subroutine read_grammar_fact_value(node, value, ok, message)
        type(sx_node_t), intent(in) :: node
        type(grammar_fact_t), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: id, expression, document, clause, source_rule, source_hash
        character(len=64) :: page_text, origin, resolution
        integer :: page, ios

        value%id = ''
        value%expression = ''
        value%source%document = ''
        value%source%clause = ''
        value%source%rule = ''
        value%source%page = 0
        value%source%source_hash = ''
        value%origin = ORIGIN_MECHANICAL
        value%resolution = RESOLUTION_RESOLVED
        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 6) then
            message = 'grammar-fact source has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), 'grammar-fact')) then
            message = 'grammar-fact source has the wrong label'
            return
        end if
        call standardir_read_pair(node%children(2), 'id', id, ok, message)
        if (.not. ok) return
        call standardir_read_pair(node%children(3), 'expression', expression, ok, message)
        if (.not. ok) return
        call standardir_read_source(node%children(4), document, clause, page_text, source_hash, ok, message)
        if (.not. ok) return
        call read_source_rule_value(node%children(4), source_rule, ok, message)
        if (.not. ok) return
        read (page_text, *, iostat=ios) page
        if (ios /= 0 .or. page <= 0) then
            ok = .false.
            message = 'grammar-fact source page is invalid'
            return
        end if
        call standardir_read_pair(node%children(5), 'origin', origin, ok, message)
        if (.not. ok) return
        call standardir_read_pair(node%children(6), 'resolution', resolution, ok, message)
        if (.not. ok) return

        value%id = id
        value%expression = expression
        value%source%document = document
        value%source%clause = clause
        value%source%rule = source_rule
        value%source%page = page
        value%source%source_hash = source_hash
        if (trim(origin) == 'mechanical') then
            value%origin = ORIGIN_MECHANICAL
        else
            message = 'grammar-fact origin is outside the bounded lookup'
            return
        end if
        if (trim(resolution) == 'resolved') then
            value%resolution = RESOLUTION_RESOLVED
        else
            message = 'grammar-fact resolution is outside the bounded lookup'
            return
        end if
        call schema_validate_grammar_fact(value, ok, message)
    end subroutine read_grammar_fact_value

    subroutine read_source_rule_value(field, value, ok, message)
        type(sx_node_t), intent(in) :: field
        character(len=*), intent(out) :: value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        value = ''
        ok = .false.
        message = ''
        do i = 2, field%child_count
            if (field%children(i)%kind /= sx_list) cycle
            if (field%children(i)%child_count < 1) cycle
            if (.not. standardir_atom_equals(field%children(i)%children(1), 'rule')) cycle
            call standardir_read_pair(field%children(i), 'rule', value, ok, message)
            return
        end do
        message = 'grammar-fact source lacks rule'
    end subroutine read_source_rule_value

    subroutine generate_type_spec_fact(node, unit, expected_id, module_name, type_spec_name, &
            type_spec_label, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        character(len=*), intent(in) :: expected_id, module_name
        character(len=*), intent(in) :: type_spec_name, type_spec_label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: id, expression, document, clause, source_rule, source_hash
        character(len=64) :: page_text, origin, resolution
        integer :: page_number

        ok = .false.
        message = ''
        if (node%kind /= sx_list .or. node%child_count /= 6) then
            message = 'grammar-fact source has the wrong shape'
            return
        end if
        if (.not. standardir_atom_equals(node%children(1), 'grammar-fact')) then
            message = 'grammar-fact source has the wrong label'
            return
        end if
        call read_field(node%children(2), 'id', id, ok, message)
        if (.not. ok) return
        call read_field(node%children(3), 'expression', expression, ok, message)
        if (.not. ok) return
        call read_source_rule(node%children(4), document, clause, source_rule, page_number, &
            source_hash, ok, message)
        if (.not. ok) return
        call read_field(node%children(5), 'origin', origin, ok, message)
        if (.not. ok) return
        call read_field(node%children(6), 'resolution', resolution, ok, message)
        if (.not. ok) return
        if (trim(id) /= trim(expected_id) .or. trim(source_rule) /= trim(expected_id) .or. &
            trim(origin) /= 'mechanical' .or. trim(resolution) /= 'resolved') then
            ok = .false.
            message = 'grammar-fact source is outside the bounded type-spec fixture'
            return
        end if
        if (.not. type_spec_source_matches(expected_id, document, clause, page_number, source_hash)) then
            ok = .false.
            message = 'grammar-fact type-spec provenance differs'
            return
        end if
        if (.not. type_spec_expression_matches(expected_id, expression)) then
            ok = .false.
            message = 'grammar-fact expression differs'
            return
        end if
        write (page_text, '(i0)') page_number

        call emit('module '//trim(module_name))
        call emit('    !! Generated from specs/grammar-facts-v0.sx; do not edit.')
        call emit('')
        call emit('    use fortsx, only: sx_node_t')
        call emit('    use schema_v0_generated, only: grammar_fact_t, ORIGIN_MECHANICAL, &')
        call emit('        RESOLUTION_RESOLVED, schema_consume_grammar_fact, schema_validate_grammar_fact, &')
        call emit('        schema_write_grammar_fact')
        call emit('    implicit none')
        call emit('    private')
        call emit('')
        call emit("    character(len=*), parameter, public :: standardir_"//trim(type_spec_name)//"_id = '"// &
            trim(id)//"'")
        call emit("    character(len=*), parameter, public :: standardir_"//trim(type_spec_name)//"_expression = &")
        call emit("        '"//trim(expression)//"'")
        call emit("    character(len=*), parameter :: standardir_"//trim(type_spec_name)//"_document = '"// &
            trim(document)//"'")
        call emit("    character(len=*), parameter :: standardir_"//trim(type_spec_name)//"_clause = '"// &
            trim(clause)//"'")
        call emit("    character(len=*), parameter :: standardir_"//trim(type_spec_name)//"_source_rule = '"// &
            trim(source_rule)//"'")
        call emit('    integer, parameter :: standardir_'//trim(type_spec_name)//'_page = '//trim(page_text))
        call emit("    character(len=*), parameter :: standardir_"//trim(type_spec_name)//"_source_hash = '"// &
            trim(source_hash)//"'")
        call emit('')
        call emit('    public :: standardir_make_'//trim(type_spec_name)//'_fact')
        call emit('    public :: standardir_write_'//trim(type_spec_name)//'_fact')
        call emit('    public :: standardir_consume_'//trim(type_spec_name)//'_fact')
        call emit('')
        call emit('contains')
        call emit('')
        call emit('    subroutine standardir_make_'//trim(type_spec_name)//'_fact(document, clause, source_rule, page, &')
        call emit('            source_hash, value, ok, message)')
        call emit('        character(len=*), intent(in) :: document, clause, source_rule, source_hash')
        call emit('        integer, intent(in) :: page')
        call emit('        type(grammar_fact_t), intent(out) :: value')
        call emit('        logical, intent(out) :: ok')
        call emit('        character(len=*), intent(out) :: message')
        call emit('')
        call emit('        if (trim(document) /= standardir_'//trim(type_spec_name)//'_document .or. &')
        call emit('            trim(clause) /= standardir_'//trim(type_spec_name)//'_clause .or. &')
        call emit('            trim(source_rule) /= standardir_'//trim(type_spec_name)//'_source_rule .or. &')
        call emit('            page /= standardir_'//trim(type_spec_name)//'_page .or. &')
        call emit('            trim(source_hash) /= standardir_'//trim(type_spec_name)//'_source_hash) then')
        call emit('            ok = .false.')
        call emit("            message = 'grammar-fact source provenance differs'")
        call emit('            return')
        call emit('        end if')
        call emit('')
        call emit('        value%id = standardir_'//trim(type_spec_name)//'_id')
        call emit('        value%expression = standardir_'//trim(type_spec_name)//'_expression')
        call emit('        value%source%document = trim(document)')
        call emit('        value%source%clause = trim(clause)')
        call emit('        value%source%rule = trim(source_rule)')
        call emit('        value%source%page = page')
        call emit('        value%source%source_hash = trim(source_hash)')
        call emit('        value%origin = ORIGIN_MECHANICAL')
        call emit('        value%resolution = RESOLUTION_RESOLVED')
        call emit('        call schema_validate_grammar_fact(value, ok, message)')
        call emit('    end subroutine standardir_make_'//trim(type_spec_name)//'_fact')
        call emit('')
        call emit('    subroutine standardir_write_'//trim(type_spec_name)//'_fact(unit, document, clause, source_rule, &')
        call emit('            page, source_hash, ok, message)')
        call emit('        integer, intent(in) :: unit, page')
        call emit('        character(len=*), intent(in) :: document, clause, source_rule, source_hash')
        call emit('        logical, intent(out) :: ok')
        call emit('        character(len=*), intent(out) :: message')
        call emit('')
        call emit('        type(grammar_fact_t) :: value')
        call emit('')
        call emit('        call standardir_make_'//trim(type_spec_name)//'_fact(document, clause, source_rule, page, &')
        call emit('            source_hash, value, ok, message)')
        call emit('        if (.not. ok) return')
        call emit('        call schema_write_grammar_fact(value, unit, ok, message)')
        call emit('    end subroutine standardir_write_'//trim(type_spec_name)//'_fact')
        call emit('')
        call emit('    subroutine standardir_consume_'//trim(type_spec_name)//'_fact(node, ok, message)')
        call emit('        type(sx_node_t), intent(in) :: node')
        call emit('        logical, intent(out) :: ok')
        call emit('        character(len=*), intent(out) :: message')
        call emit('')
        call emit('        call schema_consume_grammar_fact(node, check_fact, ok, message)')
        call emit('')
        call emit('    contains')
        call emit('')
        call emit('        subroutine check_fact(value, callback_ok, callback_message)')
        call emit('            type(grammar_fact_t), intent(in) :: value')
        call emit('            logical, intent(out) :: callback_ok')
        call emit('            character(len=*), intent(out) :: callback_message')
        call emit('')
        call emit('            callback_ok = trim(value%id) == standardir_'//trim(type_spec_name)//'_id .and. &')
        call emit('                trim(value%expression) == standardir_'//trim(type_spec_name)//'_expression')
        call emit("            callback_message = ''")
        call emit("            if (.not. callback_ok) callback_message = '"//trim(type_spec_label)// &
            " type-spec grammar fact differs'")
        call emit('        end subroutine check_fact')
        call emit('')
        call emit('    end subroutine standardir_consume_'//trim(type_spec_name)//'_fact')
        call emit('')
        call emit('end module '//trim(module_name))
        ok = .true.

    contains

        subroutine emit(line)
            character(len=*), intent(in) :: line

            write (unit, '(a)') line
        end subroutine emit

        subroutine read_field(field, label, value, result_ok, result_message)
            type(sx_node_t), intent(in) :: field
            character(len=*), intent(in) :: label
            character(len=*), intent(out) :: value
            logical, intent(out) :: result_ok
            character(len=*), intent(out) :: result_message

            call standardir_read_pair(field, label, value, result_ok, result_message)
        end subroutine read_field

        subroutine read_source_rule(field, result_document, result_clause, result_rule, result_page, &
                result_hash, result_ok, result_message)
            type(sx_node_t), intent(in) :: field
            character(len=*), intent(out) :: result_document, result_clause, result_rule, result_hash
            integer, intent(out) :: result_page
            logical, intent(out) :: result_ok
            character(len=*), intent(out) :: result_message

            character(len=64) :: page_text
            integer :: ios

            result_page = 0
            call standardir_read_source(field, result_document, result_clause, page_text, result_hash, &
                result_ok, result_message)
            if (.not. result_ok) return
            call read_rule_from_source(field, result_rule, result_ok, result_message)
            if (.not. result_ok) return
            read (page_text, *, iostat=ios) result_page
            if (ios /= 0 .or. result_page <= 0) then
                result_ok = .false.
                result_message = 'grammar-fact source page is invalid'
            end if
        end subroutine read_source_rule

        logical function type_spec_source_matches(rule, source_document, source_clause, source_page, &
                source_hash_value)
            character(len=*), intent(in) :: rule, source_document, source_clause, source_hash_value
            integer, intent(in) :: source_page

            type_spec_source_matches = trim(source_document) == 'J3-24-007' .and. &
                trim(source_hash_value) == &
                '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
            if (.not. type_spec_source_matches) return
            select case (trim(rule))
            case ('R501')
                type_spec_source_matches = trim(source_clause) == '5' .and. source_page == 53
            case ('R509')
                type_spec_source_matches = trim(source_clause) == '5' .and. source_page == 45
            case ('R1162', 'R1164')
                type_spec_source_matches = trim(source_clause) == '11' .and. source_page == 214
            case ('R1212')
                type_spec_source_matches = trim(source_clause) == '12.6.1' .and. source_page == 242
            case ('R1215')
                type_spec_source_matches = trim(source_clause) == '12.6.2.2' .and. source_page == 244
            case ('R1217')
                type_spec_source_matches = trim(source_clause) == '12.6.3' .and. source_page == 248
            case ('R705', 'R706', 'R707')
                type_spec_source_matches = trim(source_clause) == '7' .and. source_page == 67
            case ('R708')
                type_spec_source_matches = trim(source_clause) == '7' .and. source_page == 66
            case ('R704')
                type_spec_source_matches = trim(source_clause) == '7' .and. source_page == 80
            case ('R1033')
                type_spec_source_matches = trim(source_clause) == '10' .and. source_page == 188
            case ('R1006', 'R1007', 'R1009', 'R1010')
                type_spec_source_matches = trim(source_clause) == '10' .and. source_page == 155
            case ('R901', 'R902', 'R903')
                type_spec_source_matches = trim(source_clause) == '5-15' .and. source_page == 150
            case default
                type_spec_source_matches = .false.
            end select
        end function type_spec_source_matches

        logical function type_spec_expression_matches(rule, source_expression)
            character(len=*), intent(in) :: rule, source_expression

            type_spec_expression_matches = .true.
            select case (trim(rule))
            case ('R1162')
                type_spec_expression_matches = trim(source_expression) == &
                    'STOP [ stop-code ] [ , QUIET = scalar-logical-expr ]'
            case ('R1164')
                type_spec_expression_matches = trim(source_expression) == &
                    'scalar-default-char-expr | scalar-int-expr'
            case ('R1212')
                type_spec_expression_matches = trim(source_expression) == &
                    'PRINT format [ , output-item-list ]'
            case ('R1215')
                type_spec_expression_matches = trim(source_expression) == &
                    'default-char-expr | label | *'
            case ('R1217')
                type_spec_expression_matches = trim(source_expression) == 'expr | io-implied-do'
            case ('R901')
                type_spec_expression_matches = trim(source_expression) == &
                    'object-name | array-element | array-section | coindexed-named-object | '// &
                    'complex-part-designator | structure-component | substring'
            case ('R902')
                type_spec_expression_matches = trim(source_expression) == 'designator | function-reference'
            case ('R903')
                type_spec_expression_matches = trim(source_expression) == 'name'
            end select
        end function type_spec_expression_matches

        subroutine read_rule_from_source(field, value, result_ok, result_message)
            type(sx_node_t), intent(in) :: field
            character(len=*), intent(out) :: value
            logical, intent(out) :: result_ok
            character(len=*), intent(out) :: result_message

            integer :: i

            value = ''
            result_ok = .false.
            result_message = ''
            do i = 2, field%child_count
                if (field%children(i)%kind == sx_list) then
                    if (field%children(i)%child_count >= 1) then
                        if (standardir_atom_equals(field%children(i)%children(1), 'rule')) then
                            call standardir_read_pair(field%children(i), 'rule', value, result_ok, result_message)
                            return
                        end if
                    end if
                end if
            end do
            result_message = 'grammar-fact source lacks rule'
        end subroutine read_rule_from_source

    end subroutine generate_type_spec_fact

end module standardir_grammar_fact_codegen

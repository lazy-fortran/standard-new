module standardir_grammar_fact_codegen
    !! Generate bounded type-spec grammar-fact consumers from SX sources.

    use fortsx, only: sx_list, sx_node_t
    use standardir_syntax_fields, only: standardir_atom_equals, &
        standardir_read_pair, standardir_read_source
    implicit none
    private

    public :: standardir_generate_integer_type_spec_fact
    public :: standardir_generate_real_type_spec_fact

contains

    subroutine standardir_generate_integer_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R705', 'INTEGER [ kind-selector ]', &
            'standardir_grammar_fact', 'integer_type_spec', 'integer', ok, message)
    end subroutine standardir_generate_integer_type_spec_fact

    subroutine standardir_generate_real_type_spec_fact(node, unit, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call generate_type_spec_fact(node, unit, 'R706', 'REAL [ kind-selector ]', &
            'standardir_real_type_spec_fact', 'real_type_spec', 'real', ok, message)
    end subroutine standardir_generate_real_type_spec_fact

    subroutine generate_type_spec_fact(node, unit, expected_id, expected_expression, module_name, &
            type_spec_name, type_spec_label, ok, message)
        type(sx_node_t), intent(in) :: node
        integer, intent(in) :: unit
        character(len=*), intent(in) :: expected_id, expected_expression, module_name
        character(len=*), intent(in) :: type_spec_name, type_spec_label
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: id, expression, document, clause, source_rule, source_hash
        character(len=64) :: page, origin, resolution
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
            message = 'grammar-fact source is outside the bounded type-spec fixture'
            return
        end if
        if (page_number /= 67 .or. trim(document) /= 'J3-24-007' .or. trim(clause) /= '7' .or. &
            trim(source_hash) /= 'fixture') then
            message = 'grammar-fact source provenance differs'
            return
        end if
        if (trim(expression) /= trim(expected_expression)) then
            message = 'grammar-fact expression differs'
            return
        end if

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

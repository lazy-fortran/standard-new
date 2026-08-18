module standardir_lexical_codegen
    !! Generate a StandardIR lexical-facts table from declarative SX rows.

    use, intrinsic :: iso_fortran_env, only: int64
    use fortsx, only: sx_node_t
    use standardir_lexical, only: standardir_lexical_add, standardir_lexical_facts_t, &
        standardir_lexical_reset, standardir_lexical_validate
    implicit none
    private

    character(len=*), parameter :: lexical_source_hash = &
        '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'

    public :: standardir_generate_lexical_facts

contains

    subroutine standardir_generate_lexical_facts(nodes, unit, ok, message)
        type(sx_node_t), intent(in) :: nodes(:)
        integer, intent(in) :: unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(standardir_lexical_facts_t) :: facts
        integer :: i

        ok = .false.
        message = ''
        if (size(nodes) /= 5) then
            message = 'lexical-facts-v0 must contain exactly five rows'
            return
        end if
        call standardir_lexical_reset(facts)
        do i = 1, size(nodes)
            call standardir_lexical_add(nodes(i), facts, ok, message)
            if (.not. ok) return
            if (trim(facts%facts(i)%source_hash) /= lexical_source_hash) then
                ok = .false.
                message = 'lexical fact source hash differs from pinned J3 document'
                return
            end if
        end do
        call standardir_lexical_validate(facts, ok, message)
        if (.not. ok) return

        call emit_module(unit, facts)
        ok = .true.
    end subroutine standardir_generate_lexical_facts

    subroutine emit_module(unit, facts)
        integer, intent(in) :: unit
        type(standardir_lexical_facts_t), intent(in) :: facts

        integer :: i, j

        call emit(unit, 'module standardir_lexical_facts_generated')
        call emit(unit, '    !! Generated from specs/lexical-facts-v0.sx; do not edit.')
        call emit(unit, '')
        call emit(unit, '    use, intrinsic :: iso_fortran_env, only: int64')
        call emit(unit, '    use standardir_lexical, only: standardir_lexical_facts_t, &')
        call emit(unit, '        standardir_lexical_reset, standardir_lexical_validate')
        call emit(unit, '    implicit none')
        call emit(unit, '    private')
        call emit(unit, '')
        call emit(unit, '    public :: standardir_make_lexical_facts')
        call emit(unit, '')
        call emit(unit, 'contains')
        call emit(unit, '')
        call emit(unit, '    subroutine standardir_make_lexical_facts(facts, ok, message)')
        call emit(unit, '        type(standardir_lexical_facts_t), intent(out) :: facts')
        call emit(unit, '        logical, intent(out) :: ok')
        call emit(unit, '        character(len=*), intent(out) :: message')
        call emit(unit, '')
        call emit(unit, '        call standardir_lexical_reset(facts)')
        call emit(unit, '        facts%count = 5')
        do i = 1, facts%count
            call emit_field(unit, i, 'source_term', facts%facts(i)%source_term)
            call emit_field(unit, i, 'canonical_spelling', facts%facts(i)%canonical_spelling)
            call emit_field(unit, i, 'class_name', facts%facts(i)%class_name)
            call emit_field(unit, i, 'target_name', facts%facts(i)%target_name)
            call emit_field(unit, i, 'source_rule', facts%facts(i)%source_rule)
            call emit_field(unit, i, 'source_page', facts%facts(i)%source_page)
            call emit_field(unit, i, 'document', facts%facts(i)%document)
            call emit_field(unit, i, 'clause', facts%facts(i)%clause)
            call emit_field(unit, i, 'source_hash', facts%facts(i)%source_hash)
            call emit_field(unit, i, 'codepoint', facts%facts(i)%codepoint)
            call emit_integer(unit, i, 'range_count', facts%facts(i)%range_count)
            do j = 1, facts%facts(i)%range_count
                call emit_int64(unit, i, 'range_first', j, facts%facts(i)%range_first(j))
                call emit_int64(unit, i, 'range_last', j, facts%facts(i)%range_last(j))
            end do
        end do
        call emit(unit, '        call standardir_lexical_validate(facts, ok, message)')
        call emit(unit, '    end subroutine standardir_make_lexical_facts')
        call emit(unit, '')
        call emit(unit, 'end module standardir_lexical_facts_generated')
    end subroutine emit_module

    subroutine emit_field(unit, index, name, value)
        integer, intent(in) :: unit, index
        character(len=*), intent(in) :: name, value
        character(len=2048) :: line

        if (trim(name) == 'source_hash') then
            write (line, '(a,i0,a,a,a)') '        facts%facts(', index, ')%', trim(name), ' = &'
            call emit(unit, trim(line))
            call emit(unit, '            '//quote(value))
            return
        end if
        write (line, '(a,i0,a,a,a)') '        facts%facts(', index, ')%', trim(name), &
            ' = '//quote(value)
        call emit(unit, trim(line))
    end subroutine emit_field

    subroutine emit_integer(unit, index, name, value)
        integer, intent(in) :: unit, index, value
        character(len=*), intent(in) :: name
        character(len=256) :: line

        write (line, '(a,i0,a,a,a,i0)') '        facts%facts(', index, ')%', trim(name), &
            ' = ', value
        call emit(unit, trim(line))
    end subroutine emit_integer

    subroutine emit_int64(unit, index, name, component, value)
        integer, intent(in) :: unit, index, component
        integer(int64), intent(in) :: value
        character(len=*), intent(in) :: name
        character(len=256) :: line

        write (line, '(a,i0,a,a,a,i0,a,i0,a)') '        facts%facts(', index, ')%', trim(name), '(', &
            component, ') = ', value, '_int64'
        call emit(unit, trim(line))
    end subroutine emit_int64

    subroutine emit(unit, line)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: line

        write (unit, '(a)') line
    end subroutine emit

    function quote(value) result(output)
        character(len=*), intent(in) :: value
        character(len=1024) :: output
        integer :: i, position

        output = ''
        output(1:1) = "'"
        position = 1
        do i = 1, len_trim(value)
            position = position + 1
            if (value(i:i) == "'") then
                output(position:position) = "'"
                position = position + 1
            end if
            output(position:position) = value(i:i)
        end do
        position = position + 1
        output(position:position) = "'"
        output = output(:position)
    end function quote

end module standardir_lexical_codegen

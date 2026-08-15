module standardir_grammar_target_fingerprint
    !! Recompute a target expression fingerprint using canonical SX bytes.

    use fortsx, only: sx_atom, sx_list, sx_node_t
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, standardir_grammar_repeat, &
        standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_source_fingerprint, only: standardir_grammar_source_expression_sha256
    use standardir_grammar_target_records, only: standardir_target_expression_t
    implicit none
    private

    public :: standardir_target_expression_sha256

contains

    subroutine standardir_target_expression_sha256(expression, fingerprint, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        character(len=*), intent(out) :: fingerprint
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(sx_node_t) :: node

        fingerprint = ''
        ok = .false.
        message = ''
        call target_expression_to_sx(expression, node, ok, message)
        if (.not. ok) return
        call standardir_grammar_source_expression_sha256(node, fingerprint, ok, message)
    end subroutine standardir_target_expression_sha256

    recursive subroutine target_expression_to_sx(expression, node, ok, message)
        type(standardir_target_expression_t), intent(in) :: expression
        type(sx_node_t), intent(out) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i
        character(len=32) :: minimum

        call clear_node(node)
        ok = .false.
        message = ''
        select case (expression%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            call make_list(node, 2)
            if (expression%kind == standardir_grammar_reference) then
                call make_atom(node%children(1), 'ref')
            else
                call make_atom(node%children(1), 'token')
            end if
            call make_atom(node%children(2), trim(expression%name))
        case (standardir_grammar_sequence, standardir_grammar_choice)
            if (.not. allocated(expression%children) .or. size(expression%children) < 1) then
                message = 'target expression sequence or choice is empty'
                return
            end if
            call make_list(node, size(expression%children) + 1)
            if (expression%kind == standardir_grammar_sequence) then
                call make_atom(node%children(1), 'seq')
            else
                call make_atom(node%children(1), 'alt')
            end if
            do i = 1, size(expression%children)
                call target_expression_to_sx(expression%children(i), node%children(i + 1), ok, message)
                if (.not. ok) return
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            if (.not. allocated(expression%children) .or. size(expression%children) /= 1) then
                message = 'target optional or repeat expression is malformed'
                return
            end if
            call make_list(node, merge(4, 2, expression%kind == standardir_grammar_repeat))
            if (expression%kind == standardir_grammar_optional) then
                call make_atom(node%children(1), 'optional')
            else
                call make_atom(node%children(1), 'repeat')
                write (minimum, '(i0)') expression%minimum
                call make_atom(node%children(3), trim(minimum))
                call make_atom(node%children(4), 'unbounded')
            end if
            call target_expression_to_sx(expression%children(1), node%children(2), ok, message)
            if (.not. ok) return
        case default
            message = 'target expression has an unsupported kind'
            return
        end select
        ok = .true.
        message = ''
    end subroutine target_expression_to_sx

    subroutine make_list(node, count)
        type(sx_node_t), intent(out) :: node
        integer, intent(in) :: count

        call clear_node(node)
        node%kind = sx_list
        node%child_count = count
        allocate (node%children(count))
    end subroutine make_list

    subroutine make_atom(node, value)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: value

        call clear_node(node)
        node%kind = sx_atom
        node%atom = trim(value)
    end subroutine make_atom

    subroutine clear_node(node)
        type(sx_node_t), intent(inout) :: node

        if (allocated(node%children)) deallocate (node%children)
        node%kind = 0
        node%atom = ''
        node%child_count = 0
    end subroutine clear_node

end module standardir_grammar_target_fingerprint

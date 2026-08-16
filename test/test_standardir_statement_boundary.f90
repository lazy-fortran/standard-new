program test_standardir_statement_boundary
    !! Fixed source-topology witnesses are the independent plan oracle.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexical_layout, only: standardir_layout_add, standardir_layout_reset, &
        standardir_layout_t
    use standardir_statement_boundary, only: standardir_statement_boundary_build_plan, &
        standardir_statement_boundary_marker, standardir_statement_boundary_plan_t
    use standardir_statement_sequence, only: standardir_sequence_compound_repeat_item, &
        standardir_sequence_first_plus_repeat, standardir_sequence_internal, standardir_sequence_repeat_item, &
        standardir_statement_sequence_analyze, standardir_statement_sequence_candidate_t
    implicit none

    character(len=*), parameter :: hash = &
        '1cf538329c57e4f617adb36f2c7cd91a5a5561c78bcce16ec96f7ff1a9979f9e'
    character(len=4096) :: message
    type(standardir_layout_t) :: layout
    type(standardir_statement_boundary_plan_t) :: plan
    type(standardir_statement_sequence_candidate_t), allocatable :: candidates(:), broken(:)
    type(sx_node_t), allocatable :: nodes(:)
    logical :: ok

    call make_layout(layout)
    call make_nodes(nodes)
    call standardir_statement_sequence_analyze(nodes, layout, candidates, ok, message)
    call require(ok, message)
    call require(has_candidate(candidates, 'execution-part', 'R6', 'rhs/2', &
        standardir_sequence_repeat_item), 'repeat-item witness was not audited')
    call require(has_candidate(candidates, 'case-construct', 'R9', 'rhs/2', &
        standardir_sequence_compound_repeat_item), 'compound repeat-item witness was not audited')
    call require(has_candidate(candidates, 'case-construct', 'R9', 'rhs/2', &
        standardir_sequence_first_plus_repeat), 'first-plus-repeat witness was not audited')
    call require(has_candidate(candidates, 'case-construct', 'R9', 'rhs/2/1/1', &
        standardir_sequence_internal), 'sequence-internal witness was not audited')
    call require(.not. has_lhs(candidates, 'if-stmt'), 'nested action-stmt became a boundary')

    call standardir_statement_boundary_build_plan(candidates, plan, ok, message)
    call require(ok, message)
    call require(size(plan%sites) < size(candidates), &
        'same-location candidate evidence was not coalesced')
    call require(plan_evidence_count(plan) == size(candidates), &
        'coalescing discarded candidate evidence')
    call require(trim(plan%integration_boundary) == &
        'target expression mapping and token insertion remain downstream', &
        'plan did not state its remaining integration boundary')
    call require(.not. plan%insertion_supported, 'plan claimed unsupported insertion was ready')
    call require(all_marked(plan), 'boundary plan did not expose the stable marker')
    call require(has_plan_site(plan, 'execution-part', 'R6', 'rhs/2', 'execution-part-construct', &
        standardir_sequence_repeat_item), &
        'plan lost repeat-item provenance')
    call require(has_plan_site(plan, 'case-construct', 'R9', 'rhs/2', 'sequence', &
        standardir_sequence_compound_repeat_item), 'plan lost compound provenance')
    call require(has_plan_evidence(plan, 'case-construct', 'R9', 'rhs/2', 2), &
        'coalesced site did not retain both candidate kinds')
    call require(has_plan_lineage(plan, 'execution-part', 'DOC', '4.1.4', '45', '100', hash), &
        'plan lost source lineage')
    call require(has_plan_lineage(plan, 'execution-part', 'DOC', '4.1.4', '45', '101', hash), &
        'plan rejected a distinct source occurrence')

    allocate (broken(1))
    broken(1) = candidates(1)
    broken(1)%expression_path = 'rhs/0'
    call standardir_statement_boundary_build_plan(broken, plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'malformed expression path') > 0, &
        'malformed expression path was accepted')

    deallocate (broken)
    allocate (broken(2))
    broken(1) = candidates(1)
    broken(2) = candidates(1)
    call standardir_statement_boundary_build_plan(broken, plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'duplicated or ambiguous') > 0, &
        'duplicate source occurrence was accepted')

    broken(2)%item = 'conflicting-item'
    call standardir_statement_boundary_build_plan(broken, plan, ok, message)
    call require(ok .and. size(plan%sites) == 1 .and. size(plan%sites(1)%evidence) == 2, &
        'different candidate evidence was not coalesced')

    broken(1) = candidates(1)
    broken(1)%source_hash = ''
    call standardir_statement_boundary_build_plan(broken(:1), plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'source lineage') > 0, &
        'missing source lineage was accepted')

    broken(1) = candidates(1)
    broken(1)%source_hash = repeat('a', 63)
    call standardir_statement_boundary_build_plan(broken(:1), plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'invalid source hash') > 0, &
        'short source hash was accepted')

    broken(1)%source_hash = repeat('g', 64)
    call standardir_statement_boundary_build_plan(broken(:1), plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'invalid source hash') > 0, &
        'non-hex source hash was accepted')

    broken(1) = candidates(1)
    deallocate (broken)
    allocate (broken(2))
    broken(1) = candidates(1)
    broken(2) = candidates(1)
    broken(1)%source_byte_start = '100'
    broken(2)%source_byte_start = '20'
    call standardir_statement_boundary_build_plan(broken, plan, ok, message)
    call require(ok, 'distinct source occurrences were rejected')
    call require(trim(plan%sites(1)%candidate%source_byte_start) == '20' .and. &
        trim(plan%sites(2)%candidate%source_byte_start) == '100', &
        'source byte offsets were not sorted numerically')

    broken(1) = candidates(1)
    broken(1)%status = 'unsupported'
    call standardir_statement_boundary_build_plan(broken(:1), plan, ok, message)
    call require(.not. ok .and. index(trim(message), 'not supported') > 0, &
        'unsupported candidate status was accepted')

    print '(a)', 'StandardIR statement-boundary test passed'

contains

    subroutine make_layout(value)
        type(standardir_layout_t), intent(out) :: value
        type(sx_node_t) :: node
        logical :: local_ok
        character(len=256) :: local_message
        character(len=1024) :: text

        text = '(statement-class-suffix (source-form all) (suffix -stmt) '// &
            '(source (source-ref (document DOC) (clause 4.1.4) (locator statement-class) '// &
            '(page 45) (source-hash '//hash//'))) (origin mechanical))'
        call standardir_layout_reset(value)
        call sx_parse(text, node, local_ok, local_message)
        call require(local_ok, local_message)
        call standardir_layout_add(node, value, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine make_layout

    subroutine make_nodes(values)
        type(sx_node_t), allocatable, intent(out) :: values(:)
        type(sx_node_t), allocatable :: expanded(:)

        allocate (values(10))
        values = sx_node_t()
        call parse(syntax('R1', 'save-stmt', '(seq (token SAVE))', '1', '10'), values(1))
        call parse(syntax('R2', 'action-stmt', '(alt (seq (ref save-stmt)))', '2', '20'), values(2))
        call parse(syntax('R3', 'if-stmt', '(seq (token IF) (ref action-stmt))', '3', '30'), values(3))
        call parse(syntax('R4', 'executable-construct', '(alt (seq (ref action-stmt)))', '4', '40'), values(4))
        call parse(syntax('R5', 'execution-part-construct', '(alt (seq (ref executable-construct)))', &
            '5', '50'), values(5))
        call parse(syntax('R6', 'execution-part', &
            '(seq (ref executable-construct) (repeat (ref execution-part-construct) 0 unbounded))', &
            '45', '100'), values(6))
        call parse(syntax('R7', 'case-stmt', '(seq (token CASE))', '7', '70'), values(7))
        call parse(syntax('R8', 'block', '(repeat (ref case-stmt) 0 unbounded)', '8', '80'), values(8))
        call parse(syntax('R9', 'case-construct', &
            '(seq (ref case-stmt) (repeat (seq (ref case-stmt) (ref block)) 0 unbounded))', &
            '46', '200'), values(9))
        call parse(syntax('R10', 'if-construct', &
            '(seq (ref if-then-stmt) (ref block) (optional (seq (ref else-stmt) (ref block))))', &
            '10', '1000'), values(10))
        allocate (expanded(11))
        expanded(:10) = values
        call parse(syntax('R6', 'execution-part', &
            '(seq (ref executable-construct) (repeat (ref execution-part-construct) 0 unbounded))', &
            '45', '101'), expanded(11))
        call move_alloc(expanded, values)
    end subroutine make_nodes

    function syntax(rule, lhs, rhs, page, byte_start) result(text)
        character(len=*), intent(in) :: rule, lhs, rhs, page, byte_start
        character(len=4096) :: text

        write (text, '(a)') '(syntax '//trim(rule)//' (lhs '//trim(lhs)//') (rhs '//trim(rhs)//') '// &
            '(source (document DOC) (clause 4.1.4) (page '//trim(page)//') (byte-start '// &
            trim(byte_start)//') (source-sha256 '//hash//')))'
    end function syntax

    subroutine parse(text, node)
        character(len=*), intent(in) :: text
        type(sx_node_t), intent(out) :: node
        logical :: local_ok
        character(len=256) :: local_message

        call sx_parse(text, node, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine parse

    logical function has_candidate(values, lhs, rule, path, kind)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, rule, path, kind
        integer :: i

        has_candidate = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) /= trim(lhs)) cycle
            if (trim(values(i)%source_rule) /= trim(rule)) cycle
            if (trim(values(i)%expression_path) /= trim(path)) cycle
            if (trim(values(i)%kind) /= trim(kind)) cycle
            has_candidate = .true.
            return
        end do
    end function has_candidate

    logical function has_lhs(values, lhs)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i

        has_lhs = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) == trim(lhs)) has_lhs = .true.
        end do
    end function has_lhs

    logical function all_marked(value)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        integer :: i

        all_marked = .true.
        do i = 1, size(value%sites)
            if (trim(value%sites(i)%marker) /= trim(standardir_statement_boundary_marker)) all_marked = .false.
            if (trim(value%sites(i)%separator) /= trim(standardir_statement_boundary_marker)) all_marked = .false.
        end do
    end function all_marked

    logical function has_plan_site(value, lhs, rule, path, item, kind)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        character(len=*), intent(in) :: lhs, rule, path, item, kind
        integer :: i, j

        has_plan_site = .false.
        do i = 1, size(value%sites)
            if (trim(value%sites(i)%candidate%source_lhs) /= trim(lhs)) cycle
            if (trim(value%sites(i)%candidate%source_rule) /= trim(rule)) cycle
            if (trim(value%sites(i)%candidate%expression_path) /= trim(path)) cycle
            if (trim(value%sites(i)%candidate%item) == trim(item) .and. &
                trim(value%sites(i)%candidate%kind) == trim(kind)) then
                has_plan_site = .true.
                return
            end if
            if (.not. allocated(value%sites(i)%evidence)) cycle
            do j = 1, size(value%sites(i)%evidence)
                if (trim(value%sites(i)%evidence(j)%item) /= trim(item)) cycle
                if (trim(value%sites(i)%evidence(j)%kind) /= trim(kind)) cycle
                has_plan_site = .true.
                return
            end do
        end do
    end function has_plan_site

    logical function has_plan_lineage(value, lhs, document, clause, page, byte_start, source_hash)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        character(len=*), intent(in) :: lhs, document, clause, page, byte_start, source_hash
        integer :: i

        has_plan_lineage = .false.
        do i = 1, size(value%sites)
            if (trim(value%sites(i)%candidate%source_lhs) /= trim(lhs)) cycle
            if (trim(value%sites(i)%candidate%source_document) /= trim(document)) cycle
            if (trim(value%sites(i)%candidate%source_clause) /= trim(clause)) cycle
            if (trim(value%sites(i)%candidate%source_page) /= trim(page)) cycle
            if (trim(value%sites(i)%candidate%source_byte_start) /= trim(byte_start)) cycle
            if (trim(value%sites(i)%candidate%source_hash) /= trim(source_hash)) cycle
            has_plan_lineage = .true.
            return
        end do
    end function has_plan_lineage

    integer function plan_evidence_count(value)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        integer :: i

        plan_evidence_count = 0
        do i = 1, size(value%sites)
            if (allocated(value%sites(i)%evidence)) plan_evidence_count = &
                plan_evidence_count + size(value%sites(i)%evidence)
        end do
    end function plan_evidence_count

    logical function has_plan_evidence(value, lhs, rule, path, count)
        type(standardir_statement_boundary_plan_t), intent(in) :: value
        character(len=*), intent(in) :: lhs, rule, path
        integer, intent(in) :: count
        integer :: i

        has_plan_evidence = .false.
        do i = 1, size(value%sites)
            if (trim(value%sites(i)%candidate%source_lhs) /= trim(lhs)) cycle
            if (trim(value%sites(i)%candidate%source_rule) /= trim(rule)) cycle
            if (trim(value%sites(i)%candidate%expression_path) /= trim(path)) cycle
            if (.not. allocated(value%sites(i)%evidence)) cycle
            has_plan_evidence = size(value%sites(i)%evidence) == count
            return
        end do
    end function has_plan_evidence

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_standardir_statement_boundary

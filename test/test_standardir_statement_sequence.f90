program test_standardir_statement_sequence
    use fortsx, only: sx_node_t, sx_parse
    use standardir_lexical_layout, only: standardir_layout_add, standardir_layout_reset, &
        standardir_layout_t
    use standardir_statement_sequence, only: standardir_sequence_compound_internal, &
        standardir_sequence_compound_repeat_item, standardir_sequence_internal, standardir_sequence_repeat_item, &
        standardir_statement_sequence_analyze, standardir_statement_sequence_candidate_t
    implicit none

    character(len=*), parameter :: hash = &
        '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
    character(len=4096) :: message
    type(standardir_layout_t) :: layout, malformed_layout
    type(standardir_statement_sequence_candidate_t), allocatable :: values(:)
    type(sx_node_t), allocatable :: nodes(:)
    logical :: ok

    call make_layout(layout)
    call make_valid_nodes(nodes)
    call standardir_statement_sequence_analyze(nodes, layout, values, ok, message)
    call require(ok, message)
    call require(has_candidate(values, 'execution-part', standardir_sequence_repeat_item, &
        'execution-part-construct', '45', '100'), 'direct repeat candidate was not retained')
    call require(has_candidate(values, 'case-construct', standardir_sequence_compound_repeat_item, &
        'sequence', '46', '200'), 'compound repeat-item candidate was not retained')
    call require(has_candidate(values, 'case-construct', standardir_sequence_compound_internal, &
        'case-stmt', '46', '200'), 'compound internal candidate was not retained')
    call require(has_candidate(values, 'if-construct', standardir_sequence_internal, 'if-then-stmt', '10', '1000'), &
        'adjacent IF-THEN sequence boundary was not retained')
    call require(has_candidate(values, 'if-construct', standardir_sequence_internal, 'else-stmt', '10', '1000'), &
        'optional ELSE sequence boundary was not retained')
    call require(has_derivation(values, 'case-construct', 'block,case-stmt'), &
        'compound derivation names were not sorted')
    call require(has_path(values, 'execution-part', 'rhs/2'), &
        'direct repeat path was not deterministic')
    call require(has_lineage(values, 'execution-part', 'DOC', '5', hash), &
        'candidate source lineage was not retained')
    call require(.not. has_lhs(values, 'if-stmt'), &
        'nested IF action-stmt incorrectly became a boundary')
    call require(.not. has_lhs(values, 'payload-before-statement'), &
        'required payload before a statement incorrectly became a boundary')

    call make_malformed_layout(malformed_layout)
    call standardir_statement_sequence_analyze(nodes, malformed_layout, values, ok, message)
    call require(.not. ok .and. index(trim(message), 'invalid v2 lexical layout') > 0, &
        'malformed layout was not rejected before analysis')

    call make_scoped_layout(malformed_layout)
    call standardir_statement_sequence_analyze(nodes, malformed_layout, values, ok, message)
    call require(.not. ok .and. index(trim(message), 'source-form all') > 0, &
        'source-form-scoped suffix was not rejected')

    call make_malformed_rhs_nodes(nodes)
    call standardir_statement_sequence_analyze(nodes, layout, values, ok, message)
    call require(.not. ok .and. index(trim(message), 'rhs') > 0, &
        'malformed RHS was not rejected with controlled failure')

    call make_unsupported_nodes(nodes)
    call standardir_statement_sequence_analyze(nodes, layout, values, ok, message)
    call require(.not. ok .and. index(trim(message), 'unsupported repeated') > 0, &
        'unsupported repeated shape was silently ignored')
    call require(has_unsupported_row(values), 'unsupported row was not retained with provenance and status')
    print '(a)', 'StandardIR statement-sequence test passed'

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

    subroutine make_scoped_layout(value)
        type(standardir_layout_t), intent(out) :: value
        type(sx_node_t) :: node
        logical :: local_ok
        character(len=256) :: local_message
        character(len=1024) :: text

        text = '(statement-class-suffix (source-form free-form) (suffix -stmt) '// &
            '(source (source-ref (document DOC) (clause 4.1.4) (locator statement-class) '// &
            '(page 45) (source-hash '//hash//'))) (origin mechanical))'
        call standardir_layout_reset(value)
        call sx_parse(text, node, local_ok, local_message)
        call require(local_ok, local_message)
        call standardir_layout_add(node, value, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine make_scoped_layout

    subroutine make_malformed_layout(value)
        type(standardir_layout_t), intent(out) :: value

        call standardir_layout_reset(value)
        value%count = 1
        value%records(1)%kind = 'statement-class-suffix'
        value%records(1)%source_form = 'all'
        value%records(1)%suffix = '-stmt'
    end subroutine make_malformed_layout

    subroutine make_valid_nodes(values)
        type(sx_node_t), allocatable, intent(out) :: values(:)
        character(len=4096) :: text

        allocate (values(8))
        values = sx_node_t()
        text = syntax('R1', 'save-stmt', '(seq (token SAVE))', '1', '10')
        call parse(text, values(1))
        text = syntax('R2', 'action-stmt', '(alt (seq (ref save-stmt)))', '2', '20')
        call parse(text, values(2))
        text = syntax('R3', 'if-stmt', '(seq (token IF) (ref action-stmt))', '3', '30')
        call parse(text, values(3))
        text = syntax('R4', 'executable-construct', '(alt (seq (ref action-stmt)))', '4', '40')
        call parse(text, values(4))
        text = syntax('R5', 'execution-part-construct', '(alt (seq (ref executable-construct)))', '5', '50')
        call parse(text, values(5))
        text = syntax('R6', 'execution-part', &
            '(seq (ref executable-construct) (repeat (ref execution-part-construct) 0 unbounded))', '45', '100')
        call parse(text, values(6))
        text = syntax('R7', 'case-stmt', '(seq (token CASE))', '7', '70')
        call parse(text, values(7))
        text = syntax('R8', 'block', '(repeat (ref case-stmt) 0 unbounded)', '8', '80')
        call parse(text, values(8))
        call extend_case_nodes(values)
    end subroutine make_valid_nodes

    subroutine extend_case_nodes(values)
        type(sx_node_t), allocatable, intent(inout) :: values(:)
        type(sx_node_t), allocatable :: extended(:)
        character(len=4096) :: text

        allocate (extended(13))
        extended = sx_node_t()
        extended(:8) = values
        text = syntax('R9', 'case-construct', &
            '(seq (ref case-stmt) (repeat (seq (ref case-stmt) (ref block)) 0 unbounded))', '46', '200')
        call parse(text, extended(9))
        text = syntax('R10', 'if-construct', &
            '(seq (ref if-then-stmt) (ref block) (optional (seq (ref else-stmt) (ref block))))', '10', '1000')
        call parse(text, extended(10))
        text = syntax('R11', 'payload-before-statement', &
            '(seq (ref if-then-stmt) (token PAYLOAD) (ref block))', '11', '1100')
        call parse(text, extended(11))
        text = syntax('R12', 'if-then-stmt', '(seq (token IFTHEN))', '12', '1200')
        call parse(text, extended(12))
        text = syntax('R13', 'else-stmt', '(seq (token ELSE))', '13', '1300')
        call parse(text, extended(13))
        call move_alloc(extended, values)
    end subroutine extend_case_nodes

    subroutine make_malformed_rhs_nodes(values)
        type(sx_node_t), allocatable, intent(out) :: values(:)
        character(len=4096) :: text

        allocate (values(1))
        values = sx_node_t()
        text = '(syntax BAD (lhs malformed) (rhs) (source (document DOC) (clause 5) '// &
            '(page 1) (source-sha256 '//hash//')))'// &
            ''
        call parse(text, values(1))
    end subroutine make_malformed_rhs_nodes

    subroutine make_unsupported_nodes(values)
        type(sx_node_t), allocatable, intent(out) :: values(:)
        character(len=4096) :: text

        allocate (values(2))
        values = sx_node_t()
        text = syntax('U1', 'case-stmt', '(seq (token CASE))', '1', '1')
        call parse(text, values(1))
        text = syntax('U2', 'bad-sequence', &
            '(repeat (seq (ref case-stmt) (token PAYLOAD)) 0 unbounded)', '2', '2')
        call parse(text, values(2))
    end subroutine make_unsupported_nodes

    function syntax(rule, lhs, rhs, page, byte_start) result(text)
        character(len=*), intent(in) :: rule, lhs, rhs, page, byte_start
        character(len=4096) :: text

        write (text, '(a)') '(syntax '//trim(rule)//' (lhs '//trim(lhs)//') (rhs '//trim(rhs)//') '// &
            '(source (document DOC) (clause 5) (page '//trim(page)//') (byte-start '//trim(byte_start)//') '// &
            '(source-sha256 '//hash//')))'
    end function syntax

    subroutine parse(text, node)
        character(len=*), intent(in) :: text
        type(sx_node_t), intent(out) :: node
        logical :: local_ok
        character(len=256) :: local_message

        call sx_parse(text, node, local_ok, local_message)
        call require(local_ok, local_message)
    end subroutine parse

    logical function has_candidate(values, lhs, kind, item, page, byte_start)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, kind, item, page, byte_start
        integer :: i

        has_candidate = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) /= trim(lhs)) cycle
            if (trim(values(i)%kind) /= trim(kind)) cycle
            if (trim(values(i)%item) /= trim(item)) cycle
            if (trim(values(i)%source_page) /= trim(page)) cycle
            if (trim(values(i)%source_byte_start) /= trim(byte_start)) cycle
            has_candidate = .true.
            return
        end do
    end function has_candidate

    logical function has_path(values, lhs, path)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, path
        integer :: i

        has_path = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) == trim(lhs) .and. &
                trim(values(i)%expression_path) == trim(path)) then
                has_path = .true.
                return
            end if
        end do
    end function has_path

    logical function has_lineage(values, lhs, document, clause, source_hash)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, document, clause, source_hash
        integer :: i

        has_lineage = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) /= trim(lhs)) cycle
            if (trim(values(i)%source_document) /= trim(document)) cycle
            if (trim(values(i)%source_clause) /= trim(clause)) cycle
            if (trim(values(i)%source_hash) /= trim(source_hash)) cycle
            has_lineage = .true.
            return
        end do
    end function has_lineage

    logical function has_derivation(values, lhs, derivation)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs, derivation
        integer :: i

        has_derivation = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) /= trim(lhs)) cycle
            if (trim(values(i)%derivation) /= trim(derivation)) cycle
            has_derivation = .true.
            return
        end do
    end function has_derivation

    logical function has_unsupported_row(values)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        integer :: i

        has_unsupported_row = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_rule) /= 'U2') cycle
            if (trim(values(i)%source_lhs) /= 'bad-sequence') cycle
            if (trim(values(i)%source_document) /= 'DOC') cycle
            if (trim(values(i)%source_clause) /= '5') cycle
            if (trim(values(i)%source_hash) /= hash) cycle
            if (trim(values(i)%source_page) /= '2') cycle
            if (trim(values(i)%source_byte_start) /= '2') cycle
            if (trim(values(i)%expression_path) /= 'rhs') cycle
            if (trim(values(i)%status) /= 'unsupported') cycle
            has_unsupported_row = .true.
            return
        end do
    end function has_unsupported_row

    logical function has_lhs(values, lhs)
        type(standardir_statement_sequence_candidate_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i

        has_lhs = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_lhs) == trim(lhs)) has_lhs = .true.
        end do
    end function has_lhs

    subroutine require(condition, failure)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure

        if (.not. condition) error stop trim(failure)
    end subroutine require

end program test_standardir_statement_sequence

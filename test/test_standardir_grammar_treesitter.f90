program test_standardir_grammar_treesitter
    !! Independent behavioral checks for tree-sitter nullable lowering.

    use standardir_grammar_producer, only: standardir_grammar_optional, &
        standardir_grammar_reference, standardir_grammar_repeat, &
        standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_target_fingerprint, only: standardir_target_expression_sha256
    use standardir_grammar_targetnorm, only: standardir_target_expression_t, standardir_target_rule_t
    use standardir_grammar_transformation_witness, only: standardir_grammar_validate_transformation_witness
    use standardir_grammar_treesitter, only: standardir_grammar_lower_treesitter
    implicit none

    type(standardir_target_rule_t), allocatable :: values(:), bounded(:), original(:)
    logical :: entry_nullable, ok
    character(len=512) :: message
    integer :: i

    call make_fixture(values)
    original = values
    call standardir_grammar_lower_treesitter(values, 'start', entry_nullable, ok, message)
    call require(ok, 'generic nullable fixture was rejected: '//trim(message))
    call require(.not. entry_nullable, 'non-nullable selected start was classified as nullable')
    do i = 1, size(values)
        call require(.not. target_nullable(values(i)%expression), &
            'lowered non-start rule still matches the empty string')
    end do
    call require(values(2)%expression%kind == standardir_grammar_repeat .and. &
        values(2)%expression%minimum == 1 .and. &
        values(2)%expression%children(1)%kind == standardir_grammar_reference, &
        'nullable unbounded repeat was not lowered to a non-empty repeat')
    call require(values(3)%expression%kind == standardir_grammar_token .and. &
        trim(values(3)%expression%name) == 'ITEM', &
        'nullable item rule did not retain its positive language')
    call require(values(1)%expression%kind == standardir_grammar_sequence .and. &
        values(1)%expression%children(1)%kind == standardir_grammar_optional, &
        'nullable block was not propagated into its referring production')
    call require(transformation_reason(values, 'block'), &
        'changed BLOCK source alternative lacks a tree-sitter transformation witness')
    call require(transformation_reason(values, 'item'), &
        'changed ITEM source alternative lacks a tree-sitter transformation witness')
    call require(transformation_reason(values, 'start'), &
        'referrer changed by nullable propagation lacks a transformation witness')
    call standardir_grammar_validate_transformation_witness(values, ok, message)
    call require(ok, 'lowered source identity witness is invalid: '//trim(message))

    call make_bounded_fixture(bounded)
    original = bounded
    call standardir_grammar_lower_treesitter(bounded, 'start', entry_nullable, ok, message)
    call require(.not. ok .and. index(trim(message), 'target-disposition=unsupported') > 0 .and. &
        index(trim(message), 'source-rule=NULLABLE-B') > 0 .and. &
        index(trim(message), 'source-alternative=1') > 0, &
        'unbounded nullable expansion was not rejected with a machine-readable disposition')
    call require(bounded(2)%expression%kind == original(2)%expression%kind .and. &
        size(bounded(2)%expression%children) == size(original(2)%expression%children), &
        'failed nullable lowering was not transactional')

contains

    subroutine make_fixture(values)
        type(standardir_target_rule_t), allocatable, intent(out) :: values(:)
        type(standardir_target_expression_t) :: children(2)

        allocate (values(3))
        children(1) = ref_expression('block')
        children(2) = token_expression('END')
        call make_rule(values(1), 'START', 'start', sequence_expression(children), 1, '1')
        call make_rule(values(2), 'BLOCK', 'block', repeat_expression(ref_expression('item')), 1, '2')
        call make_rule(values(3), 'ITEM', 'item', optional_expression(token_expression('ITEM')), 1, '3')
    end subroutine make_fixture

    subroutine make_bounded_fixture(values)
        type(standardir_target_rule_t), allocatable, intent(out) :: values(:)
        type(standardir_target_expression_t), allocatable :: children(:)
        type(standardir_target_expression_t) :: start_children(2)
        character(len=16) :: token
        integer :: i

        allocate (values(2), children(9))
        do i = 1, size(children)
            write (token, '(a,i0)') 'N', i
            children(i) = optional_expression(token_expression(trim(token)))
        end do
        start_children(1) = ref_expression('nullable')
        start_children(2) = token_expression('END')
        call make_rule(values(1), 'START-B', 'start', sequence_expression(start_children), 1, '4')
        call make_rule(values(2), 'NULLABLE-B', 'nullable', sequence_expression(children), 1, '5')
    end subroutine make_bounded_fixture

    subroutine make_rule(value, rule, lhs, expression, alternative, page_text)
        type(standardir_target_rule_t), intent(out) :: value
        character(len=*), intent(in) :: rule, lhs, page_text
        type(standardir_target_expression_t), intent(in) :: expression
        integer, intent(in) :: alternative
        logical :: ok
        character(len=256) :: message

        value = standardir_target_rule_t()
        value%id = trim(rule)
        value%lhs = trim(lhs)
        value%alternative = alternative
        value%expression = expression
        value%source%document = 'DOC'
        value%source%clause = '1'
        value%source%rule = trim(rule)
        read (page_text, *) value%source%page
        value%source%source_hash = 'HASH-'//trim(page_text)
        value%origin = 1
        value%resolution = 1
        allocate (value%provenance(1))
        value%provenance(1)%source = value%source
        value%provenance(1)%alternative = alternative
        value%provenance(1)%source_expression_present = .true.
        value%provenance(1)%source_expression_sha256 = repeat('a', 64)
        call standardir_target_expression_sha256(expression, value%target_expression_sha256, ok, message)
        call require(ok, 'could not fingerprint test fixture: '//trim(message))
    end subroutine make_rule

    function ref_expression(name) result(value)
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_reference
        value%name = trim(name)
    end function ref_expression

    function token_expression(name) result(value)
        character(len=*), intent(in) :: name
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_token
        value%name = trim(name)
    end function token_expression

    function optional_expression(child) result(value)
        type(standardir_target_expression_t), intent(in) :: child
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_optional
        allocate (value%children(1))
        value%children(1) = child
    end function optional_expression

    function repeat_expression(child) result(value)
        type(standardir_target_expression_t), intent(in) :: child
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_repeat
        value%minimum = 0
        value%unbounded = .true.
        allocate (value%children(1))
        value%children(1) = child
    end function repeat_expression

    function sequence_expression(children) result(value)
        type(standardir_target_expression_t), intent(in) :: children(:)
        type(standardir_target_expression_t) :: value

        value = standardir_target_expression_t()
        value%kind = standardir_grammar_sequence
        allocate (value%children(size(children)))
        value%children = children
    end function sequence_expression

    recursive logical function target_nullable(expression) result(value)
        type(standardir_target_expression_t), intent(in) :: expression
        integer :: i

        select case (expression%kind)
        case (standardir_grammar_reference, standardir_grammar_token)
            value = .false.
        case (standardir_grammar_optional)
            value = .true.
        case (standardir_grammar_repeat)
            value = expression%minimum == 0
            if (.not. value) value = target_nullable(expression%children(1))
        case (standardir_grammar_sequence)
            value = .true.
            do i = 1, size(expression%children)
                if (.not. target_nullable(expression%children(i))) value = .false.
            end do
        case default
            value = .false.
            do i = 1, size(expression%children)
                if (target_nullable(expression%children(i))) value = .true.
            end do
        end select
    end function target_nullable

    logical function transformation_reason(values, lhs)
        type(standardir_target_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: lhs
        integer :: i, j

        transformation_reason = .false.
        do i = 1, size(values)
            if (trim(values(i)%lhs) /= trim(lhs)) cycle
            if (.not. allocated(values(i)%source_witnesses)) return
            do j = 1, size(values(i)%source_witnesses)
                if (trim(values(i)%source_witnesses(j)%reason) == &
                    'tree-sitter-nullable-lowering') transformation_reason = .true.
            end do
        end do
    end function transformation_reason

    subroutine require(condition, text)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: text

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(text)
            stop 1
        end if
    end subroutine require

end program test_standardir_grammar_treesitter

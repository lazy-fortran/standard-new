program test_standardir_dependencies
    !! Fixed SX rules are the independent oracle for dependency closure.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_dependencies, only: dependency_add_syntax, dependency_compute, &
        dependency_max_rules, dependency_table_t
    implicit none

    character(len=*), parameter :: source = &
        '(source (document test) (clause 1) (source-sha256 abc))'
    character(len=*), parameter :: sx1 = &
        '(syntax R1 (lhs a) (rhs (seq (ref b) (token +))) '//trim(source)//')'
    character(len=*), parameter :: sx2 = &
        '(syntax R2 (lhs b) (rhs (seq (ref c))) '//trim(source)//')'
    character(len=*), parameter :: sx3 = &
        '(syntax R3 (lhs c) (rhs (seq (token C))) '//trim(source)//')'
    character(len=*), parameter :: roots(1) = ['R1']
    character(len=16) :: rule
    character(len=65536) :: message
    integer :: closure(dependency_max_rules), closure_count, unresolved_count
    integer :: i
    logical :: ok, is_syntax
    type(dependency_table_t) :: table
    type(sx_node_t) :: node

    call add(sx1)
    call add(sx2)
    call add(sx3)
    call add(sx2)
    call dependency_compute(table, roots, 1, closure, closure_count, unresolved_count, &
        ok, message)
    if (.not. ok) call fail(trim(message))
    if (table%rule_count /= 3) call fail('unique rule count differs')
    if (table%rules(2)%occurrences /= 2) call fail('duplicate occurrence count differs')
    if (closure_count /= 3) call fail('closure count differs')
    if (unresolved_count /= 0) call fail('unexpected unresolved reference')
    do i = 1, closure_count
        rule = table%rules(closure(i))%rule
        if (rule /= 'R'//achar(48 + i)) call fail('closure order differs')
    end do
    print '(a)', 'StandardIR dependency test passed'

contains

    subroutine add(text)
        character(len=*), intent(in) :: text

        call sx_parse(text, node, ok, message)
        if (.not. ok) call fail(trim(message))
        call dependency_add_syntax(table, node, is_syntax, ok, message)
        if (.not. ok .or. .not. is_syntax) call fail('syntax record was not added')
    end subroutine add

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'FAIL: '//trim(text)
        stop 1
    end subroutine fail

end program test_standardir_dependencies

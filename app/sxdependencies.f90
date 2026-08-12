program sxdependencies
    !! Compute a profile dependency closure from StandardIR SX.

    use fortsx, only: sx_clear, sx_node_t, sx_parse
    use standardir_dependencies, only: dependency_add_syntax, dependency_compute, &
        dependency_max_rules, dependency_table_t, dependency_write
    implicit none

    character(len=4096) :: input_path, roots_path, profile, source_hash, output_path
    character(len=65536) :: line, message
    character(len=16) :: roots(dependency_max_rules)
    integer :: input_unit, roots_unit, output_unit, ios, root_count
    integer :: closure(dependency_max_rules), closure_count, unresolved_count
    integer :: i
    logical :: ok, is_syntax
    type(dependency_table_t) :: table
    type(sx_node_t) :: node

    call get_command_argument(1, input_path)
    call get_command_argument(2, roots_path)
    call get_command_argument(3, profile)
    call get_command_argument(4, source_hash)
    call get_command_argument(5, output_path)
    if (len_trim(input_path) == 0 .or. len_trim(roots_path) == 0 .or. &
        len_trim(profile) == 0 .or. len_trim(source_hash) == 0 .or. &
        len_trim(output_path) == 0) then
        print '(a)', 'usage: sxdependencies <input.sx> <roots.txt> <profile> '// &
            '<source-sha256> <output.sx>'
        stop 2
    end if

    open (newunit=roots_unit, file=trim(roots_path), status='old', action='read', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open roots file')
    roots = ''
    root_count = 0
    do
        read (roots_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#') cycle
        if (root_count >= size(roots)) call fail('too many profile roots')
        root_count = root_count + 1
        call read_root(line, roots(root_count), ok, message)
        if (.not. ok) call fail(trim(message))
    end do
    close (roots_unit)

    open (newunit=input_unit, file=trim(input_path), status='old', action='read', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open StandardIR input')
    do
        read (input_unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        call sx_parse(trim(line), node, ok, message)
        if (.not. ok) call fail(trim(message))
        call dependency_add_syntax(table, node, is_syntax, ok, message)
        call sx_clear(node)
        if (.not. ok) call fail(trim(message))
    end do
    close (input_unit)

    call dependency_compute(table, roots, root_count, closure, closure_count, &
        unresolved_count, ok, message)
    if (.not. ok) call fail(trim(message))

    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open dependency output')
    call dependency_write(output_unit, profile, source_hash, roots, root_count, table, &
        closure, closure_count, unresolved_count)
    close (output_unit)
    print '(a,i0,a,i0,a,i0,a)', 'computed ', table%rule_count, ' unique rules, ', &
        closure_count, ' closure rules, ', unresolved_count, ' unresolved references'

contains

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'error: '//trim(text)
        stop 1
    end subroutine fail

    subroutine read_root(text, root, ok, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: root
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: marker, start, finish

        root = ''
        ok = .false.
        message = ''
        marker = index(text, '"rule":"')
        if (marker == 0) then
            if (len_trim(text) > len(root)) then
                message = 'profile root exceeds rule ID buffer'
                return
            end if
            root = adjustl(trim(text))
        else
            start = marker + len('"rule":"')
            finish = index(text(start:), '"')
            if (finish == 0) then
                message = 'profile root JSON record has no closing quote'
                return
            end if
            if (finish - 1 > len(root)) then
                message = 'profile root exceeds rule ID buffer'
                return
            end if
            root = text(start:start + finish - 2)
        end if
        if (len_trim(root) == 0) then
            message = 'profile root is empty'
            return
        end if
        ok = .true.
    end subroutine read_root

end program sxdependencies

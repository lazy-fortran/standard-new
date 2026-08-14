program sxsemantic
    !! Validate and canonicalize one semantic-items SX record.

    use standardir_semantic_command, only: standardir_canonicalize_semantic_items
    implicit none

    character(len=4096) :: input_path, output_path, message
    integer :: argc
    logical :: ok

    argc = command_argument_count()
    if (argc /= 2) then
        call get_command_argument(0, input_path)
        print '(a)', 'usage: '//trim(input_path)//' <input.sx> <output.sx>'
        stop 2
    end if
    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)
    call standardir_canonicalize_semantic_items(input_path, output_path, ok, message)
    if (.not. ok) then
        print '(a)', 'error: '//trim(message)
        stop 1
    end if
    print '(a)', 'canonicalized semantic-items SX'
end program sxsemantic

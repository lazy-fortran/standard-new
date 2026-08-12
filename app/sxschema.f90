program sxschema
    !! Generate a Fortran type layer from one .sxs schema file.

    use schema_codegen, only: schema_generate_types
    use schema_ir, only: schema_parse_text, schema_t
    implicit none

    character(len=4096) :: input_path, output_path, module_name, text, message
    character(len=256) :: line
    type(schema_t) :: schema
    logical :: ok
    integer :: ios, input_unit, output_unit

    call get_command_argument(1, input_path)
    call get_command_argument(2, output_path)
    call get_command_argument(3, module_name)
    if (len_trim(input_path) == 0 .or. len_trim(output_path) == 0 .or. &
        len_trim(module_name) == 0) then
        print '(a)', 'usage: sxschema <schema.sxs> <output.f90> <module-name>'
        error stop 2
    end if

    open (newunit=input_unit, file=trim(input_path), action='read', iostat=ios)
    if (ios /= 0) call fail('could not open schema input')
    text = ''
    read (input_unit, '(a)', iostat=ios) line
    close (input_unit)
    if (ios /= 0) call fail('could not read schema input')
    text = trim(line)
    call schema_parse_text(text, schema, ok, message)
    if (.not. ok) call fail(trim(message))

    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('could not open generated output')
    call schema_generate_types(schema, output_unit, trim(module_name), ok, message)
    close (output_unit)
    if (.not. ok) call fail(trim(message))

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message

        print '(a)', 'sxschema: '//trim(message)
        error stop 1
    end subroutine fail

end program sxschema

program test_standardir_semantic_command
    !! Fixed SX records independently define command acceptance and output.

    use fortsx, only: sx_node_t, sx_parse
    use standardir_export, only: standardir_semantic_item_t
    use standardir_semantic_command, only: standardir_canonicalize_semantic_items
    use standardir_semantic_consumer, only: standardir_consume_semantic_items
    use standardir_semantic_table, only: semantic_table_find_id, semantic_table_t
    implicit none

    character(len=*), parameter :: input_path = 'test-semantic-command-input.sx'
    character(len=*), parameter :: output_path = 'test-semantic-command-output.sx'
    character(len=*), parameter :: valid_input = &
        '(semantic-items (semantic-item (id S-one) (subject assignment) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1102) '// &
        '(page 88) (source-hash fixture-hash))) (origin human) '// &
        '(resolution resolved)) (semantic-item (id S-one) '// &
        '(subject assignment-alias) (source (source-ref (document J3-24-007) '// &
        '(clause 10.1) (rule C1102) (page 88) (source-hash fixture-hash))) '// &
        '(origin human) (resolution disputed)))'
    character(len=*), parameter :: expected_output = &
        '(semantic-items (semantic-item (id S-one) (subject assignment) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1102) '// &
        '(page 88) (source-hash fixture-hash))) (origin human) '// &
        '(resolution resolved)) (semantic-item (id S-one) '// &
        '(subject assignment-alias) (source (source-ref (document J3-24-007) '// &
        '(clause 10.1) (rule C1102) (page 88) (source-hash fixture-hash))) '// &
        '(origin human) (resolution disputed)))'
    character(len=*), parameter :: bad_source = &
        '(semantic-items (semantic-item (id S-bad) (subject assignment) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1104) '// &
        '(page 0) (source-hash fixture-hash))) (origin human) '// &
        '(resolution unresolved)))'
    character(len=*), parameter :: bad_enum = &
        '(semantic-items (semantic-item (id S-bad) (subject assignment) '// &
        '(source (source-ref (document J3-24-007) (clause 10.1) (rule C1105) '// &
        '(page 90) (source-hash fixture-hash))) (origin alien) '// &
        '(resolution unresolved)))'

    character(len=65536) :: actual, message
    type(sx_node_t) :: node
    type(semantic_table_t) :: table
    type(standardir_semantic_item_t) :: item
    integer :: unit, ios
    logical :: ok, found, exists

    call write_fixture(valid_input)
    call standardir_canonicalize_semantic_items(input_path, output_path, ok, message)
    call require(ok, message)
    call read_fixture(actual)
    call require(trim(actual) == trim(expected_output), 'canonical semantic-items output differs')
    call sx_parse(trim(actual), node, ok, message)
    call require(ok, message)
    call standardir_consume_semantic_items(node, table, ok, message)
    call require(ok .and. table%item_count == 2, 'duplicate records were not preserved')
    call semantic_table_find_id(table, 'S-one', item, found, ok, message)
    call require(.not. ok .and. .not. found, 'ambiguous duplicate id was silently resolved')

    call write_fixture(bad_source)
    call write_fixture_to_output('sentinel')
    call standardir_canonicalize_semantic_items(input_path, output_path, ok, message)
    call require(.not. ok, 'malformed provenance was accepted')
    inquire (file=output_path, exist=exists)
    call require(.not. exists, 'failed provenance left an output file')

    call write_fixture(bad_enum)
    call write_fixture_to_output('sentinel')
    call standardir_canonicalize_semantic_items(input_path, output_path, ok, message)
    call require(.not. ok, 'invalid enum was accepted')
    inquire (file=output_path, exist=exists)
    call require(.not. exists, 'failed enum left an output file')

    call cleanup()
    print '(a)', 'StandardIR semantic command test passed'

contains

    subroutine write_fixture(value)
        character(len=*), intent(in) :: value

        open (newunit=unit, file=input_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open semantic command input fixture')
        write (unit, '(a)') trim(value)
        close (unit)
    end subroutine write_fixture

    subroutine write_fixture_to_output(value)
        character(len=*), intent(in) :: value

        open (newunit=unit, file=output_path, status='replace', action='write', iostat=ios)
        call require(ios == 0, 'could not open semantic command output fixture')
        write (unit, '(a)') value
        close (unit)
    end subroutine write_fixture_to_output

    subroutine read_fixture(value)
        character(len=*), intent(out) :: value

        open (newunit=unit, file=output_path, action='read', iostat=ios)
        call require(ios == 0, 'could not open semantic command output')
        read (unit, '(a)', iostat=ios) value
        call require(ios == 0, 'could not read semantic command output')
        close (unit)
    end subroutine read_fixture

    subroutine cleanup()
        inquire (file=input_path, exist=exists)
        if (exists) then
            open (newunit=unit, file=input_path, status='old', iostat=ios)
            if (ios == 0) close (unit, status='delete')
        end if
        inquire (file=output_path, exist=exists)
        if (exists) then
            open (newunit=unit, file=output_path, status='old', iostat=ios)
            if (ios == 0) close (unit, status='delete')
        end if
    end subroutine cleanup

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            call cleanup()
            error stop 1
        end if
    end subroutine require

end program test_standardir_semantic_command

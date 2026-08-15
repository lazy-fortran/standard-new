program sxgrammar
    !! Close source-backed StandardIR and emit one target grammar.

    use fortsx, only: sx_atom, sx_list, sx_node_t, sx_parse
    use standardir_bison, only: standardir_emit_bison_start
    use standardir_grammar_closure, only: standardir_grammar_close_selected_sx, &
        standardir_grammar_close_sx, standardir_grammar_disposition_omitted_helper, &
        standardir_grammar_disposition_omitted_root, standardir_grammar_disposition_selected, &
        standardir_grammar_disposition_t
    use standardir_grammar_export, only: standardir_grammar_export_batch, &
        standardir_grammar_collect_source_dispositions, &
        standardir_grammar_emit_source_disposition, &
        standardir_grammar_format_antlr4, standardir_grammar_format_bison, &
        standardir_grammar_format_ebnf, standardir_grammar_format_tree_sitter, &
        standardir_target_role_family_config_t, standardir_target_source_witness_t
    use standardir_grammar_transformation_witness, only: &
        standardir_grammar_emit_transformation_witness
    use standardir_grammar_producer, only: standardir_grammar_rule_t
    use standardir_lexical, only: standardir_lexical_add, standardir_lexical_facts_t, &
        standardir_lexical_reset
    use standardir_lexical_export, only: standardir_lexical_emit_antlr, &
        standardir_lexical_emit_bison, standardir_lexical_emit_bison_aliases, &
        standardir_lexical_emit_ebnf, &
        standardir_lexical_emit_treesitter
    use standardir_reference_closure, only: closure_classification_t
    use standardir_reference_closure_io, only: closure_read_classification, closure_read_root
    implicit none

    character(len=4096) :: syntax_path, classifications_path, roots_path, lexical_path
    character(len=4096) :: format_text, output_path, transformation_witness_path, message, line
    character(len=128) :: selected_root
    type(standardir_target_role_family_config_t) :: role_family
    type(sx_node_t), allocatable :: nodes(:)
    type(closure_classification_t), allocatable :: classifications(:)
    character(len=128), allocatable :: roots(:)
    type(standardir_grammar_rule_t), allocatable :: rules(:)
    type(standardir_lexical_facts_t) :: lexical
    type(sx_node_t) :: node
    character(len=256), allocatable :: start_names(:)
    character(len=128), allocatable :: semantic_skipped_names(:)
    character(len=512), allocatable :: semantic_skipped_details(:)
    type(standardir_grammar_disposition_t), allocatable :: dispositions(:)
    type(standardir_target_source_witness_t), allocatable :: pre_lowering_witnesses(:), one_witnesses(:)
    integer :: argc, format, input_unit, output_unit, ios, records
    integer :: classification_count, root_count, semantic_skipped, lexical_closed
    integer :: i, j
    logical :: ok, selected_mode, role_family_mode, transformation_witness_mode, source_node_found

    argc = command_argument_count()
    selected_mode = .false.
    role_family_mode = .false.
    transformation_witness_mode = .false.
    selected_root = ''
    role_family = standardir_target_role_family_config_t()
    allocate (pre_lowering_witnesses(0))
    if (argc < 6) then
        call get_command_argument(0, syntax_path)
        print '(a)', 'usage: '//trim(syntax_path)// &
            ' <source.sx> <classifications.sx> <roots.sx> <lexical.sx|-> '// &
            '<ebnf|antlr|bison|treesitter> <output> '// &
            '[--selected-root <root>] [--role-family <representative>] '// &
            '[--transformation-witness <path>]'
        stop 2
    end if
    call get_command_argument(1, syntax_path)
    call get_command_argument(2, classifications_path)
    call get_command_argument(3, roots_path)
    call get_command_argument(4, lexical_path)
    call get_command_argument(5, format_text)
    call get_command_argument(6, output_path)
    call parse_options(argc, selected_root, selected_mode, role_family, role_family_mode, &
        transformation_witness_path, transformation_witness_mode, ok, message)
    if (.not. ok) call fail(trim(message))
    call parse_format(format_text, format, ok, message)
    if (.not. ok) call fail(trim(message))

    call read_syntax_file(syntax_path, nodes, records, ok, message)
    if (.not. ok) call fail(trim(message))
    call read_classification_file(classifications_path, classifications, classification_count, &
        ok, message)
    if (.not. ok) call fail(trim(message))
    call read_root_file(roots_path, roots, root_count, ok, message)
    if (.not. ok) call fail(trim(message))
    call read_lexical_file(lexical_path, lexical, ok, message)
    if (.not. ok) call fail(trim(message))

    if (selected_mode) then
        call standardir_grammar_close_selected_sx(nodes, records, classifications, classification_count, &
            roots, root_count, selected_root, lexical, rules, semantic_skipped, lexical_closed, &
            dispositions, ok, message, semantic_skipped_names, semantic_skipped_details)
    else
        call standardir_grammar_close_sx(nodes, records, classifications, classification_count, &
            roots, root_count, lexical, rules, semantic_skipped, lexical_closed, ok, message, &
            semantic_skipped_names, semantic_skipped_details)
    end if
    if (.not. ok) call fail(trim(message))
    if (.not. allocated(rules)) call fail('closure returned no grammar rule array')
    if (size(rules) < 1) call fail('closure returned no exportable grammar rules')
    if (selected_mode) then
        allocate (start_names(1))
        start_names(1) = trim(selected_root)
    else
        call collect_start_names(rules, roots, root_count, start_names)
        call validate_root_dispositions(roots, root_count, start_names, semantic_skipped_names, &
            ok, message)
        if (.not. ok) call fail(trim(message))
    end if

    open (newunit=output_unit, file=trim(output_path), status='replace', action='write', &
        iostat=ios)
    if (ios /= 0) call fail('cannot open grammar output')
    call emit_header(output_unit, format)
    if (.not. ok) call fail_output(output_unit, message)
    if (format == standardir_grammar_format_bison) then
        call standardir_lexical_emit_bison(output_unit, lexical, ok, message)
        if (.not. ok) call fail_output(output_unit, message)
        write (output_unit, '(a)') '%glr-parser'
        write (output_unit, '(a)') '%start standardir_start'
        write (output_unit, '(a)') '%%'
        if (.not. selected_mode) then
            call standardir_emit_bison_start(output_unit, start_names, ok, message)
            if (.not. ok) call fail_output(output_unit, message)
        end if
        call standardir_lexical_emit_bison_aliases(output_unit, lexical, ok, message)
        if (.not. ok) call fail_output(output_unit, message)
    end if
    if (selected_mode) then
        if (role_family_mode) then
            call standardir_grammar_export_batch(output_unit, rules, format, ok, message, &
                selected_root=selected_root, role_family=role_family, lexical=lexical)
        else
            call standardir_grammar_export_batch(output_unit, rules, format, ok, message, &
                selected_root=selected_root, lexical=lexical)
        end if
    else
        if (role_family_mode) then
            call standardir_grammar_export_batch(output_unit, rules, format, ok, message, roots=start_names, &
                role_family=role_family, lexical=lexical)
        else
            call standardir_grammar_export_batch(output_unit, rules, format, ok, message, roots=start_names, &
                lexical=lexical)
        end if
    end if
    if (.not. ok) call fail_output(output_unit, message)
    if (selected_mode) then
        do i = 1, size(dispositions)
            if (dispositions(i)%disposition /= standardir_grammar_disposition_omitted_root) cycle
            source_node_found = .false.
            do j = 1, records
                if (.not. source_node_matches(nodes(j), dispositions(i)%source%rule, dispositions(i)%name)) cycle
                call standardir_grammar_collect_source_dispositions(nodes(j), &
                    'omitted-'//trim(dispositions(i)%name), dispositions(i)%name, &
                    'omitted-before-target-lowering', one_witnesses, ok, message)
                if (.not. ok) call fail_output(output_unit, message)
                call append_source_witnesses(pre_lowering_witnesses, one_witnesses)
                call standardir_grammar_emit_source_disposition(output_unit, format, nodes(j), &
                    'omitted-'//trim(dispositions(i)%name), dispositions(i)%name, &
                    'omitted-before-target-lowering', ok, message)
                if (.not. ok) call fail_output(output_unit, message)
                source_node_found = .true.
                exit
            end do
            if (.not. source_node_found) then
                call fail_output(output_unit, 'omitted root has no source syntax record: '// &
                    trim(dispositions(i)%name))
            end if
        end do
    end if
    select case (format)
    case (standardir_grammar_format_ebnf)
        call standardir_lexical_emit_ebnf(output_unit, lexical, ok, message)
    case (standardir_grammar_format_antlr4)
        call standardir_lexical_emit_antlr(output_unit, lexical, ok, message)
    case (standardir_grammar_format_tree_sitter)
        call standardir_lexical_emit_treesitter(output_unit, lexical, ok, message)
    case (standardir_grammar_format_bison)
        write (output_unit, '(a)') '%%'
        ok = .true.
        message = ''
    end select
    if (.not. ok) call fail_output(output_unit, message)
    call emit_footer(output_unit, format)
    close (output_unit)
    if (transformation_witness_mode) then
        call emit_transformation_witness_file(transformation_witness_path, rules, selected_mode, &
            format, selected_root, start_names, role_family_mode, role_family, pre_lowering_witnesses, ok, message)
        if (.not. ok) call fail(trim(message))
    end if
    print '(a,i0,a,i0,a,i0,a)', 'emitted ', size(rules), ' rules; skipped ', &
        semantic_skipped, ' semantic-only records; closed ', lexical_closed, ' lexical facts'
    call print_skipped_names(semantic_skipped_names)
    call print_skipped_details(semantic_skipped_details)
    if (selected_mode) call print_dispositions(dispositions)

contains

    logical function source_node_matches(node, source_rule, source_lhs)
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: source_rule, source_lhs

        source_node_matches = .false.
        if (node%kind /= sx_list .or. node%child_count < 4) return
        if (node%children(2)%kind /= sx_atom .or. node%children(3)%kind /= sx_list) return
        if (trim(node%children(2)%atom) /= trim(source_rule)) return
        if (node%children(3)%child_count /= 2) return
        if (node%children(3)%children(1)%kind /= sx_atom .or. &
            trim(node%children(3)%children(1)%atom) /= 'lhs') return
        if (node%children(3)%children(2)%kind /= sx_atom) return
        source_node_matches = trim(node%children(3)%children(2)%atom) == trim(source_lhs)
    end function source_node_matches

    subroutine read_syntax_file(path, values, count, ok, message)
        character(len=*), intent(in) :: path
        type(sx_node_t), allocatable, intent(out) :: values(:)
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: unit, ios

        allocate (values(0))
        count = 0
        ok = .false.
        message = ''
        open (newunit=unit, file=trim(path), action='read', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open source StandardIR SX'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            call sx_parse(line, node, ok, message)
            if (.not. ok) then
                close (unit)
                return
            end if
            if (is_label(node, 'standardir')) cycle
            if (.not. is_label(node, 'syntax')) then
                close (unit)
                message = 'source SX contains a non-syntax record'
                ok = .false.
                return
            end if
            call append_node(values, node)
            count = count + 1
        end do
        close (unit)
        ok = count > 0
        if (.not. ok) message = 'source SX contains no syntax records'
    end subroutine read_syntax_file

    subroutine collect_start_names(values, roots, root_count, names)
        type(standardir_grammar_rule_t), intent(in) :: values(:)
        character(len=*), intent(in) :: roots(:)
        integer, intent(in) :: root_count
        character(len=256), allocatable, intent(out) :: names(:)

        integer :: i, j, k
        logical :: found

        allocate (names(0))
        do i = 1, root_count
            found = .false.
            do j = 1, size(values)
                if (trim(roots(i)) == trim(values(j)%lhs)) then
                    found = .false.
                    do k = 1, size(names)
                        if (trim(names(k)) == trim(roots(i))) then
                            found = .true.
                            exit
                        end if
                    end do
                    if (.not. found) call append_start_name(names, roots(i))
                    exit
                end if
            end do
        end do
    end subroutine collect_start_names

    subroutine append_start_name(values, value)
        character(len=256), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: value

        character(len=256), allocatable :: grown(:)
        integer :: old_size

        old_size = size(values)
        allocate (grown(old_size + 1))
        if (old_size > 0) grown(:old_size) = values
        grown(old_size + 1) = trim(value)
        call move_alloc(grown, values)
    end subroutine append_start_name

    subroutine validate_root_dispositions(roots, root_count, start_names, skipped_names, ok, message)
        character(len=*), intent(in) :: roots(:), start_names(:), skipped_names(:)
        integer, intent(in) :: root_count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        integer :: i, j
        logical :: found

        ok = .false.
        message = ''
        do i = 1, root_count
            found = .false.
            do j = 1, size(start_names)
                if (trim(roots(i)) == trim(start_names(j))) found = .true.
            end do
            do j = 1, size(skipped_names)
                if (trim(roots(i)) == trim(skipped_names(j))) found = .true.
            end do
            if (.not. found) then
                message = 'declared root has no export or explicit skip: '//trim(roots(i))
                return
            end if
        end do
        ok = .true.
    end subroutine validate_root_dispositions

    subroutine print_skipped_names(names)
        character(len=*), intent(in) :: names(:)
        integer :: i

        do i = 1, size(names)
            print '(a)', 'root-disposition skipped-semantic-only '//trim(names(i))
        end do
    end subroutine print_skipped_names

    subroutine print_skipped_details(details)
        character(len=*), intent(in) :: details(:)
        integer :: i

        do i = 1, size(details)
            print '(a)', 'root-disposition-detail skipped-semantic-only '//trim(details(i))
        end do
    end subroutine print_skipped_details

    subroutine read_classification_file(path, values, count, ok, message)
        character(len=*), intent(in) :: path
        type(closure_classification_t), allocatable, intent(out) :: values(:)
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        type(closure_classification_t) :: value
        integer :: unit, ios, line_number

        allocate (values(0))
        count = 0
        line_number = 0
        ok = .false.
        message = ''
        open (newunit=unit, file=trim(path), action='read', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open closure classification SX'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            line_number = line_number + 1
            if (len_trim(line) == 0) cycle
            call sx_parse(line, node, ok, message)
            if (.not. ok) then
                close (unit)
                return
            end if
            if (.not. is_label(node, 'classification')) cycle
            call closure_read_classification(node, value, ok, message)
            if (.not. ok) then
                close (unit)
                message = 'classification line '//integer_text(line_number)//': '//trim(message)
                return
            end if
            call append_classification(values, value)
            count = count + 1
            if (len_trim(value%source%document) == 0 .or. &
                len_trim(value%source%clause) == 0 .or. &
                len_trim(value%source%rule) == 0 .or. value%source%page <= 0 .or. &
                len_trim(value%source%source_hash) == 0) then
                close (unit)
                ok = .false.
                message = 'classification line '//integer_text(line_number)// &
                    ' has incomplete source after parsing'
                return
            end if
        end do
        close (unit)
        ok = .true.
    end subroutine read_classification_file

    subroutine read_root_file(path, values, count, ok, message)
        character(len=*), intent(in) :: path
        character(len=128), allocatable, intent(out) :: values(:)
        integer, intent(out) :: count
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=128) :: value
        integer :: unit, ios, i

        allocate (values(0))
        count = 0
        ok = .false.
        message = ''
        open (newunit=unit, file=trim(path), action='read', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open closure roots SX'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            call sx_parse(line, node, ok, message)
            if (.not. ok) then
                close (unit)
                return
            end if
            if (is_label(node, 'root')) then
                call closure_read_root(node, value, ok, message)
                if (.not. ok) then
                    close (unit)
                    return
                end if
                call append_root(values, value)
                count = count + 1
            else if (is_label(node, 'roots')) then
                do i = 2, node%child_count
                    call closure_read_root(node%children(i), value, ok, message)
                    if (.not. ok) then
                        close (unit)
                        return
                    end if
                    call append_root(values, value)
                    count = count + 1
                end do
            end if
        end do
        close (unit)
        ok = count > 0
        if (.not. ok) message = 'closure roots contain no roots'
    end subroutine read_root_file

    subroutine read_lexical_file(path, facts, ok, message)
        character(len=*), intent(in) :: path
        type(standardir_lexical_facts_t), intent(out) :: facts
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: unit, ios

        call standardir_lexical_reset(facts)
        ok = .true.
        message = ''
        if (trim(path) == '-') return
        open (newunit=unit, file=trim(path), action='read', iostat=ios)
        if (ios /= 0) then
            ok = .false.
            message = 'cannot open lexical fact SX'
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            call sx_parse(line, node, ok, message)
            if (.not. ok) then
                close (unit)
                return
            end if
            if (is_label(node, 'lexical-fact')) then
                call standardir_lexical_add(node, facts, ok, message)
                if (.not. ok) then
                    close (unit)
                    return
                end if
            end if
        end do
        close (unit)
    end subroutine read_lexical_file

    subroutine emit_header(unit, format)
        integer, intent(in) :: unit, format

        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)') '(* origin=MECHANICAL; generated from closed source-backed StandardIR *)'
        case (standardir_grammar_format_antlr4)
            write (unit, '(a)') 'grammar StandardIR;'
            write (unit, '(a)') '// origin=MECHANICAL; generated from closed source-backed StandardIR'
        case (standardir_grammar_format_bison)
            write (unit, '(a)') '/* origin=MECHANICAL; generated from closed source-backed StandardIR */'
        case (standardir_grammar_format_tree_sitter)
            write (unit, '(a)') '// origin=MECHANICAL; generated from closed source-backed StandardIR'
            write (unit, '(a)') 'module.exports = grammar({'
            write (unit, '(a)') '  name: ''standardir'','
            write (unit, '(a)') '  rules: {'
        end select
    end subroutine emit_header

    subroutine print_dispositions(values)
        type(standardir_grammar_disposition_t), intent(in) :: values(:)
        integer :: i

        do i = 1, size(values)
            select case (values(i)%disposition)
            case (standardir_grammar_disposition_selected)
                print '(a)', 'root-disposition selected '//trim(values(i)%name)//' '//trim(values(i)%reason)
            case (standardir_grammar_disposition_omitted_root)
                print '(a)', 'root-disposition omitted-declared-root '//trim(values(i)%name)//' '// &
                    trim(values(i)%reason)
            case (standardir_grammar_disposition_omitted_helper)
                print '(a)', 'root-disposition omitted-helper '//trim(values(i)%name)//' '// &
                    trim(values(i)%reason)
            end select
        end do
    end subroutine print_dispositions

    subroutine emit_footer(unit, format)
        integer, intent(in) :: unit, format

        if (format == standardir_grammar_format_tree_sitter) then
            write (unit, '(a)') '  }'
            write (unit, '(a)') '});'
        end if
    end subroutine emit_footer

    subroutine emit_transformation_witness_file(path, rules, selected_mode, format, selected_root, roots, &
            role_family_mode, role_family, pre_lowering_witnesses, ok, message)
        character(len=*), intent(in) :: path
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        integer, intent(in) :: format
        logical, intent(in) :: selected_mode, role_family_mode
        character(len=*), intent(in) :: selected_root
        character(len=*), intent(in) :: roots(:)
        type(standardir_target_role_family_config_t), intent(in) :: role_family
        type(standardir_target_source_witness_t), intent(in) :: pre_lowering_witnesses(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: unit, ios

        ok = .false.
        message = ''
        open (newunit=unit, file=trim(path), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            message = 'cannot open transformation witness output'
            return
        end if
        if (selected_mode) then
            if (role_family_mode) then
                call standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
                    selected_root=selected_root, role_family=role_family, &
                    pre_lowering_witnesses=pre_lowering_witnesses, &
                    treesitter_lowering=format == standardir_grammar_format_tree_sitter)
            else
                call standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
                    selected_root=selected_root, pre_lowering_witnesses=pre_lowering_witnesses, &
                    treesitter_lowering=format == standardir_grammar_format_tree_sitter)
            end if
        else
            if (role_family_mode) then
                call standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
                    roots=roots, role_family=role_family, pre_lowering_witnesses=pre_lowering_witnesses, &
                    treesitter_lowering=format == standardir_grammar_format_tree_sitter)
            else
                call standardir_grammar_emit_transformation_witness(unit, rules, ok, message, &
                    roots=roots, pre_lowering_witnesses=pre_lowering_witnesses, &
                    treesitter_lowering=format == standardir_grammar_format_tree_sitter)
            end if
        end if
        if (.not. ok) then
            close (unit, status='delete')
            return
        end if
        close (unit)
    end subroutine emit_transformation_witness_file

    subroutine parse_format(text, format, ok, message)
        character(len=*), intent(in) :: text
        integer, intent(out) :: format
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        format = 0
        ok = .true.
        message = ''
        select case (trim(text))
        case ('ebnf')
            format = standardir_grammar_format_ebnf
        case ('antlr')
            format = standardir_grammar_format_antlr4
        case ('bison')
            format = standardir_grammar_format_bison
        case ('treesitter')
            format = standardir_grammar_format_tree_sitter
        case default
            ok = .false.
            message = 'unknown grammar format: '//trim(text)
        end select
    end subroutine parse_format

    subroutine parse_options(argc, selected_root, selected_mode, role_family, role_family_mode, &
            transformation_witness_path, transformation_witness_mode, ok, message)
        integer, intent(in) :: argc
        character(len=*), intent(out) :: selected_root, transformation_witness_path
        logical, intent(out) :: selected_mode, role_family_mode, transformation_witness_mode, ok
        type(standardir_target_role_family_config_t), intent(out) :: role_family
        character(len=*), intent(out) :: message

        character(len=4096) :: option, value
        integer :: i

        selected_root = ''
        selected_mode = .false.
        transformation_witness_path = ''
        transformation_witness_mode = .false.
        role_family = standardir_target_role_family_config_t()
        role_family_mode = .false.
        ok = .false.
        message = ''
        i = 7
        do while (i <= argc)
            call get_command_argument(i, option)
            if (len_trim(option) == 0) then
                message = 'empty option'
                return
            end if
            select case (trim(option))
            case ('--selected-root')
                if (selected_mode) then
                    message = 'duplicate --selected-root'
                    return
                end if
                if (i == argc) then
                    message = 'missing value for --selected-root'
                    return
                end if
                call get_command_argument(i + 1, value)
                if (len_trim(value) == 0) then
                    message = 'selected root is empty'
                    return
                end if
                if (is_option(value)) then
                    message = 'missing value for --selected-root'
                    return
                end if
                selected_root = trim(value)
                selected_mode = .true.
            case ('--role-family')
                if (role_family_mode) then
                    message = 'duplicate --role-family'
                    return
                end if
                if (i == argc) then
                    message = 'missing value for --role-family'
                    return
                end if
                call get_command_argument(i + 1, value)
                if (len_trim(value) == 0) then
                    message = 'role-family representative is empty'
                    return
                end if
                if (is_option(value)) then
                    message = 'missing value for --role-family'
                    return
                end if
                role_family%enabled = .true.
                role_family%representative = trim(value)
                role_family_mode = .true.
            case ('--transformation-witness')
                if (transformation_witness_mode) then
                    message = 'duplicate --transformation-witness'
                    return
                end if
                if (i == argc) then
                    message = 'missing value for --transformation-witness'
                    return
                end if
                call get_command_argument(i + 1, value)
                if (len_trim(value) == 0 .or. is_option(value)) then
                    message = 'missing value for --transformation-witness'
                    return
                end if
                transformation_witness_path = trim(value)
                transformation_witness_mode = .true.
            case default
                message = 'unknown option: '//trim(option)
                return
            end select
            i = i + 2
        end do
        ok = .true.
    end subroutine parse_options

    logical function is_option(text)
        character(len=*), intent(in) :: text

        is_option = .false.
        if (len_trim(text) < 2) return
        is_option = text(1:2) == '--'
    end function is_option

    subroutine append_node(values, value)
        type(sx_node_t), allocatable, intent(inout) :: values(:)
        type(sx_node_t), intent(in) :: value
        type(sx_node_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_node

    subroutine append_source_witnesses(values, incoming)
        type(standardir_target_source_witness_t), allocatable, intent(inout) :: values(:)
        type(standardir_target_source_witness_t), intent(in) :: incoming(:)
        type(standardir_target_source_witness_t), allocatable :: expanded(:)
        integer :: old_size, added

        old_size = size(values)
        added = size(incoming)
        allocate (expanded(old_size + added))
        if (old_size > 0) expanded(:old_size) = values
        if (added > 0) expanded(old_size + 1:old_size + added) = incoming
        call move_alloc(expanded, values)
    end subroutine append_source_witnesses

    subroutine append_classification(values, value)
        type(closure_classification_t), allocatable, intent(inout) :: values(:)
        type(closure_classification_t), intent(in) :: value
        type(closure_classification_t), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = value
        call move_alloc(expanded, values)
    end subroutine append_classification

    subroutine append_root(values, value)
        character(len=128), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: value
        character(len=128), allocatable :: expanded(:)
        integer :: n

        n = size(values)
        allocate (expanded(n + 1))
        if (n > 0) expanded(:n) = values
        expanded(n + 1) = trim(value)
        call move_alloc(expanded, values)
    end subroutine append_root

    logical function is_label(value, label)
        type(sx_node_t), intent(in) :: value
        character(len=*), intent(in) :: label

        is_label = .false.
        if (value%kind /= sx_list) return
        if (value%child_count < 1) return
        if (value%children(1)%kind /= sx_atom) return
        is_label = trim(value%children(1)%atom) == trim(label)
    end function is_label

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

    subroutine fail(text)
        character(len=*), intent(in) :: text

        print '(a)', 'error: '//trim(text)
        stop 1
    end subroutine fail

    subroutine fail_output(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text

        close (unit, status='delete')
        call fail(trim(text))
    end subroutine fail_output

end program sxgrammar

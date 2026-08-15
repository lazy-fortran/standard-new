module standardir_grammar_export
    !! Batch export for normalized, source-backed StandardIR grammar rules.

    use fortsx, only: sx_atom, sx_list, sx_max_atom_length, sx_node_t
    use standardir_bison, only: standardir_emit_bison_group
    use standardir_grammar, only: standardir_emit_antlr_group, standardir_emit_ebnf_group
    use standardir_grammar_export_support, only: standardir_grammar_apply_role_family, &
        standardir_grammar_validate_export_input
    use standardir_grammar_producer, only: standardir_grammar_choice, &
        standardir_grammar_optional, standardir_grammar_reference, &
        standardir_grammar_repeat, standardir_grammar_rule_t, &
        standardir_grammar_sequence, standardir_grammar_token
    use standardir_grammar_targetnorm, only: standardir_grammar_normalize, &
        standardir_grammar_factor_role_family, standardir_grammar_validate_role_family_witness, &
        standardir_target_expression_t, &
        standardir_target_provenance_t, standardir_target_role_family_config_t, &
        standardir_target_role_family_factored, standardir_target_role_family_rejected, &
        standardir_target_role_family_witness_t, standardir_target_rule_t
    use standardir_grammar_source_fingerprint, only: standardir_grammar_source_expression_sha256
    use standardir_grammar_reachability, only: standardir_grammar_select_reachable, &
        standardir_grammar_validate_reachability, standardir_target_reachability_witness_t
    use standardir_grouping, only: standardir_group_t, standardir_group_syntax, &
        standardir_max_syntax_groups
    use standardir_treesitter, only: standardir_emit_treesitter_group
    implicit none
    private

    integer, parameter, public :: standardir_grammar_format_ebnf = 1
    integer, parameter, public :: standardir_grammar_format_antlr4 = 2
    integer, parameter, public :: standardir_grammar_format_bison = 3
    integer, parameter, public :: standardir_grammar_format_tree_sitter = 4

    public :: standardir_grammar_export_batch
    public :: standardir_grammar_normalize
    public :: standardir_grammar_factor_role_family
    public :: standardir_grammar_validate_role_family_witness
    public :: standardir_target_expression_t
    public :: standardir_target_provenance_t
    public :: standardir_target_role_family_config_t
    public :: standardir_target_role_family_factored
    public :: standardir_target_role_family_rejected
    public :: standardir_target_role_family_witness_t
    public :: standardir_target_rule_t
    public :: standardir_target_reachability_witness_t
    public :: standardir_grammar_select_reachable
    public :: standardir_grammar_validate_reachability
    public :: standardir_grammar_source_expression_sha256

contains


    subroutine standardir_grammar_export_batch(unit, rules, format, ok, message, selected_root, &
            roots, reachability_witness, role_family, role_family_witness)
        integer, intent(in) :: unit, format
        type(standardir_grammar_rule_t), intent(in) :: rules(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        type(standardir_target_reachability_witness_t), allocatable, intent(out), optional :: &
            reachability_witness(:)
        type(standardir_target_role_family_config_t), intent(in), optional :: role_family
        type(standardir_target_role_family_witness_t), allocatable, intent(out), optional :: &
            role_family_witness(:)

        type(standardir_target_rule_t), allocatable :: normalized(:), suppressed(:)
        type(standardir_target_rule_t), allocatable :: pruned(:)
        type(standardir_target_reachability_witness_t), allocatable :: local_witness(:)
        type(standardir_target_role_family_witness_t), allocatable :: local_role_witness(:)
        type(sx_node_t), allocatable :: nodes(:), suppressed_nodes(:)
        type(standardir_group_t), allocatable :: groups(:)
        integer :: group_count, i, ios, scratch
        logical :: reachability_mode
        logical :: role_family_mode

        ok = .false.
        message = ''
        if (present(reachability_witness)) then
            if (allocated(reachability_witness)) deallocate (reachability_witness)
            allocate (reachability_witness(0))
        end if
        if (present(role_family_witness)) then
            if (allocated(role_family_witness)) deallocate (role_family_witness)
            allocate (role_family_witness(0))
        end if
        call standardir_grammar_validate_export_input(rules, format, standardir_grammar_format_ebnf, &
            standardir_grammar_format_tree_sitter, ok, message)
        if (.not. ok) return

        call standardir_grammar_normalize(rules, normalized, suppressed, ok, message)
        if (.not. ok) return
        call apply_reachability(normalized, selected_root, roots, pruned, local_witness, &
            reachability_mode, ok, message)
        if (.not. ok) return
        if (present(reachability_witness)) reachability_witness = local_witness
        call standardir_grammar_apply_role_family(normalized, selected_root, roots, reachability_mode, &
            role_family, role_family_mode, local_role_witness, ok, message)
        if (.not. ok) return
        if (present(role_family_witness)) role_family_witness = local_role_witness
        allocate (nodes(size(normalized)))
        do i = 1, size(normalized)
            call target_rule_to_syntax(normalized(i), nodes(i), ok, message)
            if (.not. ok) return
        end do
        allocate (suppressed_nodes(size(suppressed)))
        do i = 1, size(suppressed)
            call target_rule_to_syntax(suppressed(i), suppressed_nodes(i), ok, message)
            if (.not. ok) return
        end do
        allocate (groups(max(standardir_max_syntax_groups, size(nodes))))
        call standardir_group_syntax(nodes, size(nodes), groups, group_count, ok, message)
        if (.not. ok) return

        open (newunit=scratch, status='scratch', action='readwrite', iostat=ios)
        if (ios /= 0) then
            message = 'could not open grammar export scratch output'
            return
        end if
        if (reachability_mode) then
            call emit_reachability_witness(scratch, format, local_witness, ok, message)
            if (.not. ok) then
                close (scratch)
                return
            end if
        end if
        if (role_family_mode) then
            if (role_family%enabled) then
                call emit_role_family_witness(scratch, format, local_role_witness, ok, message)
                if (.not. ok) then
                    close (scratch)
                    return
                end if
            end if
        end if
        call emit_groups(scratch, nodes, groups, group_count, format, ok, message)
        if (.not. ok) then
            close (scratch)
            return
        end if
        rewind (scratch)
        call copy_output(scratch, unit, ok, message)
        close (scratch)
    end subroutine standardir_grammar_export_batch

    subroutine apply_reachability(normalized, selected_root, roots, pruned, witness, mode, ok, message)
        type(standardir_target_rule_t), allocatable, intent(inout) :: normalized(:)
        character(len=*), intent(in), optional :: selected_root
        character(len=*), intent(in), optional :: roots(:)
        type(standardir_target_rule_t), allocatable, intent(out) :: pruned(:)
        type(standardir_target_reachability_witness_t), allocatable, intent(out) :: witness(:)
        logical, intent(out) :: mode, ok
        character(len=*), intent(out) :: message

        type(standardir_target_rule_t), allocatable :: retained(:)
        character(len=128), allocatable :: reachability_roots(:)

        mode = present(selected_root) .or. present(roots)
        ok = .false.
        message = ''
        if (present(selected_root) .and. present(roots)) then
            message = 'grammar export cannot select a root and a root set together'
            return
        end if
        if (.not. mode) then
            allocate (pruned(0), witness(0))
            ok = .true.
            return
        end if
        if (present(selected_root)) then
            if (len_trim(selected_root) == 0) then
                message = 'grammar export selected root is empty'
                return
            end if
            allocate (reachability_roots(1))
            reachability_roots(1) = trim(selected_root)
        else
            if (size(roots) < 1) then
                message = 'grammar export root set is empty'
                return
            end if
            allocate (reachability_roots(size(roots)))
            reachability_roots = roots
        end if
        call standardir_grammar_select_reachable(normalized, reachability_roots, retained, pruned, witness, &
            ok, message)
        if (.not. ok) return
        call move_alloc(retained, normalized)
    end subroutine apply_reachability

    subroutine emit_reachability_witness(unit, format, values, ok, message)
        integer, intent(in) :: unit, format
        type(standardir_target_reachability_witness_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=:), allocatable :: lineage, expression_hashes
        integer :: i

        ok = .false.
        message = ''
        do i = 1, size(values)
            lineage = provenance_text(values(i)%provenance)
            expression_hashes = provenance_expression_text(values(i)%provenance)
            select case (format)
            case (standardir_grammar_format_ebnf)
                write (unit, '(a)', advance='no') '(* target-disposition=omitted-unreachable'
            case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
                write (unit, '(a)', advance='no') '// target-disposition=omitted-unreachable'
            case (standardir_grammar_format_bison)
                write (unit, '(a)', advance='no') '/* target-disposition=omitted-unreachable'
            case default
                message = 'grammar reachability witness format is unsupported'
                return
            end select
            call write_witness_field(unit, 'root', trim(values(i)%roots))
            call write_witness_field(unit, 'lhs', trim(values(i)%lhs))
            call write_witness_field(unit, 'rule', trim(values(i)%rule_id))
            call write_witness_field(unit, 'reason', trim(values(i)%reason))
            call write_witness_field(unit, 'document', trim(values(i)%source%document))
            call write_witness_field(unit, 'clause', trim(values(i)%source%clause))
            call write_witness_field(unit, 'source-rule', trim(values(i)%source%rule))
            call write_witness_field(unit, 'source-alternative', integer_text(values(i)%alternative))
            call write_witness_field(unit, 'page', integer_text(values(i)%source%page))
            call write_witness_field(unit, 'end-page', integer_text(values(i)%source%end_page))
            call write_witness_field(unit, 'byte-start', integer64_text(values(i)%source%byte_start))
            call write_witness_field(unit, 'byte-length', integer64_text(values(i)%source%byte_length))
            call write_witness_field(unit, 'source-sha256', trim(values(i)%source%source_hash))
            call write_witness_field(unit, 'source-lineage', trim(lineage))
            call write_witness_field(unit, 'source-expression-sha256', trim(expression_hashes))
            call write_witness_field(unit, 'target-expression-sha256', &
                trim(values(i)%target_expression_sha256))
            select case (format)
            case (standardir_grammar_format_ebnf)
                write (unit, '(a)') ' *)'
            case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
                write (unit, '(a)')
            case (standardir_grammar_format_bison)
                write (unit, '(a)') ' */'
            end select
        end do
        ok = .true.
        message = ''
    end subroutine emit_reachability_witness

    subroutine emit_role_family_witness(unit, format, values, ok, message)
        integer, intent(in) :: unit, format
        type(standardir_target_role_family_witness_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i
        character(len=:), allocatable :: roles, alias_hashes, representative_hashes

        ok = .false.
        message = ''
        do i = 1, size(values)
            roles = role_text(values(i)%source_roles)
            alias_hashes = provenance_expression_text(values(i)%alias_provenance)
            representative_hashes = provenance_expression_text(values(i)%representative_provenance)
            select case (format)
            case (standardir_grammar_format_ebnf)
                write (unit, '(a)', advance='no') '(* target-role-family'
            case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
                write (unit, '(a)', advance='no') '// target-role-family'
            case (standardir_grammar_format_bison)
                write (unit, '(a)', advance='no') '/* target-role-family'
            case default
                message = 'role-family witness format is unsupported'
                return
            end select
            call write_witness_field(unit, 'alias', trim(values(i)%alias_role))
            call write_witness_field(unit, 'representative', trim(values(i)%representative_role))
            call write_witness_field(unit, 'disposition', role_family_disposition_text(values(i)%disposition))
            call write_witness_field(unit, 'reason', trim(values(i)%reason))
            call write_witness_field(unit, 'source-roles', trim(roles))
            call write_witness_field(unit, 'alias-lineage', trim(provenance_text(values(i)%alias_provenance)))
            call write_witness_field(unit, 'source-expression-sha256', trim(alias_hashes))
            call write_witness_field(unit, 'target-expression-sha256', &
                trim(values(i)%alias_target_expression_sha256))
            call write_witness_field(unit, 'representative-lineage', &
                trim(provenance_text(values(i)%representative_provenance)))
            call write_witness_field(unit, 'representative-source-expression-sha256', &
                trim(representative_hashes))
            call write_witness_field(unit, 'representative-target-expression-sha256', &
                trim(values(i)%representative_target_expression_sha256))
            select case (format)
            case (standardir_grammar_format_ebnf)
                write (unit, '(a)') ' *)'
            case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
                write (unit, '(a)')
            case (standardir_grammar_format_bison)
                write (unit, '(a)') ' */'
            end select
        end do
        ok = .true.
        message = ''
    end subroutine emit_role_family_witness

    subroutine write_witness_field(unit, label, value)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: label, value

        write (unit, '(a)', advance='no') ' '
        write (unit, '(a)', advance='no') trim(label)
        write (unit, '(a)', advance='no') '='
        write (unit, '(a)', advance='no') trim(value)
    end subroutine write_witness_field

    subroutine emit_groups(unit, nodes, groups, group_count, format, ok, message)
        integer, intent(in) :: unit, group_count, format
        type(sx_node_t), intent(in) :: nodes(:)
        type(standardir_group_t), intent(in) :: groups(:)
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        integer :: i

        ok = .false.
        message = ''
        do i = 1, group_count
            select case (format)
            case (standardir_grammar_format_ebnf)
                call standardir_emit_ebnf_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_antlr4)
                call standardir_emit_antlr_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_bison)
                call standardir_emit_bison_group(unit, nodes, groups(i), ok, message)
            case (standardir_grammar_format_tree_sitter)
                call standardir_emit_treesitter_group(unit, nodes, groups(i), ok, message)
            end select
            if (.not. ok) return
        end do
        ok = .true.
        message = ''
    end subroutine emit_groups

    subroutine target_rule_to_syntax(rule, syntax, ok, message)
        type(standardir_target_rule_t), intent(in) :: rule
        type(sx_node_t), intent(out) :: syntax
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(syntax, 5)
        call make_atom(syntax%children(1), 'syntax')
        call make_atom(syntax%children(2), trim(rule%id))
        call make_pair(syntax%children(3), 'lhs', trim(rule%lhs), ok, message)
        if (.not. ok) return
        call make_list(syntax%children(4), 2)
        call make_atom(syntax%children(4)%children(1), 'rhs')
        call target_expression_to_syntax(rule%expression, syntax%children(4)%children(2), ok, message)
        if (.not. ok) return
        call make_target_source(syntax%children(5), rule, ok, message)
    end subroutine target_rule_to_syntax

    recursive subroutine target_expression_to_syntax(expression, node, ok, message)
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
                message = 'normalized target expression is empty'
                return
            end if
            call make_list(node, size(expression%children) + 1)
            if (expression%kind == standardir_grammar_sequence) then
                call make_atom(node%children(1), 'seq')
            else
                call make_atom(node%children(1), 'alt')
            end if
            do i = 1, size(expression%children)
                call target_expression_to_syntax(expression%children(i), node%children(i + 1), ok, message)
                if (.not. ok) return
            end do
        case (standardir_grammar_optional, standardir_grammar_repeat)
            call make_list(node, merge(4, 2, expression%kind == standardir_grammar_repeat))
            if (expression%kind == standardir_grammar_optional) then
                call make_atom(node%children(1), 'optional')
            else
                call make_atom(node%children(1), 'repeat')
                write (minimum, '(i0)') expression%minimum
                call make_atom(node%children(3), trim(minimum))
                call make_atom(node%children(4), 'unbounded')
            end if
            call target_expression_to_syntax(expression%children(1), node%children(2), ok, message)
            if (.not. ok) return
        case default
            write (message, '(a,i0)') 'normalized target expression has unsupported kind ', expression%kind
            return
        end select
        ok = .true.
        message = ''
    end subroutine target_expression_to_syntax

    subroutine suppressed_provenance(unit, node, lhs, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        character(len=*), intent(in) :: lhs
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: rule_id, node_lhs, document, clause, page, source_hash
        integer :: i

        ok = .false.
        message = ''
        if (node%child_count /= 5 .or. node%children(3)%kind /= sx_list .or. &
            node%children(3)%child_count /= 2 .or. node%children(3)%children(2)%kind /= sx_atom) then
            message = 'suppressed grammar provenance is malformed'
            return
        end if
        node_lhs = trim(node%children(3)%children(2)%atom)
        if (node_lhs /= trim(lhs)) then
            ok = .true.
            return
        end if
        if (node%children(2)%kind /= sx_atom .or. node%children(5)%kind /= sx_list) then
            message = 'suppressed grammar provenance header is malformed'
            return
        end if
        rule_id = trim(node%children(2)%atom)
        document = ''; clause = ''; page = ''; source_hash = ''
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'suppressed grammar provenance field is malformed'
                return
            end if
            select case (trim(node%children(5)%children(i)%children(1)%atom))
            case ('document')
                document = trim(node%children(5)%children(i)%children(2)%atom)
            case ('clause')
                clause = trim(node%children(5)%children(i)%children(2)%atom)
            case ('page')
                page = trim(node%children(5)%children(i)%children(2)%atom)
            case ('source-sha256')
                source_hash = trim(node%children(5)%children(i)%children(2)%atom)
            end select
        end do
        call emit_source_rule_annotation(unit, node, format, ok, message)
        if (.not. ok) return
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(*'
            call write_witness_field(unit, 'rule', trim(rule_id))
            call write_witness_field(unit, 'document', trim(document))
            call write_witness_field(unit, 'clause', trim(clause))
            call write_witness_field(unit, 'page', trim(page))
            call write_witness_field(unit, 'source-canonical-text-sha256', trim(source_hash))
            write (unit, '(a)') ' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)', advance='no') '//'
            call write_witness_field(unit, 'rule', trim(rule_id))
            call write_witness_field(unit, 'document', trim(document))
            call write_witness_field(unit, 'clause', trim(clause))
            call write_witness_field(unit, 'page', trim(page))
            call write_witness_field(unit, 'source-canonical-text-sha256', trim(source_hash))
            write (unit, '(a)') ''
        case (standardir_grammar_format_bison)
            write (unit, '(a)', advance='no') '/*'
            call write_witness_field(unit, 'rule', trim(rule_id))
            call write_witness_field(unit, 'document', trim(document))
            call write_witness_field(unit, 'clause', trim(clause))
            call write_witness_field(unit, 'page', trim(page))
            call write_witness_field(unit, 'source-canonical-text-sha256', trim(source_hash))
            write (unit, '(a)') ' */'
        end select
        ok = .true.
        message = ''
    end subroutine suppressed_provenance

    subroutine make_target_source(node, rule, ok, message)
        type(sx_node_t), intent(out) :: node
        type(standardir_target_rule_t), intent(in) :: rule
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message
        character(len=32) :: alternative

        call make_list(node, 13)
        call make_atom(node%children(1), 'source')
        call make_pair(node%children(2), 'document', trim(rule%source%document), ok, message)
        if (.not. ok) return
        call make_pair(node%children(3), 'clause', trim(rule%source%clause), ok, message)
        if (.not. ok) return
        call make_pair(node%children(4), 'rule', trim(rule%source%rule), ok, message)
        if (.not. ok) return
        write (alternative, '(i0)') rule%alternative
        call make_pair(node%children(5), 'alternative', trim(alternative), ok, message)
        if (.not. ok) return
        call make_pair(node%children(6), 'page', integer_text(rule%source%page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(7), 'end-page', integer_text(rule%source%end_page), ok, message)
        if (.not. ok) return
        call make_pair(node%children(8), 'byte-start', integer64_text(rule%source%byte_start), ok, message)
        if (.not. ok) return
        call make_pair(node%children(9), 'byte-length', integer64_text(rule%source%byte_length), ok, message)
        if (.not. ok) return
        call make_pair(node%children(10), 'source-sha256', trim(rule%source%source_hash), ok, message)
        if (.not. ok) return
        call make_pair(node%children(11), 'source-expression-sha256', &
            provenance_expression_text(rule%provenance), ok, message)
        if (.not. ok) return
        call make_pair(node%children(12), 'target-expression-sha256', &
            trim(rule%target_expression_sha256), ok, message)
        if (.not. ok) return
        call make_pair(node%children(13), 'source-lineage', provenance_text(rule%provenance), ok, message)
    end subroutine make_target_source

    subroutine emit_source_rule_annotation(unit, node, format, ok, message)
        integer, intent(in) :: unit, format
        type(sx_node_t), intent(in) :: node
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=256) :: source_rule, source_alternative
        character(len=:), allocatable :: source_lineage, source_expression_hashes, target_expression_hash
        character(len=64) :: source_start, source_length
        integer :: i

        ok = .false.
        message = ''
        source_rule = ''
        source_alternative = ''
        source_lineage = ''
        source_expression_hashes = ''
        target_expression_hash = ''
        source_start = ''
        source_length = ''
        if (node%child_count /= 5 .or. node%children(5)%kind /= sx_list .or. &
            node%children(5)%child_count < 6) then
            message = 'canonical grammar source is malformed'
            return
        end if
        do i = 2, node%children(5)%child_count
            if (node%children(5)%children(i)%kind /= sx_list .or. &
                node%children(5)%children(i)%child_count /= 2 .or. &
                node%children(5)%children(i)%children(1)%kind /= sx_atom .or. &
                node%children(5)%children(i)%children(2)%kind /= sx_atom) then
                message = 'canonical grammar source child is malformed'
                return
            end if
            if (trim(node%children(5)%children(i)%children(1)%atom) == 'rule') then
                source_rule = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'alternative') then
                source_alternative = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'source-expression-sha256') then
                source_expression_hashes = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'source-lineage') then
                source_lineage = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'target-expression-sha256') then
                target_expression_hash = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'byte-start') then
                source_start = trim(node%children(5)%children(i)%children(2)%atom)
            else if (trim(node%children(5)%children(i)%children(1)%atom) == 'byte-length') then
                source_length = trim(node%children(5)%children(i)%children(2)%atom)
            end if
        end do
        if (len_trim(source_rule) == 0) then
            message = 'canonical grammar source rule is empty'
            return
        end if
        select case (format)
        case (standardir_grammar_format_ebnf)
            write (unit, '(a)', advance='no') '(* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            if (len_trim(source_lineage) > 0) write (unit, '(a)', advance='no') &
                ' source-lineage='//trim(source_lineage)
            if (len_trim(source_expression_hashes) > 0) write (unit, '(a)', advance='no') &
                ' source-expression-sha256='//trim(source_expression_hashes)
            if (len_trim(target_expression_hash) > 0) write (unit, '(a)', advance='no') &
                ' target-expression-sha256='//trim(target_expression_hash)
            if (len_trim(source_start) > 0) write (unit, '(a)', advance='no') &
                ' source-byte-start='//trim(source_start)//' source-byte-length='//trim(source_length)
            write (unit, '(a)') ' *)'
        case (standardir_grammar_format_antlr4, standardir_grammar_format_tree_sitter)
            write (unit, '(a)', advance='no') '// source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            if (len_trim(source_lineage) > 0) write (unit, '(a)', advance='no') &
                ' source-lineage='//trim(source_lineage)
            if (len_trim(source_expression_hashes) > 0) write (unit, '(a)', advance='no') &
                ' source-expression-sha256='//trim(source_expression_hashes)
            if (len_trim(target_expression_hash) > 0) write (unit, '(a)', advance='no') &
                ' target-expression-sha256='//trim(target_expression_hash)
            if (len_trim(source_start) > 0) write (unit, '(a)', advance='no') &
                ' source-byte-start='//trim(source_start)//' source-byte-length='//trim(source_length)
            write (unit, '(a)')
        case (standardir_grammar_format_bison)
            write (unit, '(a)', advance='no') '/* source-rule='//trim(source_rule)
            if (len_trim(source_alternative) > 0) write (unit, '(a)', advance='no') &
                ' source-alternative='//trim(source_alternative)
            if (len_trim(source_lineage) > 0) write (unit, '(a)', advance='no') &
                ' source-lineage='//trim(source_lineage)
            if (len_trim(source_expression_hashes) > 0) write (unit, '(a)', advance='no') &
                ' source-expression-sha256='//trim(source_expression_hashes)
            if (len_trim(target_expression_hash) > 0) write (unit, '(a)', advance='no') &
                ' target-expression-sha256='//trim(target_expression_hash)
            if (len_trim(source_start) > 0) write (unit, '(a)', advance='no') &
                ' source-byte-start='//trim(source_start)//' source-byte-length='//trim(source_length)
            write (unit, '(a)') ' */'
        end select
        ok = .true.
        message = ''
    end subroutine emit_source_rule_annotation

    function provenance_text(values) result(text)
        type(standardir_target_provenance_t), allocatable, intent(in) :: values(:)
        character(len=:), allocatable :: text
        character(len=512) :: item
        integer :: i, length, position, total

        if (.not. allocated(values)) then
            text = 'none'
            return
        end if
        total = 0
        do i = 1, size(values)
            write (item, '(a,":",i0,"@",i0,"+",i0)') trim(values(i)%source%rule), &
                values(i)%alternative, values(i)%source%byte_start, values(i)%source%byte_length
            total = total + len_trim(item)
        end do
        total = total + max(0, size(values) - 1)
        allocate (character(len=max(1, total)) :: text)
        text = repeat(' ', len(text))
        position = 1
        do i = 1, size(values)
            write (item, '(a,":",i0,"@",i0,"+",i0)') trim(values(i)%source%rule), &
                values(i)%alternative, values(i)%source%byte_start, values(i)%source%byte_length
            if (i > 1) then
                text(position:position) = ','
                position = position + 1
            end if
            length = len_trim(item)
            text(position:position + length - 1) = trim(item)
            position = position + length
        end do
        if (len_trim(text) == 0) text = 'none'
    end function provenance_text

    function provenance_expression_text(values) result(text)
        type(standardir_target_provenance_t), allocatable, intent(in) :: values(:)
        character(len=:), allocatable :: text
        integer :: i, length, position, total

        if (.not. allocated(values)) then
            text = 'none'
            return
        end if
        total = 0
        do i = 1, size(values)
            if (values(i)%source_expression_present) then
                total = total + len_trim(values(i)%source_expression_sha256)
            else
                total = total + len('none')
            end if
        end do
        total = total + max(0, size(values) - 1)
        allocate (character(len=max(1, total)) :: text)
        text = repeat(' ', len(text))
        position = 1
        do i = 1, size(values)
            if (i > 1) then
                text(position:position) = ','
                position = position + 1
            end if
            if (values(i)%source_expression_present) then
                length = len_trim(values(i)%source_expression_sha256)
                if (length == 0) then
                    text = 'none'
                    return
                end if
                text(position:position + length - 1) = trim(values(i)%source_expression_sha256)
            else
                length = len('none')
                text(position:position + length - 1) = 'none'
            end if
            position = position + length
        end do
        if (len_trim(text) == 0) text = 'none'
    end function provenance_expression_text

    function role_text(values) result(text)
        character(len=128), allocatable, intent(in) :: values(:)
        character(len=:), allocatable :: text
        integer :: i, length, position, total

        if (.not. allocated(values)) then
            text = 'none'
            return
        end if
        total = 0
        do i = 1, size(values)
            total = total + len_trim(values(i))
        end do
        total = total + max(0, size(values) - 1)
        allocate (character(len=max(1, total)) :: text)
        text = repeat(' ', len(text))
        position = 1
        do i = 1, size(values)
            if (i > 1) then
                text(position:position) = ','
                position = position + 1
            end if
            length = len_trim(values(i))
            if (length > 0) then
                text(position:position + length - 1) = trim(values(i))
                position = position + length
            end if
        end do
        if (len_trim(text) == 0) text = 'none'
    end function role_text

    function role_family_disposition_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text

        select case (value)
        case (standardir_target_role_family_factored)
            text = 'factored'
        case (standardir_target_role_family_rejected)
            text = 'rejected'
        case default
            text = 'invalid'
        end select
        text = trim(text)
    end function role_family_disposition_text

    function integer64_text(value) result(text)
        use, intrinsic :: iso_fortran_env, only: int64
        integer(int64), intent(in) :: value
        character(len=64) :: text

        write (text, '(i0)') value
        text = trim(text)
    end function integer64_text

    subroutine copy_output(source_unit, target_unit, ok, message)
        integer, intent(in) :: source_unit, target_unit
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        character(len=sx_max_atom_length) :: line
        integer :: ios

        ok = .false.
        message = ''
        do
            read (source_unit, '(a)', iostat=ios) line
            if (ios < 0) exit
            if (ios > 0) then
                message = 'could not read staged grammar export'
                return
            end if
            write (target_unit, '(a)', iostat=ios) trim(line)
            if (ios /= 0) then
                message = 'could not write grammar export'
                return
            end if
        end do
        ok = .true.
        message = ''
    end subroutine copy_output


    subroutine make_pair(node, label, value, ok, message)
        type(sx_node_t), intent(out) :: node
        character(len=*), intent(in) :: label, value
        logical, intent(out) :: ok
        character(len=*), intent(out) :: message

        call make_list(node, 2)
        call make_atom(node%children(1), label)
        call make_atom(node%children(2), value)
        ok = len_trim(label) > 0 .and. len_trim(value) > 0
        if (ok) then
            message = ''
        else
            message = 'canonical grammar field is empty'
        end if
    end subroutine make_pair

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

    character(len=32) function integer_text(value)
        integer, intent(in) :: value

        write (integer_text, '(i0)') value
    end function integer_text

end module standardir_grammar_export

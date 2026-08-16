module standardir_grammar_correspondence
    !! Typed source-to-target correspondence rows for generic normalization.

    use standardir_export, only: standardir_source_ref_t
    implicit none
    private

    character(len=*), parameter, public :: standardir_correspondence_mapped = 'mapped'
    character(len=*), parameter, public :: standardir_correspondence_ambiguous = 'ambiguous'
    character(len=*), parameter, public :: standardir_correspondence_suppressed = 'suppressed'
    character(len=*), parameter, public :: standardir_correspondence_unsupported = 'unsupported'

    type, public :: standardir_grammar_correspondence_trace_t
        type(standardir_source_ref_t) :: source
        integer :: source_alternative = 0
        character(len=512) :: raw_source_expression_path = ''
        integer :: source_node_kind = 0
        character(len=128) :: source_node_name = ''
        character(len=64) :: source_boundary_role = ''
        character(len=128) :: target_rule_id = ''
        character(len=128) :: target_lhs = ''
        integer :: target_alternative = 0
        character(len=512) :: target_expression_path = ''
        integer :: target_sequence_boundary_slot = 0
        type(standardir_source_ref_t) :: retained_target_source
        integer :: retained_target_source_alternative = 0
        character(len=512) :: retained_target_expression_path = ''
        integer :: retained_target_sequence_boundary_slot = 0
        character(len=64) :: transformation = ''
        character(len=64) :: input_expression_sha256 = ''
        character(len=64) :: output_expression_sha256 = ''
        character(len=64) :: source_expression_sha256 = ''
        character(len=64) :: target_expression_sha256 = ''
        character(len=16) :: disposition = ''
        character(len=256) :: reason = ''
    end type standardir_grammar_correspondence_trace_t

    public :: standardir_grammar_append_correspondence_trace

contains

    subroutine standardir_grammar_append_correspondence_trace(values, value)
        type(standardir_grammar_correspondence_trace_t), allocatable, intent(inout) :: values(:)
        type(standardir_grammar_correspondence_trace_t), intent(in) :: value
        type(standardir_grammar_correspondence_trace_t), allocatable :: expanded(:)
        integer :: old_size

        old_size = size(values)
        allocate (expanded(old_size + 1))
        if (old_size > 0) expanded(:old_size) = values
        expanded(old_size + 1) = value
        call move_alloc(expanded, values)
    end subroutine standardir_grammar_append_correspondence_trace

end module standardir_grammar_correspondence

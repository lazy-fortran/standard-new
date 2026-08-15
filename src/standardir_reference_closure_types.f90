module standardir_reference_closure_types
    !! Public data contract for source-backed reference closure.

    use standardir_export, only: standardir_source_ref_t
    implicit none
    private

    ! Compatibility sizing hint only; input and result reference tables are
    ! dynamically sized from the source record.
    integer, parameter, public :: closure_max_references = 32
    integer, parameter, public :: closure_max_records = 512
    integer, parameter, public :: closure_max_classifications = 512
    integer, parameter, public :: closure_max_name_length = 128

    integer, parameter, public :: closure_kind_production = 1
    integer, parameter, public :: closure_kind_alias = 2
    integer, parameter, public :: closure_kind_list = 3
    integer, parameter, public :: closure_kind_scalar = 4
    integer, parameter, public :: closure_kind_lexical = 5
    integer, parameter, public :: closure_kind_erratum = 6
    integer, parameter, public :: closure_kind_semantic_only = 7
    integer, parameter, public :: closure_kind_unresolved = 8

    type, public :: closure_reference_t
        character(len=closure_max_name_length) :: name = ''
        type(standardir_source_ref_t) :: source
    end type closure_reference_t

    type, public :: closure_input_record_t
        character(len=closure_max_name_length) :: id = ''
        character(len=closure_max_name_length) :: lhs = ''
        integer :: reference_count = 0
        type(closure_reference_t), allocatable :: references(:)
        type(standardir_source_ref_t) :: source
    end type closure_input_record_t

    type, public :: closure_classification_t
        character(len=closure_max_name_length) :: name = ''
        integer :: kind = 0
        character(len=closure_max_name_length) :: target = ''
        character(len=closure_max_name_length) :: separator = ''
        character(len=closure_max_name_length) :: terminal = ''
        character(len=closure_max_name_length) :: family = ''
        character(len=closure_max_name_length) :: suffix = ''
        character(len=closure_max_name_length) :: prefix = ''
        type(standardir_source_ref_t) :: source
    end type closure_classification_t

    type, public :: closure_source_witness_t
        type(standardir_source_ref_t) :: source
        integer :: alternative = 0
        character(len=64) :: source_expression_sha256 = ''
    end type closure_source_witness_t

    type, public :: closure_record_t
        character(len=closure_max_name_length) :: id = ''
        character(len=closure_max_name_length) :: lhs = ''
        integer :: reference_count = 0
        type(closure_reference_t), allocatable :: references(:)
        type(standardir_source_ref_t) :: source
        type(standardir_source_ref_t) :: provenance
        integer :: kind = 0
        logical :: derived = .false.
        character(len=closure_max_name_length) :: derived_from = ''
        character(len=closure_max_name_length) :: target = ''
        character(len=closure_max_name_length) :: separator = ''
        character(len=closure_max_name_length) :: terminal = ''
        logical :: source_witness_present = .false.
        type(closure_source_witness_t) :: source_witness
    end type closure_record_t

    type, public :: closure_result_t
        integer :: record_count = 0
        integer :: normative_count = 0
        integer :: derived_count = 0
        type(closure_record_t), allocatable :: records(:)
    end type closure_result_t

end module standardir_reference_closure_types

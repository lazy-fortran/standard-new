module standardir_intrinsic_type_spec_generated
    !! Generated from specs/grammar-facts-v0.sx; do not edit.

    implicit none
    private

    type, public :: standardir_intrinsic_type_spec_t
        character(len=32) :: canonical_name = ''
        character(len=128) :: source_spelling = ''
        character(len=64) :: source_rule = ''
        character(len=128) :: document = ''
        character(len=64) :: clause = ''
        integer :: page = 0
        character(len=64) :: source_hash = ''
    end type standardir_intrinsic_type_spec_t

    integer, parameter, public :: standardir_intrinsic_type_spec_count = 6
    public :: standardir_make_intrinsic_type_spec_lookup
    public :: standardir_lookup_intrinsic_type_spec

contains

    subroutine standardir_make_intrinsic_type_spec_lookup(values)
        type(standardir_intrinsic_type_spec_t), intent(out) :: values(6)

        values(1)%canonical_name = 'integer'
        values(1)%source_spelling = 'INTEGER [ kind-selector ]'
        values(1)%source_rule = 'R705'
        values(1)%document = 'J3-24-007'
        values(1)%clause = '7'
        values(1)%page = 67
        values(1)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(2)%canonical_name = 'real'
        values(2)%source_spelling = 'REAL [ kind-selector ]'
        values(2)%source_rule = 'R706'
        values(2)%document = 'J3-24-007'
        values(2)%clause = '7'
        values(2)%page = 67
        values(2)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(3)%canonical_name = 'double_precision'
        values(3)%source_spelling = 'DOUBLE PRECISION'
        values(3)%source_rule = 'R707'
        values(3)%document = 'J3-24-007'
        values(3)%clause = '7'
        values(3)%page = 67
        values(3)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(4)%canonical_name = 'complex'
        values(4)%source_spelling = 'COMPLEX'
        values(4)%source_rule = 'R704'
        values(4)%document = 'J3-24-007'
        values(4)%clause = '7'
        values(4)%page = 80
        values(4)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(5)%canonical_name = 'logical'
        values(5)%source_spelling = 'LOGICAL [ kind-selector ]'
        values(5)%source_rule = 'R704'
        values(5)%document = 'J3-24-007'
        values(5)%clause = '7'
        values(5)%page = 80
        values(5)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
        values(6)%canonical_name = 'character'
        values(6)%source_spelling = 'CHARACTER [ char-selector ]'
        values(6)%source_rule = 'R704'
        values(6)%document = 'J3-24-007'
        values(6)%clause = '7'
        values(6)%page = 80
        values(6)%source_hash = '7371e889f231cfb0316d30365d5083fb5af34cbb6d5f7cb1e01855c73021bfa2'
    end subroutine standardir_make_intrinsic_type_spec_lookup

    subroutine standardir_lookup_intrinsic_type_spec(source_spelling, value, found)
        character(len=*), intent(in) :: source_spelling
        type(standardir_intrinsic_type_spec_t), intent(out) :: value
        logical, intent(out) :: found

        type(standardir_intrinsic_type_spec_t) :: values(6)
        integer :: i

        call standardir_make_intrinsic_type_spec_lookup(values)
        value = standardir_intrinsic_type_spec_t()
        found = .false.
        do i = 1, size(values)
            if (trim(values(i)%source_spelling) == trim(source_spelling)) then
                value = values(i)
                found = .true.
                return
            end if
        end do
    end subroutine standardir_lookup_intrinsic_type_spec

end module standardir_intrinsic_type_spec_generated

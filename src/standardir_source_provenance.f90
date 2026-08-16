module standardir_source_provenance
    !! Generic relations between StandardIR rule identifiers and clauses.

    implicit none
    private

    public :: standardir_normative_clause

contains

    subroutine standardir_normative_clause(rule, clause, found)
        character(len=*), intent(in) :: rule
        character(len=*), intent(out) :: clause
        logical, intent(out) :: found

        integer :: i, number, ios, n

        clause = ''
        found = .false.
        n = len_trim(rule)
        if (n < 4) return
        if (rule(1:1) /= 'R') return
        i = 2
        do while (i <= n)
            if (rule(i:i) < '0' .or. rule(i:i) > '9') return
            i = i + 1
        end do
        read (rule(2:n), *, iostat=ios) number
        if (ios /= 0 .or. number < 100) return
        write (clause, '(i0)') number / 100
        found = .true.
    end subroutine standardir_normative_clause

end module standardir_source_provenance

#!/usr/bin/env bash
# Enforce the No Concatenation Rule (D0011) on Fortran sources.
#
# The rule: repeated character(:) concatenation is forbidden in
# performance-relevant or unbounded code. Generated text goes through a builder
# or a streaming writer.
#
# What this detects is the accumulator pattern specifically, where a variable is
# assigned its own concatenation:
#
#     s = s // something
#     buf = trim(buf)//x
#
# That is the shape that turns linear work into quadratic work, and it is the
# shape that recurred in fortfront across `6e688073` and `96dc3314` seven months
# apart. A single one-off concatenation is not flagged; it is not the defect.
#
# Also flagged: declaring an allocatable character as a general text buffer in
# src/. Boundary code needs one occasionally, so the escape is explicit and must
# name a reason:
#
#     character(:), allocatable :: msg   ! text-policy: OS boundary, freed at once
#
# Usage:
#   check_text_policy.sh [DIR ...]     default: src app
#   check_text_policy.sh --self-test   prove the checker can fail
#
# Exit status: 0 clean, 1 violations found.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

scan() {
    local dirs=("$@") found=0 f line no
    local files=()
    for d in "${dirs[@]}"; do
        [ -d "$d" ] || continue
        while IFS= read -r f; do files+=("$f"); done \
            < <(find "$d" -name '*.f90' -o -name '*.inc' | sort)
    done
    [ ${#files[@]} -gt 0 ] || return 0

    for f in "${files[@]}"; do
        # Accumulator concatenation: lhs = ... lhs ... // ...
        while IFS=: read -r no line; do
            [ -n "$no" ] || continue
            printf '%s:%s: accumulator concatenation, use a builder or writer (D0011)\n' \
                "$f" "$no"
            printf '    %s\n' "$(printf '%s' "$line" | sed 's/^[ \t]*//')"
            found=1
        done < <(grep -nE '^[^!]*\b([a-zA-Z_][a-zA-Z0-9_%]*)[ \t]*=[^=]*\b\1\b[^=]*//' "$f" || true)

        # Allocatable character declarations without an explicit escape note.
        while IFS=: read -r no line; do
            [ -n "$no" ] || continue
            case "$line" in *text-policy:*) continue ;; esac
            printf '%s:%s: allocatable character buffer, use bytes and spans (D0011)\n' \
                "$f" "$no"
            printf '    %s\n' "$(printf '%s' "$line" | sed 's/^[ \t]*//')"
            printf '    add a trailing  ! text-policy: <reason>  if this is a boundary\n'
            found=1
        done < <(grep -nE '^[^!]*character\((len=)?:\)[ \t]*,[ \t]*allocatable' "$f" || true)
    done
    return $found
}

self_test() {
    # A gate that has never been observed failing is not evidence. This builds
    # both a violating and a clean fixture and asserts the checker separates
    # them. LESSONS.md section 6 is why this exists.
    local tmp rc
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' RETURN
    mkdir -p "$tmp/bad" "$tmp/good"

    cat > "$tmp/bad/violation.f90" <<'EOF'
module violation
contains
    subroutine build(out)
        character(:), allocatable :: out
        integer :: i
        out = ''
        do i = 1, 10
            out = out//'x'
        end do
    end subroutine
end module
EOF

    cat > "$tmp/good/compliant.f90" <<'EOF'
module compliant
    use iso_fortran_env, only: int8
    implicit none
    type :: byte_builder_t
        integer(int8), allocatable :: data(:)
        integer :: size = 0
    end type
contains
    subroutine emit(b, arg)
        type(byte_builder_t), intent(inout) :: b
        character(len=*), intent(in) :: arg
        character(:), allocatable :: os_buffer  ! text-policy: OS boundary
        os_buffer = trim(arg)
        b%size = b%size + len(os_buffer)
    end subroutine
end module
EOF

    rc=0
    scan "$tmp/bad" >/dev/null 2>&1 || rc=$?
    if [ "$rc" -eq 0 ]; then
        printf 'SELF-TEST FAILED: the checker accepted an accumulator concatenation\n' >&2
        return 1
    fi

    rc=0
    scan "$tmp/good" || rc=$?
    if [ "$rc" -ne 0 ]; then
        printf 'SELF-TEST FAILED: the checker rejected compliant code\n' >&2
        return 1
    fi

    printf 'text-policy checker self-test: ok (rejects violations, accepts compliant code)\n'
    return 0
}

if [ "${1:-}" = --self-test ]; then
    self_test
    exit $?
fi

if [ "${1:-}" = -h ] || [ "${1:-}" = --help ]; then
    sed -n '2,28p' "$0" | sed 's/^# \{0,1\}//'
    exit 0
fi

cd "$ROOT"
dirs=("$@")
[ ${#dirs[@]} -gt 0 ] || dirs=(src app)

rc=0
scan "${dirs[@]}" || rc=$?
if [ "$rc" -eq 0 ]; then
    printf 'text policy: clean (%s)\n' "${dirs[*]}"
else
    printf '\ntext policy: violations found. See D0011 and docs/text-representation.md\n' >&2
fi
exit $rc

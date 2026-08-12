program test_interner
    !! Fixed byte names establish case-folding and stable identity behavior.

    use, intrinsic :: iso_fortran_env, only: int8
    use byte_span, only: byte_span_from_array, byte_span_t
    use interner
    implicit none

    integer(int8), target :: foo_upper(3) = [int(70, int8), int(79, int8), int(79, int8)]
    integer(int8), target :: foo_mixed(3) = [int(102, int8), int(79, int8), int(111, int8)]
    integer(int8), target :: bar(3) = [int(66, int8), int(97, int8), int(114, int8)]
    integer(int8), target :: baz(3) = [int(66, int8), int(97, int8), int(122, int8)]
    integer(int8), parameter :: expected_foo(3) = [int(102, int8), int(111, int8), int(111, int8)]
    integer(int8), allocatable :: key(:)
    type(interner_t) :: table
    type(byte_span_t) :: source
    logical :: is_new, ok
    character(len=256) :: message
    integer :: foo_id, same_id, bar_id, baz_id

    call interner_init(table, 2, ok, message)
    call require(ok, message)
    call byte_span_from_array(foo_upper, 1, size(foo_upper), source, ok, message)
    call require(ok, message)
    call interner_intern(table, source, foo_id, is_new, ok, message)
    call require(ok .and. is_new, 'first name was not inserted')
    call byte_span_from_array(foo_mixed, 1, size(foo_mixed), source, ok, message)
    call require(ok, message)
    call interner_intern(table, source, same_id, is_new, ok, message)
    call require(ok .and. .not. is_new, 'case variant was inserted twice')
    call require(same_id == foo_id, 'case variant received another ID')

    call byte_span_from_array(bar, 1, size(bar), source, ok, message)
    call require(ok, message)
    call interner_intern(table, source, bar_id, is_new, ok, message)
    call require(ok .and. is_new, 'distinct name was not inserted')
    call require(bar_id /= foo_id, 'distinct name reused the ID')
    call byte_span_from_array(baz, 1, size(baz), source, ok, message)
    call require(ok, message)
    call interner_intern(table, source, baz_id, is_new, ok, message)
    call require(ok .and. is_new, 'name after table growth was not inserted')
    call interner_intern(table, source, same_id, is_new, ok, message)
    call require(ok .and. .not. is_new .and. same_id == baz_id, &
        'name after table growth was not stable')
    call interner_key(table, foo_id, key, ok, message)
    call require(ok, message)
    call require(all(key == expected_foo), 'interned key differs from oracle')
    call require(interner_count(table) == 3, 'interner count differs')

    call byte_span_from_array(foo_upper, 1, 0, source, ok, message)
    call require(ok, message)
    call interner_intern(table, source, same_id, is_new, ok, message)
    call require(.not. ok, 'empty name was accepted')

    print '(a)', 'interner test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_interner

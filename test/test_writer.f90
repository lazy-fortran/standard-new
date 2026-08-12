program test_writer
    !! Fixed vectors independently establish writer bytes and SHA-256 output.

    use, intrinsic :: iso_fortran_env, only: int8, int64
    use writer
    use byte_span, only: byte_span_from_array, byte_span_get, byte_span_t
    implicit none

    integer(int8), parameter :: source(3) = [int(65, int8), int(0, int8), int(10, int8)]
    integer(int8), parameter :: expected_memory(6) = [ &
        int(65, int8), int(0, int8), int(10, int8), int(65, int8), int(66, int8), &
        int(67, int8)]
    integer(int8), parameter :: empty_digest(32) = [ &
        int(z'E3', int8), int(z'B0', int8), int(z'C4', int8), int(z'42', int8), &
        int(z'98', int8), int(z'FC', int8), int(z'1C', int8), int(z'14', int8), &
        int(z'9A', int8), int(z'FB', int8), int(z'F4', int8), int(z'C8', int8), &
        int(z'99', int8), int(z'6F', int8), int(z'B9', int8), int(z'24', int8), &
        int(z'27', int8), int(z'AE', int8), int(z'41', int8), int(z'E4', int8), &
        int(z'64', int8), int(z'9B', int8), int(z'93', int8), int(z'4C', int8), &
        int(z'A4', int8), int(z'95', int8), int(z'99', int8), int(z'1B', int8), &
        int(z'78', int8), int(z'52', int8), int(z'B8', int8), int(z'55', int8)]
    integer(int8), parameter :: abc_digest(32) = [ &
        int(z'BA', int8), int(z'78', int8), int(z'16', int8), int(z'BF', int8), &
        int(z'8F', int8), int(z'01', int8), int(z'CF', int8), int(z'EA', int8), &
        int(z'41', int8), int(z'41', int8), int(z'40', int8), int(z'DE', int8), &
        int(z'5D', int8), int(z'AE', int8), int(z'22', int8), int(z'23', int8), &
        int(z'B0', int8), int(z'03', int8), int(z'61', int8), int(z'A3', int8), &
        int(z'96', int8), int(z'17', int8), int(z'7A', int8), int(z'9C', int8), &
        int(z'B4', int8), int(z'10', int8), int(z'FF', int8), int(z'61', int8), &
        int(z'F2', int8), int(z'00', int8), int(z'15', int8), int(z'AD', int8)]
    integer(int8) :: digest(32), file_bytes(3), value
    type(writer_t) :: memory, hash, counting
    type(byte_span_t) :: source_span, output_span
    logical :: ok
    character(len=256) :: message
    integer(int64) :: file_size
    integer :: i, unit, ios

    call byte_span_from_array(source, 1, size(source), source_span, ok, message)
    call require(ok, message)

    call writer_init_memory(memory, 1, ok, message)
    call require(ok, message)
    call writer_write_span(memory, source_span, ok, message)
    call require(ok, message)
    call writer_write_ascii(memory, 'ABC', ok, message)
    call require(ok, message)
    call writer_memory_span(memory, output_span, ok, message)
    call require(ok, message)
    call require(writer_size(memory) == int(size(expected_memory), int64), &
        'memory writer size differs')
    do i = 1, size(expected_memory)
        call byte_span_get(output_span, i, value, ok, message)
        call require(ok, message)
        call require(value == expected_memory(i), 'memory writer changed source bytes')
    end do

    call writer_init_counting(counting, ok, message)
    call require(ok, message)
    call writer_write_span(counting, source_span, ok, message)
    call require(ok, message)
    call require(writer_size(counting) == 3_int64, 'counting writer size differs')

    call writer_init_hash(hash, ok, message)
    call require(ok, message)
    call writer_write_ascii(hash, 'abc', ok, message)
    call require(ok, message)
    call writer_digest(hash, digest, ok, message)
    call require(ok, message)
    call require(all(digest == abc_digest), 'SHA-256 abc vector differs')
    call writer_init_hash(hash, ok, message)
    call require(ok, message)
    call writer_digest(hash, digest, ok, message)
    call require(ok, message)
    call require(all(digest == empty_digest), 'SHA-256 empty vector differs')

    open (newunit=unit, file='build/writer_fixture.bin', status='replace', &
        access='stream', form='unformatted', action='write', iostat=ios)
    call require(ios == 0, 'could not open writer file fixture')
    call writer_init_file(memory, unit, ok, message)
    call require(ok, message)
    call writer_write_bytes(memory, source, ok, message)
    call require(ok, message)
    call writer_close(memory, ok, message)
    close (unit)
    inquire (file='build/writer_fixture.bin', size=file_size)
    call require(file_size == 3_int64, 'file writer size differs')
    open (newunit=unit, file='build/writer_fixture.bin', access='stream', &
        form='unformatted', action='read', iostat=ios)
    call require(ios == 0, 'could not reopen writer file fixture')
    read (unit, iostat=ios) file_bytes
    close (unit)
    call require(ios == 0 .and. all(file_bytes == source), &
        'file writer bytes differ from oracle')

    print '(a)', 'writer test passed'

contains

    subroutine require(condition, failure_message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: failure_message

        if (.not. condition) then
            print '(a)', 'FAIL: '//trim(failure_message)
            stop 1
        end if
    end subroutine require

end program test_writer

program day2
    use day2_data, only: read_id_ranges, id_kind
    implicit none

    character(len=*), parameter :: filename = "data/day02/test_input.txt"
    integer(id_kind), dimension(:,:), allocatable :: id_ranges
    integer :: i

    call read_id_ranges(filename, id_ranges)

    do i = 1, size(id_ranges, 2)
        print *, "ID Range ", i, ": ", id_ranges(1, i), " to ", id_ranges(2, i)
    end do
contains

end program day2
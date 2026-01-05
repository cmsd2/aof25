module day2_data
    use stdlib_strings, only: count
    use stdlib_str2num, only: to_num
    implicit none

    integer, parameter :: id_kind = selected_int_kind(10)
contains
    subroutine read_id_ranges(filename, id_ranges)
        character(len=*), intent(in) :: filename
        integer(id_kind), dimension(:,:), allocatable, intent(out) :: id_ranges
        character(len=20), dimension(:), allocatable :: lines
        integer :: i
        integer(id_kind) :: left_id, right_id
        integer :: pos
        integer :: ios
        ! Placeholder implementation
        call read_data(filename, lines, ios)

        if (ios /= 0) then
            print *, "Error reading ID ranges from file:", filename
            deallocate(id_ranges)
            return
        end if

        allocate (id_ranges(2, size(lines)))

        do i = 1, size(lines)
            pos = scan(lines(i), '-')
            if (pos /= 0) then
                left_id = to_num(lines(i)(1:pos-1), id_kind)
                right_id = to_num(lines(i)(pos+1:), id_kind)

                id_ranges(1, i) = left_id
                id_ranges(2, i) = right_id
            end if
        end do
    end subroutine read_id_ranges

    subroutine read_data(filename, lines, ios)
        character(len=*), intent(in) :: filename
        character(len=512) :: msg
        integer :: nu
        integer, intent(out) :: ios
        character(len=1024) :: line
        character(len=20), dimension(:), allocatable, intent(out) :: lines
        character(len=20), dimension(:), allocatable :: lines_temp
        character(len=20) :: field
        integer :: num_fields
        integer :: i
        integer :: start
        integer :: end
        integer :: pos
        integer :: capacity
        integer :: num_items

        capacity = 1
        num_items = 0
        allocate (lines(1:capacity))

        print *, "Reading data from file:", filename
        
        open(newunit=nu, file=filename, iostat=ios, iomsg=msg, status='old', action='read')
        if (ios /= 0) then
            print *, "Error opening file:", filename
            print *, "IOMSG:", msg
            return
        end if

        do
            read (nu, '(A)', iostat=ios, iomsg=msg) line

            if (is_iostat_end(ios)) then
                print *, "End of file reached."
                ios = 0
                exit
            else if (ios /= 0) then
                print *, "Error reading line from file:", filename
                print *, msg
                exit
            end if

            num_fields = count(line, ',') + 1
            start = 1
            end = len_trim(line)
            do i = 1, num_fields
                pos = scan(line(start:), ',')
                if (pos == 0) then
                    field = line(start:end)
                else
                    field = line(start:start+pos-2)
                end if
                start = start + pos

                if (field /= '') then

                    num_items = num_items + 1

                    if (num_items > capacity) then
                        capacity = capacity * 2
                        allocate (lines_temp(1:capacity))
                        lines_temp(1:size(lines)) = lines
                        call move_alloc(lines_temp, lines)
                    end if
                    
                    lines(num_items) = field
                end if
            end do
        end do        

        close(nu)

        allocate (lines_temp(1:num_items))
        lines_temp(1:num_items) = lines(1:num_items)
        call move_alloc(lines_temp, lines)
    end subroutine read_data
end module day2_data
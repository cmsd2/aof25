module day1_data
    use direction, only: direction_type, direction_t
    use rotation, only: rotation_t
    implicit none

    

    
contains
    subroutine read_rotations(filename, rotations)
        character(len=*), intent(in) :: filename
        type(rotation_t), dimension(:), allocatable, intent(out) :: rotations
        character(len=4), dimension(:), allocatable :: lines
        type(rotation_t) :: rot
        integer :: ios
        integer :: i

        call read_data(filename, lines, ios)

        if (ios /= 0) then
            print *, "Error reading data from file:", filename
            return
        end if

        allocate (rotations(size(lines)))

        do i = 1, size(lines)
            rot = rotation_t(direction_type%left(), 0)
            
            if (lines(i)(1:1) == 'L') then
                rot%dir = direction_type%left()
            else if (lines(i)(1:1) == 'R') then
                rot%dir = direction_type%right()
            else
                print *, "Invalid direction in line:", lines(i)
                deallocate(rotations)
                return
            end if

            read (lines(i)(2:), *) rot%size

            rotations(i) = rot
        end do
    end subroutine read_rotations

    subroutine read_data(filename, lines, ios)
        character(len=*), intent(in) :: filename
        character(len=512) :: msg
        integer :: nu
        integer, intent(out) :: ios
        character(len=4) :: line
        character(len=4), dimension(:), allocatable, intent(out) :: lines
        character(len=4), dimension(:), allocatable :: lines_temp
        integer :: capacity
        integer :: count

        capacity = 1
        count = 0
        allocate (lines(1:capacity))

        print *, "Reading data from file:", filename
        
        open(newunit=nu, file=filename, iostat=ios, iomsg=msg, status='old', action='read', access='sequential', recl=4)
        if (ios /= 0) then
            print *, "Error opening file:", filename
            print *, "IOMSG:", msg
            return
        end if

        do
            read (nu, *, iostat=ios) line

            if (is_iostat_end(ios)) then
                print *, "End of file reached."
                ios = 0
                exit
            else if (ios /= 0) then
                print *, "Error reading line from file:", filename
                exit
            end if

            count = count + 1

            if (count > capacity) then
                capacity = capacity * 2
                allocate (lines_temp(1:capacity))
                lines_temp(1:size(lines)) = lines
                call move_alloc(lines_temp, lines)
            end if
            
            lines(count) = line
        end do        

        close(nu)

        allocate (lines_temp(1:count))
        lines_temp(1:count) = lines(1:count)
        call move_alloc(lines_temp, lines)
    end subroutine read_data
end module day1_data
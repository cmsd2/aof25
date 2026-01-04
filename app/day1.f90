program day1
  use direction, only: direction_type, direction_t
  use rotation, only: rotation_t, rotate_dial
  use day1_data, only: read_rotations
  implicit none

  call day1_final()
  ! call day1_test()
contains
  subroutine day1_test()
    character(len=*), parameter :: filename = "data/day01/test_input.txt"
    call day1_run_with_datafile(filename)
  end subroutine day1_test

  subroutine day1_final()
    character(len=*), parameter :: filename = "data/day01/input.txt"
    call day1_run_with_datafile(filename)
  end subroutine day1_final

  subroutine day1_run_with_datafile(filename)
    character(len=*), intent(in) :: filename
    type(rotation_t), dimension(:), allocatable :: rotations
    integer :: i
    integer :: dial
    integer :: zero_times
    integer :: zero_clicks
    integer :: total_zero_clicks

    call read_rotations(filename, rotations)

    if (.not. allocated(rotations)) then
      print *, "Error reading rotations; exiting."
      return
    end if

    dial = 50

    zero_times = 0
    total_zero_clicks = 0

    do i = 1, size(rotations)
      dial = rotate_dial(dial, rotations(i), zero_clicks)

      print *, "Dial after rotation ", rotations(i), ": ", dial, &
               " (zero clicks: ", zero_clicks, ")"

      if (dial == 0) then
        zero_times = zero_times + 1
      end if

      total_zero_clicks = total_zero_clicks + zero_clicks
    end do

    print *, "Dial landed on zero ", zero_times, " times."
    print *, "Dial passed through zero ", total_zero_clicks, " times."

  end subroutine day1_run_with_datafile

end program day1
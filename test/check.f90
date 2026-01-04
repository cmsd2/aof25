program check
    use direction, only: direction_type, direction_t
    use rotation, only: rotation_t, rotate_dial
    implicit none

    integer :: dial
    integer :: zero_clicks

    dial = 50
    dial = rotate_dial(dial, rotation_t(direction_type%left(), 1000), zero_clicks)

    print *, "Dial after rotation: ", dial
    print *, "Zero clicks: ", zero_clicks

end program check

module rotation
    use direction, only: direction_t, direction_type
    implicit none

    integer, parameter :: size_kind = selected_int_kind(2)

    type rotation_t
        type(direction_t) :: dir
        integer(kind(size_kind)) :: size
    contains
        procedure :: write
        generic :: write(formatted) => write
    end type rotation_t

contains
    subroutine write(dtv, unit, iotype, v_list, iostat, iomsg)
        class(rotation_t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
    
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%dir, dtv%size
    end subroutine write

    function rotate_dial(dial, rotation, zero_clicks) result(new_dial)
        integer, intent(in) :: dial
        type(rotation_t), intent(in) :: rotation
        integer :: new_dial
        integer, intent(out) :: zero_clicks

        zero_clicks = 0

        if (rotation%dir == direction_type%left()) then
            new_dial = modulo(dial - rotation%size, 100)
            zero_clicks = (rotation%size - dial) / 100
            if (dial - rotation%size <= 0 .and. dial /= 0) zero_clicks = zero_clicks + 1
        else
            new_dial = modulo(dial + rotation%size, 100)
            zero_clicks = (dial + rotation%size) / 100
        end if
    end function rotate_dial
end module rotation
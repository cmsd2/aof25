module direction
    type :: direction_factory_t
    contains
        procedure :: left => direction_factory_left
        procedure :: right => direction_factory_right
    end type direction_factory_t

    type :: direction_t
        private
        integer :: dir
    contains
        procedure :: is_equal => direction_is_equal
        generic :: operator(==) => is_equal

        procedure :: write
        generic :: write(formatted) => write
    end type direction_t

    type(direction_factory_t) :: direction_type
contains
    function direction_factory_left(self) result(res)
        class(direction_factory_t), intent(in) :: self
        type(direction_t) :: res
        res%dir = -1
    end function direction_factory_left
    
    function direction_factory_right(self) result(res)
        class(direction_factory_t), intent(in) :: self
        type(direction_t) :: res
        res%dir = 1
    end function direction_factory_right

    pure function direction_is_equal(self, other) result(res)
        class(direction_t), intent(in) :: self
        class(direction_t), intent(in) :: other
        logical :: res

        res = (self%dir == other%dir)
    end function direction_is_equal

    subroutine write(dtv, unit, iotype, v_list, iostat, iomsg)
        class(direction_t), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
    
        if (dtv == direction_type%left()) then
            write (unit, "(A5)", iostat=iostat, iomsg=iomsg) "Left"
        else
            write (unit, "(A5)", iostat=iostat, iomsg=iomsg) "Right"
        end if
    end subroutine write
end module direction
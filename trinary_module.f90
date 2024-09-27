module trinary_module
    implicit none
    private
    public :: trinary, low, medium, high, up, down

    type :: trinary
        private
        integer :: value
    contains
        procedure :: set => set_trinary
        procedure :: get => get_trinary
        procedure :: shift => change_trinary_state
    end type trinary

    integer, parameter :: low = 0, medium = 1, high = 2, up = 1, down = -1

contains

    subroutine set_trinary(this, state)
        class(trinary), intent(inout) :: this
        integer, intent(in) :: state

        if (state == low .or. state == medium .or. state == high) then
            this%value = state
        else
            print *, "Invalid state for trinary type."
            stop
        end if
    end subroutine set_trinary

    function get_trinary(this) result(state)
        class(trinary), intent(in) :: this
        integer :: state

        state = this%value
    end function get_trinary

    subroutine change_trinary_state(this, direction)
        class(trinary), intent(inout) :: this
        integer, intent(in) :: direction

        ! Apply the change based on the direction parameter
        ! direction: +1 for up, -1 for down
        if ((direction == up .and. this%value < high) .or. (direction == down .and. this%value > low)) then
            this%value = min(max(this%value + direction, low), high)
        end if
    end subroutine change_trinary_state


end module trinary_module

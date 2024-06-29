module trinary_module
    implicit none
    private
    public :: trinary, low, medium, high, set_trinary, get_trinary

    type :: trinary
        private
        integer :: value
    contains
        procedure :: set => set_trinary
        procedure :: get => get_trinary
    end type trinary

    integer, parameter :: low = 1, medium = 2, high = 3

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

end module trinary_module

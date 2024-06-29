module inputter_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_inputter(inputter, cols)
        type(trinary), allocatable :: inputter(:)
        integer, intent(in) :: cols
        integer :: j

        allocate(inputter(cols))

        ! Initialize the inputter array with alternating trinary states
        do j = 1, cols
            if (mod(j, 3) == 0) then
                call inputter(j)%set(low)
            elseif (mod(j, 3) == 1) then
                call inputter(j)%set(medium)
            else
                call inputter(j)%set(high)
            end if
        end do
    end subroutine initialize_inputter

end module inputter_module

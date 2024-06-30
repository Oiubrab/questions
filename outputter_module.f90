module outputter_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_outputter(outputter, input_length)
        type(trinary), allocatable :: outputter(:)
        integer, intent(in) :: input_length
        integer :: j

        allocate(outputter(input_length))

        ! Initialize the outputter array with all lows (0's)
        do j = 1, input_length
            call outputter(j)%set(low)
        end do
    end subroutine initialize_outputter

end module outputter_module

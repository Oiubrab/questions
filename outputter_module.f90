module outputter_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_outputter(outputter, inputter, input_length)
        type(trinary), allocatable :: outputter(:)
        type(trinary), allocatable :: inputter(:)
        integer, intent(in) :: input_length
        integer :: j

        allocate(outputter(input_length))

        ! Initialize the outputter array with the same values as inputter
        do j = 1, input_length
            call outputter(j)%set(inputter(j)%get())
        end do
    end subroutine initialize_outputter

end module outputter_module

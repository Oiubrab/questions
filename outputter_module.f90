module outputter_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_outputter(outputter, inputter, cols)
        type(trinary), allocatable :: outputter(:)
        type(trinary), allocatable :: inputter(:)
        integer, intent(in) :: cols
        integer :: j

        allocate(outputter(cols))

        ! Initialize the outputter array with the same values as inputter
        do j = 1, cols
            call outputter(j)%set(inputter(j)%get())
        end do
    end subroutine initialize_outputter

end module outputter_module

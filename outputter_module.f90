module outputter_module
    use trinary_module
    implicit none
    type(trinary), allocatable :: backup_outputter(:)

    contains

    subroutine initialize_outputter(outputter, input_length)
        type(trinary), allocatable :: outputter(:)
        integer, intent(in) :: input_length
        integer :: j

        allocate(outputter(input_length))
        allocate(backup_outputter(input_length))

        ! Initialize the outputter array and backup array with all lows (0's)
        do j = 1, input_length
            call outputter(j)%set(low)
            call backup_outputter(j)%set(low)
        end do
    end subroutine initialize_outputter

    subroutine save_and_reset_outputter(outputter)
        type(trinary), allocatable :: outputter(:)
        integer :: j

        ! Save the current outputter state to the backup array
        do j = 1, size(outputter)
            call backup_outputter(j)%set(outputter(j)%get())
        end do

        ! Reset all elements in the outputter to low
        do j = 1, size(outputter)
            call outputter(j)%set(low)
        end do
    end subroutine save_and_reset_outputter

end module outputter_module

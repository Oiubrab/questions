module inputter_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_inputter(inputter, input_length)
        type(trinary), allocatable :: inputter(:)
        integer, intent(in) :: input_length
        integer :: j

        if (allocated(inputter)) deallocate(inputter)
        allocate(inputter(input_length))

        ! Initialize the inputter array with alternating trinary states
        do j = 1, input_length
            if (mod(j, 3) == 0) then
                call inputter(j)%set(low)
            elseif (mod(j, 3) == 1) then
                call inputter(j)%set(medium)
            else
                call inputter(j)%set(high)
            end if
        end do
    end subroutine initialize_inputter

    subroutine copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        type(trinary), allocatable :: inputter(:)
        type(trinary), allocatable :: brain(:,:)
        integer, intent(in) :: input_offset, cols
        integer :: i, current_state, new_state

        ! Add inputter states to the existing brain top row states, capped at high (2)
        do i = 1, size(inputter)
            current_state = brain(1, input_offset - 1 + i)%get()
            new_state = min(current_state + inputter(i)%get(), high)
            call brain(1, input_offset - 1 + i)%set(new_state)
        end do
    end subroutine copy_non_low_to_brain_top_row

end module inputter_module

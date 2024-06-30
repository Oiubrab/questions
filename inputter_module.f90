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

    subroutine copy_non_low_to_brain_top_row(inputter, brain, offset, inputter_len, brain_cols)
        type(trinary), allocatable :: inputter(:)
        type(trinary), allocatable :: brain(:,:)
        integer, intent(in) :: offset, inputter_len, brain_cols
        integer :: j

        ! Copy non-low states from inputter to the top row of the brain matrix
        do j = 1, inputter_len
            if (inputter(j)%get() /= low .and. (offset + j) <= brain_cols) then
                if (brain(1, offset + j)%get() < inputter(j)%get()) then
                    call brain(1, offset + j)%set(inputter(j)%get())
                end if
            end if
        end do
    end subroutine copy_non_low_to_brain_top_row

end module inputter_module

module brain_module
    use trinary_module
    use synapses_module
    use outputter_module
    implicit none
    contains

    subroutine initialize_brain(brain, rows, cols)
        type(trinary), allocatable :: brain(:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j

        allocate(brain(rows, cols))

        ! Initialize the 2D brain with all lows (0's)
        do i = 1, rows
            do j = 1, cols
                call brain(i, j)%set(low)
            end do
        end do
    end subroutine initialize_brain

    subroutine update_brain_state_based_on_synapses(brain, synapses, outputter, rows, cols, offset)
        type(trinary), allocatable :: brain(:,:)
        type(trinary), allocatable :: outputter(:)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols, offset
        integer :: i, j, k, total_value, index
        real :: cumulative_prob(8), random_num
        real, allocatable :: synapse_values(:)
        integer, dimension(8, 2) :: directions = reshape([ &
            -1, -1, & ! Up and left
            -1,  0, & ! Up
            -1,  1, & ! Up and right
             0, -1, & ! Left
             0,  1, & ! Right
             1, -1, & ! Down and left
             1,  0, & ! Down
             1,  1  & ! Down and right
        ], shape=[8, 2])

        do i = 1, rows
            do j = 1, cols
                if (brain(i, j)%get() /= low) then
                    ! Extract the synapse values
                    allocate(synapse_values(8))
                    do k = 1, 8
                        synapse_values(k) = synapses(i, j, k)
                    end do

                    ! Calculate cumulative probabilities
                    total_value = sum(synapse_values)
                    if (total_value == 0) cycle
                    cumulative_prob(1) = synapse_values(1) / total_value
                    do k = 2, 8
                        cumulative_prob(k) = cumulative_prob(k-1) + synapse_values(k) / total_value
                    end do

                    ! Generate a random number
                    call random_number(random_num)

                    ! Find which synapse the random number maps to
                    do k = 1, 8
                        if (random_num <= cumulative_prob(k)) then
                            index = k
                            exit
                        end if
                    end do

                    ! Update the corresponding direction if it is not already high
                    if (i + directions(index, 1) >= 1 .and. i + directions(index, 1) <= rows .and. &
                        j + directions(index, 2) >= 1 .and. j + directions(index, 2) <= cols) then
                        if (brain(i + directions(index, 1), j + directions(index, 2))%get() /= high) then
                            call brain(i + directions(index, 1), j + directions(index, 2))%shift(up)
                            call brain(i, j)%shift(down)
                            synapses(i, j, index) = synapses(i, j, index) + rows * cols
                        end if
                    else if (i + directions(index, 1) > rows .and. (j - offset + 1) >= 1 .and. (j - offset + 1) <= size(outputter)) then
                        ! If the direction is off the bottom of the matrix, update the outputter
                        if (outputter(j - offset + 1)%get() /= high) then
                            call outputter(j - offset + 1)%shift(up)
                            call brain(i, j)%shift(down)
                            synapses(i, j, index) = synapses(i, j, index) + rows * cols
                        end if
                    end if

                    deallocate(synapse_values)
                end if
            end do
        end do
    end subroutine update_brain_state_based_on_synapses

end module brain_module

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
        use trinary_module
        use outputter_module
        implicit none
        ! Declarations
        type(trinary), allocatable :: brain(:,:)
        type(trinary), allocatable :: outputter(:)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols, offset
        ! Add temporary brain matrix
        type(trinary), allocatable :: brain_next(:,:)
        integer :: i, j, k, total_value, index
        real :: cumulative_prob(8), random_num
        real, allocatable :: synapse_values(:)
        integer, dimension(8, 2) :: directions
        integer :: ni, nj
        logical :: valid_move

        ! Initialize directions array
        directions = reshape([ &
            -1, -1, & ! Up and left
            -1,  0, & ! Up
            -1,  1, & ! Up and right
            0, -1, & ! Left
            0,  1, & ! Right
            1, -1, & ! Down and left
            1,  0, & ! Down
            1,  1  & ! Down and right
        ], shape=[8, 2])

        ! Allocate and initialize brain_next
        allocate(brain_next(rows, cols))
        do i = 1, rows
            do j = 1, cols
                call brain_next(i, j)%set(brain(i, j)%get())
            end do
        end do

        do i = 1, rows
            do j = 1, cols
                if (brain(i, j)%get() /= low) then
                    ! Extract the synapse values
                    allocate(synapse_values(8))
                    do k = 1, 8
                        synapse_values(k) = synapses(i, j, k)
                    end do

                    ! Check each direction for viability
                    do k = 1, 8
                        ni = i + directions(k, 1)
                        nj = j + directions(k, 2)
                        valid_move = .false.

                        ! Check if the target position is within the brain matrix
                        if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                            if (brain(ni, nj)%get() /= high) then
                                valid_move = .true.
                            end if
                        ! Check if the move is from the last row into the outputter array
                        else if (i == rows .and. ni == rows + 1 .and. (nj - offset + 1) >= 1 .and. (nj - offset + 1) <= size(outputter)) then
                            if (outputter(nj - offset + 1)%get() /= high) then
                                valid_move = .true.
                            end if
                        end if

                        ! If the move is not valid, set the synapse value to zero
                        if (.not. valid_move) then
                            synapse_values(k) = 0
                        end if
                    end do

                    ! Calculate total value of valid synapses
                    total_value = sum(synapse_values)
                    if (total_value == 0) then
                        ! No valid moves; skip to the next cell
                        deallocate(synapse_values)
                        cycle
                    end if

                    ! Calculate cumulative probabilities for valid directions
                    cumulative_prob(1) = synapse_values(1) / total_value
                    do k = 2, 8
                        cumulative_prob(k) = cumulative_prob(k - 1) + synapse_values(k) / total_value
                    end do

                    ! Generate a random number between 0 and 1
                    call random_number(random_num)

                    ! Find which synapse the random number maps to
                    do k = 1, 8
                        if (random_num <= cumulative_prob(k)) then
                            index = k
                            exit
                        end if
                    end do

                    ! Compute target position based on selected direction
                    ni = i + directions(index, 1)
                    nj = j + directions(index, 2)

                    ! Perform the move if it's within the brain matrix
                    if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                        call brain_next(ni, nj)%shift(up)
                        call brain_next(i, j)%shift(down)
                        synapses(i, j, index) = synapses(i, j, index) + rows * cols
                    ! Perform the move into the outputter array if from the last row moving down
                    else if (i == rows .and. ni == rows + 1 .and. (nj - offset + 1) >= 1 .and. (nj - offset + 1) <= size(outputter)) then
                        call outputter(nj - offset + 1)%shift(up)
                        call brain_next(i, j)%shift(down)
                        synapses(i, j, index) = synapses(i, j, index) + rows * cols
                    end if

                    ! Deallocate the synapse_values array
                    deallocate(synapse_values)
                end if
            end do
        end do

        ! Replace the current brain state with the next state
        do i = 1, rows
            do j = 1, cols
                call brain(i, j)%set(brain_next(i, j)%get())
            end do
        end do

        ! Deallocate brain_next
        deallocate(brain_next)

    end subroutine update_brain_state_based_on_synapses




end module brain_module

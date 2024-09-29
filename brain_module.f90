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

subroutine update_brain_state_based_on_synapses(brain, synapses, outputter, rows, cols, input_offset, output_offset, output_length)
    use trinary_module
    use outputter_module
    implicit none
    ! Declarations
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer, intent(in) :: rows, cols, input_offset, output_offset, output_length
    type(trinary), allocatable :: brain_next(:,:)
    integer :: i, j, k, index
    real :: total_value, random_num
    real, allocatable :: synapse_values(:)
    integer, dimension(8, 2) :: directions
    integer :: ni, nj
    logical :: valid_move
    real, dimension(8) :: direction_bias
    integer :: num_valid_directions
    integer, allocatable :: valid_indices(:)
    real, allocatable :: valid_synapse_values(:), cumulative_prob(:)
    integer, parameter :: max_synapse_strength = 200000, reinforcement_amount=10

    ! Initialize directions array explicitly
    directions(1,1) = -1   ! Up-Left
    directions(1,2) = -1
    directions(2,1) = -1   ! Up
    directions(2,2) =  0
    directions(3,1) = -1   ! Up-Right
    directions(3,2) =  1
    directions(4,1) =  0   ! Left
    directions(4,2) = -1
    directions(5,1) =  0   ! Right
    directions(5,2) =  1
    directions(6,1) =  1   ! Down-Left
    directions(6,2) = -1
    directions(7,1) =  1   ! Down
    directions(7,2) =  0
    directions(8,1) =  1   ! Down-Right
    directions(8,2) =  1

    ! Initialize bias factors for each direction explicitly
    direction_bias(1) = 0.5  ! Up-Left
    direction_bias(2) = 0.5  ! Up
    direction_bias(3) = 0.5  ! Up-Right
    direction_bias(4) = 1.0  ! Left
    direction_bias(5) = 1.0  ! Right
    direction_bias(6) = 1.5  ! Down-Left
    direction_bias(7) = 1.8  ! Down
    direction_bias(8) = 1.5  ! Down-Right

    ! Allocate and initialize brain_next
    allocate(brain_next(rows, cols))
    brain_next = brain  ! Copy current brain state

    ! Rest of your existing code remains the same, with no changes needed in the logic
    ! ...

    do i = 1, rows
        do j = 1, cols
            if (brain(i, j)%get() /= low) then
                ! Extract the synapse values
                allocate(synapse_values(8))
                do k = 1, 8
                    synapse_values(k) = synapses(i, j, k)
                end do

                num_valid_directions = 0

                ! Check each direction for viability and apply bias
                do k = 1, 8
                    ni = i + directions(k, 1)
                    nj = j + directions(k, 2)
                    valid_move = .false.

                    ! Validity checks
                    if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                        if (brain(ni, nj)%get() /= high) then
                            valid_move = .true.
                        end if
                    ! Check if the move is from the last row into the outputter array
                    else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                        if (outputter(nj - output_offset + 1)%get() /= high) then
                            valid_move = .true.
                        end if
                    end if

                    ! Apply bias if move is valid
                    if (valid_move) then
                        synapse_values(k) = synapse_values(k) * direction_bias(k)
                        if (synapse_values(k) > 0.0) then
                            num_valid_directions = num_valid_directions + 1
                        else
                            synapse_values(k) = 0.0
                        end if
                    else
                        synapse_values(k) = 0.0
                    end if
                end do

                ! Check for valid directions
                if (num_valid_directions == 0) then
                    deallocate(synapse_values)
                    cycle
                end if

                ! Allocate arrays for valid directions
                allocate(valid_indices(num_valid_directions))
                allocate(valid_synapse_values(num_valid_directions))

                num_valid_directions = 0
                do k = 1, 8
                    if (synapse_values(k) > 0.0) then
                        num_valid_directions = num_valid_directions + 1
                        valid_indices(num_valid_directions) = k
                        valid_synapse_values(num_valid_directions) = synapse_values(k)
                    end if
                end do

                ! Calculate total value
                total_value = sum(valid_synapse_values)

                ! Calculate cumulative probabilities
                allocate(cumulative_prob(num_valid_directions))
                cumulative_prob(1) = valid_synapse_values(1) / total_value
                do k = 2, num_valid_directions
                    cumulative_prob(k) = cumulative_prob(k - 1) + valid_synapse_values(k) / total_value
                end do

                ! Generate random number
                call random_number(random_num)

                ! Select direction
                do k = 1, num_valid_directions
                    if (random_num <= cumulative_prob(k)) then
                        index = valid_indices(k)
                        exit
                    end if
                end do

                ! Compute target position based on selected direction
                ni = i + directions(index, 1)
                nj = j + directions(index, 2)

                ! Perform the move
                if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                    call brain_next(ni, nj)%shift(up)
                    call brain_next(i, j)%shift(down)
                    ! Reinforce the synapse, but cap its strength
                    synapses(i, j, index) = min(synapses(i, j, index) + reinforcement_amount, max_synapse_strength)
                ! Perform the move into the outputter array if from the last row moving down
                else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                    call outputter(nj - output_offset + 1)%shift(up)
                    call brain_next(i, j)%shift(down)
                    ! Reinforce the synapse, but cap its strength
                    synapses(i, j, index) = min(synapses(i, j, index) + reinforcement_amount, max_synapse_strength)
                end if

                ! Deallocate arrays
                deallocate(synapse_values)
                deallocate(valid_indices)
                deallocate(valid_synapse_values)
                deallocate(cumulative_prob)
            end if
        end do
    end do

    ! Replace the current brain state with the next state
    brain = brain_next

    ! Deallocate brain_next
    deallocate(brain_next)
end subroutine update_brain_state_based_on_synapses



end module brain_module

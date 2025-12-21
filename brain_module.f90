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

        if (allocated(brain)) deallocate(brain)
        allocate(brain(rows, cols))

        ! Initialize the 2D brain with all lows (0's)
        do i = 1, rows
            do j = 1, cols
                call brain(i, j)%set(low)
            end do
        end do
    end subroutine initialize_brain

subroutine copy_non_low_to_brain_top_row(inputter, brain, incoming_direction, input_offset, cols)
    type(trinary), allocatable :: inputter(:), brain(:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    integer, intent(in) :: input_offset, cols
    integer :: i, brain_col
    
    do i = 1, size(inputter)
        brain_col = input_offset + i - 1
        if (brain_col >= 1 .and. brain_col <= cols) then
            if (inputter(i)%get() /= low) then
                call brain(1, brain_col)%set(inputter(i)%get())
                ! Set incoming direction to 7 (Down) - signal came from "above" (inputter)
                incoming_direction(1, brain_col, 1) = 7
            end if
        end if
    end do
end subroutine copy_non_low_to_brain_top_row

subroutine update_brain_state_based_on_synapses(brain, synapses, outputter, synapse_usage, incoming_direction, &
                                                rows, cols, input_offset, output_offset, output_length)
    use trinary_module
    use outputter_module
    implicit none
    ! Declarations
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:,:)  ! Now 4D: (row, col, incoming_dir, outgoing_dir)
    logical, allocatable :: synapse_usage(:,:,:,:)  ! Now 4D: track which synapses were used
    integer, allocatable :: incoming_direction(:,:,:)  ! (rows, cols, 2) - track up to 2 incoming directions
    integer, intent(in) :: rows, cols, input_offset, output_offset, output_length
    type(trinary), allocatable :: brain_next(:,:)
    integer, allocatable :: incoming_direction_next(:,:,:)
    integer :: i, j, k, index, incoming_dir, incoming_dir2
    real :: total_value, random_num
    real, allocatable :: synapse_values(:)
    integer, dimension(8, 2) :: directions
    integer, dimension(8) :: direction_opposites
    integer :: ni, nj
    logical :: valid_move
    real, dimension(8) :: direction_bias
    integer :: num_valid_directions
    integer, allocatable :: valid_indices(:)
    real, allocatable :: valid_synapse_values(:), cumulative_prob(:)
    integer, parameter :: max_synapse_strength = 2000000, reinforcement_amount=1000
    integer :: current_state, target_state

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

    ! Initialize direction opposites (for tracking incoming direction)
    direction_opposites(1) = 8  ! Up-Left ↔ Down-Right
    direction_opposites(2) = 7  ! Up ↔ Down
    direction_opposites(3) = 6  ! Up-Right ↔ Down-Left
    direction_opposites(4) = 5  ! Left ↔ Right
    direction_opposites(5) = 4  ! Right ↔ Left
    direction_opposites(6) = 3  ! Down-Left ↔ Up-Right
    direction_opposites(7) = 2  ! Down ↔ Up
    direction_opposites(8) = 1  ! Down-Right ↔ Up-Left

    ! Initialize bias factors for each direction explicitly
    direction_bias(1) = 0.5  ! Up-Left
    direction_bias(2) = 0.5  ! Up
    direction_bias(3) = 0.5  ! Up-Right
    direction_bias(4) = 1.0  ! Left
    direction_bias(5) = 1.0  ! Right
    direction_bias(6) = 1.5  ! Down-Left
    direction_bias(7) = 1.8  ! Down
    direction_bias(8) = 1.5  ! Down-Right

    ! Allocate and initialize brain_next and incoming_direction_next
    allocate(brain_next(rows, cols))
    allocate(incoming_direction_next(rows, cols, 2))
    brain_next = brain  ! Copy current brain state
    incoming_direction_next = incoming_direction  ! Copy incoming directions

    ! Process each neuron
    do i = 1, rows
        do j = 1, cols
            current_state = brain(i, j)%get()
            if (current_state /= low) then
                ! Get incoming direction(s)
                incoming_dir = incoming_direction(i, j, 1)
                incoming_dir2 = incoming_direction(i, j, 2)
                
                ! Skip if no incoming direction set (shouldn't happen for active neurons)
                if (incoming_dir == 0) cycle
                
                ! Allocate synapse values array
                allocate(synapse_values(8))
                
                ! For MEDIUM neurons: use single incoming direction
                ! For HIGH neurons: average the two incoming direction groups
                if (current_state == medium .or. incoming_dir2 == 0) then
                    ! Single incoming direction
                    do k = 1, 8
                        synapse_values(k) = synapses(i, j, incoming_dir, k)
                    end do
                else
                    ! HIGH state with two incoming directions - average them
                    do k = 1, 8
                        synapse_values(k) = (synapses(i, j, incoming_dir, k) + &
                                           synapses(i, j, incoming_dir2, k)) / 2.0
                    end do
                end if

                num_valid_directions = 0

                ! Check each direction for viability and apply bias
                do k = 1, 8
                    ni = i + directions(k, 1)
                    nj = j + directions(k, 2)
                    valid_move = .false.

                    ! Validity checks
                    if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                        if (brain_next(ni, nj)%get() /= high) then
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
                    target_state = brain_next(ni, nj)%get()
                    call brain_next(ni, nj)%shift(up)
                    call brain_next(i, j)%shift(down)
                    
                    ! Set incoming direction for target neuron
                    if (target_state == low) then
                        ! Target was LOW, now MEDIUM - set primary incoming direction
                        incoming_direction_next(ni, nj, 1) = direction_opposites(index)
                    else if (target_state == medium) then
                        ! Target was MEDIUM, now HIGH - set secondary incoming direction
                        incoming_direction_next(ni, nj, 2) = direction_opposites(index)
                    end if
                    
                    ! Clear incoming direction if source neuron becomes LOW
                    if (brain_next(i, j)%get() == low) then
                        incoming_direction_next(i, j, 1) = 0
                        incoming_direction_next(i, j, 2) = 0
                    end if
                    
                    ! Reinforce the synapse, but cap its strength
                    ! Use the actual incoming direction(s) for reinforcement
                    if (current_state == medium .or. incoming_dir2 == 0) then
                        synapses(i, j, incoming_dir, index) = min(synapses(i, j, incoming_dir, index) + &
                                                                   reinforcement_amount, max_synapse_strength)
                        synapse_usage(i, j, incoming_dir, index) = .true.
                    else
                        ! HIGH state - reinforce both incoming direction groups
                        synapses(i, j, incoming_dir, index) = min(synapses(i, j, incoming_dir, index) + &
                                                                   reinforcement_amount, max_synapse_strength)
                        synapses(i, j, incoming_dir2, index) = min(synapses(i, j, incoming_dir2, index) + &
                                                                    reinforcement_amount, max_synapse_strength)
                        synapse_usage(i, j, incoming_dir, index) = .true.
                        synapse_usage(i, j, incoming_dir2, index) = .true.
                    end if
                ! Perform the move into the outputter array if from the last row moving down
                else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                    call outputter(nj - output_offset + 1)%shift(up)
                    call brain_next(i, j)%shift(down)
                    
                    ! Clear incoming direction if source neuron becomes LOW
                    if (brain_next(i, j)%get() == low) then
                        incoming_direction_next(i, j, 1) = 0
                        incoming_direction_next(i, j, 2) = 0
                    end if
                    
                    ! Reinforce the synapse
                    if (current_state == medium .or. incoming_dir2 == 0) then
                        synapses(i, j, incoming_dir, index) = min(synapses(i, j, incoming_dir, index) + &
                                                                   reinforcement_amount, max_synapse_strength)
                        synapse_usage(i, j, incoming_dir, index) = .true.
                    else
                        ! HIGH state - reinforce both incoming direction groups
                        synapses(i, j, incoming_dir, index) = min(synapses(i, j, incoming_dir, index) + &
                                                                   reinforcement_amount, max_synapse_strength)
                        synapses(i, j, incoming_dir2, index) = min(synapses(i, j, incoming_dir2, index) + &
                                                                    reinforcement_amount, max_synapse_strength)
                        synapse_usage(i, j, incoming_dir, index) = .true.
                        synapse_usage(i, j, incoming_dir2, index) = .true.
                    end if
                end if

                ! Deallocate arrays
                deallocate(synapse_values)
                deallocate(valid_indices)
                deallocate(valid_synapse_values)
                deallocate(cumulative_prob)
            end if
        end do
    end do

    ! Replace the current brain state and incoming directions with the next state
    brain = brain_next
    incoming_direction = incoming_direction_next

    ! Deallocate temporary arrays
    deallocate(brain_next)
    deallocate(incoming_direction_next)
end subroutine update_brain_state_based_on_synapses



end module brain_module

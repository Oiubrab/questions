module brain_module
    use trinary_module
    use synapses_module
    use outputter_module
    implicit none
    ! Precomputed direction metadata to avoid per-call initialization
    integer, parameter :: directions(8, 2) = reshape([ &
        -1, -1, -1, 0, 0, 1, 1, 1, & ! row deltas
        -1,  0,  1, -1, 1, -1, 0, 1  & ! col deltas
    ], [8, 2])
    integer, parameter :: direction_opposites(8) = [8, 7, 6, 5, 4, 3, 2, 1]
    real,    parameter :: direction_bias(8) = [0.5, 0.5, 0.5, 1.0, 1.0, 1.5, 1.8, 1.5]

    ! Reusable buffers to avoid per-call allocation churn
    type(trinary), allocatable, save, target :: brain_next_cache(:,:)
    integer,    allocatable, save, target :: incoming_direction_next_cache(:,:,:)
    contains

    subroutine initialize_brain(brain, rows, cols)
        type(trinary), allocatable :: brain(:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j

        if (allocated(brain)) deallocate(brain)
        allocate(brain(rows, cols))

        ! Initialize the 2D brain with all lows (0's)
        !$omp parallel do collapse(2) if(rows*cols > 100)
        do i = 1, rows
            do j = 1, cols
                brain(i, j)%value = low
            end do
        end do
        !$omp end parallel do
    end subroutine initialize_brain

subroutine copy_non_low_to_brain_top_row(inputter, brain, incoming_direction, input_offset, cols)
    type(trinary), allocatable :: inputter(:), brain(:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    integer, intent(in) :: input_offset, cols
    integer :: i, brain_col
    
    ! Note: Don't parallelize this loop - it's very short (8 elements) and has conditionals
    do i = 1, size(inputter)
        brain_col = input_offset + i - 1
        if (brain_col >= 1 .and. brain_col <= cols) then
            if (inputter(i)%value /= low) then
                brain(1, brain_col)%value = inputter(i)%value
                ! Set incoming direction to 7 (Down) - signal came from "above" (inputter)
                incoming_direction(1, brain_col, 1) = 7
            end if
        end if
    end do
end subroutine copy_non_low_to_brain_top_row

! Copy binocular vision (two input vectors) to brain top row
subroutine copy_binocular_to_brain_top_row(left_inputter, right_inputter, brain, incoming_direction, &
                                            left_offset, right_offset, cols)
    type(trinary), allocatable :: left_inputter(:), right_inputter(:), brain(:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    integer, intent(in) :: left_offset, right_offset, cols
    integer :: i, brain_col
    
    ! Copy left eye input
    do i = 1, size(left_inputter)
        brain_col = left_offset + i - 1
        if (brain_col >= 1 .and. brain_col <= cols) then
            if (left_inputter(i)%value /= low) then
                brain(1, brain_col)%value = left_inputter(i)%value
                incoming_direction(1, brain_col, 1) = 7  ! Signal from above
            end if
        end if
    end do
    
    ! Copy right eye input
    do i = 1, size(right_inputter)
        brain_col = right_offset + i - 1
        if (brain_col >= 1 .and. brain_col <= cols) then
            if (right_inputter(i)%value /= low) then
                brain(1, brain_col)%value = right_inputter(i)%value
                incoming_direction(1, brain_col, 1) = 7  ! Signal from above
            end if
        end if
    end do
end subroutine copy_binocular_to_brain_top_row

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
    type(trinary), pointer :: brain_next(:,:)
    integer, pointer :: incoming_direction_next(:,:,:)
    integer :: i, j, k, index, incoming_dir, incoming_dir2
    real :: total_value, random_num, threshold
    real :: synapse_values(8)
    integer :: ni, nj
    logical :: valid_move
    integer :: num_valid_directions
    integer :: valid_indices(8)
    real :: valid_synapse_values(8)
    real :: cumulative_prob
    integer, parameter :: max_synapse_strength = 2000000, reinforcement_amount=1000
    integer :: current_state, target_state
    integer :: buf_rows, buf_cols
    real :: output_center, dist_to_center, lateral_bias

    ! Ensure reusable buffers are allocated for current size
    buf_rows = rows
    buf_cols = cols
    if (.not. allocated(brain_next_cache) .or. size(brain_next_cache,1) /= buf_rows .or. size(brain_next_cache,2) /= buf_cols) then
        if (allocated(brain_next_cache)) deallocate(brain_next_cache)
        allocate(brain_next_cache(buf_rows, buf_cols))
    end if
    if (.not. allocated(incoming_direction_next_cache) .or. size(incoming_direction_next_cache,1) /= buf_rows .or. &
        size(incoming_direction_next_cache,2) /= buf_cols) then
        if (allocated(incoming_direction_next_cache)) deallocate(incoming_direction_next_cache)
        allocate(incoming_direction_next_cache(buf_rows, buf_cols, 2))
    end if

    brain_next => brain_next_cache
    incoming_direction_next => incoming_direction_next_cache

    brain_next = brain  ! Copy current brain state
    incoming_direction_next = incoming_direction  ! Copy incoming directions

    ! Process each neuron - parallelized with atomic updates for write conflicts
    !$omp parallel do collapse(2) private(j, k, index, incoming_dir, incoming_dir2, &
    !$omp& synapse_values, ni, nj, valid_move, num_valid_directions, valid_indices, &
    !$omp& valid_synapse_values, total_value, random_num, threshold, cumulative_prob, &
    !$omp& current_state, target_state) &
    !$omp& shared(brain, brain_next, synapses, synapse_usage, incoming_direction, &
    !$omp& incoming_direction_next, outputter, rows, cols, output_offset, output_length) &
    !$omp& if(rows*cols > 20)
    do i = 1, rows
        do j = 1, cols
            current_state = brain(i, j)%value
            if (current_state /= low) then
                ! Get incoming direction(s)
                incoming_dir = incoming_direction(i, j, 1)
                incoming_dir2 = incoming_direction(i, j, 2)
                
                ! Skip if no incoming direction set (shouldn't happen for active neurons)
                if (incoming_dir == 0) cycle

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

                ! Calculate output center and distance for lateral bias
                output_center = output_offset + (output_length - 1) / 2.0
                dist_to_center = abs(real(j) - output_center)

                ! Check each direction for viability and apply bias
                do k = 1, 8
                    ni = i + directions(k, 1)
                    nj = j + directions(k, 2)
                    valid_move = .false.

                    ! Validity checks
                    if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                        if (brain_next(ni, nj)%value /= high) then
                            valid_move = .true.
                        end if
                    ! Check if the move is from the last row into the outputter array
                    else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                        if (outputter(nj - output_offset + 1)%value /= high) then
                            valid_move = .true.
                        end if
                    end if

                    ! Apply bias if move is valid
                    if (valid_move) then
                        ! Start with base directional bias (downward preference)
                        lateral_bias = direction_bias(k)
                        
                        ! Add lateral bias toward output center
                        ! If current column is LEFT of center and direction moves RIGHT: boost
                        ! If current column is RIGHT of center and direction moves LEFT: boost
                        if (dist_to_center > 0.5) then
                            if (j < output_center .and. directions(k, 2) > 0) then
                                ! Moving right toward center - boost
                                lateral_bias = lateral_bias * 1.2
                            else if (j > output_center .and. directions(k, 2) < 0) then
                                ! Moving left toward center - boost
                                lateral_bias = lateral_bias * 1.2
                            end if
                        end if
                        
                        synapse_values(k) = synapse_values(k) * lateral_bias
                        if (synapse_values(k) > 0.0) then
                            num_valid_directions = num_valid_directions + 1
                        else
                            synapse_values(k) = 0.0
                        end if
                    else
                        synapse_values(k) = 0.0
                    end if
                end do

                if (num_valid_directions == 0) cycle

                num_valid_directions = 0
                total_value = 0.0
                do k = 1, 8
                    if (synapse_values(k) > 0.0) then
                        num_valid_directions = num_valid_directions + 1
                        valid_indices(num_valid_directions) = k
                        valid_synapse_values(num_valid_directions) = synapse_values(k)
                        total_value = total_value + synapse_values(k)
                    end if
                end do

                ! Draw a direction using cumulative sum without heap allocations
                call random_number(random_num)
                threshold = random_num * total_value
                cumulative_prob = 0.0
                index = valid_indices(1)
                do k = 1, num_valid_directions
                    cumulative_prob = cumulative_prob + valid_synapse_values(k)
                    if (threshold <= cumulative_prob) then
                        index = valid_indices(k)
                        exit
                    end if
                end do

                ! Compute target position based on selected direction
                ni = i + directions(index, 1)
                nj = j + directions(index, 2)

                ! Perform the move
                if (ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) then
                    target_state = brain_next(ni, nj)%value
                    ! Critical section to prevent race conditions from parallel threads
                    !$omp critical
                    if (brain_next(ni, nj)%value < high) brain_next(ni, nj)%value = brain_next(ni, nj)%value + 1
                    !$omp end critical
                    if (brain_next(i, j)%value > low) brain_next(i, j)%value = brain_next(i, j)%value - 1
                    
                    ! Set incoming direction for target neuron
                    if (target_state == low) then
                        ! Target was LOW, now MEDIUM - set primary incoming direction
                        incoming_direction_next(ni, nj, 1) = direction_opposites(index)
                    else if (target_state == medium) then
                        ! Target was MEDIUM, now HIGH - set secondary incoming direction
                        incoming_direction_next(ni, nj, 2) = direction_opposites(index)
                    end if
                    
                    ! Clear incoming directions appropriately based on state change
                    if (brain_next(i, j)%value == low) then
                        ! Going to LOW - clear all incoming directions
                        incoming_direction_next(i, j, 1) = 0
                        incoming_direction_next(i, j, 2) = 0
                    else if (current_state == high .and. brain_next(i, j)%value == medium) then
                        ! Going from HIGH to MEDIUM - clear secondary incoming direction
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
                    ! Critical section to prevent race conditions from parallel threads
                    !$omp critical
                    if (outputter(nj - output_offset + 1)%value < high) outputter(nj - output_offset + 1)%value = outputter(nj - output_offset + 1)%value + 1
                    !$omp end critical
                    if (brain_next(i, j)%value > low) brain_next(i, j)%value = brain_next(i, j)%value - 1
                    
                    ! Clear incoming directions appropriately based on state change
                    if (brain_next(i, j)%value == low) then
                        ! Going to LOW - clear all incoming directions
                        incoming_direction_next(i, j, 1) = 0
                        incoming_direction_next(i, j, 2) = 0
                    else if (current_state == high .and. brain_next(i, j)%value == medium) then
                        ! Going from HIGH to MEDIUM - clear secondary incoming direction
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

            end if
        end do
    end do
    !$omp end parallel do

    ! Replace the current brain state and incoming directions with the next state
    brain = brain_next
    incoming_direction = incoming_direction_next
end subroutine update_brain_state_based_on_synapses



end module brain_module

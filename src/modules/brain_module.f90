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
    ! Vertical bias (for sensory signals from top): downward directions favored
    real,    parameter :: direction_bias(8) = [0.5, 0.5, 0.5, 1.0, 1.0, 1.5, 1.8, 1.5]
    ! Horizontal bias (for meta signals from right): leftward directions favored
    real,    parameter :: horizontal_bias(8) = [1.5, 1.0, 0.5, 0.5, 0.5, 1.0, 1.5, 1.8]

    ! Side I/O arrays (module-level, like outputter_module)
    type(trinary), allocatable, save :: side_meta_inputter(:)      ! Right side input
    type(trinary), allocatable, save :: overflow_outputter(:) ! Left side output
    type(trinary), allocatable, save :: backup_overflow(:)    ! Previous overflow state

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

subroutine update_brain_state_based_on_synapses(brain, synapses, outputter, synapse_usage, incoming_direction, &
                                                rows, cols, input_offset, output_offset, output_length, pressure)
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
    real, intent(in), optional :: pressure  ! Brain pressure for valve control
    type(trinary), pointer :: brain_next(:,:)
    integer, pointer :: incoming_direction_next(:,:,:)
    integer :: i, j, k, index, incoming_dir, incoming_dir2
    real :: total_value, random_num, threshold
    real :: synapse_values(8)
    real :: active_bias(8)  ! Context-dependent bias selection
    real :: leftward_pull, vertical_mult, current_pressure
    integer :: ni, nj
    logical :: valid_move
    integer :: num_valid_directions
    integer :: valid_indices(8)
    real :: valid_synapse_values(8)
    real :: cumulative_prob
    integer, parameter :: max_synapse_strength = 2000000, reinforcement_amount=1000
    integer :: current_state, target_state
    integer :: buf_rows, buf_cols
    logical :: move_succeeded  ! Track if move actually happened for energy conservation

    ! Set pressure (default to 0.0 if not provided)
    if (present(pressure)) then
        current_pressure = pressure
    else
        current_pressure = 0.0
    end if
    
    ! Calculate pressure-based multipliers (Valve 2 and 3)
    leftward_pull = calculate_leftward_pull(current_pressure)
    vertical_mult = 1.0 + current_pressure * 1.0  ! 1.0 → 2.0

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
    !$omp& synapse_values, active_bias, ni, nj, valid_move, num_valid_directions, valid_indices, &
    !$omp& valid_synapse_values, total_value, random_num, threshold, cumulative_prob, &
    !$omp& current_state, target_state, move_succeeded) &
    !$omp& shared(brain, brain_next, synapses, synapse_usage, incoming_direction, &
    !$omp& incoming_direction_next, outputter, rows, cols, output_offset, output_length, &
    !$omp& leftward_pull, vertical_mult) &
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
                
                ! ============================================================
                ! CONTEXT-DEPENDENT BIAS SELECTION (Phase 2)
                ! ============================================================
                ! Select bias based on incoming direction to determine signal origin
                if (incoming_dir == 6 .or. incoming_dir == 2 .or. incoming_dir == 7) then
                    ! Vertical flow (from top): use vertical bias with pressure scaling
                    active_bias = direction_bias * vertical_mult
                else if (incoming_dir == 4 .or. incoming_dir == 8) then
                    ! Horizontal flow (from right): use horizontal bias
                    active_bias = horizontal_bias
                else
                    ! Diagonal incoming: default to vertical bias
                    active_bias = direction_bias * vertical_mult
                end if
                
                ! Apply leftward pull (Valve 2) - boosts leftward directions
                active_bias(1) = active_bias(1) * leftward_pull  ! NW
                active_bias(7) = active_bias(7) * leftward_pull  ! SW
                active_bias(8) = active_bias(8) * leftward_pull  ! W

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
                        if (brain_next(ni, nj)%value /= high) then
                            valid_move = .true.
                        end if
                    ! Check if the move is from the last row into the outputter array
                    else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                        if (outputter(nj - output_offset + 1)%value /= high) then
                            valid_move = .true.
                        end if
                    end if

                    ! Apply context-dependent bias if move is valid
                    if (valid_move) then
                        synapse_values(k) = synapse_values(k) * active_bias(k)
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
                    move_succeeded = .false.
                    
                    ! Critical section: ONLY increment target if < HIGH
                    ! Track success so we only decrement source if move actually happened
                    !$omp critical
                    if (brain_next(ni, nj)%value < high) then
                        brain_next(ni, nj)%value = brain_next(ni, nj)%value + 1
                        move_succeeded = .true.
                    end if
                    !$omp end critical
                    
                    ! ENERGY CONSERVATION: Only decrement source if target was incremented
                    if (move_succeeded) then
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
                    end if
                    
                ! Perform the move into the outputter array if from the last row moving down
                else if (i == rows .and. ni == rows + 1 .and. (nj - output_offset + 1) >= 1 .and. (nj - output_offset + 1) <= output_length) then
                    move_succeeded = .false.
                    
                    ! Critical section: ONLY increment outputter if < HIGH
                    !$omp critical
                    if (outputter(nj - output_offset + 1)%value < high) then
                        outputter(nj - output_offset + 1)%value = outputter(nj - output_offset + 1)%value + 1
                        move_succeeded = .true.
                    end if
                    !$omp end critical
                    
                    ! ENERGY CONSERVATION: Only decrement source if outputter was incremented
                    if (move_succeeded) then
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

            end if
        end do
    end do
    !$omp end parallel do

    ! Replace the current brain state and incoming directions with the next state
    brain = brain_next
    incoming_direction = incoming_direction_next
end subroutine update_brain_state_based_on_synapses

! ============================================================
! SIDE I/O SUBROUTINES (Cross-Flow Architecture)
! ============================================================

subroutine initialize_side_io(rows, meta_input_length, overflow_length)
    integer, intent(in) :: rows, meta_input_length, overflow_length
    integer :: i
    
    ! Allocate side I/O arrays
    if (allocated(side_meta_inputter)) deallocate(side_meta_inputter)
    if (allocated(overflow_outputter)) deallocate(overflow_outputter)
    if (allocated(backup_overflow)) deallocate(backup_overflow)
    
    allocate(side_meta_inputter(meta_input_length))
    allocate(overflow_outputter(overflow_length))
    allocate(backup_overflow(overflow_length))
    
    ! Initialize to low state
    do i = 1, meta_input_length
        side_meta_inputter(i)%value = low
    end do
    do i = 1, overflow_length
        overflow_outputter(i)%value = low
        backup_overflow(i)%value = low
    end do
end subroutine initialize_side_io

subroutine apply_meta_input_to_brain(brain, incoming_direction, meta_input_offset, rows, cols)
    ! Apply meta-brain output to rightmost column at variable position
    type(trinary), allocatable :: brain(:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    integer, intent(in) :: meta_input_offset, rows, cols
    integer :: i, brain_row
    
    do i = 1, size(side_meta_inputter)
        brain_row = meta_input_offset + i - 1
        if (brain_row >= 1 .and. brain_row <= rows) then
            if (side_meta_inputter(i)%value /= low) then
                brain(brain_row, cols)%value = side_meta_inputter(i)%value
                ! Direction 8 = West (going left)
                incoming_direction(brain_row, cols, 1) = 8
            end if
        end if
    end do
end subroutine apply_meta_input_to_brain

subroutine copy_overflow_from_brain_left_column(brain, overflow_offset, rows)
    ! Capture overflow from leftmost column at variable position (like output vector)
    type(trinary), allocatable :: brain(:,:)
    integer, intent(in) :: overflow_offset, rows
    integer :: i, brain_row
    
    ! Backup previous overflow state
    backup_overflow = overflow_outputter
    
    do i = 1, size(overflow_outputter)
        brain_row = overflow_offset + i - 1
        if (brain_row >= 1 .and. brain_row <= rows) then
            overflow_outputter(i)%value = brain(brain_row, 1)%value
        end if
    end do
end subroutine copy_overflow_from_brain_left_column

! ============================================================
! PRESSURE CALCULATION (Phase 3)
! ============================================================

function calculate_brain_activity(brain, rows, cols) result(activity)
    ! Sum all trinary cell values (0, 1, or 2)
    type(trinary), allocatable :: brain(:,:)
    integer, intent(in) :: rows, cols
    integer :: activity, i, j
    
    activity = 0
    do i = 1, rows
        do j = 1, cols
            activity = activity + brain(i, j)%value
        end do
    end do
end function calculate_brain_activity

function calculate_brain_pressure(brain, rows, cols) result(pressure)
    ! Pressure = activity / max_activity (range: 0.0 to 1.0)
    type(trinary), allocatable :: brain(:,:)
    integer, intent(in) :: rows, cols
    real :: pressure
    integer :: activity, max_activity
    
    activity = calculate_brain_activity(brain, rows, cols)
    max_activity = rows * cols * 2  ! All cells at HIGH = 2
    pressure = real(activity) / real(max_activity)
end function calculate_brain_pressure

function calculate_leftward_pull(pressure) result(leftward_multiplier)
    ! Valve 2: Leftward bias increases with pressure
    ! Lower thresholds to start draining earlier and prevent energy accumulation
    real, intent(in) :: pressure
    real :: leftward_multiplier
    
    if (pressure < 0.1) then
        leftward_multiplier = 1.0  ! No pull - drain closed (very low pressure)
    else if (pressure < 0.3) then
        ! Gradual increase: 1.0 → 2.0 as pressure goes 0.1 → 0.3
        leftward_multiplier = 1.0 + (pressure - 0.1) / 0.2 * 1.0
    else
        leftward_multiplier = 2.0  ! Strong pull - drain wide open
    end if
end function calculate_leftward_pull

function get_overflow_activity() result(activity)
    ! Return total activity in overflow_outputter
    integer :: activity, i
    activity = 0
    if (allocated(overflow_outputter)) then
        do i = 1, size(overflow_outputter)
            activity = activity + overflow_outputter(i)%value
        end do
    end if
end function get_overflow_activity

subroutine set_side_meta_input(values)
    ! Copy values from external array into side_meta_inputter
    type(trinary), intent(in) :: values(:)
    integer :: i, n
    n = min(size(values), size(side_meta_inputter))
    do i = 1, n
        side_meta_inputter(i)%value = values(i)%value
    end do
end subroutine set_side_meta_input

subroutine apply_throttled_meta_input(brain, incoming_direction, pressure, &
                                       meta_input_offset, rows, cols)
    ! Valve 1: Throttle meta-brain input based on pressure
    type(trinary), allocatable :: brain(:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    real, intent(in) :: pressure
    integer, intent(in) :: meta_input_offset, rows, cols
    real :: flow_rate, rand_val
    integer :: i, brain_row
    
    ! Calculate flow rate based on pressure
    if (pressure < 0.3) then
        flow_rate = 1.0  ! Full flow
    else if (pressure < 0.6) then
        flow_rate = 1.0 - (pressure - 0.3) / 0.3  ! Linear throttle
    else
        flow_rate = 0.0  ! Cut off
    end if
    
    ! Apply meta_inputter with probability = flow_rate
    do i = 1, size(side_meta_inputter)
        brain_row = meta_input_offset + i - 1
        if (brain_row >= 1 .and. brain_row <= rows) then
            if (side_meta_inputter(i)%value /= low) then
                call random_number(rand_val)
                if (rand_val < flow_rate) then
                    brain(brain_row, cols)%value = side_meta_inputter(i)%value
                    incoming_direction(brain_row, cols, 1) = 8  ! From West
                end if
            end if
        end if
    end do
end subroutine apply_throttled_meta_input

! ============================================================
! BRAIN ENERGY DRAIN (Prevents accumulation/saturation)
! ============================================================
! This subroutine probabilistically drains energy from the brain
! to prevent runaway accumulation that blocks signal propagation.
! 
! The drain rate is adaptive: stronger when pressure is high,
! gentler when pressure is low. This maintains a healthy energy
! balance while preserving learned activation patterns.

subroutine drain_brain_energy(brain, incoming_direction, rows, cols, drain_probability)
    type(trinary), allocatable, intent(inout) :: brain(:,:)
    integer, allocatable, intent(inout) :: incoming_direction(:,:,:)
    integer, intent(in) :: rows, cols
    real, intent(in) :: drain_probability
    integer :: i, j
    real :: rand_val
    
    do i = 1, rows
        do j = 1, cols
            if (brain(i, j)%value > low) then
                call random_number(rand_val)
                if (rand_val < drain_probability) then
                    brain(i, j)%value = brain(i, j)%value - 1
                    ! Clear incoming direction if now LOW
                    if (brain(i, j)%value == low) then
                        incoming_direction(i, j, 1) = 0
                        incoming_direction(i, j, 2) = 0
                    else if (brain(i, j)%value == medium) then
                        ! Clear secondary incoming direction if now MEDIUM
                        incoming_direction(i, j, 2) = 0
                    end if
                end if
            end if
        end do
    end do
end subroutine drain_brain_energy


end module brain_module

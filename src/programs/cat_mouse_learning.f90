program cat_mouse_learning
    use trinary_module
    use brain_module
    use brain_engine_module
    use synapses_module
    use vision_simulation_module
    implicit none
    
    ! Primary Brain system
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:,:)  ! Now 4D: (row, col, incoming_dir, outgoing_dir)
    logical, allocatable :: synapse_usage(:,:,:,:)  ! Now 4D: track which synapses fire during Bar
    integer, allocatable :: incoming_direction(:,:,:)  ! Track up to 2 incoming directions per neuron
    integer :: rows, cols, input_length, output_length
    integer :: input_offset, output_offset
    
    ! Cross-flow architecture: side I/O parameters
    integer :: meta_input_offset, meta_input_map_length  ! Right column meta-brain input
    integer :: overflow_offset, overflow_map_length      ! Left column overflow output
    real :: brain_pressure  ! Pressure regulation
    
    ! Secondary "Meta-Learning" Brain system
    type(trinary), allocatable :: meta_brain(:,:), meta_inputter(:), meta_outputter(:)
    integer, allocatable :: meta_synapses(:,:,:,:)
    logical, allocatable :: meta_synapse_usage(:,:,:,:)
    integer, allocatable :: meta_incoming_direction(:,:,:)
    integer :: meta_rows, meta_cols, meta_input_length, meta_output_length
    
    ! Meta-brain catch rate tracking
    integer :: rate_counter  ! d(catches)/dt counter
    integer :: rate_decay_timer  ! Counts up to 60 bars for decay
    integer, parameter :: rate_decay_interval = 60  ! Bars between decay events
    
    ! Vision system
    type(position) :: cat_pos, mouse_pos
    real :: field_size, step_size
    integer :: num_vision_slices
    
    ! Movement
    integer, dimension(8, 2) :: move_directions
    
    ! Simulation
    integer :: bar, max_bars, snapshot_interval, mouse_move_interval
    integer :: brain_step, steps_per_bar
    integer :: i, j, k, ni, nj, brain_energy, output_energy
    integer :: output_action, move_distance
    real :: new_x, new_y
    
    ! Reinforcement tracking
    real :: current_distance, previous_distance
    real :: dx, dy
    real :: cat_move_x, cat_move_y, desired_dx, desired_dy, dot_product
    real :: old_cat_x, old_cat_y, path_dx, path_dy, path_length
    real :: t, closest_x, closest_y, closest_dist
    real :: desired_length  ! For normalized direction-based reward
    
    ! Anti-oscillation mechanisms
    integer, parameter :: history_size = 10
    integer :: movement_history(history_size)
    integer :: history_index, repeated_count
    real :: adaptive_threshold, threshold_min, threshold_max
    real :: direction_multiplier, epoch_progress  ! For graduated rewards
    integer :: last_rewarded_direction, momentum_bonus, momentum_streak
    integer :: bars_since_progress, progress_check_interval
    real :: best_distance, progress_factor
    
    ! Success-based pathway boosting
    integer, parameter :: success_history_size = 100
    logical, allocatable :: synapse_history(:,:,:,:,:)  ! 5D: (history_index, row, col, incoming, outgoing)
    integer :: history_write_index
    
    ! Continuous hunting performance tracking
    integer :: catches_count
    integer :: moves_towards, moves_away, moves_perpendicular, total_moves
    
    ! Epoch tracking for temporal analysis
    integer, parameter :: epoch_size = 2000  ! Bars per epoch
    integer :: current_epoch, num_epochs
    integer, allocatable :: epoch_catches(:)
    integer :: catches_this_epoch
    integer, allocatable :: epoch_moves_towards(:), epoch_moves_away(:), epoch_moves_perpendicular(:)
    integer :: moves_towards_this_epoch, moves_away_this_epoch, moves_perpendicular_this_epoch
    integer :: epoch_total_moves  ! For calculating percentages
    
    ! Logging
    integer :: csv_unit
    
    ! Output accumulation across brain steps within a Bar
    type(trinary), allocatable :: accumulated_output(:), accumulated_meta_output(:)
    character(len=100) :: csv_filename
    
    ! Cross-flow tracking
    integer :: overflow_activity, cumulative_overflow
    
    ! Meta-brain energy diagnostic
    integer :: meta_input_energy, meta_brain_energy, meta_output_energy
    
    ! Random seed variables
    integer :: seed_size, clock
    integer, allocatable :: seed(:)
    integer :: user_seed
    character(len=32) :: arg
    
    ! Weight save/load functionality
    logical :: load_weights, disable_direct_rewards
    character(len=256) :: weight_file
    integer :: weight_unit
    
    ! Output directory functionality
    character(len=512) :: output_dir
    character(len=512) :: full_path
    integer :: arg_idx
    
    ! Get seed from command line if provided, otherwise use system clock
    load_weights = .false.
    disable_direct_rewards = .false.
    weight_file = ''
    output_dir = ''  ! Default: save to current directory
    
    if (command_argument_count() > 0) then
        call get_command_argument(1, arg)
        read(arg, *) user_seed
    else
        call system_clock(count=user_seed)
    end if
    
    ! Parse optional flags (can appear in any order after seed)
    arg_idx = 2
    do while (arg_idx <= command_argument_count())
        call get_command_argument(arg_idx, arg)
        
        if (trim(arg) == '--load-weights') then
            load_weights = .true.
            arg_idx = arg_idx + 1
            call get_command_argument(arg_idx, weight_file)
        else if (trim(arg) == '--no-direct-rewards') then
            disable_direct_rewards = .true.
        else if (trim(arg) == '--output-dir') then
            arg_idx = arg_idx + 1
            call get_command_argument(arg_idx, output_dir)
            ! Ensure output_dir ends with /
            if (len_trim(output_dir) > 0) then
                if (output_dir(len_trim(output_dir):len_trim(output_dir)) /= '/') then
                    output_dir = trim(output_dir) // '/'
                end if
            end if
        end if
        
        arg_idx = arg_idx + 1
    end do
    
    ! Parameters
    rows = 6
    cols = 12
    input_offset = 3       ! Centered on brain (8 slices centered on 12 columns)
    input_length = 8       ! 8 vision slices (45° each, was 6 @ 60°)
    output_offset = 3      ! Centered on brain (8 directions centered on 12 columns)
    output_length = 8      ! 8 movement directions
    max_bars = 20000       ! Real-world time steps (balanced for learning)
    steps_per_bar = 12     ! Brain steps per Bar (reduced for better temporal precision)
    snapshot_interval = 1000  ! Print full state every N Bars
    mouse_move_interval = 10   ! Mouse moves every N Bars (was 25)
    
    ! Vision parameters
    field_size = 100.0
    step_size = 5.0
    num_vision_slices = input_length
    
    ! Define movement directions (same as brain directions)
    move_directions(1,1) = -1; move_directions(1,2) = -1  ! Up-Left
    move_directions(2,1) = -1; move_directions(2,2) =  0  ! Up
    move_directions(3,1) = -1; move_directions(3,2) =  1  ! Up-Right
    move_directions(4,1) =  0; move_directions(4,2) = -1  ! Left
    move_directions(5,1) =  0; move_directions(5,2) =  1  ! Right
    move_directions(6,1) =  1; move_directions(6,2) = -1  ! Down-Left
    move_directions(7,1) =  1; move_directions(7,2) =  0  ! Down
    move_directions(8,1) =  1; move_directions(8,2) =  1  ! Down-Right
    
    ! Initialize primary brain system using brain engine
    call initialize_brain_system(brain, inputter, outputter, synapses, synapse_usage, &
                                 incoming_direction, rows, cols, input_length, output_length, &
                                 input_offset, output_offset)
    
    ! Initialize cross-flow side I/O (meta-brain feeds into right column)
    ! Map meta-brain's 5-element output to rows 2-6 of the 6-row brain
    meta_input_offset = 2
    meta_input_map_length = 5
    overflow_offset = 2
    overflow_map_length = 4
    call initialize_side_io(rows, meta_input_map_length, overflow_map_length)
    cumulative_overflow = 0
    
    ! Initialize meta-brain system (3x5 brain - SMALLER for faster propagation)
    ! With 12 steps/bar, energy can propagate 2 rows in 2 steps, reaching output easily
    meta_rows = 3
    meta_cols = 5
    meta_input_length = 5
    meta_output_length = 5
    call initialize_brain_system(meta_brain, meta_inputter, meta_outputter, meta_synapses, &
                                 meta_synapse_usage, meta_incoming_direction, &
                                 meta_rows, meta_cols, meta_input_length, meta_output_length, &
                                 1, 1)  ! Meta-brain uses offset 1 for both input and output
    
    ! Initialize rate counter and timer
    rate_counter = 0
    rate_decay_timer = 0
    
    ! Initialize output accumulators
    allocate(accumulated_output(output_length))
    allocate(accumulated_meta_output(meta_output_length))
    do i = 1, output_length
        call accumulated_output(i)%set(low)
    end do
    do i = 1, meta_output_length
        call accumulated_meta_output(i)%set(low)
    end do
    
    ! Initialize success history buffer (circular buffer for last 100 Bars)
    allocate(synapse_history(success_history_size, rows, cols, 8, 8))
    synapse_history = .false.
    history_write_index = 1
    
    ! Seed random number generator with user-provided seed or system time
    ! This ensures different brain structures across trials
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = user_seed + 37 * (/ (i, i=1,seed_size) /)  ! Mix seed with varied values
    call random_seed(put=seed)
    deallocate(seed)
    
    ! Load weights from file if specified
    if (load_weights) then
        print *, "Loading weights from:", trim(weight_file)
        open(newunit=weight_unit, file=weight_file, status='old', action='read', form='unformatted')
        read(weight_unit) synapses
        read(weight_unit) meta_synapses
        close(weight_unit)
        print *, "Weights loaded successfully"
        if (disable_direct_rewards) then
            print *, "Direct rewards DISABLED - using meta-brain only"
        end if
    end if
    
    ! Initialize mouse at center (stationary target)
    mouse_pos%x = field_size / 2.0
    mouse_pos%y = field_size / 2.0
    
    ! Initialize cat at random position far from center (distance > 30)
    call random_number(dx)
    call random_number(dy)
    ! Generate position with distance 30-45 from center
    dx = (dx - 0.5) * 2.0  ! Range: -1 to 1
    dy = (dy - 0.5) * 2.0
    ! Normalize and scale
    previous_distance = sqrt(dx*dx + dy*dy)
    if (previous_distance > 0.0) then
        dx = dx / previous_distance
        dy = dy / previous_distance
    end if
    call random_number(previous_distance)
    previous_distance = 30.0 + previous_distance * 15.0  ! Distance 30-45
    cat_pos%x = mouse_pos%x + dx * previous_distance
    cat_pos%y = mouse_pos%y + dy * previous_distance
    ! Clamp to field
    if (cat_pos%x < 0.0) cat_pos%x = 0.0
    if (cat_pos%x > field_size) cat_pos%x = field_size
    if (cat_pos%y < 0.0) cat_pos%y = 0.0
    if (cat_pos%y > field_size) cat_pos%y = field_size
    
    ! Calculate initial distance
    dx = mouse_pos%x - cat_pos%x
    dy = mouse_pos%y - cat_pos%y
    if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
    if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
    previous_distance = sqrt(dx*dx + dy*dy)
    best_distance = previous_distance
    
    ! Initialize anti-oscillation mechanisms
    movement_history = 0  ! No movements yet
    history_index = 1
    adaptive_threshold = 0.7  ! STRICT: Only reward ~45° cone or better (cos(45°) ≈ 0.707)
    threshold_min = 0.6      ! Minimum 53° cone
    threshold_max = 0.85     ! Maximum 32° cone (tightens with progress)
    last_rewarded_direction = 0  ! No momentum yet
    momentum_streak = 0  ! No streak yet
    bars_since_progress = 0
    progress_check_interval = 50
    
    ! Initialize continuous hunting counter
    catches_count = 0
    moves_towards = 0
    moves_away = 0
    moves_perpendicular = 0
    total_moves = 0
    
    ! Initialize epoch tracking
    num_epochs = max_bars / epoch_size
    allocate(epoch_catches(num_epochs))
    allocate(epoch_moves_towards(num_epochs))
    allocate(epoch_moves_away(num_epochs))
    allocate(epoch_moves_perpendicular(num_epochs))
    epoch_catches = 0
    epoch_moves_towards = 0
    epoch_moves_away = 0
    epoch_moves_perpendicular = 0
    current_epoch = 1
    catches_this_epoch = 0
    moves_towards_this_epoch = 0
    moves_away_this_epoch = 0
    moves_perpendicular_this_epoch = 0
    
    ! Open CSV file for logging (use output_dir if specified)
    csv_filename = trim(output_dir) // 'simulation_log.csv'
    open(newunit=csv_unit, file=csv_filename, status='replace', action='write')
    write(csv_unit, '(A)') 'bar,mouse_x,mouse_y,cat_x,cat_y,vision_slice,brain_energy,output_energy,output_action,move_dist,catches,pressure,overflow'
    
    print *, "=== CAT & MOUSE CONTINUOUS HUNTING SIMULATION ==="
    print *, "Max Bars (real-world steps):", max_bars
    print *, "Brain steps per Bar:", steps_per_bar
    print *, "Mouse movement: every", mouse_move_interval, "Bars, 2 units random direction"
    print *, "Epoch size:", epoch_size, "Bars (", num_epochs, "epochs total)"
    print *, "Logging to:", trim(csv_filename)
    print *, "Snapshot interval:", snapshot_interval
    print *
    
    ! Simulation loop - each Bar is one real-world time step
    do bar = 1, max_bars
        ! Check for epoch boundary
        if (bar > 1 .and. mod(bar - 1, epoch_size) == 0) then
            ! Save data for completed epoch
            epoch_catches(current_epoch) = catches_this_epoch
            epoch_moves_towards(current_epoch) = moves_towards_this_epoch
            epoch_moves_away(current_epoch) = moves_away_this_epoch
            epoch_moves_perpendicular(current_epoch) = moves_perpendicular_this_epoch
            print *, "Epoch", current_epoch, "complete:", catches_this_epoch, "catches"
            ! Move to next epoch
            current_epoch = current_epoch + 1
            catches_this_epoch = 0
            moves_towards_this_epoch = 0
            moves_away_this_epoch = 0
            moves_perpendicular_this_epoch = 0
        end if
        
        ! Update rate decay timer and decrement counter every 60 bars
        rate_decay_timer = rate_decay_timer + 1
        if (rate_decay_timer >= rate_decay_interval) then
            rate_counter = max(0, rate_counter - 1)
            rate_decay_timer = 0
        end if
        
        ! Encode rate_counter into meta_inputter (positional encoding)
        ! Rate 1-5: Single MEDIUM at positions 5,4,3,2,1 (right to left)
        ! Rate 6-10: Single HIGH at positions 5,4,3,2,1 (right to left)
        ! Rate >10: Saturates at position 1 with HIGH
        ! First, reset all to LOW
        do i = 1, meta_input_length
            call meta_inputter(i)%set(low)
        end do
        ! Then set the single active position
        if (rate_counter >= 1 .and. rate_counter <= 5) then
            ! MEDIUM state at specific position (rate 1→pos 5, rate 2→pos 4, etc.)
            call meta_inputter(meta_input_length - rate_counter + 1)%set(medium)
        else if (rate_counter >= 6 .and. rate_counter <= 10) then
            ! HIGH state at specific position (rate 6→pos 5, rate 7→pos 4, etc.)
            call meta_inputter(meta_input_length - (rate_counter - 5) + 1)%set(high)
        else if (rate_counter > 10) then
            ! Saturate at position 1 (leftmost) with HIGH
            call meta_inputter(1)%set(high)
        end if
        
        ! Record distance at start of Bar (before any actions)
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        previous_distance = sqrt(dx*dx + dy*dy)
        
        ! Reset synapse usage tracker for this Bar
        call reset_synapse_usage(synapse_usage, rows, cols)
        call reset_synapse_usage(meta_synapse_usage, meta_rows, meta_cols)
        
        ! Mouse movement: every mouse_move_interval Bars, move in random direction by small amount
        if (mod(bar, mouse_move_interval) == 0) then
            ! Generate random direction (0 to 2*PI radians)
            call random_number(dx)
            dx = dx * 2.0 * 3.14159265359
            
            ! Small movement: 2 units in random direction
            mouse_pos%x = mouse_pos%x + 2.0 * cos(dx)
            mouse_pos%y = mouse_pos%y + 2.0 * sin(dx)
            
            ! Keep mouse within field boundaries
            if (mouse_pos%x < 0.0) mouse_pos%x = 0.0
            if (mouse_pos%x > field_size) mouse_pos%x = field_size
            if (mouse_pos%y < 0.0) mouse_pos%y = 0.0
            if (mouse_pos%y > field_size) mouse_pos%y = field_size
        end if
        
        ! Update vision input
        call update_vision_input(inputter, cat_pos, mouse_pos, num_vision_slices)
        
        ! Determine which vision slice is active
        output_action = 0
        do i = 1, input_length
            if (inputter(i)%get() > 0) then
                output_action = i
                exit
            end if
        end do
        
        ! Reset synapse usage tracking for this Bar
        call reset_synapse_usage(synapse_usage, rows, cols)
        call reset_synapse_usage(meta_synapse_usage, meta_rows, meta_cols)
        
        ! Clear accumulated meta-output from previous bar
        do i = 1, meta_output_length
            call accumulated_meta_output(i)%set(low)
        end do
        
        ! Reset output accumulators for this Bar
        do i = 1, output_length
            call accumulated_output(i)%set(low)
        end do
        do i = 1, meta_output_length
            call accumulated_meta_output(i)%set(low)
        end do
        
        ! Write vision input to inputter (will be copied to brain on first run_brain_cycle call)
        ! Note: inputter already updated by update_vision_input above
        ! Note: meta_inputter already updated by rate encoding above
        
        ! ============================================================
        ! CROSS-FLOW ARCHITECTURE: Apply meta-brain output to right column
        ! Copy accumulated meta_outputter from previous bar to side_meta_inputter module variable
        ! Then apply with throttling based on pressure from PREVIOUS bar
        ! ============================================================
        call set_side_meta_input(accumulated_meta_output)
        ! Apply throttled meta-input to primary brain's right column
        call apply_throttled_meta_input(brain, incoming_direction, brain_pressure, &
                                        meta_input_offset, rows, cols)
        
        ! Run brain processing: steps_per_bar iterations
        do brain_step = 1, steps_per_bar
            ! Run ONE propagation step for primary brain (copies input, clears output, propagates, clears input)
            call run_brain_cycle(brain, inputter, outputter, synapses, synapse_usage, &
                                incoming_direction, rows, cols, input_offset, output_offset, &
                                output_length)
            
            ! Accumulate primary brain output from this step
            do i = 1, output_length
                if (outputter(i)%get() > accumulated_output(i)%get()) then
                    call accumulated_output(i)%set(outputter(i)%get())
                end if
            end do
            
            ! Run ONE propagation step for meta-brain (SIMPLE - no pressure/cross-flow effects)
            ! Meta-brain uses traditional pass-through architecture without pressure buildup
            ! Run TWICE per primary brain step to ensure energy reaches output (14 rows distance)
            call run_brain_cycle_simple(meta_brain, meta_inputter, meta_outputter, meta_synapses, &
                                        meta_synapse_usage, meta_incoming_direction, meta_rows, meta_cols, &
                                        1, 1, meta_output_length)
            call run_brain_cycle_simple(meta_brain, meta_inputter, meta_outputter, meta_synapses, &
                                        meta_synapse_usage, meta_incoming_direction, meta_rows, meta_cols, &
                                        1, 1, meta_output_length)
            
            ! Accumulate meta-brain output from this step
            do i = 1, meta_output_length
                if (meta_outputter(i)%get() > accumulated_meta_output(i)%get()) then
                    call accumulated_meta_output(i)%set(meta_outputter(i)%get())
                end if
            end do
        end do
        
        ! Copy accumulated outputs back to main output arrays for reading
        do i = 1, output_length
            call outputter(i)%set(accumulated_output(i)%get())
        end do
        do i = 1, meta_output_length
            call meta_outputter(i)%set(accumulated_meta_output(i)%get())
        end do
        
        ! ============================================================
        ! CROSS-FLOW ARCHITECTURE: Calculate pressure AFTER processing
        ! ============================================================
        brain_pressure = calculate_brain_pressure(brain, rows, cols)
        
        ! ============================================================
        ! META-BRAIN ENERGY DIAGNOSTIC (every 1000 bars)
        ! ============================================================
        if (mod(bar, 1000) == 0) then
            meta_input_energy = 0
            do i = 1, meta_input_length
                meta_input_energy = meta_input_energy + meta_inputter(i)%get()
            end do
            meta_brain_energy = 0
            do i = 1, meta_rows
                do j = 1, meta_cols
                    meta_brain_energy = meta_brain_energy + meta_brain(i, j)%get()
                end do
            end do
            meta_output_energy = 0
            do i = 1, meta_output_length
                meta_output_energy = meta_output_energy + meta_outputter(i)%get()
            end do
            print *, "BAR", bar, "META ENERGY: input=", meta_input_energy, " brain=", meta_brain_energy, &
                     " output=", meta_output_energy, " rate_counter=", rate_counter
        end if
        
        ! ============================================================
        ! CROSS-FLOW ARCHITECTURE: Capture overflow from left column
        ! ============================================================
        call copy_overflow_from_brain_left_column(brain, overflow_offset, rows)
        overflow_activity = get_overflow_activity()
        cumulative_overflow = cumulative_overflow + overflow_activity
        
        ! META-BRAIN STRATEGY REINFORCEMENT SYSTEM
        ! Meta-brain learns to trigger broad strategy reinforcement based on catch rate performance
        ! Output 1: Temporal scope (how many bars back to reinforce)
        ! Output 2: Reinforcement magnitude multiplier
        ! Output 3: Strategy selectivity (which types of pathways to boost)
        
        ! Calculate current strategy reinforcement parameters from meta-brain output
        brain_step = max(20, meta_outputter(1)%get() * 40)  ! 20-120 bars scope
        ni = max(1, meta_outputter(2)%get())               ! 1-2x magnitude multiplier
        nj = max(1, meta_outputter(3)%get())               ! Selectivity (not used yet, for future)
        
        ! Apply meta-brain controlled strategy reinforcement if catch rate is high
        if (rate_counter >= 3) then  ! Only when cat has caught multiple mice recently
            ! Reinforce primary brain strategies across the specified temporal scope
            do k = max(1, bar - brain_step), bar - 1  ! Look back 'brain_step' bars
                ! Apply broad reinforcement to all synapses that were active in this historical period
                ! This reinforces the general strategy that led to high catch rates
                if (k > 0 .and. k <= success_history_size) then
                    ! Calculate circular buffer index for this historical bar
                    i = history_write_index - (bar - k)
                    if (i <= 0) i = i + success_history_size
                    if (i > success_history_size) i = i - success_history_size
                    
                    ! Apply meta-controlled reinforcement to all synapses active in this bar
                    do output_energy = 1, rows
                        do move_distance = 1, cols
                            do output_action = 1, 8  ! incoming directions
                                do total_moves = 1, 8  ! outgoing directions
                                    if (synapse_history(i, output_energy, move_distance, output_action, total_moves)) then
                                        ! Apply reinforcement scaled by meta-brain magnitude control
                                        do brain_energy = 1, ni  ! Repeat based on meta-output magnitude
                                            synapses(output_energy, move_distance, output_action, total_moves) = &
                                                int(synapses(output_energy, move_distance, output_action, total_moves) * 1.1)
                                            ! Cap at max strength
                                            if (synapses(output_energy, move_distance, output_action, total_moves) > 2000000) &
                                                synapses(output_energy, move_distance, output_action, total_moves) = 2000000
                                        end do
                                    end if
                                end do
                            end do
                        end do
                    end do
                end if
            end do
            
            ! REWARD META-BRAIN for triggering successful strategy reinforcement
            ! Extra reward when catch rate is high AND strategy reinforcement triggers
            do k = 1, rate_counter  ! More catches = more meta-brain reinforcement
                call apply_adaptive_reinforcement(meta_synapses, meta_synapse_usage, meta_rows, meta_cols, steps_per_bar)
            end do
        end if
        
        ! ============================================================
        ! META-BRAIN INPUT-BASED REWARD (EVERY BAR)
        ! Reward meta-brain when catches are happening, punish when not
        ! SCALED reward (not proportional) to prevent saturation
        ! ============================================================
        if (rate_counter >= 3) then
            ! Good catch rate - reward meta-brain moderately
            ! Fixed reward (not proportional) to avoid saturation
            do k = 1, 2  ! 2 reinforcements regardless of exact rate_counter value
                call apply_adaptive_reinforcement(meta_synapses, meta_synapse_usage, meta_rows, meta_cols, steps_per_bar)
            end do
        else if (rate_counter >= 1) then
            ! Some catches - small reward
            call apply_adaptive_reinforcement(meta_synapses, meta_synapse_usage, meta_rows, meta_cols, steps_per_bar)
        else
            ! No catches recently - punish meta-brain pathways that are active
            ! This prevents meta-brain from settling into ineffective states
            call apply_adaptive_punishment(meta_synapses, meta_synapse_usage, meta_rows, meta_cols, steps_per_bar)
        end if
        
        ! Apply decay only every 5 Bars (much less aggressive) to preserve learned pathways
        if (mod(bar, 5) == 0) then
            call apply_decay(synapses, rows, cols)
            call apply_decay(meta_synapses, meta_rows, meta_cols)
        end if
        
        ! Store current Bar's synapse usage in circular history buffer
        synapse_history(history_write_index, :, :, :, :) = synapse_usage(:, :, :, :)
        history_write_index = history_write_index + 1
        if (history_write_index > success_history_size) history_write_index = 1
        
        ! Calculate brain energy
        brain_energy = 0
        do i = 1, rows
            do j = 1, cols
                brain_energy = brain_energy + brain(i, j)%get()
            end do
        end do
        
        ! Calculate output energy and find strongest output
        output_energy = 0
        output_action = 0
        move_distance = 0
        do i = 1, output_length
            output_energy = output_energy + outputter(i)%get()
            if (outputter(i)%get() > move_distance) then
                move_distance = outputter(i)%get()
                output_action = i
            end if
        end do
        
        ! Move cat based on output (if any)
        if (move_distance > 0 .and. output_action > 0) then
            ! Store old position for path intersection check
            old_cat_x = cat_pos%x
            old_cat_y = cat_pos%y
            
            ! Track movement history for oscillation detection
            movement_history(history_index) = output_action
            history_index = history_index + 1
            if (history_index > history_size) history_index = 1
            
            ! Calculate movement vector
            cat_move_x = move_directions(output_action, 2) * move_distance * 5.0
            cat_move_y = move_directions(output_action, 1) * move_distance * 5.0
            
            new_x = cat_pos%x + cat_move_x
            new_y = cat_pos%y + cat_move_y
            
            ! Clamp to field boundaries
            if (new_x < 0.0) new_x = 0.0
            if (new_x > field_size) new_x = field_size
            if (new_y < 0.0) new_y = 0.0
            if (new_y > field_size) new_y = field_size
            
            ! Check if movement path crosses mouse (line segment to point distance)
            path_dx = new_x - old_cat_x
            path_dy = new_y - old_cat_y
            path_length = sqrt(path_dx*path_dx + path_dy*path_dy)
            
            if (path_length > 0.0) then
                ! Project mouse onto cat's movement line segment
                t = ((mouse_pos%x - old_cat_x) * path_dx + (mouse_pos%y - old_cat_y) * path_dy) / &
                    (path_length * path_length)
                t = max(0.0, min(1.0, t))  ! Clamp to segment
                
                ! Find closest point on path to mouse
                closest_x = old_cat_x + t * path_dx
                closest_y = old_cat_y + t * path_dy
                closest_dist = sqrt((mouse_pos%x - closest_x)**2 + (mouse_pos%y - closest_y)**2)
                
                ! Check if path passed within success threshold
                if (closest_dist < 2.0) then
                    ! Cat caught the mouse!
                    catches_count = catches_count + 1
                    catches_this_epoch = catches_this_epoch + 1
                    
                    ! Increment rate counter for meta-brain
                    rate_counter = rate_counter + 1
                    
                    print *, "CATCH #", catches_count, "at Bar", bar, "(distance:", closest_dist, ")"
                    
                    ! SUCCESS-BASED PATHWAY BOOSTING: Now integrated with meta-brain strategy system
                    ! Primary reinforcement still happens immediately for recent pathways
                    do brain_step = max(1, success_history_size - 20), success_history_size  ! Last 20 bars get immediate boost
                        do i = 1, rows
                            do j = 1, cols
                                do ni = 1, 8  ! incoming directions
                                    do nj = 1, 8  ! outgoing directions
                                        if (synapse_history(brain_step, i, j, ni, nj)) then
                                            ! Apply immediate success boost to recent pathways
                                            synapses(i, j, ni, nj) = int(synapses(i, j, ni, nj) * 1.5)
                                            ! Cap at max strength
                                            if (synapses(i, j, ni, nj) > 2000000) synapses(i, j, ni, nj) = 2000000
                                        end if
                                    end do
                                end do
                            end do
                        end do
                    end do
                    
                    ! ADDITIONAL META-BRAIN LEARNING: Extra reward for high catch rate
                    if (rate_counter >= 5) then
                        ! Cat is on a hunting streak - massively reward meta-brain strategy control
                        do k = 1, 3  ! Triple reinforcement for sustained success
                            call apply_adaptive_reinforcement(meta_synapses, meta_synapse_usage, meta_rows, meta_cols, steps_per_bar)
                        end do
                    end if
                    
                    ! Respawn mouse at random location (distance 30-45 from cat)
                    call random_number(dx)
                    call random_number(dy)
                    dx = (dx - 0.5) * 2.0  ! Range: -1 to 1
                    dy = (dy - 0.5) * 2.0
                    ! Normalize and scale
                    previous_distance = sqrt(dx*dx + dy*dy)
                    if (previous_distance > 0.0) then
                        dx = dx / previous_distance
                        dy = dy / previous_distance
                    end if
                    call random_number(previous_distance)
                    previous_distance = 30.0 + previous_distance * 15.0  ! Distance 30-45
                    mouse_pos%x = cat_pos%x + dx * previous_distance
                    mouse_pos%y = cat_pos%y + dy * previous_distance
                    ! Clamp to field
                    if (mouse_pos%x < 0.0) mouse_pos%x = 0.0
                    if (mouse_pos%x > field_size) mouse_pos%x = field_size
                    if (mouse_pos%y < 0.0) mouse_pos%y = 0.0
                    if (mouse_pos%y > field_size) mouse_pos%y = field_size
                    
                    ! Reset adaptive mechanisms for new hunt
                    best_distance = sqrt((mouse_pos%x - cat_pos%x)**2 + (mouse_pos%y - cat_pos%y)**2)
                    bars_since_progress = 0
                    
                    ! Continue hunting - don't exit
                end if
            end if
            
            cat_pos%x = new_x
            cat_pos%y = new_y
            
            ! Calculate NEW distance immediately after movement
            dx = mouse_pos%x - cat_pos%x
            dy = mouse_pos%y - cat_pos%y
            if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
            if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
            current_distance = sqrt(dx*dx + dy*dy)
            
            ! Direction-based reward: did cat move TOWARDS mouse?
            ! Calculate unit vector from cat → mouse
            desired_dx = mouse_pos%x - old_cat_x
            desired_dy = mouse_pos%y - old_cat_y
            desired_length = sqrt(desired_dx*desired_dx + desired_dy*desired_dy)
            
            if (desired_length > 0.001) then
                ! Normalize to unit vector
                desired_dx = desired_dx / desired_length
                desired_dy = desired_dy / desired_length
                
                ! Dot product with movement vector gives component in desired direction
                ! This is the magnitude of movement towards the mouse
                dot_product = cat_move_x * desired_dx + cat_move_y * desired_dy
                
                ! Track movement directionality for analysis
                total_moves = total_moves + 1
                if (dot_product > 0.01) then
                    moves_towards = moves_towards + 1
                    moves_towards_this_epoch = moves_towards_this_epoch + 1
                else if (dot_product < -0.01) then
                    moves_away = moves_away + 1
                    moves_away_this_epoch = moves_away_this_epoch + 1
                else
                    moves_perpendicular = moves_perpendicular + 1
                    moves_perpendicular_this_epoch = moves_perpendicular_this_epoch + 1
                end if
                
                ! Mechanism 1: Check for repetitive movements (oscillation)
                repeated_count = 0
                do i = 1, history_size
                    if (movement_history(i) == output_action) repeated_count = repeated_count + 1
                end do
                
                ! Mechanism 2: Adaptive threshold - tighten when making progress, loosen when stuck
                ! Check progress periodically
                bars_since_progress = bars_since_progress + 1
                if (bars_since_progress >= progress_check_interval) then
                    if (current_distance < best_distance * 0.9) then
                        ! Made significant progress - tighten threshold (be more selective)
                        adaptive_threshold = min(threshold_max, adaptive_threshold + 0.03)
                        best_distance = current_distance
                        bars_since_progress = 0
                    else if (bars_since_progress >= progress_check_interval * 2) then
                        ! Stuck for too long - loosen threshold (explore more)
                        adaptive_threshold = max(threshold_min, adaptive_threshold - 0.02)
                        bars_since_progress = progress_check_interval  ! Don't reset to 0, keep pressure
                    end if
                end if
                
                ! Mechanism 3: Direction momentum - EXPONENTIAL bonus for continuing successful direction
                if (output_action == last_rewarded_direction .and. last_rewarded_direction > 0) then
                    momentum_streak = momentum_streak + 1
                else
                    momentum_streak = 0  ! Reset streak if direction changes
                end if
                
                ! Exponential momentum bonus: 2× for 1 step, 3× for 2 steps, 4× for 3+ steps
                momentum_bonus = min(momentum_streak + 1, 4)  ! Cap at 4× multiplier
                
                ! Calculate directional accuracy multiplier based on dot product magnitude
                ! dot_product ranges from -1 (opposite) to +1 (directly towards)
                ! Scale reward: barely towards (0.01) = 0.5× base, directly towards (1.0) = 2.0× base
                direction_multiplier = 1.0
                if (dot_product > adaptive_threshold) then
                    ! Map dot product [0.01, 1.0] to multiplier [0.5, 2.0]
                    direction_multiplier = 0.5 + 1.5 * dot_product
                end if
                
                ! Progressive punishment based on epoch (stronger in later epochs)
                epoch_progress = real(current_epoch) / real(num_epochs)  ! 0.0 to 1.0
                
                ! Apply selective reinforcement with strict threshold
                if (.not. disable_direct_rewards) then
                    if (dot_product > adaptive_threshold) then
                    ! Moved DIRECTLY towards mouse - ALWAYS REWARD (removed repetition check)
                    ! Apply graduated reward based on directional accuracy
                    do k = 1, nint(direction_multiplier * 2.0)  ! 1-4 reinforcements based on accuracy
                        call apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, steps_per_bar)
                    end do
                    last_rewarded_direction = output_action  ! Track for momentum
                    
                    ! Exponential momentum bonus: 1-3 extra reinforcements based on streak
                    do k = 1, momentum_bonus - 1
                        call apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, steps_per_bar)
                    end do
                else if (dot_product < -0.01) then
                    ! Moved away from mouse - PROGRESSIVE PUNISH (stronger in later epochs)
                    do k = 1, nint(1.0 + epoch_progress * 2.0)  ! 1-3 punishments based on epoch
                        call apply_adaptive_punishment(synapses, synapse_usage, rows, cols, steps_per_bar)
                    end do
                    last_rewarded_direction = 0  ! Break momentum
                    momentum_streak = 0  ! Reset streak
                    else
                        ! Between -0.01 and adaptive_threshold: either perpendicular or weak towards
                        ! PUNISH after epoch 1 - we want precision, not vague wandering
                        if (current_epoch > 1) then
                            call apply_adaptive_punishment(synapses, synapse_usage, rows, cols, steps_per_bar)
                            last_rewarded_direction = 0  ! Break momentum
                            momentum_streak = 0  ! Reset streak
                        end if
                    end if
                end if  ! .not. disable_direct_rewards
            end if
        end if
        
        ! If cat didn't move, still update distance for progress tracking
        if (move_distance == 0) then
            dx = mouse_pos%x - cat_pos%x
            dy = mouse_pos%y - cat_pos%y
            if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
            if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
            current_distance = sqrt(dx*dx + dy*dy)
        end if
        
        ! Log to CSV
        write(csv_unit, '(I0,10(A,I0),A,F0.4,A,I0)') bar, ',', nint(mouse_pos%x), ',', nint(mouse_pos%y), &
                                        ',', nint(cat_pos%x), ',', nint(cat_pos%y), &
                                        ',', output_action, ',', brain_energy, &
                                        ',', output_energy, ',', output_action, &
                                        ',', move_distance, ',', catches_count, &
                                        ',', brain_pressure, ',', overflow_activity
        
        ! Periodic snapshots
        if (mod(bar, snapshot_interval) == 0) then
            print *, "========================================"
            print *, "SNAPSHOT AT BAR", bar
            print *, "========================================"
            
            call print_field(cat_pos, mouse_pos, field_size, 20)
            
            print *, "Mouse:", nint(mouse_pos%x), ",", nint(mouse_pos%y)
            print *, "Cat:  ", nint(cat_pos%x), ",", nint(cat_pos%y)
            
            print *, "Vision input:"
            write(*, '(A)', advance='no') "  "
            do i = 1, input_length
                write(*, '(I3)', advance='no') inputter(i)%get()
            end do
            print *
            
            print *, "Brain state:"
            do i = 1, rows
                write(*, '(A)', advance='no') "  "
                do j = 1, cols
                    write(*, '(I3)', advance='no') brain(i, j)%get()
                end do
                print *
            end do
            
            print *, "Output (movement directions):"
            write(*, '(A)', advance='no') "  "
            do i = 1, output_length
                write(*, '(I3)', advance='no') outputter(i)%get()
            end do
            print *
            
            print *, "Brain energy:", brain_energy
            print *, "Output energy:", output_energy
            if (move_distance > 0) then
                print *, "Cat moved:", move_distance, "steps in direction", output_action
            end if
            
            ! Meta-brain state
            print *, "--- META-BRAIN ---"
            print *, "Rate counter:", rate_counter
            print *, "Meta input:"
            write(*, '(A)', advance='no') "  "
            do i = 1, meta_input_length
                write(*, '(I3)', advance='no') meta_inputter(i)%get()
            end do
            print *
            print *, "Meta brain state:"
            do i = 1, meta_rows
                write(*, '(A)', advance='no') "  "
                do j = 1, meta_cols
                    write(*, '(I3)', advance='no') meta_brain(i, j)%get()
                end do
                print *
            end do
            print *, "Meta output:"
            write(*, '(A)', advance='no') "  "
            do i = 1, meta_output_length
                write(*, '(I3)', advance='no') meta_outputter(i)%get()
            end do
            print *
            print *, "Meta strategy control:"
            print *, "  Temporal scope:", max(20, meta_outputter(1)%get() * 40), "bars"
            print *, "  Magnitude multiplier:", max(1, meta_outputter(2)%get()), "x"
            print *, "  Selectivity:", max(1, meta_outputter(3)%get())
            if (rate_counter >= 3) then
                print *, "  STATUS: Meta-brain applying strategy reinforcement"
            else
                print *, "  STATUS: Meta-brain learning (catch rate too low for reinforcement)"
            end if
        end if
        
        ! Progress indicator every 1000 Bars
        if (mod(bar, 1000) == 0) then
            print *, "Progress:", bar, "/", max_bars
        end if
    end do
    
    close(csv_unit)
    
    ! DEBUG: Print final pressure before saving brain state
    print *, "=== FINAL PRESSURE DEBUG ==="
    print *, "Activity:", calculate_brain_activity(brain, rows, cols)
    print *, "Max activity:", rows * cols * 2
    print *, "Pressure:", calculate_brain_pressure(brain, rows, cols)
    print *, "=========================="
    
    ! Save final brain state for visualization
    full_path = trim(output_dir) // 'brain_state.csv'
    open(newunit=csv_unit, file=full_path, status='replace', action='write')
    write(csv_unit, '(A)') 'row,col,state'
    do i = 1, rows
        do j = 1, cols
            write(csv_unit, '(I0,A,I0,A,I0)') i, ',', j, ',', brain(i, j)%get()
        end do
    end do
    close(csv_unit)
    
    ! Save final inputter state for visualization
    full_path = trim(output_dir) // 'inputter_state.csv'
    open(newunit=csv_unit, file=full_path, status='replace', action='write')
    write(csv_unit, '(A)') 'col,state'
    do j = 1, input_length
        write(csv_unit, '(I0,A,I0)') input_offset + j - 1, ',', inputter(j)%get()
    end do
    close(csv_unit)
    
    ! Save final outputter state for visualization
    full_path = trim(output_dir) // 'outputter_state.csv'
    open(newunit=csv_unit, file=full_path, status='replace', action='write')
    write(csv_unit, '(A)') 'col,state'
    do j = 1, output_length
        write(csv_unit, '(I0,A,I0)') output_offset + j - 1, ',', outputter(j)%get()
    end do
    close(csv_unit)
    
    ! Save final synapse strengths for visualization
    ! Note: Now 4D (incoming_dir, outgoing_dir), so we'll aggregate for visualization
    ! Save maximum strength across all incoming directions for each connection
    full_path = trim(output_dir) // 'synapse_state.csv'
    open(newunit=csv_unit, file=full_path, status='replace', action='write')
    write(csv_unit, '(A)') 'from_row,from_col,to_row,to_col,strength,dominant_incoming_dir'
    do i = 1, rows
        do j = 1, cols
            do k = 1, 8
                ! Calculate target position for this direction
                ni = i + move_directions(k, 1)
                nj = j + move_directions(k, 2)
                ! Only record valid synapses (within brain bounds or to output)
                if ((ni >= 1 .and. ni <= rows .and. nj >= 1 .and. nj <= cols) .or. &
                    (i == rows .and. ni == rows + 1)) then
                    ! Find maximum strength across all incoming directions for this outgoing direction
                    output_energy = 0  ! Reuse variable for max strength
                    move_distance = 0  ! Reuse variable for dominant incoming direction
                    do brain_step = 1, 8  ! Loop through incoming directions
                        if (synapses(i, j, brain_step, k) > output_energy) then
                            output_energy = synapses(i, j, brain_step, k)
                            move_distance = brain_step  ! Track which incoming direction is dominant
                        end if
                    end do
                    write(csv_unit, '(I0,A,I0,A,I0,A,I0,A,I0,A,I0)') &
                        i, ',', j, ',', ni, ',', nj, ',', output_energy, ',', move_distance
                end if
            end do
        end do
    end do
    close(csv_unit)
    
    ! Save final epoch if not already saved
    if (catches_this_epoch > 0 .or. moves_towards_this_epoch > 0) then
        epoch_catches(current_epoch) = catches_this_epoch
        epoch_moves_towards(current_epoch) = moves_towards_this_epoch
        epoch_moves_away(current_epoch) = moves_away_this_epoch
        epoch_moves_perpendicular(current_epoch) = moves_perpendicular_this_epoch
    end if
    
    print *, "=== SIMULATION COMPLETE ==="
    print *, "Total mice caught:", catches_count
    print *, "Catch rate:", real(catches_count) / real(max_bars), "per Bar"
    print *
    print *, "=== TEMPORAL LEARNING ANALYSIS (EPOCH BREAKDOWN) ==="
    print *, "Epoch size:", epoch_size, "Bars"
    do i = 1, num_epochs
        ! Calculate directionality percentages for this epoch
        epoch_total_moves = epoch_moves_towards(i) + epoch_moves_away(i) + epoch_moves_perpendicular(i)
        if (epoch_total_moves > 0) then
            print *, "  Epoch", i, ":", epoch_catches(i), "catches (rate:", &
                     real(epoch_catches(i))/real(epoch_size), "per Bar)", &
                     "Directionality:", &
                     real(epoch_moves_towards(i))/real(epoch_total_moves)*100.0, "% towards,", &
                     real(epoch_moves_away(i))/real(epoch_total_moves)*100.0, "% away,", &
                     real(epoch_moves_perpendicular(i))/real(epoch_total_moves)*100.0, "% perpendicular"
        else
            print *, "  Epoch", i, ":", epoch_catches(i), "catches (rate:", &
                     real(epoch_catches(i))/real(epoch_size), "per Bar)"
        end if
    end do
    ! Calculate learning trend (early vs late)
    if (num_epochs >= 2) then
        brain_energy = 0  ! Reuse for early sum
        output_energy = 0  ! Reuse for late sum
        do i = 1, num_epochs/3
            brain_energy = brain_energy + epoch_catches(i)
        end do
        do i = (2*num_epochs/3)+1, num_epochs
            output_energy = output_energy + epoch_catches(i)
        end do
        print *, "Early third avg:", real(brain_energy) / real(num_epochs/3), "catches/epoch"
        print *, "Late third avg:", real(output_energy) / real(num_epochs - 2*num_epochs/3), "catches/epoch"
        if (output_energy > brain_energy * 1.2) then
            print *, "✓ TEMPORAL IMPROVEMENT - Cat learning over time"
        else if (output_energy > brain_energy) then
            print *, "~ SLIGHT IMPROVEMENT - Modest learning over time"
        else
            print *, "✗ NO TEMPORAL IMPROVEMENT - Performance stable or declining"
        end if
    end if
    print *
    print *, "=== MOVEMENT DIRECTIONALITY ANALYSIS ==="
    print *, "Total moves:", total_moves
    print *, "Moves TOWARDS mouse:", moves_towards, "(", 100.0*real(moves_towards)/real(total_moves), "%)"
    print *, "Moves AWAY from mouse:", moves_away, "(", 100.0*real(moves_away)/real(total_moves), "%)"
    print *, "Moves PERPENDICULAR:", moves_perpendicular, "(", 100.0*real(moves_perpendicular)/real(total_moves), "%)"
    if (moves_towards > moves_away * 1.5) then
        print *, "✓ DIRECTIONAL LEARNING DETECTED - Cat preferentially moves towards mouse"
    else if (moves_towards > moves_away) then
        print *, "~ WEAK DIRECTIONAL BIAS - Slight preference for moving towards mouse"
    else
        print *, "✗ NO DIRECTIONAL LEARNING - Movement appears random"
    end if
    print *
    print *, "Results saved to:", trim(csv_filename)
    print *, "Brain state saved to:", trim(output_dir) // "brain_state.csv"
    print *, "Synapse state saved to:", trim(output_dir) // "synapse_state.csv"
    
    ! Save binary weight file for potential loading (use output_dir if specified)
    write(weight_file, '(A,A,I0,A)') trim(output_dir), 'weights_seed', user_seed, '.bin'
    open(newunit=weight_unit, file=weight_file, status='replace', action='write', form='unformatted')
    write(weight_unit) synapses
    write(weight_unit) meta_synapses
    close(weight_unit)
    print *, "Binary weights saved to:", trim(weight_file)
    
end program cat_mouse_learning

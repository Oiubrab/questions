program binocular_cat_mouse
    use trinary_module
    use brain_engine_module
    use synapses_module
    use vision_simulation_module
    implicit none
    
    ! Primary Binocular Brain system
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: left_inputter(:), right_inputter(:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:,:)
    logical, allocatable :: synapse_usage(:,:,:,:)
    integer, allocatable :: incoming_direction(:,:,:)
    integer :: rows, cols, slices_per_eye, output_length
    integer :: left_offset, right_offset, output_offset
    
    ! Cat body with heading
    type(cat_body) :: cat
    real :: eye_separation, catch_radius, close_threshold
    
    ! Single mouse
    type(position) :: mice(1)
    integer, parameter :: num_mice = 1
    
    ! Vision system
    real :: field_size, mouse_step_size
    
    ! Movement
    integer, dimension(8, 2) :: move_directions
    real :: cat_step_size
    
    ! Simulation
    integer :: bar, max_bars, snapshot_interval, mouse_move_interval
    integer :: brain_step, steps_per_bar
    integer :: i, j, brain_energy, output_energy
    integer :: output_action, move_distance
    real :: dx, dy, move_length
    type(position) :: cat_pos_temp
    
    ! Catch tracking
    integer :: total_catches
    logical :: caught, mouse_was_caught
    
    ! Reinforcement tracking
    real :: closest_mouse_dist
    real :: desired_dx, desired_dy, desired_length
    real :: cat_move_x, cat_move_y, dot_product
    
    ! Directional tracking
    integer :: moves_towards, moves_away, moves_perpendicular, total_moves
    
    ! Epoch tracking
    integer, parameter :: epoch_size = 2000
    integer :: current_epoch, num_epochs
    integer, allocatable :: epoch_catches(:)
    integer :: catches_this_epoch
    
    ! Logging
    integer :: csv_unit
    character(len=100) :: csv_filename
    
    ! Output accumulation
    type(trinary), allocatable :: accumulated_output(:)
    
    ! Random seed
    integer :: seed_size, user_seed
    integer, allocatable :: seed(:)
    character(len=32) :: arg
    
    ! Get seed from command line
    if (command_argument_count() > 0) then
        call get_command_argument(1, arg)
        read(arg, *) user_seed
    else
        call system_clock(count=user_seed)
    end if
    
    ! ==================
    ! BINOCULAR BRAIN PARAMETERS
    ! ==================
    rows = 10               ! Expanded from 6
    cols = 18               ! Expanded from 12 (9 per eye)
    slices_per_eye = 9      ! 9 vision slices per eye (20° each = 180° FOV)
    left_offset = 1         ! Left eye inputs to columns 1-9
    right_offset = 10       ! Right eye inputs to columns 10-18
    output_offset = 6       ! Output centered (columns 6-13)
    output_length = 8       ! 8 movement directions
    
    ! Cat body parameters
    eye_separation = 5.0    ! Distance between eyes
    catch_radius = 8.0      ! Catch zone radius
    close_threshold = 20.0  ! Distance for HIGH intensity (close mouse)
    
    ! Simulation parameters
    max_bars = 20000
    steps_per_bar = 12      ! Brain steps per Bar (signals now persist across bars)
    snapshot_interval = 2000
    mouse_move_interval = 10
    
    ! Field parameters
    field_size = 100.0
    mouse_step_size = 2.0
    cat_step_size = 5.0
    
    ! Define movement directions (8-direction)
    move_directions(1,1) = -1; move_directions(1,2) = -1  ! Up-Left
    move_directions(2,1) = -1; move_directions(2,2) =  0  ! Up
    move_directions(3,1) = -1; move_directions(3,2) =  1  ! Up-Right
    move_directions(4,1) =  0; move_directions(4,2) = -1  ! Left
    move_directions(5,1) =  0; move_directions(5,2) =  1  ! Right
    move_directions(6,1) =  1; move_directions(6,2) = -1  ! Down-Left
    move_directions(7,1) =  1; move_directions(7,2) =  0  ! Down
    move_directions(8,1) =  1; move_directions(8,2) =  1  ! Down-Right
    
    ! Initialize random seed
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = user_seed + 37 * (/ (i, i=1,seed_size) /)
    call random_seed(put=seed)
    deallocate(seed)
    
    ! Initialize binocular brain system
    call initialize_binocular_brain_system(brain, left_inputter, right_inputter, outputter, &
                                            synapses, synapse_usage, incoming_direction, &
                                            rows, cols, slices_per_eye, output_length, &
                                            left_offset, right_offset, output_offset)
    
    ! Initialize output accumulator
    allocate(accumulated_output(output_length))
    do i = 1, output_length
        call accumulated_output(i)%set(low)
    end do
    
    ! Initialize cat at center of field with random heading
    call initialize_cat(cat, field_size / 2.0, field_size / 2.0, eye_separation, catch_radius)
    
    ! Initialize mice at random positions far from cat
    cat_pos_temp%x = cat%x
    cat_pos_temp%y = cat%y
    call initialize_mouse_far_from(mice(1), cat_pos_temp, field_size, 30.0)
    ! Only one mouse in this version - removed mice(2) initialization
    
    ! Initialize tracking
    total_catches = 0
    mouse_was_caught = .false.
    moves_towards = 0
    moves_away = 0
    moves_perpendicular = 0
    total_moves = 0
    
    ! Initialize epoch tracking
    num_epochs = max_bars / epoch_size
    allocate(epoch_catches(num_epochs))
    epoch_catches = 0
    current_epoch = 1
    catches_this_epoch = 0
    
    ! Calculate initial distance to mouse
    cat_pos_temp%x = cat%x
    cat_pos_temp%y = cat%y
    closest_mouse_dist = calc_distance(cat_pos_temp, mice(1), field_size)
    
    ! Open CSV file for logging
    csv_filename = 'binocular_simulation_log.csv'
    open(newunit=csv_unit, file=csv_filename, status='replace', action='write')
    write(csv_unit, '(A)') 'bar,cat_x,cat_y,cat_heading,mouse_x,mouse_y,' // &
                           'brain_energy,output_action,catches'
    
    print *, "=== BINOCULAR CAT & MOUSE SIMULATION ==="
    print *, "Brain: ", rows, "x", cols, " (", slices_per_eye, " slices per eye)"
    print *, "Cat body: eye_sep=", eye_separation, ", catch_radius=", catch_radius
    print *, "Mouse: 1 (testing binocular vision)"
    print *, "Max Bars:", max_bars
    print *, "Steps per Bar:", steps_per_bar
    print *
    
    ! Main simulation loop
    do bar = 1, max_bars
        ! Check for epoch boundary
        if (bar > 1 .and. mod(bar - 1, epoch_size) == 0) then
            epoch_catches(current_epoch) = catches_this_epoch
            current_epoch = current_epoch + 1
            catches_this_epoch = 0
        end if
        
        ! Move mouse occasionally
        if (mod(bar, mouse_move_interval) == 0) then
            call move_mouse(mice(1), field_size, mouse_step_size)
        end if
        
        ! Update binocular vision
        call update_binocular_vision(left_inputter, right_inputter, cat, mice, num_mice, &
                                      slices_per_eye, field_size, close_threshold)
        
        ! Reset synapse usage for this Bar
        synapse_usage = .false.
        
        ! DO NOT reset brain state - signals must persist across bars to propagate through 10 rows!
        ! Only reset the output accumulator for this bar
        
        ! Reset output accumulator
        do i = 1, output_length
            call accumulated_output(i)%set(low)
        end do
        
        ! Run brain steps
        do brain_step = 1, steps_per_bar
            call run_binocular_brain_cycle(brain, left_inputter, right_inputter, outputter, &
                                            synapses, synapse_usage, incoming_direction, &
                                            rows, cols, left_offset, right_offset, &
                                            output_offset, output_length)
            
            ! Accumulate output
            do i = 1, output_length
                if (outputter(i)%get() > accumulated_output(i)%get()) then
                    call accumulated_output(i)%set(outputter(i)%get())
                end if
            end do
        end do
        
        ! Calculate brain and output energy
        brain_energy = 0
        do i = 1, rows
            do j = 1, cols
                brain_energy = brain_energy + brain(i,j)%get()
            end do
        end do
        
        output_energy = 0
        output_action = 0
        do i = 1, output_length
            output_energy = output_energy + accumulated_output(i)%get()
            ! Pick the strongest output direction (HIGH > MEDIUM > LOW)
            if (output_action == 0) then
                if (accumulated_output(i)%get() > 0) output_action = i
            else
                if (accumulated_output(i)%get() > accumulated_output(output_action)%get()) then
                    output_action = i
                end if
            end if
        end do
        
        ! Move cat if there's output
        if (output_action > 0) then
            ! Calculate movement
            dx = real(move_directions(output_action, 2)) * cat_step_size
            dy = -real(move_directions(output_action, 1)) * cat_step_size  ! Flip Y
            
            ! Move cat (updates position and heading)
            call move_cat(cat, dx, dy, field_size)
            
            ! Calculate if movement was towards mouse
            cat_pos_temp%x = cat%x
            cat_pos_temp%y = cat%y
            desired_dx = mice(1)%x - cat_pos_temp%x
            desired_dy = mice(1)%y - cat_pos_temp%y
            
            ! Handle toroidal wrapping
            if (abs(desired_dx) > field_size / 2.0) desired_dx = desired_dx - sign(field_size, desired_dx)
            if (abs(desired_dy) > field_size / 2.0) desired_dy = desired_dy - sign(field_size, desired_dy)
            
            desired_length = sqrt(desired_dx*desired_dx + desired_dy*desired_dy)
            move_length = sqrt(dx*dx + dy*dy)
            
            if (desired_length > 0.001 .and. move_length > 0.001) then
                cat_move_x = dx / move_length
                cat_move_y = dy / move_length
                desired_dx = desired_dx / desired_length
                desired_dy = desired_dy / desired_length
                dot_product = cat_move_x * desired_dx + cat_move_y * desired_dy
                
                ! Track directionality
                total_moves = total_moves + 1
                if (dot_product > 0.3) then
                    moves_towards = moves_towards + 1
                    call apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, steps_per_bar)
                else if (dot_product < -0.3) then
                    moves_away = moves_away + 1
                    call apply_adaptive_punishment(synapses, synapse_usage, rows, cols, steps_per_bar)
                else
                    moves_perpendicular = moves_perpendicular + 1
                end if
            end if
        end if
        
        ! Apply decay once per Bar
        call apply_decay(synapses, rows, cols)
        
        ! Decay inactive neurons to prevent brain saturation
        ! Neurons that didn't propagate signals this bar should lose activation
        do i = 1, rows
            do j = 1, cols
                ! If neuron has activation but no incoming direction, it's stale - decay it
                if (brain(i,j)%get() > low .and. incoming_direction(i,j,1) == 0) then
                    call brain(i,j)%shift(down)
                end if
            end do
        end do
        
        ! Log to CSV BEFORE respawn (so we record actual catch position)
        write(csv_unit, '(I6,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,I4,A,I2,A,I5)') &
            bar, ',', cat%x, ',', cat%y, ',', cat%heading, ',', &
            mice(1)%x, ',', mice(1)%y, ',', &
            brain_energy, ',', output_action, ',', total_catches
        
        ! Check for catch - only count when ENTERING catch zone, not while staying in it
        caught = check_catch(cat, mice(1), field_size)
        if (caught .and. .not. mouse_was_caught) then
            ! First time entering catch zone
            total_catches = total_catches + 1
            catches_this_epoch = catches_this_epoch + 1
            mouse_was_caught = .true.
            
            ! Respawn caught mouse far from cat
            cat_pos_temp%x = cat%x
            cat_pos_temp%y = cat%y
            call initialize_mouse_far_from(mice(1), cat_pos_temp, field_size, 30.0)
        else if (.not. caught) then
            ! Cat left catch zone, reset flag
            mouse_was_caught = .false.
        end if
        
        ! Periodic snapshot
        if (mod(bar, snapshot_interval) == 0) then
            print *, "Bar", bar, ": Catches=", total_catches
            call print_field_binocular(cat, mice, num_mice, field_size, 20)
        end if
    end do
    
    ! Save final epoch
    if (current_epoch <= num_epochs) then
        epoch_catches(current_epoch) = catches_this_epoch
    end if
    
    close(csv_unit)
    
    ! Print final statistics
    print *
    print *, "=== FINAL STATISTICS ==="
    print *, "Total catches:", total_catches
    print *
    print *, "Movement directionality:"
    print *, "  Towards closest:", moves_towards, "(", 100.0 * moves_towards / max(1, total_moves), "%)"
    print *, "  Away from closest:", moves_away, "(", 100.0 * moves_away / max(1, total_moves), "%)"
    print *, "  Perpendicular:", moves_perpendicular
    print *
    print *, "Epoch breakdown:"
    do i = 1, num_epochs
        print *, "  Epoch", i, ":", epoch_catches(i), "catches"
    end do
    print *
    print *, "Results saved to:", trim(csv_filename)
    
contains

    ! Find the closest mouse to the cat
    subroutine find_closest_mouse(cat, mice, num_mice, field_size, closest_id, closest_dist)
        type(cat_body), intent(in) :: cat
        type(position), intent(in) :: mice(:)
        integer, intent(in) :: num_mice
        real, intent(in) :: field_size
        integer, intent(out) :: closest_id
        real, intent(out) :: closest_dist
        
        type(position) :: cat_pos
        real :: dist
        integer :: m
        
        cat_pos%x = cat%x
        cat_pos%y = cat%y
        
        closest_id = 1
        closest_dist = calc_distance(cat_pos, mice(1), field_size)
        
        do m = 2, num_mice
            dist = calc_distance(cat_pos, mice(m), field_size)
            if (dist < closest_dist) then
                closest_dist = dist
                closest_id = m
            end if
        end do
    end subroutine find_closest_mouse
    
end program binocular_cat_mouse

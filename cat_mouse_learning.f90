program cat_mouse_learning
    use trinary_module
    use brain_module, only: initialize_brain, update_brain_state_based_on_synapses, copy_non_low_to_brain_top_row
    use inputter_module, only: initialize_inputter
    use outputter_module
    use synapses_module
    use vision_simulation_module
    implicit none
    
    ! Brain system
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:)
    logical, allocatable :: synapse_usage(:,:,:)  ! Track which synapses fire during Bar
    integer :: rows, cols, input_length, output_length
    integer :: input_offset, output_offset
    
    ! Vision system
    type(position) :: cat_pos, mouse_pos
    real :: field_size, step_size
    integer :: num_vision_slices
    
    ! Movement
    integer, dimension(8, 2) :: move_directions
    
    ! Simulation
    integer :: bar, max_bars, snapshot_interval
    integer :: brain_step, steps_per_bar
    integer :: i, j, brain_energy, output_energy
    integer :: output_action, move_distance
    real :: new_x, new_y
    
    ! Reinforcement tracking
    real :: current_distance, previous_distance
    real :: dx, dy
    
    ! Logging
    integer :: csv_unit
    character(len=100) :: csv_filename
    
    ! Parameters
    rows = 6
    cols = 12
    input_offset = 1
    input_length = 6       ! 6 vision slices
    output_offset = 1
    output_length = 8      ! 8 movement directions
    max_bars = 20000       ! Real-world time steps (balanced for learning)
    steps_per_bar = 12     ! Brain steps per Bar (reduced for better temporal precision)
    snapshot_interval = 1000  ! Print full state every N Bars
    
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
    
    ! Initialize brain system
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, input_length)
    call initialize_outputter(outputter, output_length)
    call initialize_synapses(synapses, rows, cols)
    call reset_synapse_usage(synapse_usage, rows, cols)
    
    ! Initialize cat at center
    cat_pos%x = field_size / 2.0
    cat_pos%y = field_size / 2.0
    
    ! Initialize mouse at random position
    call initialize_mouse(mouse_pos, field_size)
    
    ! Calculate initial distance
    dx = mouse_pos%x - cat_pos%x
    dy = mouse_pos%y - cat_pos%y
    if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
    if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
    previous_distance = sqrt(dx*dx + dy*dy)
    
    ! Open CSV file for logging
    csv_filename = 'simulation_log.csv'
    open(newunit=csv_unit, file=csv_filename, status='replace', action='write')
    write(csv_unit, '(A)') 'bar,mouse_x,mouse_y,cat_x,cat_y,vision_slice,brain_energy,output_energy,output_action,move_dist'
    
    print *, "=== CAT & MOUSE LEARNING SIMULATION ==="
    print *, "Max Bars (real-world steps):", max_bars
    print *, "Brain steps per Bar:", steps_per_bar
    print *, "Reinforcement: distance-based (global)"
    print *, "Logging to:", trim(csv_filename)
    print *, "Snapshot interval:", snapshot_interval
    print *
    
    ! Simulation loop - each Bar is one real-world time step
    do bar = 1, max_bars
        ! Record distance at start of Bar (before any actions)
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        previous_distance = sqrt(dx*dx + dy*dy)
        
        ! Reset synapse usage tracker for this Bar
        call reset_synapse_usage(synapse_usage, rows, cols)
        
        ! Move mouse (real-world action)
        call move_mouse(mouse_pos, field_size, step_size)
        
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
        
        ! Apply input to brain ONCE at start of Bar
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Reset outputter for this Bar
        call save_and_reset_outputter(outputter)
        
        ! Run multiple brain steps within this Bar
        do brain_step = 1, steps_per_bar
            ! Update brain state (tracks which synapses are used)
            call update_brain_state_based_on_synapses(brain, synapses, outputter, synapse_usage, &
                                                       rows, cols, input_offset, &
                                                       output_offset, output_length)
            
            ! Apply decay every 2 brain steps - optimal balance between persistence and learning
            if (mod(brain_step, 2) == 0) then
                call apply_decay(synapses, rows, cols)
            end if
        end do
        
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
            new_x = cat_pos%x + move_directions(output_action, 2) * move_distance * 5.0
            new_y = cat_pos%y + move_directions(output_action, 1) * move_distance * 5.0
            
            ! Clamp to field boundaries
            if (new_x < 0.0) new_x = 0.0
            if (new_x > field_size) new_x = field_size
            if (new_y < 0.0) new_y = 0.0
            if (new_y > field_size) new_y = field_size
            
            cat_pos%x = new_x
            cat_pos%y = new_y
        end if
        
        ! Now measure distance AFTER cat moved to determine reward/punishment
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        current_distance = sqrt(dx*dx + dy*dy)
        
        ! Apply selective reinforcement ONLY if cat actually moved AND distance change is significant
        if (move_distance > 0) then
            if (current_distance < previous_distance - 1.0) then
                ! Distance decreased - reward proportional to steps_per_bar
                call apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, steps_per_bar)
            else if (current_distance > previous_distance + 1.0) then
                ! Distance increased - punish proportional to steps_per_bar
                call apply_adaptive_punishment(synapses, synapse_usage, rows, cols, steps_per_bar)
            end if
        end if
        ! No reinforcement if cat didn't move or distance change < 1.0 units
        
        ! Log to CSV
        write(csv_unit, '(I0,9(A,I0))') bar, ',', nint(mouse_pos%x), ',', nint(mouse_pos%y), &
                                        ',', nint(cat_pos%x), ',', nint(cat_pos%y), &
                                        ',', output_action, ',', brain_energy, &
                                        ',', output_energy, ',', output_action, &
                                        ',', move_distance
        
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
            print *
        end if
        
        ! Progress indicator every 1000 Bars
        if (mod(bar, 1000) == 0) then
            print *, "Progress:", bar, "/", max_bars
        end if
    end do
    
    close(csv_unit)
    
    print *, "=== SIMULATION COMPLETE ==="
    print *, "Results saved to:", trim(csv_filename)
    
end program cat_mouse_learning

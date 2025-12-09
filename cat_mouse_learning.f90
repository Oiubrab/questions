program cat_mouse_learning
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    use vision_simulation_module
    implicit none
    
    ! Brain system
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: rows, cols, input_length, output_length
    integer :: input_offset, output_offset
    
    ! Vision system
    type(position) :: cat_pos, mouse_pos
    real :: field_size, step_size
    integer :: num_vision_slices
    
    ! Movement
    integer, dimension(8, 2) :: move_directions
    
    ! Simulation
    integer :: step, max_steps, snapshot_interval
    integer :: i, j, brain_energy, output_energy
    integer :: output_action, move_distance
    real :: new_x, new_y
    
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
    max_steps = 10000
    snapshot_interval = 500  ! Print full state every N steps
    
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
    
    ! Initialize cat at center
    cat_pos%x = field_size / 2.0
    cat_pos%y = field_size / 2.0
    
    ! Initialize mouse at random position
    call initialize_mouse(mouse_pos, field_size)
    
    ! Open CSV file for logging
    csv_filename = 'simulation_log.csv'
    open(newunit=csv_unit, file=csv_filename, status='replace', action='write')
    write(csv_unit, '(A)') 'step,mouse_x,mouse_y,cat_x,cat_y,vision_slice,brain_energy,output_energy,output_action,move_dist'
    
    print *, "=== CAT & MOUSE LEARNING SIMULATION ==="
    print *, "Max steps:", max_steps
    print *, "Logging to:", trim(csv_filename)
    print *, "Snapshot interval:", snapshot_interval
    print *
    
    ! Simulation loop
    do step = 1, max_steps
        ! Move mouse
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
        
        ! Apply input to brain
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Update brain
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        ! Apply decay to synapses
        call apply_decay(synapses, rows, cols)
        
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
        
        ! Log to CSV
        write(csv_unit, '(I0,9(A,I0))') step, ',', nint(mouse_pos%x), ',', nint(mouse_pos%y), &
                                        ',', nint(cat_pos%x), ',', nint(cat_pos%y), &
                                        ',', output_action, ',', brain_energy, &
                                        ',', output_energy, ',', output_action, &
                                        ',', move_distance
        
        ! Periodic snapshots
        if (mod(step, snapshot_interval) == 0) then
            print *, "========================================"
            print *, "SNAPSHOT AT STEP", step
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
        
        ! Progress indicator every 1000 steps
        if (mod(step, 1000) == 0) then
            print *, "Progress:", step, "/", max_steps
        end if
    end do
    
    close(csv_unit)
    
    print *, "=== SIMULATION COMPLETE ==="
    print *, "Results saved to:", trim(csv_filename)
    
end program cat_mouse_learning

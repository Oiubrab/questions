program cat_mouse_gui_demo
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
    integer :: bar, max_bars
    integer :: brain_step, steps_per_bar
    integer :: i, j, brain_energy, output_energy
    integer :: active_slice
    integer :: output_action, move_distance
    real :: new_x, new_y
    
    ! Reinforcement tracking
    real :: current_distance, previous_distance
    real :: dx, dy
    
    ! Parameters
    rows = 6
    cols = 12
    input_offset = 1
    input_length = 6       ! 6 vision slices
    output_offset = 1
    output_length = 8      ! 8 movement directions
    max_bars = 1000        ! Real-world time steps
    steps_per_bar = 12     ! Brain steps per real-world step (aligned with learning version)
    
    ! Vision parameters
    field_size = 100.0
    step_size = 5.0
    num_vision_slices = input_length
    
    ! Movement directions (8-way)
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
    call load_evolved_synapses(synapses, rows, cols)  ! Load evolved brain instead of random
    call reset_synapse_usage(synapse_usage, rows, cols)
    
    ! Initialize cat at center of field
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
    
    ! Print header for GUI script (to stderr so it doesn't interfere)
    write(0, '(A)') "# Cat & Mouse GUI Simulation Starting..."
    write(0, '(A,I0,A)') "# ", steps_per_bar, " brain steps per Bar (real-world step)"
    write(0, '(A)') "# Reinforcement: distance decrease = strengthen, increase = weaken"
    
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
        
        ! Update vision input based on mouse position
        call update_vision_input(inputter, cat_pos, mouse_pos, num_vision_slices)
        
        ! Find which slice is active
        active_slice = 0
        do i = 1, input_length
            if (inputter(i)%get() /= low) then
                active_slice = i - 1  ! 0-indexed for output
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
            
        ! Apply single decay per Bar - allows patterns to persist across brain steps
        call apply_decay(synapses, rows, cols)
        end do
        
        ! After all brain steps in this Bar, calculate brain energy
        brain_energy = 0
        do i = 1, rows
            do j = 1, cols
                brain_energy = brain_energy + brain(i, j)%get()
            end do
        end do
        
        ! Calculate output energy and find dominant action
        output_energy = 0
        output_action = 0
        do i = 1, output_length
            output_energy = output_energy + outputter(i)%get()
            if (outputter(i)%get() > 0 .and. output_action == 0) then
                output_action = i
            end if
        end do
        
        ! Move cat based on output (real-world action)
        if (output_action > 0 .and. output_action <= 8) then
            move_distance = outputter(output_action)%get()  ! 1 or 2 units
            new_x = cat_pos%x + move_directions(output_action, 2) * move_distance * 5.0
            new_y = cat_pos%y + move_directions(output_action, 1) * move_distance * 5.0
            
            ! Apply toroidal boundary conditions
            if (new_x < 0.0) new_x = new_x + field_size
            if (new_x >= field_size) new_x = new_x - field_size
            if (new_y < 0.0) new_y = new_y + field_size
            if (new_y >= field_size) new_y = new_y - field_size
            
            cat_pos%x = new_x
            cat_pos%y = new_y
        end if
        
        ! Now measure distance AFTER cat moved to determine reward/punishment
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        current_distance = sqrt(dx*dx + dy*dy)
        
        ! Apply selective reinforcement based on whether cat's action significantly improved distance
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
        
        ! Output state for GUI: bar,mouse_x,mouse_y,cat_x,cat_y,slice,brain_energy,output_energy
        write(*, '(I0,",",F0.1,",",F0.1,",",F0.1,",",F0.1,",",I0,",",I0,",",I0)') &
            bar, mouse_pos%x, mouse_pos%y, cat_pos%x, cat_pos%y, &
            active_slice, brain_energy, output_energy
        
        ! Flush output immediately so GUI can read it
        call flush()
    end do
    
    write(0, '(A)') "# Simulation complete"
    
contains

    subroutine load_evolved_synapses(synapses, rows, cols)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k, unit_num
        integer :: file_rows, file_cols
        character(len=50) :: filename
        logical :: file_exists
        
        ! Try to load the hand-crafted hunting brain first
        filename = "hunting_brain.dat"
        inquire(file=filename, exist=file_exists)
        
        if (file_exists) then
            open(newunit=unit_num, file=filename, form='unformatted', access='stream', status='old')
            read(unit_num) file_rows, file_cols
            if (file_rows == rows .and. file_cols == cols) then
                ! Allocate synapses array
                allocate(synapses(rows, cols, 8))
                do i = 1, rows
                    do j = 1, cols
                        do k = 1, 8
                            read(unit_num) synapses(i, j, k)
                        end do
                    end do
                end do
                close(unit_num)
                write(0, '(A)') "# Loaded hand-crafted hunting brain from " // trim(filename)
                return
            else
                close(unit_num)
            end if
        end if
        
        ! Fallback: try evolved brain
        filename = "best_brain_gen_15.dat"
        inquire(file=filename, exist=file_exists)
        
        if (file_exists) then
            open(newunit=unit_num, file=filename, form='unformatted', access='stream', status='old')
            read(unit_num) file_rows, file_cols
            if (file_rows == rows .and. file_cols == cols) then
                ! Allocate synapses array
                allocate(synapses(rows, cols, 8))
                do i = 1, rows
                    do j = 1, cols
                        do k = 1, 8
                            read(unit_num) synapses(i, j, k)
                        end do
                    end do
                end do
                close(unit_num)
                write(0, '(A)') "# Loaded evolved brain from " // trim(filename)
            else
                close(unit_num)
                write(0, '(A)') "# Brain file dimensions mismatch, using random initialization"
                call initialize_synapses(synapses, rows, cols)
            end if
        else
            write(0, '(A)') "# Evolved brain file not found, using random initialization"
            call initialize_synapses(synapses, rows, cols)
        end if
    end subroutine load_evolved_synapses
    
end program cat_mouse_gui_demo

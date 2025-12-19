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
    integer :: rows, cols, input_length, output_length
    integer :: input_offset, output_offset
    
    ! Vision system
    type(position) :: cat_pos, mouse_pos
    real :: field_size, step_size
    integer :: num_vision_slices
    
    ! Movement
    integer, dimension(8, 2) :: move_directions
    
    ! Simulation
    integer :: step, max_steps
    integer :: i, j, brain_energy, output_energy
    integer :: active_slice
    integer :: output_action, move_distance
    real :: new_x, new_y
    
    ! Parameters
    rows = 6
    cols = 12
    input_offset = 1
    input_length = 6       ! 6 vision slices
    output_offset = 1
    output_length = 8      ! 8 movement directions
    max_steps = 1000
    
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
    call initialize_synapses(synapses, rows, cols)
    
    ! Initialize cat at center of field
    cat_pos%x = field_size / 2.0
    cat_pos%y = field_size / 2.0
    
    ! Initialize mouse at random position
    call initialize_mouse(mouse_pos, field_size)
    
    ! Print header for GUI script (to stderr so it doesn't interfere)
    write(0, '(A)') "# Cat & Mouse GUI Simulation Starting..."
    
    ! Simulation loop
    do step = 1, max_steps
        ! Move mouse
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
        
        ! Calculate output energy and find dominant action
        output_energy = 0
        output_action = 0
        do i = 1, output_length
            output_energy = output_energy + outputter(i)%get()
            if (outputter(i)%get() > 0 .and. output_action == 0) then
                output_action = i
            end if
        end do
        
        ! Move cat based on output
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
        
        ! Output state for GUI: step,mouse_x,mouse_y,cat_x,cat_y,slice,brain_energy,output_energy
        write(*, '(I0,",",F0.1,",",F0.1,",",F0.1,",",F0.1,",",I0,",",I0,",",I0)') &
            step, mouse_pos%x, mouse_pos%y, cat_pos%x, cat_pos%y, &
            active_slice, brain_energy, output_energy
        
        ! Flush output immediately so GUI can read it
        call flush()
    end do
    
    write(0, '(A)') "# Simulation complete"
    
end program cat_mouse_gui_demo

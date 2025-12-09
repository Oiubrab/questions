program cat_mouse_vision
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
    
    ! Simulation
    integer :: step, max_steps
    integer :: i, j
    
    ! Parameters
    rows = 6
    cols = 12
    input_offset = 6
    input_length = 6
    output_offset = 1
    output_length = 6
    max_steps = 30
    
    ! Vision parameters
    field_size = 100.0
    step_size = 5.0
    num_vision_slices = input_length  ! Each vision slice maps to one input
    
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
    
    print *, "=== CAT & MOUSE VISION SIMULATION ==="
    print *, "Field size:", field_size
    print *, "Cat position: (", cat_pos%x, ",", cat_pos%y, ")"
    print *, "Vision slices:", num_vision_slices
    print *, "Brain dimensions:", rows, "x", cols
    print *
    
    ! Simulation loop
    do step = 1, max_steps
        print *, "========================================"
        print *, "STEP", step
        print *, "========================================"
        
        ! Move mouse
        call move_mouse(mouse_pos, field_size, step_size)
        
        ! Update vision input based on mouse position
        call update_vision_input(inputter, cat_pos, mouse_pos, num_vision_slices)
        
        ! Print field visualization
        call print_field(cat_pos, mouse_pos, field_size, 20)
        
        ! Print mouse position
        print *, "Mouse position: (", nint(mouse_pos%x), ",", nint(mouse_pos%y), ")"
        
        ! Print vision input
        print *, "Vision input (angular slices):"
        write(*, '(A)', advance='no') "  "
        do i = 1, input_length
            write(*, '(I3)', advance='no') inputter(i)%get()
        end do
        print *
        print *
        
        ! Apply input to brain
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Update brain
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        ! Apply decay to synapses
        call apply_decay(synapses, rows, cols)
        
        ! Print brain state
        print *, "Brain state (after update):"
        do i = 1, rows
            write(*, '(A)', advance='no') "  "
            do j = 1, cols
                write(*, '(I3)', advance='no') brain(i, j)%get()
            end do
            print *
        end do
        print *
        
        ! Print output
        print *, "Output array:"
        write(*, '(A)', advance='no') "  "
        do i = 1, output_length
            write(*, '(I3)', advance='no') outputter(i)%get()
        end do
        print *
        print *
        
    end do
    
    print *, "=== SIMULATION COMPLETE ==="
    
end program cat_mouse_vision

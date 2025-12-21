program evolved_brain_gui_demo
    use trinary_module
    use brain_module, only: initialize_brain
    use inputter_module 
    use outputter_module
    use evolutionary_helpers
    implicit none
    
    ! Brain system
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: evolved_synapses(:,:,:)
    integer :: rows = 6, cols = 12, input_length = 6, output_length = 8
    integer :: input_offset = 1, output_offset = 1, steps_per_bar = 12
    
    ! Simulation
    type(position) :: cat_pos, mouse_pos
    real :: field_size = 100.0, step_size = 5.0
    integer :: num_vision_slices = 6
    integer :: step, brain_step, vision_slice, output_action, move_distance
    real :: dx, dy, distance
    integer :: i
    
    ! Initialize arrays
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, input_length)
    call initialize_outputter(outputter, output_length)
    allocate(evolved_synapses(rows, cols, 8))
    
    ! Load the best evolved brain
    call load_best_brain(evolved_synapses, rows, cols)
    
    ! Initialize field positions
    call initialize_field_positions(cat_pos, mouse_pos, field_size)
    
    ! Output CSV header for GUI
    write(*, '(A)') "step,cat_x,cat_y,mouse_x,mouse_y,distance,vision_slice,output_action"
    
    ! Run simulation for GUI visualization
    do step = 1, 500
        ! Move mouse randomly
        call move_mouse_random(mouse_pos, field_size, step_size)
        
        ! Calculate vision
        call calculate_vision_slice(cat_pos, mouse_pos, field_size, num_vision_slices, vision_slice)
        call set_vision_input(inputter, vision_slice, input_length)
        
        ! Copy vision input to brain
        do i = 1, input_length
            if (i <= cols .and. inputter(i)%get() /= 0) then
                call brain(1, i)%set(inputter(i)%get())
            end if
        end do
        
        ! Clear output for this step
        do i = 1, output_length
            call outputter(i)%set(0)  ! low
        end do
        
        ! Process brain with evolved synapses
        do brain_step = 1, steps_per_bar
            call update_brain_state_based_on_synapses_simple(brain, evolved_synapses, &
                outputter, rows, cols, input_offset, output_offset, output_length)
        end do
        
        ! Get cat action from brain output
        call get_dominant_output_simple(outputter, output_action, move_distance, output_length)
        
        ! Move cat based on brain output
        if (move_distance > 0) then
            call move_cat_by_output(cat_pos, output_action, field_size, step_size)
        end if
        
        ! Calculate distance for display
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        distance = sqrt(dx*dx + dy*dy)
        
        ! Output state for GUI
        write(*, '(I0,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,I0,A,I0)') &
            step, ",", cat_pos%x, ",", cat_pos%y, ",", mouse_pos%x, ",", mouse_pos%y, &
            ",", distance, ",", vision_slice, ",", output_action
    end do
    
contains

    subroutine load_best_brain(synapses, rows, cols)
        integer :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k, unit_num
        integer :: file_rows, file_cols
        character(len=50) :: filename
        logical :: file_exists
        
        ! Try to load the evolved brain file
        filename = "best_brain_gen_15.dat"
        inquire(file=filename, exist=file_exists)
        
        if (file_exists) then
            open(newunit=unit_num, file=filename, form='unformatted', access='stream', status='old')
            read(unit_num) file_rows, file_cols
            if (file_rows == rows .and. file_cols == cols) then
                do i = 1, rows
                    do j = 1, cols
                        do k = 1, 8
                            read(unit_num) synapses(i, j, k)
                        end do
                    end do
                end do
                close(unit_num)
                write(*, '(A)') "# Loaded evolved brain from " // filename
            else
                close(unit_num)
                write(*, '(A)') "# Brain file dimensions mismatch, using fallback"
                call initialize_fallback_synapses(synapses, rows, cols)
            end if
        else
            ! Fallback: initialize with good values if file not found
            call initialize_fallback_synapses(synapses, rows, cols)
            write(*, '(A)') "# Using fallback brain configuration"
        end if
    end subroutine
    
    subroutine initialize_fallback_synapses(synapses, rows, cols)
        integer :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k
        real :: rand_val
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    call random_number(rand_val)
                    ! Slightly biased toward downward movement (better than random)
                    if (k >= 6) then
                        synapses(i, j, k) = 200 + int(800 * rand_val)
                    else
                        synapses(i, j, k) = 50 + int(150 * rand_val)
                    end if
                end do
            end do
        end do
    end subroutine

end program
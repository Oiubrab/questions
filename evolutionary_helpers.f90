module evolutionary_helpers
    use trinary_module
    use vision_simulation_module
    use outputter_module
    implicit none
    
contains

    subroutine initialize_field_positions(cat_pos, mouse_pos, field_size)
        type(position), intent(out) :: cat_pos, mouse_pos
        real, intent(in) :: field_size
        real :: rand_val
        
        ! Initialize cat at center
        cat_pos%x = field_size / 2.0
        cat_pos%y = field_size / 2.0
        
        ! Initialize mouse randomly
        call random_number(rand_val)
        mouse_pos%x = rand_val * field_size
        call random_number(rand_val) 
        mouse_pos%y = rand_val * field_size
    end subroutine

    subroutine move_mouse_random(mouse_pos, field_size, step_size)
        type(position), intent(inout) :: mouse_pos
        real, intent(in) :: field_size, step_size
        call move_mouse(mouse_pos, field_size, step_size)
    end subroutine

    subroutine calculate_vision_slice(cat_pos, mouse_pos, field_size, num_slices, slice)
        type(position), intent(in) :: cat_pos, mouse_pos
        real, intent(in) :: field_size
        integer, intent(in) :: num_slices
        integer, intent(out) :: slice
        real :: dx, dy, angle, slice_angle
        real, parameter :: PI = 3.14159265359
        
        ! Calculate relative position with toroidal wrapping
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        
        ! Handle wrapping
        if (abs(dx) > field_size / 2.0) then
            dx = dx - sign(field_size, dx)
        end if
        if (abs(dy) > field_size / 2.0) then
            dy = dy - sign(field_size, dy)
        end if
        
        ! Calculate angle
        angle = atan2(dy, dx)
        if (angle < 0.0) angle = angle + 2.0 * PI
        
        ! Determine slice
        slice_angle = 2.0 * PI / num_slices
        slice = int(angle / slice_angle) + 1
        if (slice < 1) slice = 1
        if (slice > num_slices) slice = num_slices
    end subroutine

    subroutine set_vision_input(inputter, vision_slice, input_length)
        type(trinary), intent(inout) :: inputter(:)
        integer, intent(in) :: vision_slice, input_length
        integer :: i
        
        ! Clear all inputs
        do i = 1, input_length
            call inputter(i)%set(0)  ! low
        end do
        
        ! Set active slice
        if (vision_slice >= 1 .and. vision_slice <= input_length) then
            call inputter(vision_slice)%set(2)  ! high
        end if
    end subroutine

    subroutine move_cat_by_output(cat_pos, output_action, field_size, step_size)
        type(position), intent(inout) :: cat_pos
        integer, intent(in) :: output_action
        real, intent(in) :: field_size, step_size
        real :: dx, dy
        
        ! Movement directions (8-directional)
        select case(output_action)
        case(1)  ! Up-Left
            dx = -step_size; dy = -step_size
        case(2)  ! Up  
            dx = 0.0; dy = -step_size
        case(3)  ! Up-Right
            dx = step_size; dy = -step_size
        case(4)  ! Left
            dx = -step_size; dy = 0.0
        case(5)  ! Right
            dx = step_size; dy = 0.0
        case(6)  ! Down-Left
            dx = -step_size; dy = step_size
        case(7)  ! Down
            dx = 0.0; dy = step_size
        case(8)  ! Down-Right
            dx = step_size; dy = step_size
        case default
            dx = 0.0; dy = 0.0
        end select
        
        ! Update position with wrapping
        cat_pos%x = cat_pos%x + dx
        cat_pos%y = cat_pos%y + dy
        
        ! Wrap around boundaries
        if (cat_pos%x < 0.0) cat_pos%x = cat_pos%x + field_size
        if (cat_pos%x > field_size) cat_pos%x = cat_pos%x - field_size
        if (cat_pos%y < 0.0) cat_pos%y = cat_pos%y + field_size
        if (cat_pos%y > field_size) cat_pos%y = cat_pos%y - field_size
    end subroutine

    subroutine update_brain_state_based_on_synapses_simple(brain, synapses, outputter, rows, cols, &
                                                          input_offset, output_offset, output_length)
        type(trinary), intent(inout) :: brain(:,:)
        integer, intent(in) :: synapses(:,:,:)
        type(trinary), intent(inout) :: outputter(:)
        integer, intent(in) :: rows, cols, input_offset, output_offset, output_length
        integer :: i, j, k, target_row, target_col, index
        real :: rand_val, total_weight, cumulative_weight
        real :: direction_bias(8)
        integer, dimension(8, 2) :: directions
        
        ! Direction mappings (same as brain_module)
        directions(1, :) = [-1, -1]  ! Up-Left
        directions(2, :) = [-1,  0]  ! Up
        directions(3, :) = [-1,  1]  ! Up-Right
        directions(4, :) = [ 0, -1]  ! Left
        directions(5, :) = [ 0,  1]  ! Right
        directions(6, :) = [ 1, -1]  ! Down-Left
        directions(7, :) = [ 1,  0]  ! Down
        directions(8, :) = [ 1,  1]  ! Down-Right
        
        ! Direction biases
        direction_bias(1) = 0.5; direction_bias(2) = 0.5; direction_bias(3) = 0.5
        direction_bias(4) = 1.0; direction_bias(5) = 1.0
        direction_bias(6) = 1.5; direction_bias(7) = 1.8; direction_bias(8) = 1.5
        
        do i = 1, rows
            do j = 1, cols
                if (brain(i, j)%get() /= 0) then  ! low = 0
                    ! Calculate total weight for this cell
                    total_weight = 0.0
                    do k = 1, 8
                        total_weight = total_weight + synapses(i, j, k) * direction_bias(k)
                    end do
                    
                    if (total_weight > 0.0) then
                        call random_number(rand_val)
                        cumulative_weight = 0.0
                        
                        do k = 1, 8
                            cumulative_weight = cumulative_weight + &
                                (synapses(i, j, k) * direction_bias(k)) / total_weight
                            
                            if (rand_val <= cumulative_weight) then
                                target_row = i + directions(k, 1)
                                target_col = j + directions(k, 2)
                                
                                ! Check if target is within bounds
                                if (target_row >= 1 .and. target_row <= rows .and. &
                                    target_col >= 1 .and. target_col <= cols) then
                                    
                                    ! Check if target isn't already high
                                    if (brain(target_row, target_col)%get() /= 2) then  ! high = 2
                                        call brain(i, j)%shift(-1)  ! shift down
                                        call brain(target_row, target_col)%shift(1)  ! shift up
                                    end if
                                    
                                else if (target_row > rows) then
                                    ! Propagate to outputter
                                    index = target_col - output_offset + 1
                                    if (index >= 1 .and. index <= output_length) then
                                        call outputter(index)%shift(1)
                                        call brain(i, j)%shift(-1)
                                    end if
                                end if
                                exit
                            end if
                        end do
                    end if
                end if
            end do
        end do
    end subroutine

    subroutine get_dominant_output_simple(outputter, output_action, move_distance, output_length)
        type(trinary), intent(in) :: outputter(:)
        integer, intent(out) :: output_action, move_distance
        integer, intent(in) :: output_length
        integer :: i, max_value, max_index
        
        max_value = -1
        max_index = 0
        
        do i = 1, output_length
            if (outputter(i)%get() > max_value) then
                max_value = outputter(i)%get()
                max_index = i
            end if
        end do
        
        if (max_value > 0) then
            output_action = max_index
            move_distance = 1
        else
            output_action = 0
            move_distance = 0
        end if
    end subroutine

end module evolutionary_helpers
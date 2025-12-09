module vision_simulation_module
    use trinary_module
    implicit none
    
    type :: position
        real :: x, y
    end type position
    
    contains
    
    ! Initialize mouse at random position in field
    subroutine initialize_mouse(mouse_pos, field_size)
        type(position), intent(out) :: mouse_pos
        real, intent(in) :: field_size
        real :: rand_x, rand_y
        
        call random_number(rand_x)
        call random_number(rand_y)
        
        mouse_pos%x = rand_x * field_size
        mouse_pos%y = rand_y * field_size
    end subroutine initialize_mouse
    
    ! Move mouse randomly (simple random walk)
    subroutine move_mouse(mouse_pos, field_size, step_size)
        type(position), intent(inout) :: mouse_pos
        real, intent(in) :: field_size, step_size
        real :: rand_angle, dx, dy
        real, parameter :: PI = 3.14159265359
        
        ! Random angle for movement
        call random_number(rand_angle)
        rand_angle = rand_angle * 2.0 * PI
        
        ! Calculate movement
        dx = cos(rand_angle) * step_size
        dy = sin(rand_angle) * step_size
        
        ! Update position with wrapping (toroidal field)
        mouse_pos%x = mouse_pos%x + dx
        mouse_pos%y = mouse_pos%y + dy
        
        ! Wrap around boundaries
        if (mouse_pos%x < 0.0) mouse_pos%x = mouse_pos%x + field_size
        if (mouse_pos%x > field_size) mouse_pos%x = mouse_pos%x - field_size
        if (mouse_pos%y < 0.0) mouse_pos%y = mouse_pos%y + field_size
        if (mouse_pos%y > field_size) mouse_pos%y = mouse_pos%y - field_size
    end subroutine move_mouse
    
    ! Update input vector based on mouse position relative to cat
    subroutine update_vision_input(inputter, cat_pos, mouse_pos, num_slices)
        type(trinary), allocatable, intent(inout) :: inputter(:)
        type(position), intent(in) :: cat_pos, mouse_pos
        integer, intent(in) :: num_slices
        real :: dx, dy, angle, slice_angle
        integer :: slice_index
        real, parameter :: PI = 3.14159265359
        integer :: i
        
        ! Clear all inputs first
        do i = 1, num_slices
            call inputter(i)%set(low)
        end do
        
        ! Calculate relative position
        dx = mouse_pos%x - cat_pos%x
        dy = mouse_pos%y - cat_pos%y
        
        ! Calculate angle (atan2 returns angle in radians, -PI to PI)
        angle = atan2(dy, dx)
        
        ! Normalize to 0 to 2*PI
        if (angle < 0.0) angle = angle + 2.0 * PI
        
        ! Determine which slice the mouse is in
        slice_angle = 2.0 * PI / num_slices
        slice_index = int(angle / slice_angle) + 1
        
        ! Ensure index is within bounds
        if (slice_index < 1) slice_index = 1
        if (slice_index > num_slices) slice_index = num_slices
        
        ! Set the corresponding input to medium
        call inputter(slice_index)%set(medium)
    end subroutine update_vision_input
    
    ! Print field visualization (ASCII art)
    subroutine print_field(cat_pos, mouse_pos, field_size, grid_res)
        type(position), intent(in) :: cat_pos, mouse_pos
        real, intent(in) :: field_size
        integer, intent(in) :: grid_res
        character(len=1) :: grid(grid_res, grid_res)
        integer :: cat_i, cat_j, mouse_i, mouse_j
        integer :: i, j
        
        ! Initialize grid
        do i = 1, grid_res
            do j = 1, grid_res
                grid(i, j) = '.'
            end do
        end do
        
        ! Place mouse first
        mouse_i = int((mouse_pos%y / field_size) * grid_res) + 1
        mouse_j = int((mouse_pos%x / field_size) * grid_res) + 1
        if (mouse_i < 1) mouse_i = 1
        if (mouse_i > grid_res) mouse_i = grid_res
        if (mouse_j < 1) mouse_j = 1
        if (mouse_j > grid_res) mouse_j = grid_res
        grid(mouse_i, mouse_j) = 'M'
        
        ! Place cat (may overwrite mouse if same position - show as 'X')
        cat_i = int((cat_pos%y / field_size) * grid_res) + 1
        cat_j = int((cat_pos%x / field_size) * grid_res) + 1
        if (cat_i < 1) cat_i = 1
        if (cat_i > grid_res) cat_i = grid_res
        if (cat_j < 1) cat_j = 1
        if (cat_j > grid_res) cat_j = grid_res
        
        ! Check if cat and mouse at same location
        if (cat_i == mouse_i .and. cat_j == mouse_j) then
            grid(cat_i, cat_j) = 'X'  ! Both at same location
        else
            grid(cat_i, cat_j) = 'C'
        end if
        
        ! Print grid (flip Y axis for visual consistency)
        print *
        print *, "Field (C=Cat, M=Mouse):"
        do i = grid_res, 1, -1
            write(*, '(100A1)') (grid(i, j), j = 1, grid_res)
        end do
        print *
    end subroutine print_field
    
end module vision_simulation_module

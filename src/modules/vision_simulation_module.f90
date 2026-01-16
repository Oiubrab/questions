module vision_simulation_module
    use trinary_module
    implicit none
    
    real, parameter :: PI = 3.14159265359
    real, parameter :: DEG_TO_RAD = PI / 180.0
    real, parameter :: RAD_TO_DEG = 180.0 / PI
    
    type :: position
        real :: x, y
    end type position
    
    ! Cat body with heading/orientation
    type :: cat_body
        real :: x, y           ! Center position
        real :: heading        ! Direction cat is facing (radians, 0 = right/east)
        real :: eye_separation ! Distance between eyes
        real :: catch_radius   ! Body zone for catching mice
    end type cat_body
    
    contains
    
    ! Initialize cat body at position with random heading
    subroutine initialize_cat(cat, x, y, eye_sep, catch_rad)
        type(cat_body), intent(out) :: cat
        real, intent(in) :: x, y, eye_sep, catch_rad
        real :: rand_heading
        
        cat%x = x
        cat%y = y
        cat%eye_separation = eye_sep
        cat%catch_radius = catch_rad
        
        ! Random initial heading
        call random_number(rand_heading)
        cat%heading = rand_heading * 2.0 * PI
    end subroutine initialize_cat
    
    ! Calculate eye positions based on cat center and heading
    subroutine get_eye_positions(cat, left_eye, right_eye)
        type(cat_body), intent(in) :: cat
        type(position), intent(out) :: left_eye, right_eye
        real :: half_sep, perp_angle
        
        half_sep = cat%eye_separation / 2.0
        ! Perpendicular to heading (90° counterclockwise for left, clockwise for right)
        perp_angle = cat%heading + PI / 2.0  ! 90° counterclockwise
        
        left_eye%x = cat%x + half_sep * cos(perp_angle)
        left_eye%y = cat%y + half_sep * sin(perp_angle)
        
        right_eye%x = cat%x - half_sep * cos(perp_angle)
        right_eye%y = cat%y - half_sep * sin(perp_angle)
    end subroutine get_eye_positions
    
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
    
    ! Initialize mouse at random position far from a point
    subroutine initialize_mouse_far_from(mouse_pos, avoid_pos, field_size, min_distance)
        type(position), intent(out) :: mouse_pos
        type(position), intent(in) :: avoid_pos
        real, intent(in) :: field_size, min_distance
        real :: dx, dy, dist
        integer :: attempts
        
        attempts = 0
        do
            call initialize_mouse(mouse_pos, field_size)
            dx = mouse_pos%x - avoid_pos%x
            dy = mouse_pos%y - avoid_pos%y
            ! Handle toroidal wrapping
            if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
            if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
            dist = sqrt(dx*dx + dy*dy)
            attempts = attempts + 1
            if (dist >= min_distance .or. attempts > 100) exit
        end do
    end subroutine initialize_mouse_far_from
    
    ! Move mouse randomly (simple random walk)
    subroutine move_mouse(mouse_pos, field_size, step_size)
        type(position), intent(inout) :: mouse_pos
        real, intent(in) :: field_size, step_size
        real :: rand_angle, dx, dy
        
        ! Random angle for movement
        call random_number(rand_angle)
        rand_angle = rand_angle * 2.0 * PI
        
        ! Calculate movement
        dx = cos(rand_angle) * step_size
        dy = sin(rand_angle) * step_size
        
        ! Update position with wrapping (toroidal field)
        mouse_pos%x = mouse_pos%x + dx
        mouse_pos%y = mouse_pos%y + dy
        
        ! Wrap around boundaries (handles arbitrarily large movements)
        do while (mouse_pos%x < 0.0)
            mouse_pos%x = mouse_pos%x + field_size
        end do
        do while (mouse_pos%x >= field_size)
            mouse_pos%x = mouse_pos%x - field_size
        end do
        do while (mouse_pos%y < 0.0)
            mouse_pos%y = mouse_pos%y + field_size
        end do
        do while (mouse_pos%y >= field_size)
            mouse_pos%y = mouse_pos%y - field_size
        end do
    end subroutine move_mouse
    
    ! Move cat body (updates position and heading)
    subroutine move_cat(cat, dx, dy, field_size)
        type(cat_body), intent(inout) :: cat
        real, intent(in) :: dx, dy, field_size
        real :: move_length
        
        ! Update heading to match movement direction (if moving)
        move_length = sqrt(dx*dx + dy*dy)
        if (move_length > 0.001) then
            cat%heading = atan2(dy, dx)
        end if
        
        ! Update position
        cat%x = cat%x + dx
        cat%y = cat%y + dy
        
        ! Wrap around boundaries (handles arbitrarily large movements)
        do while (cat%x < 0.0)
            cat%x = cat%x + field_size
        end do
        do while (cat%x >= field_size)
            cat%x = cat%x - field_size
        end do
        do while (cat%y < 0.0)
            cat%y = cat%y + field_size
        end do
        do while (cat%y >= field_size)
            cat%y = cat%y - field_size
        end do
    end subroutine move_cat
    
    ! Check if mouse is within catch radius of cat
    function check_catch(cat, mouse_pos, field_size) result(caught)
        type(cat_body), intent(in) :: cat
        type(position), intent(in) :: mouse_pos
        real, intent(in) :: field_size
        logical :: caught
        real :: dx, dy, dist
        
        dx = mouse_pos%x - cat%x
        dy = mouse_pos%y - cat%y
        
        ! Handle toroidal wrapping
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        
        dist = sqrt(dx*dx + dy*dy)
        caught = (dist <= cat%catch_radius)
    end function check_catch
    
    ! Calculate distance from a point to a mouse (with toroidal wrapping)
    function calc_distance(from_pos, to_pos, field_size) result(dist)
        type(position), intent(in) :: from_pos, to_pos
        real, intent(in) :: field_size
        real :: dist, dx, dy
        
        dx = to_pos%x - from_pos%x
        dy = to_pos%y - from_pos%y
        
        ! Handle toroidal wrapping
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        
        dist = sqrt(dx*dx + dy*dy)
    end function calc_distance
    
    ! Calculate angle from eye to target (relative to cat heading)
    function calc_relative_angle(eye_pos, target_pos, heading, field_size) result(rel_angle)
        type(position), intent(in) :: eye_pos, target_pos
        real, intent(in) :: heading, field_size
        real :: rel_angle, dx, dy, abs_angle
        
        dx = target_pos%x - eye_pos%x
        dy = target_pos%y - eye_pos%y
        
        ! Handle toroidal wrapping
        if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
        if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
        
        ! Absolute angle to target
        abs_angle = atan2(dy, dx)
        
        ! Relative to cat heading
        rel_angle = abs_angle - heading
        
        ! Normalize to -PI to PI
        do while (rel_angle > PI)
            rel_angle = rel_angle - 2.0 * PI
        end do
        do while (rel_angle < -PI)
            rel_angle = rel_angle + 2.0 * PI
        end do
    end function calc_relative_angle
    
    ! Update binocular vision for one eye viewing multiple mice
    ! eye_fov_start/end are relative angles from cat heading (in radians)
    subroutine update_eye_vision(inputter, eye_pos, mice, num_mice, num_slices, &
                                  fov_start, fov_end, heading, field_size, close_threshold)
        type(trinary), allocatable, intent(inout) :: inputter(:)
        type(position), intent(in) :: eye_pos
        type(position), intent(in) :: mice(:)
        integer, intent(in) :: num_mice, num_slices
        real, intent(in) :: fov_start, fov_end  ! FOV bounds relative to heading
        real, intent(in) :: heading, field_size, close_threshold
        
        integer :: i, m, slice_index
        real :: rel_angle, dist, slice_width, fov_range
        real :: normalized_angle
        integer :: current_state
        
        ! Clear all inputs first
        do i = 1, num_slices
            call inputter(i)%set(low)
        end do
        
        ! Calculate slice width
        fov_range = fov_end - fov_start
        slice_width = fov_range / real(num_slices)
        
        ! Check each mouse
        do m = 1, num_mice
            ! Get angle from this eye to this mouse (relative to heading)
            rel_angle = calc_relative_angle(eye_pos, mice(m), heading, field_size)
            
            ! Check if mouse is within this eye's field of view
            if (rel_angle >= fov_start .and. rel_angle < fov_end) then
                ! Calculate which slice
                normalized_angle = rel_angle - fov_start
                slice_index = int(normalized_angle / slice_width) + 1
                
                ! Clamp to valid range
                if (slice_index < 1) slice_index = 1
                if (slice_index > num_slices) slice_index = num_slices
                
                ! Get distance to mouse
                dist = calc_distance(eye_pos, mice(m), field_size)
                
                ! Set intensity based on distance
                current_state = inputter(slice_index)%get()
                if (dist <= close_threshold) then
                    ! Very close - HIGH priority
                    call inputter(slice_index)%set(high)
                else if (current_state < medium) then
                    ! Distant but visible - MEDIUM
                    call inputter(slice_index)%set(medium)
                end if
                ! If already HIGH from another mouse, keep it HIGH
            end if
        end do
    end subroutine update_eye_vision
    
    ! Update full binocular vision system
    subroutine update_binocular_vision(left_inputter, right_inputter, cat, mice, num_mice, &
                                        num_slices, field_size, close_threshold)
        type(trinary), allocatable, intent(inout) :: left_inputter(:), right_inputter(:)
        type(cat_body), intent(in) :: cat
        type(position), intent(in) :: mice(:)
        integer, intent(in) :: num_mice, num_slices
        real, intent(in) :: field_size, close_threshold
        
        type(position) :: left_eye, right_eye
        real :: left_fov_start, left_fov_end, right_fov_start, right_fov_end
        
        ! Get eye positions
        call get_eye_positions(cat, left_eye, right_eye)
        
        ! Each eye needs 225° FOV to cover 360° with overlap
        ! Left eye: -180° to +45° (225° FOV, covers left side + front)
        left_fov_start = -180.0 * DEG_TO_RAD  ! -180° (straight back on left)
        left_fov_end = 45.0 * DEG_TO_RAD      ! +45° (front-right)
        
        ! Right eye: -45° to +180° (225° FOV, covers right side + front)  
        right_fov_start = -45.0 * DEG_TO_RAD   ! -45° (front-left)
        right_fov_end = 180.0 * DEG_TO_RAD     ! +180° (straight back on right)
        
        ! This gives:
        ! - Full 360° coverage (no blind spot)
        ! - 90° binocular overlap (-45° to +45° in front for depth perception)
        
        ! Update each eye's vision
        call update_eye_vision(left_inputter, left_eye, mice, num_mice, num_slices, &
                               left_fov_start, left_fov_end, cat%heading, field_size, close_threshold)
        call update_eye_vision(right_inputter, right_eye, mice, num_mice, num_slices, &
                               right_fov_start, right_fov_end, cat%heading, field_size, close_threshold)
    end subroutine update_binocular_vision

    ! Legacy: Update input vector based on mouse position relative to cat (single eye, 360° FOV)
    subroutine update_vision_input(inputter, cat_pos, mouse_pos, num_slices)
        type(trinary), allocatable, intent(inout) :: inputter(:)
        type(position), intent(in) :: cat_pos, mouse_pos
        integer, intent(in) :: num_slices
        real :: dx, dy, angle, slice_angle
        integer :: slice_index
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
    
    ! Print field visualization (ASCII art) - legacy single mouse version
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
    
    ! Print field visualization with cat body and two mice
    subroutine print_field_binocular(cat, mice, num_mice, field_size, grid_res)
        type(cat_body), intent(in) :: cat
        type(position), intent(in) :: mice(:)
        integer, intent(in) :: num_mice
        real, intent(in) :: field_size
        integer, intent(in) :: grid_res
        character(len=1) :: grid(grid_res, grid_res)
        type(position) :: left_eye, right_eye
        integer :: cat_i, cat_j, left_i, left_j, right_i, right_j
        integer :: mouse_i, mouse_j
        integer :: i, j, m
        
        ! Initialize grid
        do i = 1, grid_res
            do j = 1, grid_res
                grid(i, j) = '.'
            end do
        end do
        
        ! Place mice
        do m = 1, num_mice
            mouse_i = int((mice(m)%y / field_size) * grid_res) + 1
            mouse_j = int((mice(m)%x / field_size) * grid_res) + 1
            if (mouse_i < 1) mouse_i = 1
            if (mouse_i > grid_res) mouse_i = grid_res
            if (mouse_j < 1) mouse_j = 1
            if (mouse_j > grid_res) mouse_j = grid_res
            ! Use 1, 2 for different mice
            if (m == 1) then
                grid(mouse_i, mouse_j) = '1'
            else
                grid(mouse_i, mouse_j) = '2'
            end if
        end do
        
        ! Get eye positions
        call get_eye_positions(cat, left_eye, right_eye)
        
        ! Place left eye
        left_i = int((left_eye%y / field_size) * grid_res) + 1
        left_j = int((left_eye%x / field_size) * grid_res) + 1
        if (left_i >= 1 .and. left_i <= grid_res .and. left_j >= 1 .and. left_j <= grid_res) then
            grid(left_i, left_j) = 'L'
        end if
        
        ! Place right eye
        right_i = int((right_eye%y / field_size) * grid_res) + 1
        right_j = int((right_eye%x / field_size) * grid_res) + 1
        if (right_i >= 1 .and. right_i <= grid_res .and. right_j >= 1 .and. right_j <= grid_res) then
            grid(right_i, right_j) = 'R'
        end if
        
        ! Place cat center
        cat_i = int((cat%y / field_size) * grid_res) + 1
        cat_j = int((cat%x / field_size) * grid_res) + 1
        if (cat_i < 1) cat_i = 1
        if (cat_i > grid_res) cat_i = grid_res
        if (cat_j < 1) cat_j = 1
        if (cat_j > grid_res) cat_j = grid_res
        grid(cat_i, cat_j) = 'C'
        
        ! Print grid (flip Y axis for visual consistency)
        print *
        print *, "Field (C=Cat center, L/R=Eyes, 1/2=Mice):"
        do i = grid_res, 1, -1
            write(*, '(100A1)') (grid(i, j), j = 1, grid_res)
        end do
        print *, "Cat heading:", nint(cat%heading * RAD_TO_DEG), "degrees"
        print *
    end subroutine print_field_binocular
    
end module vision_simulation_module

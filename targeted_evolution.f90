program targeted_evolution
    use trinary_module
    use brain_module, only: initialize_brain
    use inputter_module 
    use outputter_module
    use evolutionary_helpers
    implicit none
    
    ! Enhanced evolution parameters  
    integer, parameter :: population_size = 20
    integer, parameter :: num_generations = 25
    integer, parameter :: eval_steps = 1000  ! Longer evaluation
    integer, parameter :: survivors = 2      ! Much higher selection pressure
    
    ! Brain system
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: population(:,:,:,:)  ! (individual, rows, cols, 8)
    real, allocatable :: fitness(:)             ! Fitness score per individual
    integer :: rows, cols, input_length, output_length
    integer :: input_offset, output_offset
    
    ! Vision and simulation
    type(position) :: cat_pos, mouse_pos
    real :: field_size, step_size
    integer :: num_vision_slices, steps_per_bar
    
    ! Evolution variables
    integer :: generation, individual, step, brain_step, i, j, k
    real :: total_distance, avg_distance, dx, dy, current_distance
    integer :: vision_slice, output_action, move_distance
    character(len=100) :: filename
    
    ! Initialize parameters
    rows = 6
    cols = 12
    input_offset = 1
    input_length = 6
    output_offset = 1
    output_length = 8
    steps_per_bar = 12
    field_size = 100.0
    step_size = 5.0
    num_vision_slices = 6
    
    ! Allocate arrays
    allocate(population(population_size, rows, cols, 8))
    allocate(fitness(population_size))
    
    print *, "=== TARGETED EVOLUTION FOR HUNTING BEHAVIOR ==="
    print *, "Population size:", population_size
    print *, "Generations:", num_generations  
    print *, "Evaluation steps per individual:", eval_steps
    print *, "Survivors per generation:", survivors
    print *, "Selection pressure: EXTREME (top 10%)"
    print *, ""
    
    ! Initialize population with directional bias
    do individual = 1, population_size
        call initialize_directional_synapses(population(individual,:,:,:), rows, cols)
    end do
    
    ! Evolution loop
    do generation = 1, num_generations
        print *, "=== GENERATION", generation, "==="
        
        ! Evaluate each individual
        do individual = 1, population_size
            call initialize_brain(brain, rows, cols)
            call initialize_inputter(inputter, input_length)
            call initialize_field_positions(cat_pos, mouse_pos, field_size)
            
            total_distance = 0.0
            
            ! Run evaluation with aggressive fitness
            do step = 1, eval_steps
                ! Move mouse
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
                
                ! Clear output
                do i = 1, output_length
                    call outputter(i)%set(0)
                end do
                
                ! Brain processing
                do brain_step = 1, steps_per_bar
                    call update_brain_state_based_on_synapses_simple(brain, &
                        population(individual,:,:,:), outputter, rows, cols, &
                        input_offset, output_offset, output_length)
                end do
                
                ! Get output and move cat
                call get_dominant_output_simple(outputter, output_action, move_distance, output_length)
                if (move_distance > 0) then
                    call move_cat_by_output(cat_pos, output_action, field_size, step_size)
                end if
                
                ! Calculate distance with severe penalty for large distances
                dx = mouse_pos%x - cat_pos%x
                dy = mouse_pos%y - cat_pos%y
                if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
                if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
                current_distance = sqrt(dx*dx + dy*dy)
                
                ! Exponential penalty for distance > 20
                if (current_distance > 20.0) then
                    total_distance = total_distance + current_distance * 2.0
                else
                    total_distance = total_distance + current_distance
                end if
            end do
            
            ! Aggressive fitness = heavy penalty for large distances
            avg_distance = total_distance / eval_steps
            if (avg_distance < 15.0) then
                fitness(individual) = 1000.0 / (avg_distance + 1.0)  ! Reward close tracking
            else
                fitness(individual) = 10.0 / (avg_distance + 1.0)   ! Heavy penalty for poor tracking
            end if
            
            write(*, '(A,I2,A,F6.2,A,F8.1)') "  Individual ", individual, &
                ": avg_dist=", avg_distance, ", fitness=", fitness(individual)
        end do
        
        ! Sort by fitness
        call sort_population_by_fitness(population, fitness, population_size, rows, cols)
        
        ! Show best and worst
        write(*, '(A,F10.1)') " Best fitness this generation: ", fitness(1)
        write(*, '(A,F10.1)') " Worst fitness: ", fitness(population_size)
        write(*, '(A,F6.2)') " Best avg distance: ", 1000.0/fitness(1) - 1.0
        print *, ""
        
        ! Save best brain each generation  
        if (generation >= 10) then
            write(filename, '(A,I0,A)') "target_brain_gen_", generation, ".dat"
            call save_brain_config(population(1,:,:,:), filename, rows, cols)
        end if
        
        ! Extreme breeding: only top 2 breed
        if (generation < num_generations) then
            call breed_elite_population(population, population_size, survivors, rows, cols)
        end if
    end do
    
    print *, "=== TARGETED EVOLUTION COMPLETE ==="
    write(filename, '(A,I0,A)') "best_target_brain_gen_", num_generations, ".dat"
    call save_brain_config(population(1,:,:,:), filename, rows, cols)
    print *, "Best brain saved as: ", trim(filename)
    
contains

    subroutine initialize_directional_synapses(synapses, rows, cols)
        integer :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k
        real :: rand_val
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    call random_number(rand_val)
                    
                    ! Heavy bias toward downward and directional movement
                    if (k >= 6 .and. k <= 8) then
                        ! Downward directions get strong base + random
                        synapses(i, j, k) = 500 + int(1500 * rand_val)
                    else
                        ! Other directions get weaker base
                        synapses(i, j, k) = 50 + int(200 * rand_val)
                    end if
                end do
            end do
        end do
        
        ! Special boost for vision-to-output connectivity
        ! Vision input columns should strongly connect to corresponding output directions
        do j = 1, min(6, cols)  ! First 6 columns are vision inputs
            do k = 1, 8
                call random_number(rand_val)
                ! Vision slice j should bias toward movement direction k
                if (abs(j - k) <= 1 .or. abs(j - k) >= 7) then  ! Adjacent or wrap-around
                    synapses(rows, j, k) = 800 + int(1200 * rand_val)  ! Strong connection
                end if
            end do
        end do
    end subroutine

    subroutine breed_elite_population(population, pop_size, survivors, rows, cols)
        integer :: population(:,:,:,:)
        integer, intent(in) :: pop_size, survivors, rows, cols
        integer :: i, j, k, parent1, parent2, child
        real :: rand_val, mutation_rate = 0.3
        
        ! Fill population with children of top survivors
        do child = survivors + 1, pop_size
            ! Pick two random parents from survivors
            call random_number(rand_val)
            parent1 = 1 + int(rand_val * survivors)
            call random_number(rand_val) 
            parent2 = 1 + int(rand_val * survivors)
            
            ! Crossover + mutation
            do i = 1, rows
                do j = 1, cols
                    do k = 1, 8
                        call random_number(rand_val)
                        if (rand_val < 0.5) then
                            population(child, i, j, k) = population(parent1, i, j, k)
                        else
                            population(child, i, j, k) = population(parent2, i, j, k)
                        end if
                        
                        ! Mutation
                        call random_number(rand_val)
                        if (rand_val < mutation_rate) then
                            call random_number(rand_val)
                            population(child, i, j, k) = population(child, i, j, k) + &
                                int((rand_val - 0.5) * 200)
                            ! Keep within bounds
                            if (population(child, i, j, k) < 25) population(child, i, j, k) = 25
                            if (population(child, i, j, k) > 2000) population(child, i, j, k) = 2000
                        end if
                    end do
                end do
            end do
        end do
    end subroutine

    ! Include other necessary subroutines...
    subroutine sort_population_by_fitness(population, fitness, pop_size, rows, cols)
        integer :: population(:,:,:,:)
        real :: fitness(:)
        integer, intent(in) :: pop_size, rows, cols
        integer :: i, j, best_idx
        real :: best_fitness
        integer :: temp_brain(rows, cols, 8)
        real :: temp_fitness
        
        ! Simple selection sort by fitness (descending)
        do i = 1, pop_size - 1
            best_idx = i
            best_fitness = fitness(i)
            
            do j = i + 1, pop_size
                if (fitness(j) > best_fitness) then
                    best_fitness = fitness(j)
                    best_idx = j
                end if
            end do
            
            if (best_idx /= i) then
                ! Swap brains
                temp_brain = population(i,:,:,:)
                population(i,:,:,:) = population(best_idx,:,:,:)
                population(best_idx,:,:,:) = temp_brain
                
                ! Swap fitness
                temp_fitness = fitness(i)
                fitness(i) = fitness(best_idx)
                fitness(best_idx) = temp_fitness
            end if
        end do
    end subroutine

    subroutine save_brain_config(synapses, filename, rows, cols)
        integer :: synapses(:,:,:)
        character(len=*) :: filename
        integer, intent(in) :: rows, cols
        integer :: unit, i, j, k
        
        open(newunit=unit, file=filename, form='unformatted', access='stream')
        write(unit) rows, cols
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    write(unit) synapses(i, j, k)
                end do
            end do
        end do
        close(unit)
    end subroutine

end program targeted_evolution
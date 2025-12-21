program evolutionary_learning
    use trinary_module
    use brain_module, only: initialize_brain, copy_non_low_to_brain_top_row
    use inputter_module, only: initialize_inputter
    use outputter_module
    use synapses_module, only: initialize_synapses
    use vision_simulation_module
    use evolutionary_helpers
    implicit none
    
    ! Evolution parameters
    integer, parameter :: population_size = 12
    integer, parameter :: num_generations = 15
    integer, parameter :: eval_steps = 800  ! Longer evaluation for better fitness assessment
    integer, parameter :: survivors = 4     ! Top N to breed from
    
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
    real :: total_distance, avg_distance
    real :: dx, dy, current_distance
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
    allocate(brain(rows, cols))
    allocate(inputter(input_length))
    allocate(outputter(output_length))
    allocate(population(population_size, rows, cols, 8))
    allocate(fitness(population_size))
    
    print *, "=== EVOLUTIONARY BRAIN LEARNING ==="
    print *, "Population size:", population_size
    print *, "Generations:", num_generations
    print *, "Evaluation steps per individual:", eval_steps
    print *, "Survivors per generation:", survivors
    print *, ""
    
    ! Initialize random population
    do individual = 1, population_size
        call initialize_synapses_individual(population(individual,:,:,:), rows, cols)
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
            
            ! Run evaluation
            do step = 1, eval_steps
                ! Random mouse movement
                call move_mouse_random(mouse_pos, field_size, step_size)
                
                ! Calculate vision
                call calculate_vision_slice(cat_pos, mouse_pos, field_size, &
                                          num_vision_slices, vision_slice)
                call set_vision_input(inputter, vision_slice, input_length)
                ! Manual copy instead of copy_non_low_to_brain_top_row
                do i = 1, input_length
                    if (i <= cols .and. inputter(i)%get() /= 0) then  ! not low
                        call brain(1, i)%set(inputter(i)%get())
                    end if
                end do
                
                ! Brain processing
                do brain_step = 1, steps_per_bar
                    call update_brain_state_based_on_synapses_simple(brain, &
                        population(individual,:,:,:), outputter, rows, cols, &
                        input_offset, output_offset, output_length)
                    
                    ! Apply decay every 2 steps
                    if (mod(brain_step, 2) == 0) then
                        call apply_decay_individual(population(individual,:,:,:), rows, cols)
                    end if
                end do
                
                ! Get output and move cat
                call get_dominant_output_simple(outputter, output_action, move_distance, output_length)
                if (move_distance > 0) then
                    call move_cat_by_output(cat_pos, output_action, field_size, step_size)
                end if
                
                ! Calculate distance for fitness
                dx = mouse_pos%x - cat_pos%x
                dy = mouse_pos%y - cat_pos%y
                if (abs(dx) > field_size / 2.0) dx = dx - sign(field_size, dx)
                if (abs(dy) > field_size / 2.0) dy = dy - sign(field_size, dy)
                current_distance = sqrt(dx*dx + dy*dy)
                total_distance = total_distance + current_distance
            end do
            
            ! Fitness = inverse of average distance (lower distance = higher fitness)
            avg_distance = total_distance / eval_steps
            fitness(individual) = 100.0 / (avg_distance + 1.0)  ! +1 to avoid division by zero
            
            write(*, '(A,I2,A,F6.3,A,F6.2)') "  Individual ", individual, &
                ": avg_dist=", avg_distance, ", fitness=", fitness(individual)
        end do
        
        ! Sort by fitness (simple bubble sort)
        call sort_population_by_fitness(population, fitness, population_size, rows, cols)
        
        print *, ""
        print *, "Best fitness this generation:", fitness(1)
        print *, "Worst fitness:", fitness(population_size)
        
        ! Save best individual
        write(filename, '(A,I0,A)') "best_brain_gen_", generation, ".dat"
        call save_brain_config(population(1,:,:,:), filename, rows, cols)
        
        ! Create next generation (if not last generation)
        if (generation < num_generations) then
            call create_next_generation(population, fitness, population_size, &
                                      survivors, rows, cols)
        end if
        
        print *, ""
    end do
    
    print *, "=== EVOLUTION COMPLETE ==="
    print *, "Best brain saved as: best_brain_gen_", num_generations, ".dat"
    
contains

    subroutine initialize_synapses_individual(synapses, rows, cols)
        integer :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k
        real :: rand_val
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    call random_number(rand_val)
                    synapses(i, j, k) = 25 + int(975 * rand_val)
                end do
            end do
        end do
    end subroutine

    subroutine apply_decay_individual(synapses, rows, cols)
        integer :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k
        real :: rand_decay
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    call random_number(rand_decay)
                    rand_decay = 0.92 + 0.06 * rand_decay
                    synapses(i, j, k) = max(25, int(synapses(i, j, k) * rand_decay))
                end do
            end do
        end do
    end subroutine

    subroutine sort_population_by_fitness(population, fitness, pop_size, rows, cols)
        integer :: population(:,:,:,:)
        real :: fitness(:)
        integer, intent(in) :: pop_size, rows, cols
        integer :: i, j
        real :: temp_fitness
        integer :: temp_individual(rows, cols, 8)
        
        ! Simple bubble sort (descending order - highest fitness first)
        do i = 1, pop_size - 1
            do j = i + 1, pop_size
                if (fitness(j) > fitness(i)) then
                    ! Swap fitness scores
                    temp_fitness = fitness(i)
                    fitness(i) = fitness(j)
                    fitness(j) = temp_fitness
                    
                    ! Swap individuals
                    temp_individual = population(i,:,:,:)
                    population(i,:,:,:) = population(j,:,:,:)
                    population(j,:,:,:) = temp_individual
                end if
            end do
        end do
    end subroutine

    subroutine create_next_generation(population, fitness, pop_size, survivors, rows, cols)
        integer :: population(:,:,:,:)
        real :: fitness(:)
        integer, intent(in) :: pop_size, survivors, rows, cols
        integer :: i, j, k, l, parent1, parent2
        real :: rand_val, mutation_rate
        
        mutation_rate = 0.1  ! 10% mutation rate
        
        ! Keep best survivors, create children for the rest
        do i = survivors + 1, pop_size
            ! Select two parents from survivors
            call random_number(rand_val)
            parent1 = 1 + int(rand_val * survivors)
            call random_number(rand_val)
            parent2 = 1 + int(rand_val * survivors)
            
            ! Crossover and mutation
            do j = 1, rows
                do k = 1, cols
                    do l = 1, 8
                        ! Crossover (average of parents)
                        population(i,j,k,l) = (population(parent1,j,k,l) + &
                                             population(parent2,j,k,l)) / 2
                        
                        ! Mutation
                        call random_number(rand_val)
                        if (rand_val < mutation_rate) then
                            call random_number(rand_val)
                            ! Add random variation
                            population(i,j,k,l) = population(i,j,k,l) + &
                                int((rand_val - 0.5) * 200)
                            population(i,j,k,l) = max(25, min(1000, population(i,j,k,l)))
                        end if
                    end do
                end do
            end do
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

end program evolutionary_learning
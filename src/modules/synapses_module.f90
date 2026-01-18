module synapses_module
    implicit none
    contains

    subroutine initialize_synapses(synapses, rows, cols)
        integer, allocatable :: synapses(:,:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k, m
        integer :: seed(8)
        real :: rand_val

        ! Initialize the random number generator with system clock
        call random_seed()

        allocate(synapses(rows, cols, 8, 8))  ! Now 4D: (row, col, incoming_dir, outgoing_dir)

        ! Initialize the 4D synapses with random integers between 25 and 1000
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        call random_number(rand_val)
                        synapses(i, j, k, m) = 25 + int(975 * rand_val)
                    end do
                end do
            end do
        end do
    end subroutine initialize_synapses

    ! Decay function to multiply the synapse value by a random number between 0.98 and 0.995
    ! Much gentler decay to preserve learned pathways when mouse moves to different sectors
    ! NOTE: rand_decay must be passed in for GPU compatibility (random_number not supported on GPU)
    !$acc routine seq
    pure integer function decay_value_gpu(value, rand_decay)
        integer, intent(in) :: value
        real, intent(in) :: rand_decay
        real :: actual_decay

        actual_decay = 0.98 + 0.015 * rand_decay  ! Range: 0.98 to 0.995 (0.5-2% loss per decay)
        decay_value_gpu = max(25, int(value * actual_decay))
    end function decay_value_gpu
    
    ! CPU version that generates its own random number
    integer function decay_value(value)
        integer, intent(in) :: value
        real :: rand_decay

        call random_number(rand_decay)
        rand_decay = 0.98 + 0.015 * rand_decay  ! Range: 0.98 to 0.995 (0.5-2% loss per decay)

        decay_value = max(25, int(value * rand_decay))
    end function decay_value

    ! Inverse decay (reinforcement) function to multiply by a random number between 1.5 and 2.0
    integer function reinforce_value(value)
        integer, intent(in) :: value
        real :: rand_reinforce
        integer, parameter :: max_synapse_strength = 200000

        call random_number(rand_reinforce)
        rand_reinforce = 1.5 + 0.5 * rand_reinforce

        reinforce_value = min(max_synapse_strength, int(value * rand_reinforce))
    end function reinforce_value

    ! Apply single step of decay to synapses
    subroutine apply_decay(synapses, rows, cols)
        integer, allocatable :: synapses(:,:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k, m
        real, allocatable :: rand_vals(:,:,:,:)
        
        ! Pre-generate random numbers on CPU for GPU compatibility
        allocate(rand_vals(rows, cols, 8, 8))
        call random_number(rand_vals)

        ! Apply decay factor to all elements - parallelized
        ! OpenACC for GPU with data region, OpenMP for CPU fallback
        !$acc data copyin(rand_vals) present(synapses)
        !$acc parallel loop collapse(4) if(rows*cols > 10)
        !$omp parallel do collapse(4) private(i,j,k,m) if(rows*cols > 10)
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        synapses(i, j, k, m) = decay_value_gpu(synapses(i, j, k, m), rand_vals(i, j, k, m))
                    end do
                end do
            end do
        end do
        !$omp end parallel do
        !$acc end parallel loop
        !$acc end data
        
        deallocate(rand_vals)
    end subroutine apply_decay

    ! Reset synapse usage tracker
    subroutine reset_synapse_usage(synapse_usage, rows, cols)
        logical, allocatable :: synapse_usage(:,:,:,:)
        integer, intent(in) :: rows, cols

        if (.not. allocated(synapse_usage)) then
            allocate(synapse_usage(rows, cols, 8, 8))
        end if
        synapse_usage = .false.
    end subroutine reset_synapse_usage

    ! Adaptive reinforcement that scales with number of decay steps
    subroutine apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, num_steps)
        integer, allocatable :: synapses(:,:,:,:)
        logical, allocatable :: synapse_usage(:,:,:,:)
        integer, intent(in) :: rows, cols, num_steps
        integer :: i, j, k, m
        real :: target_multiplier
        real, allocatable :: rand_vals(:,:,:,:)
        integer, parameter :: max_synapse_strength = 200000
        
        ! Calculate reinforcement: 1.2 / (0.95^num_steps)
        target_multiplier = 1.2 / (0.95 ** num_steps)
        
        ! Pre-generate random numbers on CPU for GPU compatibility
        allocate(rand_vals(rows, cols, 8, 8))
        call random_number(rand_vals)
        ! Scale to 0.9-1.1 range
        rand_vals = 0.9 + 0.2 * rand_vals
        
        !$acc data copyin(rand_vals) present(synapses, synapse_usage)
        !$acc parallel loop collapse(4) if(rows*cols > 10)
        !$omp parallel do collapse(4) private(i,j,k,m) if(rows*cols > 10)
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        if (synapse_usage(i, j, k, m)) then
                            synapses(i, j, k, m) = min(max_synapse_strength, &
                                                    int(synapses(i, j, k, m) * target_multiplier * rand_vals(i,j,k,m)))
                        end if
                    end do
                end do
            end do
        end do
        !$omp end parallel do
        !$acc end parallel loop
        !$acc end data
        
        deallocate(rand_vals)
    end subroutine apply_adaptive_reinforcement

    ! Adaptive punishment that scales with number of decay steps
    subroutine apply_adaptive_punishment(synapses, synapse_usage, rows, cols, num_steps)
        integer, allocatable :: synapses(:,:,:,:)
        logical, allocatable :: synapse_usage(:,:,:,:)
        integer, intent(in) :: rows, cols, num_steps
        integer :: i, j, k, m
        real :: target_multiplier
        real, allocatable :: rand_vals(:,:,:,:)
        
        ! Calculate punishment: 0.8 * (0.95^num_steps) - amplifies decay effect
        target_multiplier = 0.8 * (0.95 ** num_steps)
        
        ! Pre-generate random numbers on CPU for GPU compatibility
        allocate(rand_vals(rows, cols, 8, 8))
        call random_number(rand_vals)
        ! Scale to 0.9-1.1 range
        rand_vals = 0.9 + 0.2 * rand_vals
        
        !$acc data copyin(rand_vals) present(synapses, synapse_usage)
        !$acc parallel loop collapse(4) if(rows*cols > 10)
        !$omp parallel do collapse(4) private(i,j,k,m) if(rows*cols > 10)
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        if (synapse_usage(i, j, k, m)) then
                            synapses(i, j, k, m) = max(25, int(synapses(i, j, k, m) * target_multiplier * rand_vals(i,j,k,m)))
                        end if
                    end do
                end do
            end do
        end do
        !$omp end parallel do
        !$acc end parallel loop
        !$acc end data
        
        deallocate(rand_vals)
    end subroutine apply_adaptive_punishment

end module synapses_module

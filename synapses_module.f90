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

    ! Decay function to multiply the synapse value by a random number between 0.92 and 0.98
    integer function decay_value(value)
        integer, intent(in) :: value
        real :: rand_decay

        call random_number(rand_decay)
        rand_decay = 0.92 + 0.06 * rand_decay

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

        ! Apply decay factor to all elements
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        synapses(i, j, k, m) = decay_value(synapses(i, j, k, m))
                    end do
                end do
            end do
        end do
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
        real :: target_multiplier, rand_factor
        integer, parameter :: max_synapse_strength = 200000
        
        ! Calculate reinforcement: 1.2 / (0.95^num_steps)
        target_multiplier = 1.2 / (0.95 ** num_steps)
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        if (synapse_usage(i, j, k, m)) then
                            call random_number(rand_factor)
                            rand_factor = 0.9 + 0.2 * rand_factor
                            synapses(i, j, k, m) = min(max_synapse_strength, &
                                                    int(synapses(i, j, k, m) * target_multiplier * rand_factor))
                        end if
                    end do
                end do
            end do
        end do
    end subroutine apply_adaptive_reinforcement

    ! Adaptive punishment that scales with number of decay steps
    subroutine apply_adaptive_punishment(synapses, synapse_usage, rows, cols, num_steps)
        integer, allocatable :: synapses(:,:,:,:)
        logical, allocatable :: synapse_usage(:,:,:,:)
        integer, intent(in) :: rows, cols, num_steps
        integer :: i, j, k, m
        real :: target_multiplier, rand_factor
        
        ! Calculate punishment: 0.8 / (0.95^num_steps)
        target_multiplier = 0.8 / (0.95 ** num_steps)
        
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8  ! Incoming direction
                    do m = 1, 8  ! Outgoing direction
                        if (synapse_usage(i, j, k, m)) then
                            call random_number(rand_factor)
                            rand_factor = 0.9 + 0.2 * rand_factor
                            synapses(i, j, k, m) = max(25, int(synapses(i, j, k, m) * target_multiplier * rand_factor))
                        end if
                    end do
                end do
            end do
        end do
    end subroutine apply_adaptive_punishment

end module synapses_module

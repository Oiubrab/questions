module synapses_module
    implicit none
    contains

    subroutine initialize_synapses(synapses, rows, cols)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k
        integer :: seed(8)
        real :: rand_val

        ! Initialize the random number generator with system clock
        call random_seed()

        allocate(synapses(rows, cols, 8))

        ! Initialize the 3D synapses with random integers between 1 and 1000
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    call random_number(rand_val)
                    synapses(i, j, k) = 1 + int(999 * rand_val)
                end do
            end do
        end do
    end subroutine initialize_synapses

    ! Decay function to multiply the synapse value by a random number between 0.9 and 1.0
    integer function decay_value(value)
        integer, intent(in) :: value
        real :: rand_decay

        call random_number(rand_decay)
        rand_decay = 0.9 + 0.1 * rand_decay

        decay_value = max(1, int(value * rand_decay))
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
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k

        ! Apply decay factor to all elements
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    synapses(i, j, k) = decay_value(synapses(i, j, k))
                end do
            end do
        end do
    end subroutine apply_decay

    ! Apply reinforcement (inverse decay) to all synapses - reward for good behavior
    subroutine apply_reinforcement(synapses, rows, cols)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k

        ! Apply reinforcement factor to all elements
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    synapses(i, j, k) = reinforce_value(synapses(i, j, k))
                end do
            end do
        end do
    end subroutine apply_reinforcement

    ! Apply selective reinforcement only to synapses that were used
    subroutine apply_selective_reinforcement(synapses, synapse_usage, rows, cols)
        integer, allocatable :: synapses(:,:,:)
        logical, allocatable :: synapse_usage(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k

        ! Apply reinforcement only to synapses that were used
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    if (synapse_usage(i, j, k)) then
                        synapses(i, j, k) = reinforce_value(synapses(i, j, k))
                    end if
                end do
            end do
        end do
    end subroutine apply_selective_reinforcement

    ! Apply selective decay only to synapses that were used
    subroutine apply_selective_decay(synapses, synapse_usage, rows, cols)
        integer, allocatable :: synapses(:,:,:)
        logical, allocatable :: synapse_usage(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k

        ! Apply decay only to synapses that were used
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    if (synapse_usage(i, j, k)) then
                        synapses(i, j, k) = decay_value(synapses(i, j, k))
                    end if
                end do
            end do
        end do
    end subroutine apply_selective_decay

    ! Reset synapse usage tracker
    subroutine reset_synapse_usage(synapse_usage, rows, cols)
        logical, allocatable :: synapse_usage(:,:,:)
        integer, intent(in) :: rows, cols

        if (.not. allocated(synapse_usage)) then
            allocate(synapse_usage(rows, cols, 8))
        end if
        synapse_usage = .false.
    end subroutine reset_synapse_usage

    ! Adaptive reinforcement that scales with number of decay steps
    ! Target: counteract decay from N steps plus add small growth
    ! Decay per step: ~0.95, so N steps = 0.95^N
    ! We want final effect ~1.05 (slight growth), so reinforcement = 1.05 / (0.95^N)
    subroutine apply_adaptive_reinforcement(synapses, synapse_usage, rows, cols, num_steps)
        integer, allocatable :: synapses(:,:,:)
        logical, allocatable :: synapse_usage(:,:,:)
        integer, intent(in) :: rows, cols, num_steps
        integer :: i, j, k
        real :: target_multiplier, rand_factor
        integer, parameter :: max_synapse_strength = 200000
        
        ! Calculate reinforcement needed: 1.05 / (0.95^num_steps)
        ! This counteracts decay and adds ~5% growth
        target_multiplier = 1.05 / (0.95 ** num_steps)
        
        ! Apply reinforcement with some randomness (±10%)
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    if (synapse_usage(i, j, k)) then
                        call random_number(rand_factor)
                        rand_factor = 0.9 + 0.2 * rand_factor  ! Range: 0.9 to 1.1
                        synapses(i, j, k) = min(max_synapse_strength, &
                                                int(synapses(i, j, k) * target_multiplier * rand_factor))
                    end if
                end do
            end do
        end do
    end subroutine apply_adaptive_reinforcement

    ! Adaptive punishment that scales with number of decay steps
    ! Target: amplify decay effect for failed pathways
    subroutine apply_adaptive_punishment(synapses, synapse_usage, rows, cols, num_steps)
        integer, allocatable :: synapses(:,:,:)
        logical, allocatable :: synapse_usage(:,:,:)
        integer, intent(in) :: rows, cols, num_steps
        integer :: i, j, k
        real :: target_multiplier, rand_factor
        
        ! Calculate punishment: 0.95 / (0.95^num_steps) = 0.95^(1-num_steps)
        ! This makes decay stronger for failed pathways
        target_multiplier = 0.95 / (0.95 ** num_steps)
        
        ! Apply punishment with some randomness
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    if (synapse_usage(i, j, k)) then
                        call random_number(rand_factor)
                        rand_factor = 0.9 + 0.2 * rand_factor
                        synapses(i, j, k) = max(1, int(synapses(i, j, k) * target_multiplier * rand_factor))
                    end if
                end do
            end do
        end do
    end subroutine apply_adaptive_punishment

end module synapses_module

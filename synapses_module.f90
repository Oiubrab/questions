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

end module synapses_module

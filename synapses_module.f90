module synapses_module
    implicit none
    contains

    subroutine initialize_synapses(synapses, rows, cols)
        real, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, k

        allocate(synapses(rows, cols, 8))

        ! Initialize the 3D synapses with all 0.5
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    synapses(i, j, k) = 0.5
                end do
            end do
        end do
    end subroutine initialize_synapses

    ! Logarithmic decay function with asymptote at 0.005
    real function decay_factor(step)
        integer, intent(in) :: step
        real :: log_factor
        intrinsic :: log

        log_factor = log(real(step) + 1.0) / log(real(step) + 2.0)
        decay_factor = 0.005 + (1.0 - 0.005) * log_factor
    end function decay_factor

    ! Apply single step of decay to synapses
    subroutine apply_decay(synapses, rows, cols, step)
        real, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols, step
        integer :: i, j, k
        real :: decay

        ! Get decay factor
        decay = decay_factor(step)

        ! Debugging output
        print *, "Step:", step, "Decay factor:", decay

        ! Apply decay factor to all elements
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    synapses(i, j, k) = synapses(i, j, k) * decay
                end do
            end do
        end do
    end subroutine apply_decay

end module synapses_module

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

end module synapses_module

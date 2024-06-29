module brain_module
    use trinary_module
    implicit none
    contains

    subroutine initialize_brain(brain, rows, cols)
        type(trinary), allocatable :: brain(:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j

        allocate(brain(rows, cols))

        ! Initialize the 2D brain with all lows (0's)
        do i = 1, rows
            do j = 1, cols
                call brain(i, j)%set(low)
            end do
        end do
    end subroutine initialize_brain

end module brain_module

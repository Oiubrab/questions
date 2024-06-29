program forWhoseAdvantage
    use trinary_module
    implicit none
    integer :: rows, cols
    integer, allocatable :: matrix(:,:)
    integer :: i, j
    character(len=10) :: arg1, arg2
    integer :: ios
    type(trinary) :: tri_state

    ! Read command line arguments
    call get_command_argument(1, arg1)
    call get_command_argument(2, arg2)

    ! Convert command line arguments to integers
    read(arg1, *, iostat=ios) rows
    if (ios /= 0) then
        print *, "Error: Invalid input for rows."
        stop
    end if

    read(arg2, *, iostat=ios) cols
    if (ios /= 0) then
        print *, "Error: Invalid input for cols."
        stop
    end if

    ! Allocate the 2D matrix
    allocate(matrix(rows, cols))

    ! Initialize the 2D matrix
    do i = 1, rows
        do j = 1, cols
            matrix(i, j) = (i - 1) * cols + j
        end do
    end do

    ! Set and get trinary state example
    call tri_state%set(low)
    print *, "Trinary state set to low:", tri_state%get()

    call tri_state%set(medium)
    print *, "Trinary state set to medium:", tri_state%get()

    call tri_state%set(high)
    print *, "Trinary state set to high:", tri_state%get()

    ! Print the 2D matrix
    print *, "2D Matrix of integers:"
    do i = 1, rows
        print *, matrix(i, :)
    end do

end program forWhoseAdvantage

program forWhoseAdvantage
    use trinary_module
    implicit none
    integer :: rows, cols
    type(trinary), allocatable :: matrix(:,:)
    type(trinary), allocatable :: inputer(:)
    type(trinary), allocatable :: outputter(:)
    integer :: i, j
    character(len=10) :: arg1, arg2
    integer :: ios

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

    ! Allocate the 2D matrix and 1D arrays
    allocate(matrix(rows, cols))
    allocate(inputer(cols))
    allocate(outputter(cols))

    ! Initialize the 2D matrix with all lows (0's)
    do i = 1, rows
        do j = 1, cols
            call matrix(i, j)%set(low)
        end do
    end do

    ! Initialize the inputer array with alternating trinary states
    do j = 1, cols
        if (mod(j, 3) == 0) then
            call inputer(j)%set(low)
        elseif (mod(j, 3) == 1) then
            call inputer(j)%set(medium)
        else
            call inputer(j)%set(high)
        end if
    end do

    ! Initialize the outputter array with the same values as inputer
    do j = 1, cols
        call outputter(j)%set(inputer(j)%get())
    end do

    ! Print the inputer array
    print *, "Inputer array of trinary states:"
    write(*, "(100(I3,1X))") (inputer(j)%get(), j=1, cols)

    ! Print the 2D matrix
    print *, "2D Matrix of trinary states:"
    do i = 1, rows
        write(*, "(100(I3,1X))") (matrix(i, j)%get(), j=1, cols)
    end do

    ! Print the outputter array
    print *, "Outputter array of trinary states:"
    write(*, "(100(I3,1X))") (outputter(j)%get(), j=1, cols)

end program forWhoseAdvantage

program forWhoseAdvantage
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    integer :: rows, cols
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: inputter(:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: i, j, k, step, max_steps
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

    ! Initialize the brain, inputter, outputter, and synapses
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, cols)
    call initialize_outputter(outputter, inputter, cols)
    call initialize_synapses(synapses, rows, cols)

    ! Print the initial 3D synapses
    print *, "Initial 3D Synapses with 8 integers per cell:"
    do i = 1, rows
        do j = 1, cols
            write(*, "(8I6, 1X)") (synapses(i, j, k), k=1, 8)
        end do
        print *
    end do

    ! Apply decay in a loop
    max_steps = 50
    do step = 1, max_steps
        call apply_decay(synapses, rows, cols)
        if (all(synapses <= 1)) exit
    end do

    ! Print the inputter array
    print *, "Inputter array of trinary states:"
    write(*, "(100(I3,1X))") (inputter(j)%get(), j=1, cols)

    ! Print the 2D brain
    print *, "2D Brain of trinary states:"
    do i = 1, rows
        write(*, "(100(I3,1X))") (brain(i, j)%get(), j=1, cols)
    end do

    ! Print the outputter array
    print *, "Outputter array of trinary states:"
    write(*, "(100(I3,1X))") (outputter(j)%get(), j=1, cols)

    ! Print the 3D synapses after decay
    print *, "3D Synapses with 8 integers per cell after decay:"
    do i = 1, rows
        do j = 1, cols
            write(*, "(8I6, 1X)") (synapses(i, j, k), k=1, 8)
        end do
        print *
    end do

end program forWhoseAdvantage

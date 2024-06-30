program forWhoseAdvantage
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    integer :: rows, cols, offset, input_length
    logical :: print_synapses
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: inputter(:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: i, j, k, step, max_steps
    character(len=10) :: arg1, arg2, arg3, arg4, arg5
    integer :: ios

    ! Read command line arguments
    call get_command_argument(1, arg1)
    call get_command_argument(2, arg2)
    call get_command_argument(3, arg3)
    call get_command_argument(4, arg4)
    call get_command_argument(5, arg5)

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

    read(arg3, *, iostat=ios) offset
    if (ios /= 0) then
        print *, "Error: Invalid input for offset."
        stop
    end if

    read(arg4, *, iostat=ios) input_length
    if (ios /= 0 .or. input_length > cols) then
        print *, "Error: Invalid input for input length."
        stop
    end if

    ! Convert the boolean flag for synapse printout
    if (trim(arg5) == "true") then
        print_synapses = .true.
    else if (trim(arg5) == "false") then
        print_synapses = .false.
    else
        print *, "Error: Invalid input for synapse printout flag."
        stop
    end if

    ! Initialize the brain, inputter, outputter, and synapses
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, input_length)
    call initialize_outputter(outputter, inputter, input_length)
    call initialize_synapses(synapses, rows, cols)

    ! Main loop of the entire system
    max_steps = 100
    do step = 1, max_steps
        ! Copy non-low states from inputter to the top row of the brain matrix
        call copy_non_low_to_brain_top_row(inputter, brain, offset, size(inputter), cols)

        ! Update synapses based on brain state
        call update_brain_state_based_on_synapses(brain, synapses, rows, cols)

        ! Apply decay
        call apply_decay(synapses, rows, cols)

        ! Print the inputter array with offset alignment
        print *, "Step ", step, ": Inputter array of trinary states:"
        do j = 1, offset - 1
            write(*, "(A)", advance='no') '    '  ! 4 spaces instead of 5
        end do
        write(*, "(100(I3,1X))", advance='no') (inputter(j)%get(), j=1, input_length)
        print *

        ! Print the 2D brain
        print *, "Step ", step, ": 2D Brain of trinary states:"
        do i = 1, rows
            write(*, "(100(I3,1X))") (brain(i, j)%get(), j=1, cols)
        end do

        ! Print the outputter array with offset alignment
        print *, "Step ", step, ": Outputter array of trinary states:"
        do j = 1, offset - 1
            write(*, "(A)", advance='no') '    '  ! 4 spaces instead of 5
        end do
        write(*, "(100(I3,1X))", advance='no') (outputter(j)%get(), j=1, input_length)
        print *

        ! Print the 3D synapses after decay if flag is true
        if (print_synapses) then
            print *, "Step ", step, ": 3D Synapses with 8 integers per cell:"
            do i = 1, rows
                do j = 1, cols
                    write(*, "(8I6, 1X)") (synapses(i, j, k), k=1, 8)
                end do
                print *
            end do
        end if

        ! Pause for 1 second between steps
        call sleep(1)
    end do

end program forWhoseAdvantage

program forWhoseAdvantage
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    integer :: rows, cols
    integer :: input_offset, input_length
    integer :: output_offset, output_length
    logical :: print_synapses
    type(trinary), allocatable :: brain(:,:)
    type(trinary), allocatable :: inputter(:)
    type(trinary), allocatable :: outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: i, j, k, step, max_steps
    character(len=10) :: arg1, arg2, arg3, arg4, arg5, arg6, arg7
    integer :: ios

    ! Read command line arguments
    call get_command_argument(1, arg1)
    call get_command_argument(2, arg2)
    call get_command_argument(3, arg3)
    call get_command_argument(4, arg4)
    call get_command_argument(5, arg5)
    call get_command_argument(6, arg6)
    call get_command_argument(7, arg7)

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

    read(arg3, *, iostat=ios) input_offset
    if (ios /= 0) then
        print *, "Error: Invalid input for input offset."
        stop
    end if

    read(arg4, *, iostat=ios) input_length
    if (ios /= 0 .or. input_length > cols) then
        print *, "Error: Invalid input for input length."
        stop
    end if

    read(arg5, *, iostat=ios) output_offset
    if (ios /= 0) then
        print *, "Error: Invalid input for output offset."
        stop
    end if

    read(arg6, *, iostat=ios) output_length
    if (ios /= 0 .or. output_length > cols) then
        print *, "Error: Invalid input for output length."
        stop
    end if

    ! Convert the boolean flag for synapse printout
    if (trim(arg7) == "true") then
        print_synapses = .true.
    else if (trim(arg7) == "false") then
        print_synapses = .false.
    else
        print *, "Error: Invalid input for synapse printout flag."
        stop
    end if

    ! Validate offsets and lengths
    if (input_offset < 1 .or. input_offset + input_length - 1 > cols) then
        print *, "Error: Inputter array exceeds brain matrix dimensions."
        stop
    end if

    if (output_offset < 1 .or. output_offset + output_length - 1 > cols) then
        print *, "Error: Outputter array exceeds brain matrix dimensions."
        stop
    end if

    ! Initialize the brain, inputter, outputter, and synapses
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, input_length)
    call initialize_outputter(outputter, output_length)
    call initialize_synapses(synapses, rows, cols)

    ! Main loop of the entire system
    max_steps = 100
    do step = 1, max_steps
        ! Save and reset the outputter array
        call save_and_reset_outputter(outputter)

        ! Print the step number
        print *, "Step ", step, ":"

        ! Copy non-low states from inputter to the top row of the brain matrix
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)

        ! Update synapses based on brain state
        call update_brain_state_based_on_synapses(brain, synapses, outputter, rows, cols, input_offset, output_offset, output_length)

        ! Apply decay
        call apply_decay(synapses, rows, cols)

        ! Print the inputter array with offset alignment
        print *, "Inputter array of trinary states:"
        do j = 1, input_offset - 1
            write(*, "(A)", advance='no') '    '  ! 4 spaces
        end do
        write(*, "(100(I3,1X))", advance='no') (inputter(j)%get(), j=1, input_length)
        print *

        ! Print the 2D brain
        print *, "2D Brain of trinary states:"
        do i = 1, rows
            write(*, "(100(I3,1X))") (brain(i, j)%get(), j=1, cols)
        end do

        ! Print the outputter array with offset alignment
        print *, "Outputter array of trinary states:"
        do j = 1, output_offset - 1
            write(*, "(A)", advance='no') '    '  ! 4 spaces
        end do
        write(*, "(100(I3,1X))", advance='no') (outputter(j)%get(), j=1, output_length)
        print *

        ! Print the 3D synapses after decay if flag is true
        if (print_synapses) then
            print *, "3D Synapses with 8 integers per cell:"
            do i = 1, rows
                do j = 1, cols
                    write(*, "(8I6, 1X)") (synapses(i, j, k), k=1, 8)
                end do
                print *
            end do
        end if

        ! Pause for 1 second between steps (optional)
        call sleep(1)
    end do

end program forWhoseAdvantage

program test_transitions
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    
    integer :: rows, cols, i, j, step
    integer :: input_offset, input_length, output_offset, output_length
    type(trinary), allocatable :: brain(:,:), brain_before(:,:), inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: before_state, after_state, delta
    integer :: num_increases, num_decreases, total_increase, total_decrease
    integer :: output_increase
    logical :: valid
    
    ! Small test case
    rows = 4
    cols = 8
    input_offset = 2
    input_length = 4
    output_offset = 3
    output_length = 4
    
    ! Initialize
    call initialize_brain(brain, rows, cols)
    call initialize_inputter(inputter, input_length)
    call initialize_outputter(outputter, output_length)
    call initialize_synapses(synapses, rows, cols)
    
    allocate(brain_before(rows, cols))
    
    print *, "=== TRANSITION VALIDATION TEST ==="
    print *, "Verifying all state changes are ±1"
    print *
    
    do step = 1, 10
        ! Save brain state before input
        do i = 1, rows
            do j = 1, cols
                call brain_before(i, j)%set(brain(i, j)%get())
            end do
        end do
        
        ! Apply input (this can add +1 or +2 depending on saturation)
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Save state after input, before propagation
        do i = 1, rows
            do j = 1, cols
                call brain_before(i, j)%set(brain(i, j)%get())
            end do
        end do
        
        ! Update brain (this should only produce ±1 changes)
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        ! Validate all transitions are ±1
        num_increases = 0
        num_decreases = 0
        total_increase = 0
        total_decrease = 0
        output_increase = 0
        valid = .true.
        
        print *, "Step", step, ":"
        
        ! Check brain transitions
        do i = 1, rows
            do j = 1, cols
                before_state = brain_before(i, j)%get()
                after_state = brain(i, j)%get()
                delta = after_state - before_state
                
                if (delta /= 0) then
                    ! Check that delta is ±1
                    if (abs(delta) > 1) then
                        print *, "  *** INVALID TRANSITION at (", i, ",", j, "): ", &
                                 before_state, "->", after_state, " (delta=", delta, ")"
                        valid = .false.
                    end if
                    
                    if (delta > 0) then
                        num_increases = num_increases + 1
                        total_increase = total_increase + delta
                    else
                        num_decreases = num_decreases + 1
                        total_decrease = total_decrease + abs(delta)
                    end if
                end if
            end do
        end do
        
        ! Check output transitions
        do i = 1, output_length
            output_increase = output_increase + outputter(i)%get()
        end do
        
        if (output_increase > 0) then
            num_increases = num_increases + output_increase
            total_increase = total_increase + output_increase
        end if
        
        print *, "  Increases: ", num_increases, " cells, total: +", total_increase
        print *, "  Decreases: ", num_decreases, " cells, total: -", total_decrease
        print *, "  Output:    ", output_increase
        print *, "  Net:       ", total_increase - total_decrease
        
        if (total_increase /= total_decrease) then
            print *, "  *** CONSERVATION VIOLATED: increases /= decreases"
            valid = .false.
        end if
        
        if (valid) then
            print *, "  ✓ All transitions valid, conservation holds"
        end if
        print *
    end do
    
end program test_transitions

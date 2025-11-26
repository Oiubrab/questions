program test_single_state_propagation
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    
    integer :: rows, cols, i, j, step
    integer :: input_offset, input_length, output_offset, output_length
    type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: state_count(0:2)
    
    ! Small controlled test
    rows = 6
    cols = 6
    input_offset = 1
    input_length = 1
    output_offset = 1
    output_length = 6
    
    ! Initialize
    call initialize_brain(brain, rows, cols)
    allocate(inputter(input_length))
    call initialize_outputter(outputter, output_length)
    call initialize_synapses(synapses, rows, cols)
    
    ! Set single high state in input
    call inputter(1)%set(high)
    
    print *, "=== SINGLE STATE PROPAGATION TEST ==="
    print *, "Injecting single HIGH state at position (1,1)"
    print *
    
    do step = 1, 10
        ! Count states
        state_count = 0
        do i = 1, rows
            do j = 1, cols
                state_count(brain(i,j)%get()) = state_count(brain(i,j)%get()) + 1
            end do
        end do
        
        print *, "Step", step, ":"
        print *, "  LOW states:    ", state_count(0)
        print *, "  MEDIUM states: ", state_count(1)
        print *, "  HIGH states:   ", state_count(2)
        
        ! Print brain matrix
        print *, "  Brain matrix:"
        do i = 1, rows
            write(*, "(6X, 6(I3))") (brain(i,j)%get(), j=1,cols)
        end do
        print *
        
        ! Update
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        call apply_decay(synapses, rows, cols)
        
        ! Stop injecting after first step
        if (step == 1) then
            call inputter(1)%set(low)
        end if
    end do
    
end program test_single_state_propagation

program test_conservation
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
    integer :: total_before, total_after, input_total, output_total
    
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
    
    print *, "=== STATE CONSERVATION TEST ==="
    print *
    
    do step = 1, 5
        ! Calculate input energy
        input_total = 0
        do i = 1, input_length
            input_total = input_total + inputter(i)%get()
        end do
        
        ! Calculate brain energy before update
        total_before = 0
        do i = 1, rows
            do j = 1, cols
                total_before = total_before + brain(i, j)%get()
            end do
        end do
        
        ! Apply input
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Update brain
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        ! Calculate brain energy after update
        total_after = 0
        do i = 1, rows
            do j = 1, cols
                total_after = total_after + brain(i, j)%get()
            end do
        end do
        
        ! Calculate output energy
        output_total = 0
        do i = 1, output_length
            output_total = output_total + outputter(i)%get()
        end do
        
        print *, "Step", step
        print *, "  Input energy:        ", input_total
        print *, "  Brain before input:  ", total_before
        print *, "  Brain after update:  ", total_after
        print *, "  Output energy:       ", output_total
        print *, "  Expected (approx):   ", total_before + input_total
        print *, "  Actual (brain+out):  ", total_after + output_total
        print *, "  Difference:          ", (total_after + output_total) - (total_before + input_total)
        print *
    end do
    
end program test_conservation

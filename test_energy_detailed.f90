program test_energy_conservation_detailed
    use trinary_module
    use brain_module
    use inputter_module
    use outputter_module
    use synapses_module
    implicit none
    
    integer :: rows, cols, i, j, step
    integer :: input_offset, input_length, output_offset, output_length
    type(trinary), allocatable :: brain(:,:), brain_before_prop(:,:)
    type(trinary), allocatable :: inputter(:), outputter(:)
    integer, allocatable :: synapses(:,:,:)
    integer :: energy_m0, energy_m1, energy_m2, energy_output
    integer :: actual_injected, expected_m2
    logical :: conservation_violated
    
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
    
    allocate(brain_before_prop(rows, cols))
    
    print *, "=== DETAILED ENERGY CONSERVATION TEST ==="
    print *
    
    conservation_violated = .false.
    
    do step = 1, 8
        print *, "=============== STEP", step, "==============="
        
        ! M0: Initial matrix energy
        energy_m0 = 0
        do i = 1, rows
            do j = 1, cols
                energy_m0 = energy_m0 + brain(i, j)%get()
            end do
        end do
        print *, "M0 (matrix before input):   ", energy_m0
        
        ! Apply input addition
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! M1: Matrix energy after input
        energy_m1 = 0
        do i = 1, rows
            do j = 1, cols
                energy_m1 = energy_m1 + brain(i, j)%get()
            end do
        end do
        actual_injected = energy_m1 - energy_m0
        print *, "M1 (matrix after input):    ", energy_m1
        print *, "Actual energy injected:     ", actual_injected
        
        ! Save brain state before propagation
        brain_before_prop = brain
        
        ! Reset and propagate
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        ! M2: Matrix energy after propagation
        energy_m2 = 0
        do i = 1, rows
            do j = 1, cols
                energy_m2 = energy_m2 + brain(i, j)%get()
            end do
        end do
        
        ! Output energy gained
        energy_output = 0
        do i = 1, output_length
            energy_output = energy_output + outputter(i)%get()
        end do
        
        print *, "M2 (matrix after propagation):", energy_m2
        print *, "Output energy gained:        ", energy_output
        
        ! Check conservation: M2 should equal M1 - O
        expected_m2 = energy_m1 - energy_output
        print *, "Expected M2 (M1 - Output):   ", expected_m2
        
        if (energy_m2 /= expected_m2) then
            print *, "*** CONSERVATION VIOLATED ***"
            print *, "*** Energy lost/gained:      ", energy_m2 - expected_m2
            conservation_violated = .true.
            
            ! Show detailed state changes
            print *, "Brain state changes during propagation:"
            do i = 1, rows
                do j = 1, cols
                    if (brain(i,j)%get() /= brain_before_prop(i,j)%get()) then
                        print *, "  Cell (", i, ",", j, "):", &
                                brain_before_prop(i,j)%get(), "->", brain(i,j)%get(), &
                                " (delta=", brain(i,j)%get() - brain_before_prop(i,j)%get(), ")"
                    end if
                end do
            end do
        else
            print *, "*** CONSERVATION OK ***"
        end if
        
        print *
        
        ! Apply decay for next iteration
        call apply_decay(synapses, rows, cols)
    end do
    
    if (conservation_violated) then
        print *, "===================================="
        print *, "OVERALL: CONSERVATION VIOLATIONS DETECTED"
        print *, "===================================="
    else
        print *, "===================================="
        print *, "OVERALL: ALL CONSERVATION CHECKS PASSED"
        print *, "===================================="
    end if
    
end program test_energy_conservation_detailed

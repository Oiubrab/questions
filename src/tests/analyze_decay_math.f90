program analyze_decay_math
    implicit none
    
    real :: decay_min, decay_max, decay_avg
    real :: expected_steps
    integer :: reinforcement, delta
    integer :: step
    real :: value, initial
    
    decay_min = 0.9
    decay_max = 1.0
    decay_avg = (decay_min + decay_max) / 2.0  ! Average decay = 0.95
    
    print *, "=== MATHEMATICAL ANALYSIS OF DECAY ==="
    print *
    print *, "Decay multiplier range: [", decay_min, ",", decay_max, "]"
    print *, "Average decay multiplier:", decay_avg
    print *
    
    ! For a reinforcement of +10 on initial value 1000:
    initial = 1000.0
    reinforcement = 10
    
    print *, "Starting scenario:"
    print *, "  Initial value:     ", int(initial)
    print *, "  Reinforcement:     +", reinforcement
    print *, "  New value:         ", int(initial + reinforcement)
    print *
    
    ! Expected number of steps calculation
    ! We need: (initial + delta) * decay_avg^n <= initial
    ! So: decay_avg^n <= initial / (initial + delta)
    ! n >= log(initial / (initial + delta)) / log(decay_avg)
    
    print *, "Expected decay steps (using average decay of 0.95):"
    print *, "Delta  |  Steps to return to initial"
    print *, "-------|----------------------------"
    
    do delta = 10, 200, 10
        expected_steps = log(initial / (initial + delta)) / log(decay_avg)
        print *, delta, "   |  ", int(-expected_steps + 0.5)
    end do
    
    print *
    print *, "=== SIMULATION: Step-by-step decay with average multiplier ==="
    print *
    
    value = initial + 10.0
    print *, "Step  |  Value   |  Ratio to Initial"
    print *, "------|----------|------------------"
    write(*, '(I5, A, F9.2, A, F8.5)') 0, " | ", value, " | ", value/initial
    
    do step = 1, 10
        value = value * decay_avg
        write(*, '(I5, A, F9.2, A, F8.5)') step, " | ", value, " | ", value/initial
        if (value <= initial) then
            print *
            print *, "Returns to initial at step", step
            exit
        end if
    end do
    
    print *
    print *, "=== KEY INSIGHT ==="
    print *, "With reinforcement of +10 and average decay of 0.95:"
    print *, "  After 1 step: 1010 * 0.95 = 959.5 (BELOW initial!)"
    print *
    print *, "The decay is so aggressive that a +10 reinforcement"
    print *, "typically drops BELOW the original value in just 1 step."
    print *
    print *, "For equilibrium (return to initial in ~1 step):"
    print *, "  Required reinforcement ≈ initial * (1/decay_avg - 1)"
    print *, "  For initial=1000, decay_avg=0.95:"
    print *, "  Required ≈", int(initial * (1.0/decay_avg - 1.0))
    print *
    print *, "Current reinforcement of +10 is FAR too small!"
    
end program analyze_decay_math

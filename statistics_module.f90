! Add this module to track and report statistics
module statistics_module
    implicit none
    
contains
    
    subroutine print_statistics(brain, inputter, outputter, synapses, rows, cols, step)
        use trinary_module
        implicit none
        type(trinary), allocatable :: brain(:,:), inputter(:), outputter(:)
        integer, allocatable :: synapses(:,:,:)
        integer, intent(in) :: rows, cols, step
        integer :: i, j, k
        integer :: brain_counts(0:2), input_counts(0:2), output_counts(0:2)
        integer :: total_synapses, avg_synapse
        real :: synapse_variance
        
        ! Count brain states
        brain_counts = 0
        do i = 1, rows
            do j = 1, cols
                brain_counts(brain(i,j)%get()) = brain_counts(brain(i,j)%get()) + 1
            end do
        end do
        
        ! Count input states
        input_counts = 0
        do i = 1, size(inputter)
            input_counts(inputter(i)%get()) = input_counts(inputter(i)%get()) + 1
        end do
        
        ! Count output states
        output_counts = 0
        do i = 1, size(outputter)
            output_counts(outputter(i)%get()) = output_counts(outputter(i)%get()) + 1
        end do
        
        ! Synapse statistics
        total_synapses = 0
        do i = 1, rows
            do j = 1, cols
                do k = 1, 8
                    total_synapses = total_synapses + synapses(i,j,k)
                end do
            end do
        end do
        avg_synapse = total_synapses / (rows * cols * 8)
        
        print *, "=== STATISTICS FOR STEP", step, "==="
        print *, "Brain state counts: LOW=", brain_counts(0), &
                 " MED=", brain_counts(1), " HIGH=", brain_counts(2)
        print *, "Brain total energy:", brain_counts(1) + 2*brain_counts(2)
        print *, "Input total energy:", input_counts(1) + 2*input_counts(2)
        print *, "Output total energy:", output_counts(1) + 2*output_counts(2)
        print *, "Average synapse strength:", avg_synapse
        print *
    end subroutine print_statistics
    
    subroutine validate_state_transitions(brain_before, brain_after, rows, cols)
        use trinary_module
        implicit none
        type(trinary), allocatable :: brain_before(:,:), brain_after(:,:)
        integer, intent(in) :: rows, cols
        integer :: i, j, violations
        
        violations = 0
        do i = 1, rows
            do j = 1, cols
                ! States should only change by -1, 0, or +1
                if (abs(brain_after(i,j)%get() - brain_before(i,j)%get()) > 1) then
                    print *, "VIOLATION at (", i, ",", j, "): changed by", &
                             brain_after(i,j)%get() - brain_before(i,j)%get()
                    violations = violations + 1
                end if
            end do
        end do
        
        if (violations > 0) then
            print *, "WARNING:", violations, "state transition violations detected!"
        end if
    end subroutine validate_state_transitions
    
end module statistics_module

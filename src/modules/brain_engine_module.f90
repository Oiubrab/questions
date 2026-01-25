module brain_engine_module
    use trinary_module
    use brain_module, only: initialize_brain, update_brain_state_based_on_synapses, &
                            copy_non_low_to_brain_top_row
    use inputter_module, only: initialize_inputter
    use outputter_module, only: initialize_outputter, save_and_reset_outputter
    use synapses_module, only: initialize_synapses
    implicit none
    
contains

    ! Initialize a complete brain system with all components
    subroutine initialize_brain_system(brain, inputter, outputter, synapses, synapse_usage, &
                                       incoming_direction, rows, cols, input_length, output_length, &
                                       input_offset, output_offset)
        type(trinary), allocatable, intent(out) :: brain(:,:)
        type(trinary), allocatable, intent(out) :: inputter(:)
        type(trinary), allocatable, intent(out) :: outputter(:)
        integer, allocatable, intent(out) :: synapses(:,:,:,:)
        logical, allocatable, intent(out) :: synapse_usage(:,:,:,:)
        integer, allocatable, intent(out) :: incoming_direction(:,:,:)
        integer, intent(in) :: rows, cols, input_length, output_length
        integer, intent(in) :: input_offset, output_offset
        
        ! Initialize brain matrix
        call initialize_brain(brain, rows, cols)
        
        ! Initialize input vector
        call initialize_inputter(inputter, input_length)
        
        ! Initialize output vector
        call initialize_outputter(outputter, output_length)
        
        ! Initialize synapses (4D: row, col, incoming_dir, outgoing_dir)
        call initialize_synapses(synapses, rows, cols)
        
        ! Initialize synapse usage tracking
        allocate(synapse_usage(rows, cols, 8, 8))
        synapse_usage = .false.
        
        ! Initialize incoming direction tracking (2 incoming directions per neuron)
        allocate(incoming_direction(rows, cols, 2))
        incoming_direction = 0
        
    end subroutine initialize_brain_system
    
    ! Run one single brain propagation step
    ! 1. Copy input to brain top row (only has data on first call per Bar)
    ! 2. Clear output (set all to LOW)
    ! 3. Run ONE propagation step through brain to output
    ! 4. Clear input (so subsequent calls within Bar have no new input)
    ! Caller is responsible for:
    !   - Writing input ONCE before Bar loop starts
    !   - Reading/accumulating output after each call
    !   - Calling this multiple times per Bar
    subroutine run_brain_cycle(brain, inputter, outputter, synapses, synapse_usage, &
                               incoming_direction, rows, cols, input_offset, output_offset, &
                               output_length)
        type(trinary), allocatable, intent(inout) :: brain(:,:)
        type(trinary), allocatable, intent(inout) :: inputter(:)
        type(trinary), allocatable, intent(inout) :: outputter(:)
        integer, allocatable, intent(inout) :: synapses(:,:,:,:)
        logical, allocatable, intent(inout) :: synapse_usage(:,:,:,:)
        integer, allocatable, intent(inout) :: incoming_direction(:,:,:)
        integer, intent(in) :: rows, cols, input_offset, output_offset, output_length
        integer :: i
        
        ! Step 1: Copy input to brain top row
        call copy_non_low_to_brain_top_row(inputter, brain, incoming_direction, input_offset, size(brain, 2))
        
        ! Step 2: Clear output - set all to LOW
        do i = 1, size(outputter)
            call outputter(i)%set(low)
        end do
        
        ! Step 3: Run ONE propagation step
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   synapse_usage, incoming_direction, &
                                                   rows, cols, output_offset, output_offset, &
                                                   output_length)
        
        ! Step 4: Clear input - subsequent calls within Bar will have no new input
        do i = 1, size(inputter)
            call inputter(i)%set(low)
        end do
        
    end subroutine run_brain_cycle
    
    ! Simple brain cycle without cross-flow/pressure effects (for meta-brain)
    ! Same as run_brain_cycle but explicitly passes pressure=0.0 to disable
    ! leftward pull, vertical multipliers, and valve effects
    subroutine run_brain_cycle_simple(brain, inputter, outputter, synapses, synapse_usage, &
                                      incoming_direction, rows, cols, input_offset, output_offset, &
                                      output_length)
        type(trinary), allocatable, intent(inout) :: brain(:,:)
        type(trinary), allocatable, intent(inout) :: inputter(:)
        type(trinary), allocatable, intent(inout) :: outputter(:)
        integer, allocatable, intent(inout) :: synapses(:,:,:,:)
        logical, allocatable, intent(inout) :: synapse_usage(:,:,:,:)
        integer, allocatable, intent(inout) :: incoming_direction(:,:,:)
        integer, intent(in) :: rows, cols, input_offset, output_offset, output_length
        integer :: i
        
        ! Step 1: Copy input to brain top row
        call copy_non_low_to_brain_top_row(inputter, brain, incoming_direction, input_offset, size(brain, 2))
        
        ! Step 2: Clear output - set all to LOW
        do i = 1, size(outputter)
            call outputter(i)%set(low)
        end do
        
        ! Step 3: Run ONE propagation step WITHOUT pressure effects (pressure=0.0)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   synapse_usage, incoming_direction, &
                                                   rows, cols, output_offset, output_offset, &
                                                   output_length, pressure=0.0)
        
        ! Step 4: Clear input - subsequent calls within Bar will have no new input
        do i = 1, size(inputter)
            call inputter(i)%set(low)
        end do
        
    end subroutine run_brain_cycle_simple
    
end module brain_engine_module

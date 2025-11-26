program test_visual_diff
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
    integer :: before_state, after_state
    character(len=10) :: color_code
    
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
    
    print *, "=== VISUAL DIFF TEST ==="
    print *, "Legend: →↑ (increased), →↓ (decreased), →= (unchanged)"
    print *
    
    do step = 1, 5
        ! Save brain state before update
        do i = 1, rows
            do j = 1, cols
                call brain_before(i, j)%set(brain(i, j)%get())
            end do
        end do
        
        ! Apply input
        call copy_non_low_to_brain_top_row(inputter, brain, input_offset, cols)
        
        ! Update brain
        call save_and_reset_outputter(outputter)
        call update_brain_state_based_on_synapses(brain, synapses, outputter, &
                                                   rows, cols, input_offset, &
                                                   output_offset, output_length)
        
        print *, "=============== STEP", step, "==============="
        print *
        print *, "Before propagation:"
        do i = 1, rows
            write(*, '(A)', advance='no') "  "
            do j = 1, cols
                write(*, '(I3)', advance='no') brain_before(i, j)%get()
            end do
            print *
        end do
        print *
        
        print *, "After propagation:"
        do i = 1, rows
            write(*, '(A)', advance='no') "  "
            do j = 1, cols
                write(*, '(I3)', advance='no') brain(i, j)%get()
            end do
            print *
        end do
        print *
        
        print *, "Changes (before→after):"
        do i = 1, rows
            write(*, '(A)', advance='no') "  "
            do j = 1, cols
                before_state = brain_before(i, j)%get()
                after_state = brain(i, j)%get()
                
                if (after_state > before_state) then
                    ! Increase
                    write(*, '(I1,A,I1,A)', advance='no') before_state, '↑', after_state, ' '
                else if (after_state < before_state) then
                    ! Decrease
                    write(*, '(I1,A,I1,A)', advance='no') before_state, '↓', after_state, ' '
                else
                    ! Unchanged
                    write(*, '(I1,A,I1,A)', advance='no') before_state, '=', after_state, ' '
                end if
            end do
            print *
        end do
        print *
        
        print *, "Output state:"
        write(*, '(A)', advance='no') "  "
        do i = 1, output_length
            write(*, '(I3)', advance='no') outputter(i)%get()
        end do
        print *
        print *
    end do
    
end program test_visual_diff

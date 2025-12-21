program create_hunting_brain
    implicit none
    
    integer, parameter :: rows = 6, cols = 12
    integer :: synapses(rows, cols, 8)
    integer :: i, j, k, unit
    
    print *, "Creating a hand-crafted hunting brain..."
    
    ! Initialize all synapses to minimum
    synapses = 25
    
    ! Create direct vision-to-movement mapping
    ! Vision slice 1 (0-60°) -> Move right-down (direction 8)
    ! Vision slice 2 (60-120°) -> Move down (direction 7) 
    ! Vision slice 3 (120-180°) -> Move left-down (direction 6)
    ! Vision slice 4 (180-240°) -> Move left (direction 4)
    ! Vision slice 5 (240-300°) -> Move left-up (direction 1)
    ! Vision slice 6 (300-360°) -> Move up-right (direction 3)
    
    ! Strong pathways from vision inputs (top row) to outputs (bottom row)
    do i = 1, rows-1
        ! Propagate downward strongly
        do j = 1, cols
            synapses(i, j, 7) = 1500  ! Down direction
            synapses(i, j, 6) = 800   ! Down-left  
            synapses(i, j, 8) = 800   ! Down-right
        end do
    end do
    
    ! Bottom row: vision slice to movement direction mapping
    ! Each vision column biases toward appropriate movement
    if (cols >= 6) then
        ! Vision slice 1 -> right-down movement
        synapses(rows, 1, 8) = 2000
        synapses(rows, 1, 5) = 1000  ! Also some right
        
        ! Vision slice 2 -> down movement  
        synapses(rows, 2, 7) = 2000
        synapses(rows, 2, 8) = 800
        synapses(rows, 2, 6) = 800
        
        ! Vision slice 3 -> left-down movement
        synapses(rows, 3, 6) = 2000  
        synapses(rows, 3, 4) = 1000  ! Also some left
        
        ! Vision slice 4 -> left movement
        synapses(rows, 4, 4) = 2000
        synapses(rows, 4, 1) = 800   ! Also left-up
        synapses(rows, 4, 6) = 800   ! Also left-down
        
        ! Vision slice 5 -> left-up movement
        synapses(rows, 5, 1) = 2000
        synapses(rows, 5, 2) = 1000  ! Also some up
        
        ! Vision slice 6 -> right-up movement
        synapses(rows, 6, 3) = 2000
        synapses(rows, 6, 5) = 1000  ! Also some right
    end if
    
    ! Save the hand-crafted brain
    open(newunit=unit, file="hunting_brain.dat", form='unformatted', access='stream')
    write(unit) rows, cols
    do i = 1, rows
        do j = 1, cols
            do k = 1, 8
                write(unit) synapses(i, j, k)
            end do
        end do
    end do
    close(unit)
    
    print *, "Hand-crafted hunting brain saved as hunting_brain.dat"
    print *, "This brain directly maps vision slices to hunting movements"
    
end program
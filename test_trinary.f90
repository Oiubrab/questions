program test_trinary
    use trinary_module
    implicit none
    type(trinary) :: cell
    integer :: failures
    
    failures = 0
    
    ! Test 1: Set and get low
    call cell%set(low)
    if (cell%get() /= low) then
        print *, "FAIL: set/get low"
        failures = failures + 1
    else
        print *, "PASS: set/get low"
    end if
    
    ! Test 2: Set and get medium
    call cell%set(medium)
    if (cell%get() /= medium) then
        print *, "FAIL: set/get medium"
        failures = failures + 1
    else
        print *, "PASS: set/get medium"
    end if
    
    ! Test 3: Set and get high
    call cell%set(high)
    if (cell%get() /= high) then
        print *, "FAIL: set/get high"
        failures = failures + 1
    else
        print *, "PASS: set/get high"
    end if
    
    ! Test 4: Shift up from low
    call cell%set(low)
    call cell%shift(up)
    if (cell%get() /= medium) then
        print *, "FAIL: shift up from low"
        failures = failures + 1
    else
        print *, "PASS: shift up from low"
    end if
    
    ! Test 5: Shift up from medium
    call cell%set(medium)
    call cell%shift(up)
    if (cell%get() /= high) then
        print *, "FAIL: shift up from medium"
        failures = failures + 1
    else
        print *, "PASS: shift up from medium"
    end if
    
    ! Test 6: Shift up from high (should cap)
    call cell%set(high)
    call cell%shift(up)
    if (cell%get() /= high) then
        print *, "FAIL: shift up from high (cap)"
        failures = failures + 1
    else
        print *, "PASS: shift up from high (cap)"
    end if
    
    ! Test 7: Shift down from high
    call cell%set(high)
    call cell%shift(down)
    if (cell%get() /= medium) then
        print *, "FAIL: shift down from high"
        failures = failures + 1
    else
        print *, "PASS: shift down from high"
    end if
    
    ! Test 8: Shift down from low (should floor)
    call cell%set(low)
    call cell%shift(down)
    if (cell%get() /= low) then
        print *, "FAIL: shift down from low (floor)"
        failures = failures + 1
    else
        print *, "PASS: shift down from low (floor)"
    end if
    
    print *
    if (failures == 0) then
        print *, "ALL TESTS PASSED"
    else
        print *, "FAILURES:", failures
    end if
    
end program test_trinary

module random_utils
    implicit none
contains
    ! Function to clamp values between 0 and 1
    real function clamp(val)
        real, intent(in) :: val
        clamp = max(0.0, min(1.0, val))
    end function clamp

    ! Arbitrary function that modifies array elements
    real function modify(val)
        real, intent(in) :: val
        modify = clamp(val*0.9)
    end function modify
end module random_utils

program test_random_array
    use random_utils
    use iso_fortran_env, only: real32
    implicit none
    real(real32) :: arr(8)
    integer :: i, j

    ! Initialize the random number generator with system clock
    call random_seed()

    ! Initialize the array with random values between 0 and 1
    call random_number(arr)

    ! Print the initial array
    do i = 1, 8
        write(*,'(F6.3,1X)', advance='no') arr(i)
    end do
    write(*,*)   ! Move to the next line

    ! Main loop for arbitrary function application
    do j = 1, 10   ! Example: run the modification 10 times
        do i = 1, 8
            arr(i) = modify(arr(i))   ! Apply the arbitrary function and modify the array
        end do

        ! Print the array
        do i = 1, 8
            write(*,'(F6.3,1X)', advance='no') arr(i)
        end do
        write(*,*)   ! Move to the next line
    end do

end program test_random_array

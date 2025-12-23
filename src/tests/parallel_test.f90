program parallel_test
    use omp_lib
    implicit none
    integer :: nthreads, tid, i

    ! Begin a parallel region with each thread having its own private tid variable
    !$omp parallel private(tid)

    ! Each thread executes this block independently
    tid = omp_get_thread_num()
    nthreads = omp_get_num_threads()
    print *, 'Hello from thread ', tid, ' out of ', nthreads

    ! End of the parallel region
    !$omp end parallel

    ! Example of a parallel loop where iterations are distributed among threads
    !$omp parallel do
    do i = 1, 10
        print *, 'Iteration ', i, ' executed by thread ', omp_get_thread_num()
    end do
    !$omp end parallel do

end program parallel_test

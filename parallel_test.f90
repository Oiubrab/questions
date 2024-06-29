program parallel_test
    use omp_lib
    implicit none
    integer :: nthreads, tid, i

    ! Print the number of threads
    !$omp parallel private(tid)
    tid = omp_get_thread_num()
    nthreads = omp_get_num_threads()
    print *, 'Hello from thread ', tid, ' out of ', nthreads
    !$omp end parallel

    ! Example of a parallel loop
    !$omp parallel do
    do i = 1, 10
        print *, 'Iteration ', i, ' executed by thread ', omp_get_thread_num()
    end do
    !$omp end parallel do

end program parallel_test

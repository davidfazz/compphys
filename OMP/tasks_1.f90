program tasks
  use omp_lib
  implicit none
  integer :: index, copy, count

  count = 25

!$omp parallel shared(copy)
!$omp single

  do index=1,count
    copy = index

!$omp task firstprivate(copy)
    call waitprint(copy)
!$omp end task
  enddo

!$omp taskwait
!$omp end single
!$omp end parallel

end program tasks

subroutine waitprint(copy)
  use omp_lib
  implicit none
  integer :: copy

  call sleep(1)
  print *,omp_get_thread_num(),copy
  return
end subroutine waitprint
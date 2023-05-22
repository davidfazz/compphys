program tasks
  use omp_lib
  implicit none
  double precision  :: a, b, c
  integer :: count

  count = 6

!$omp parallel
!$omp single

!$omp task depend(out:a)
    a = 5.d0
    print *,'a is',a,omp_get_thread_num()
!$omp end task

!$omp task depend(out:b)
    b = 2.d0
    print *,'b is',b,omp_get_thread_num()
!$omp end task

!$omp task depend(in:a,b)
    c = a*b
    print *,'c is',c,omp_get_thread_num()
!$omp end task

!$omp end single
!$omp end parallel

end program tasks

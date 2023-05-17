! sherrington kirkpatrick model:

program skmod 
  use omp_lib 
  implicit none 
  double precision, dimension(:,:), allocatable :: J
  double precision, dimension(:), allocatable :: s, sdummy 
  double precision  :: tstart, tend, random, energy 
  integer :: i, k, isize, ncfgs 

! set array size:

  isize = 1000

! allocate space for arrays:

  allocate(J(isize,isize))
  allocate(s(isize))
  allocate(sdummy(isize))

! fill arrays mata and matb with random numbers:

  J = 0.d0
  do i=1,isize 
    do k=i+1,isize 
      call random_number(random)
      J(i,k) = random 
      J(k,i) = random
    enddo 
    call random_number(random)
    s(i) = random
  enddo 

! calculate ncfgs configurations:

!$ tstart = omp_get_wtime()

  ncfgs = 1000
  do i=1,ncfgs

! calculate energy:

    do k=1,isize 
      sdummy(k) = dot_product(J(k,:),s(:))
    enddo 
    energy = dot_product(s,sdummy)
  enddo

!$ tend = omp_get_wtime()
!$ print *,tend - tstart

end program skmod
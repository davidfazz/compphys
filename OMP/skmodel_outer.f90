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
  enddo 

! calculate ncfgs configurations:

!$ tstart = omp_get_wtime()

  ncfgs = 1000

!$omp parallel do private(energy, s, sdummy)

  do i=1,ncfgs
    call random_number(random)
    do k=1,isize 
      s(k) = random 
    enddo

! calculate energy:

    do k=1,isize 
      sdummy(k) = dot_product(J(k,:),s(:))
    enddo 
    energy = dot_product(s,sdummy)
  enddo

!$omp end parallel do

!$ tend = omp_get_wtime()
!$ print *,tend - tstart

end program skmod
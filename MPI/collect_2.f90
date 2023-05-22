program pi
  use mpi
  use ifport
  implicit none
	double precision	:: x, y, t1, t2
	integer	:: inside, nprocs, nrank, allinside, ierr, i, iter

! initialize mpi env:

	call mpi_init(ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)
	call mpi_comm_size(mpi_comm_world,nprocs,ierr)


! initialize random number generator:

  call srand(12345 + nrank*4321)

	t1 = mpi_wtime()
	inside = 0
  iter = huge(iter)
	do i=1,iter/nprocs
    x = rand()
    y = rand()
		if (sqrt(x*x + y*y) .le. 1.d0) then
			inside = inside + 1
		endif
	enddo

	call mpi_reduce(inside,allinside,1,mpi_integer,mpi_sum,0,mpi_comm_world,ierr)

	t2 = mpi_wtime()

	if (nrank .eq. 0) then
		print *,'pi is approximately',4.d0*dble(allinside)/dble(iter)
		print *,'it took',t2-t1,'seconds to calculate.'
	endif
	call mpi_finalize(ierr)

end program pi

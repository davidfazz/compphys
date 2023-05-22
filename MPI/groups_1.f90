program group
  use mpi
  integer, parameter  :: nprocs = 8
	integer :: nrank, newrank, sendbuf ,recvbuf, nthreads
	integer :: ierr, orig_group, new_group
	integer	:: new_comm,newranks
  integer, dimension(4) :: ranks1, ranks2 

! initialize ranks:

  ranks1 = [ 0, 1, 2, 3 ]
  ranks2 = [ 4, 5, 6, 7 ]

! initialize mpi env:

	call mpi_init(ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)
	call mpi_comm_size(mpi_comm_world,nthreads,ierr)

! example is only for 8 processes:

	if (nthreads .ne. nprocs) then
		print *,'Must specify nprocs= ',nprocs,' Terminating.'
		call mpi_finalize(ierr)
		stop
	endif

	sendbuf = nrank

! extract the original group handle:

	call mpi_comm_group(mpi_comm_world,orig_group,ierr)

! divide tasks into two distinct groups based upon rank

	if (nrank .lt. nprocs/2) then
		call mpi_group_incl(orig_group,nprocs/2,ranks1,new_group,ierr)
	else
		call mpi_group_incl(orig_group,nprocs/2,ranks2,new_group,ierr)
	endif

! create new communicator and then perform collective communications

	call mpi_comm_create(mpi_comm_world,new_group,new_comm,ierr)
	call mpi_allreduce(sendbuf,recvbuf,1,mpi_integer,mpi_sum,new_comm,ierr)

! get rank in new group
	
	call mpi_group_rank(new_group,new_rank,ierr)
	print *, 'rank= ',nrank,' newrank= ',new_rank,' recvbuf= ', recvbuf

	call mpi_finalize(ierr)
	
end program group
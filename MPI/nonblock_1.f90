program nonblock
  use mpi
  implicit none
	integer	:: nthreads, rank, next, prev, tag1, tag2, ierr
  integer, dimension(2) :: buf 
  integer, dimension(4) :: reqs
  integer, dimension(mpi_status_size,4) :: stats 
  integer, dimension(mpi_status_size) :: stat

	tag1 = 1
	tag2 = 2

	call mpi_init(ierr)
	call mpi_comm_rank(mpi_comm_world,rank,ierr)
	call mpi_comm_size(mpi_comm_world,nthreads,ierr)

! determine left and right neighbors
	
	prev = rank - 1
	next = rank + 1
	if (rank .eq. 0) then
		prev = nthreads - 1
	endif
	if (rank .eq. nthreads - 1) then
		next = 0
	endif

! post non-blocking receives and sends for neighbors

	call mpi_irecv(buf(1),1,mpi_integer,prev,tag1,mpi_comm_world,reqs(1),ierr)
	call mpi_irecv(buf(2),1,mpi_integer,next,tag2,mpi_comm_world,reqs(2),ierr)

	call mpi_isend(rank,1,mpi_integer,prev,tag2,mpi_comm_world,reqs(3),ierr)
	call mpi_isend(rank,1,mpi_integer,next,tag1,mpi_comm_world,reqs(4),ierr)

	!call mpi_recv(buf(1),1,mpi_integer,prev,tag1,mpi_comm_world,stat,ierr)
	!call mpi_recv(buf(2),1,mpi_integer,next,tag2,mpi_comm_world,stat,ierr)

	!call mpi_send(rank,1,mpi_integer,prev,tag2,mpi_comm_world,ierr)
	!call mpi_send(rank,1,mpi_integer,next,tag1,mpi_comm_world,ierr)

! do some work while sends/receives progress in background

! wait for all non-blocking operations to complete

	call mpi_waitall(4,reqs,stats,ierr)

! continue - do more work

	print *,rank,buf

	call mpi_finalize(ierr)

end program nonblock


program scatter
  use mpi
  implicit none
	integer, parameter  :: size = 4
	integer :: nprocs, rank, sendcount, recvcount, source, ierr
	double precision, dimension(size,size) :: sendbuf
	double precision, dimension(size) :: recvbuf

! define array:
		
  sendbuf(:,1) = [ 1.d0, 5.d0, 9.d0, 13.d0 ]
  sendbuf(:,2) = [ 2.d0, 6.d0, 10.d0, 14.d0 ]
  sendbuf(:,3) = [ 3.d0, 7.d0, 11.d0, 15.d0 ]
  sendbuf(:,4) = [ 4.d0, 8.d0, 12.d0, 16.d0 ]

! initialize mpi env:

	call mpi_init(ierr)
	call mpi_comm_rank(mpi_comm_world,rank,ierr)
	call mpi_comm_size(mpi_comm_world,nprocs,ierr)

	if (nprocs .eq. size) then

! define source task and elements to send/receive, then perform collective 
! scatter

		source = 0
		sendcount = size
		recvcount = size
		call mpi_scatter(sendbuf,sendcount,mpi_double,recvbuf,recvcount,mpi_double,&
										 source,mpi_comm_world,ierr)

		print *, 'rank= ',rank,' results: ',recvbuf
	else
		print *, 'Must specify',size,' processors.  Terminating.'
	endif

	call mpi_finalize(ierr)

end program scatter

program p2p
  use mpi
  implicit none
	integer	:: ierr, aerr, nrank, nprocs
	integer	:: dest, source, tag, comm
  integer, dimension(mpi_status_size)	:: stat
	character :: inmsg, outmsg

! initialize mpi environment:

	call mpi_init(ierr)
	call mpi_comm_size(mpi_comm_world,nprocs,ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)
  comm = mpi_comm_world

! task 0 sends to task 1 and waits to receive a return message

  inmsg = 'y'
	outmsg = 'x'
	tag = 1
	if (nrank .eq. 0) then
		dest = 1
		source = 1
		call mpi_send(outmsg,1,mpi_character,dest,tag,comm,ierr)
	elseif (nrank .eq. 1) then
		dest = 0
		source = 0
		call mpi_recv(inmsg,1,mpi_character,source,tag,comm,stat,ierr)
	endif

! print variables:

  print *,nrank,outmsg,inmsg

! close mpi environment

	call mpi_finalize(ierr)

end program p2p

! mpi_get_count <> number of received elements (integer) 
! notes: If the size of the datatype is zero, this routine will return a count 
! of zero. If the amount of data in status is not an exact multiple of the size 
! of datatype (so that count would not be integral), a count of MPI_UNDEFINED 
! is returned instead.

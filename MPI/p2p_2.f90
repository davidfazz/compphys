program p2p
  use mpi
  implicit none
	integer :: msg, nsg, nrank, nprocs, ierr, prev, next
	integer, dimension(mpi_status_size)	:: stat

! initialize mpi environment:

	call mpi_init(ierr)
	call mpi_comm_size(mpi_comm_world,nprocs,ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)

! send message:

  msg = nrank 

! previous and next thread in circle:

  prev = nrank - 1
  if (prev .lt. 0) then
    prev = nprocs - 1
  endif 

  next = nrank + 1
  if (next .eq. nprocs) then 
    next = 0 
  endif

! send messages in a circle:

  if (mod(nrank,2) .eq. 0) then 
    call mpi_send(msg,1,mpi_integer,next,next,mpi_comm_world,ierr)
    call mpi_recv(nsg,1,mpi_integer,prev,nrank,mpi_comm_world,stat,ierr)
  else 
		call mpi_recv(nsg,1,mpi_integer,prev,nrank,mpi_comm_world,stat,ierr)
    call mpi_send(msg,1,mpi_integer,next,next,mpi_comm_world,ierr)
  endif 

  print *,'thread',nrank,'received',nsg,'from thread',prev

  call mpi_finalize(ierr)
  
end program p2p

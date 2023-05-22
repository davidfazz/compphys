program bcast
  use mpi
  implicit none
  double precision  :: tstart, tend
	integer	:: nthreads, nrank, ierr, msg, i
	integer :: req
	integer :: stat(mpi_status_size)
  logical :: flag

! intialize mpi env:

	call mpi_init(ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)
	call mpi_comm_size(mpi_comm_world,nthreads,ierr)

  tstart = mpi_wtime()

! determine left and right neighbors
	
	if (nrank .eq. 0) then 
    msg=-1
		do i=1,nthreads-1
			call mpi_isend(i,1,mpi_integer,i,i,mpi_comm_world,req,ierr)
      !do 
      !  call mpi_test(req,flag,stat,ierr)
      !  if (flag) then 
      !   exit 
      !  endif 
      !enddo
    enddo 
  else 
    call mpi_irecv(msg,1,mpi_integer,0,nrank,mpi_comm_world,req,ierr)
  endif 


  call mpi_wait(req,stat,ierr)
  call mpi_barrier(mpi_comm_world,ierr)
  tend = mpi_wtime()

  if (nrank .eq. 0) then
    print *,nrank,msg,tend-tstart
  endif 
  
	call mpi_finalize(ierr)

end program bcast


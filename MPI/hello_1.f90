program hello
  use mpi
  implicit none
	integer	:: ierr
	integer	:: nrank
  integer	:: nprocs

! initialize mpi environment:

	call mpi_init(ierr)
	call mpi_comm_size(mpi_comm_world,nprocs,ierr)
	call mpi_comm_rank(mpi_comm_world,nrank,ierr)

	print *,'hello from thread:',nrank,'of',nprocs

	call mpi_finalize(ierr)
	
end program hello

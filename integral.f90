program numint 
  use omp_lib
  implicit none
  double precision  :: xstart, xend, x, width, integral 
  double precision  :: tstart, tend
  integer :: nbins, i 

! initialize limits of integral, number of bins, and bin width:

  xstart = 0.d0 
  xend = 1.d0
  nbins = 1000000
  width = (xend - xstart) / dble(nbins)

! perform numerical integration:

!$ tstart = omp_get_wtime()

  integral = 0.d0 
  do i=1,nbins 
    x = dble(i-1)*width + width/2.d0
    integral = integral + func(x)*width
  enddo
  print *, integral

!$ tend = omp_get_wtime()
!$ print *, tstart-tend

contains 

  function func(x)
    implicit none 
    double precision, intent(in)  :: x 
    double precision  :: func 

    func = 4.d0 / (1.d0 + x*x)
    return 
  end function func

end program numint

program list
  use omp_lib
  implicit none
	type node
		integer :: value
		type (node), pointer :: next
	end type node

	type (node), pointer  :: t
	integer :: number,ios,i

	call srand(12345)

	nullify (t)
	do i=1,10
	  number = int(rand()*1000.d0)
	  call insert(t, number)
  end do

  call printer(t)

  call multiply_list(t)

  call printer(t)


contains

  recursive subroutine insert(t,number)
    type (node), pointer :: t
    integer, intent (in) :: number

    if (.not. associated (t)) then
      allocate(t)
      t%value = number
      nullify (t%next)
    else
      call insert(t%next,number)
    endif

    return
  end subroutine insert

  recursive subroutine printer(t)
    type (node), pointer :: t

    if (associated(t)) then
      print *,t%value
      call printer(t%next)
    endif
    return
  end subroutine printer

  subroutine multiply_list(head)
    type (node), pointer :: p,head

!$omp parallel private(p)
!$omp single

    p => head
    do
      !$omp task
      p%value = omp_get_thread_num()
      !$omp end task
      p => p%next

      if (.not. associated(p)) then
        exit
      endif
    enddo

!$omp end single
!$omp end parallel

  end subroutine multiply_list

end program list



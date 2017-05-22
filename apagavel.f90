program main
  integer n
  integer, allocatable :: m(:)

  read(*,*) n
  allocate(m(n))

  do i=1, n
     read(*,*) r
     m(i) = r
  end do

  print *, m
end program main

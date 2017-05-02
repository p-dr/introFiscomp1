program main
  real*8 ref, h
  !cuidado com constantes nao double
  
  ref = 4.d0/65.d0 * (dexp(2.d0) * (dsin(.25d0)+8*dcos(.25d0))-8)
!  print*, REF, NEW_LINE('C'), trap(1.d0, 0.000001d0), NEW_LINE('C'), simp(1.d0, 0.000001d0)
  print*, 2**13
  do j=1, 13
     
     h=1.d0/2.d0**j
     print*, h, dabs(ref-trap(h)), dabs(ref-simp(h))
!     print*, h, trap(h), simp(h)
  end do
  
contains

  real*8  function f(x)
    real*8 x
    f = dexp(2*x)*dcos(x/4.d0)
    return
  end function f
  
real*8  function trap(h)
  real*8 h

  trap=0.
  do i=1, 1/h-1
     trap = trap + f(i*h)
  end do

  trap = h * (trap + 0.5d0 * (f(0.0d0) + f(1.d0)))
  return
end function trap


real*8  function simp(h)
  real*8 h

  simp=0.
  do i=1, 1/h-1
     simp = simp + (3+(-1)**(i+1)) *f(i*h)
  end do

  simp = h/3.d0 * (simp + (f(0.0d0) + f(1.d0)))
  return
end function simp


end program main
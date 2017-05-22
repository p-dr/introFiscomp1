program main
  real*8 v, t, rho, x
  integer*8 col, i
  real*8, parameter :: P=400.d0, m=70, dt=.1d0, tmax = 300, v0 = 4.d0
  real*8 res(int(tmax/dt), 12)
  rho = 0
  v = v0
  x = 0
  rho =0
  col=2
  !  Print*, p,rho,a,m,dt
!  print *, (2*p/rho/a)**(1.d0/3.d0) !velocidade terminal para a=.333


!- escreve tempo e velocidades exatas ----
  do i = 1, int(tmax/dt)
     res(i,1) = i*dt
!     res(i,2) = v
!    v = sqrt(v0**2+2*P*i*dt/m)
  end do
  i=1
  v=v0
!----------------------------------------

  call writecol(col, .333d0)
 
  ! do col=2, 12
  !    call writecol(col, dfloat(col-2))
  !    print*, dfloat(col-2)
  ! end do

!====================  Gravação ========================
  
  open(10, file='c.dat')
  do i = 1, int(tmax/dt)
     do j=1, 12
        write(10,'(T1,F25.16)', advance='no') res(i,j)
     end do
     write(10,*)
  end do
  close(10)
  
!=======================================================

contains

  subroutine writecol(col, a)
    integer*8 col
    real*8 a
    
    do i = 1, int(tmax/dt)
       res(i, col) = v
       v = v + P/(m*v)*dt - (rho*a*v**2)/(2*m) * dt
       x = x+v*dt
    end do
    Print*,"x, a", x, a
    i=1
    v=v0
    x=0
  end subroutine writecol
  
end program main

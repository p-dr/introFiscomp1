program main
  real*8 teta, oldteta, w, e
  real*8, parameter :: pi=4*datan(1.d0), dt=.005d0, g=10d0
  character(len=20) :: label
  label = '\xq\f{} = **** \xp'

!  do i=10, 45, 5
     call zerar() 

!     write(label(11:14), '(F4.2)') teta/pi
!     print*, 'label, vx, vy', label, vx, vy

     open(10, file='./dat/teta')
     open(20, file='./dat/e')

     do i=0, 20.d0/dt
        write(10,*) i * dt, teta/pi
        write(20,*) i * dt, e
        oldteta = teta
        teta = teta + w * dt
        w = w - g * dsin(oldteta) * dt
        e = .5d0 * w ** 2 + g * (1.d0-dcos(teta))
     end do
     close(10)
!end do

contains
  
subroutine zerar()
  teta = pi/6.d0
  w = 0
  e = g*(1-dcos(teta))
end subroutine zerar

end program main

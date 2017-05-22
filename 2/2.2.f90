program main2

!  implicit none

  REAL ran2, dt, tmax
  INTEGER A, ats,n , decaidos, r, n0, j, sumats, tesc, ri, pos
  integer, allocatable :: mat(:,:), hist(:,:)
  real, allocatable :: med(:), desvpad(:), desvpadmed(:)
  logical inedito

  a = 12345
  r = 1000
  decaidos = 0
  tmax = 8
  dt = 0.01
  n0=1000
  ats = n0 !quantos atomos temos
  sumats = 0
 
  allocate(mat(tmax/dt, r))
  allocate(med(tmax/dt))
  allocate(desvpad(tmax/dt))
  allocate(desvpadmed(tmax/dt))

  
  do j=1, r
     
     do t=1, tmax/dt

        do n=1, ats
           if (ran2(a) .lt. dt) then
              decaidos = decaidos + 1
              !print*, 'o atomo', n,'decaiu no tempo', t
           end if
        end do

        ats = ats - decaidos
        !if (decaidos /= 0) print*, 'agora temos', ats, 'atomos'
        mat(t, j) = ats
 !       print*, '----------------'
        decaidos = 0
     end do

     ats = n0
  end do

  open(10, file='mat.txt')
  write(10,*) mat
  close(10)

!! printa primeira coluna
!  
!  do t=dt, tmax, dt
!     print*, mat(t/dt,1)
!  end do

  !calcular média
  nominador = 0
  
  do i=1, tmax/dt
     do j=1, r
        sumats = sumats + mat(i, j)
!        print*, sumats
     end do

     med(i) = dfloat(sumats)/dfloat(r)
     sumats = 0

     do j=1, r
        nominador = nominador + (mat(i, j)-med(i))**2
        
     end do

     desvpad(i) = sqrt(dfloat(nominador)/dfloat(r))
     desvpadmed(i) = dfloat(nominador)/(dfloat(r)*med(i))
     nominador = 0

  end do

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! ! open(20, file='med.dat')    !
  ! ! do i=1, size(med)           !
  ! !    write(20,*) i*dt, med(i) !
  ! ! end do                      !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! open(30, file='desvpad.dat')
  ! do i=1, size(desvpad)
  !    write(30,*) i*dt, desvpad(i)
  ! end do

  ! open(40, file='desvpadmed.dat')
  ! do i=1, size(desvpadmed)
  !    write(40,*) i*dt, desvpadmed(i)
  ! end do
  
  ! close(20)
  ! close(30)
  ! close(40)

  ! deallocate(mat)

  !=========== HISTOGRAMA =============!
  
  allocate(hist(2,r))

do tesc= 100, 800, 100 ! loop mosntruoso

   !-------- limpa lixo ----------!
  do i=1, r
     hist(1,i) = 0
     hist(2,i) = 0
  end do
  !------------------------------!
  
  POS = 1
  inedito = .true.

  ! tesc é a posição do tempo escolhido
  ! (ex. se tesc = 5, teremos os dados
  ! referentes ao quinto instante:
  !       (5 * dt) = 0.5)

  do ri = 1, r
     do i =1, r
        if (mat(tesc, ri) .eq. hist(1,i)) then
           hist(2,i) = hist(2,i)+1
           inedito = .false.
           !print*, mat(tesc, ri), 'encontrado pela vez', hist(2,i)
           exit
        end if
     end do

     if (inedito) then
        hist(1, pos) = mat(tesc, ri)
        hist(2, pos) = 1
        pos = pos+1
        !print*, mat(tesc, ri), 'é inédito'
     end if

     inedito=.true.
  end do

  open(50, file='hist'//char(tesc/100+48)//'.dat')
  !nomear arquivos adicionando índices de 1 a 8
  do i=1, r
     if(hist(2,i)/=0) then
        write(50,*) hist(1,i), hist(2,i)
     end if
  end do
  close(50)

end do


!!!!!!!!
  do i=1, r
     print*, '------',mat(100,i)
  end do
  !!!!!
!print*, hist
end program main2



SUBROUTINE PARKMILLER(R)
  
  INTEGER*16 :: R
  INTEGER*16, parameter :: A = 7**5

  R = MOD(A*R, 2147483647)

END SUBROUTINE PARKMILLER



FUNCTION ran2(idum)
  INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
  REAL ran2,AM,EPS,RNMX
  PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1, &
       IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
       NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
  INTEGER idum2,j,k,iv(NTAB),iy
  SAVE iv,iy,idum2
  DATA idum2/123456789/, iv/NTAB*0/, iy/0/
  if (idum.le.0) then
     idum=max(-idum,1)
     idum2=idum
     do j=NTAB+8,1,-1
        k=idum/IQ1
        idum=IA1*(idum-k*IQ1)-k*IR1
        if (idum.lt.0) idum=idum+IM1
        if (j.le.NTAB) iv(j)=idum
     end do
     iy=iv(1)
  endif
  k=idum/IQ1
  idum=IA1*(idum-k*IQ1)-k*IR1
  if (idum.lt.0) idum=idum+IM1
  k=idum2/IQ2
  idum2=IA2*(idum2-k*IQ2)-k*IR2
  if (idum2.lt.0) idum2=idum2+IM2
  j=1+iy/NDIV
  iy=iv(j)-idum2
  iv(j)=idum
  if(iy.lt.1)iy=iy+IMM1
  ran2=min(AM*iy,RNMX)
  return
END function ran2

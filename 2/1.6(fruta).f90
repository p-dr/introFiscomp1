program media
  implicit none
  integer*16 i, J,n      
  parameter (n=100)  
real*16 lista(n),ma,mg,dp
!!!!!!!!!!!!!!!!!!!
DO J=82, 86
   
  open(10,file=CHAR(J)//'.dat') 
  do i=1,n
     read(10,*)lista(i)
  enddo
  close(10)

!!!!!!!!!!!!!!!!!!!111
!  write(*,*)lista
  PRINT*,'====================================================='
  print*, 'SERIE '//CHAR(J)
  PRINT*,'-----------------------------------------------------'
  call calcdp(dp,lista,n)
  write(*,*)"DESVIO PADRAO",dp
  
  call calcma(ma,lista,n) 
  write(*,*)"MED ARIT.    ",ma
  
  call calcmg(mg,lista,n) 
  write(*,*)"MED GEOM.    ",mg
  
ENd DO
end program media

!!!!!!!!!!!!!!!!subrotinas!!!!!!!!!!!!!!!!!!!

subroutine calcma(ma,lista,n)
	integer*16 i,n
real*16 ma,lista(n)
	ma=0

	do i=1,n
		ma=ma+(lista(i)/n)
	enddo

	return
end subroutine calcma

!!!!!!!!!!!!!!!!!!!!

subroutine calcmg(mg,lista,n)
	integer*16 i,n
  real*16 mg,lista(n)
	mg=1

	do i=1,n
		mg=mg*lista(i)
	enddo

	mg=mg**(1.0/dfloat(n))

	return
end subroutine calcmg

!!!!!!!!!!!!!!!!!!!

subroutine calcdp(dp,lista,n)

  integer*16 i, n
  real*16 dp,lista(n),listaquad(n),a,b
  do i=1,n
     listaquad(i)=lista(i)**2
  end do
  
  call calcma(a,listaquad,n)
  call calcma(b,lista,n)
  
  dp=sqrt(a-(b**2))
!  print*, "a", a, 'b', b
  return

end subroutine calcdp
!!!ma!=mg se a lista for composta por números não iguais

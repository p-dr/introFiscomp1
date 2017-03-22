PROGRAM multiplica

  IMPLICIT NONE
  INTEGER i, j, n
  PARAMETER (n=3)
  REAL*8 mat(n,n),x(n),y(n)

  OPEN(10, FILE='mmultiplica.in')

  DO i=1, n
     READ(10,*) (mat(i,j), j=1, n)
  END DO

  DO i=1, n
     READ(10, *) x(i)
  ENDDO

  CLOSE(10)

  OPEN(20, FILE='mmultiplica.dat') 
  CALL submultiplica(n, mat, x, y)
  WRITE(20, *)y
  CLOSE(20)
  
END PROGRAM multiplica

SUBROUTINE submultiplica(n, mat, x, y)
  IMPLICIT NONE
  INTEGER i, j, n
  REAL*8 mat(n,n), x(n), y(n)

  DO i=1,n
     DO j=1, n
        y(i) = y(i) + x(j)*mat(i,j)
     END DO
  END DO

END SUBROUTINE submultiplica

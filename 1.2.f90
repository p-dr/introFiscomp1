RECURSIVE FUNCTION fact(n) RESULT(res)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER res

  IF (n == 0) THEN
     res = 1
       
  ELSE
     res = n * fact(n-1)
     
  ENDIF

END FUNCTION fact

PROGRAM P
  IMPLICIT NONE

  INTEGER :: fact, F, i
  REAL :: STIRLING, PI2, e, S
  PI2 = 8*ATAN(1.0)
  e = 2.718281828459

  OPEN(10, FILE='1.2.dat')

  DO i=1, 20
     S = SQRT(PI2*i)*(i/e)**i
     F = fact(i)
     WRITE(10,*) i, F, S, ABS(F-S)/F
  END DO

  CLOSE(10)
     
END PROGRAM P

PROGRAM P
  REAL*16 SEN
  F = SEN(3, 4)
!  G = SEN(5.0, 3)
!  H = SEN(4.0, 3)
  
END PROGRAM P

RECURSIVE FUNCTION fact(n) RESULT(res)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER*16 res

  IF (n == 0) THEN
     res = 1
       
  ELSE
     res = n * fact(n-1)
     
  ENDIF

END FUNCTION fact


FUNCTION sen(n, O) RESULT(res)
  REAL, INTENT(IN) :: n
  INTEGER, INTENT(IN) :: O
  REAL*16 :: res, AN, TGT, DIFF
  INTEGER*16 J, I

  I=0
  TGT = SIN(N)
  RES = 0.0
  DIFF = (ABS(RES - TGT))
  
  DO WHILE (DIFF > 10.0**(-O))
     DO J = I*2+1, 1, -1
        AN = (-1.0)**(I) * N**(I*2+1)/J
     END DO
     
     RES = RES + AN
     PRINT*,'AN', AN,'RES', RES
     I=I+1
     DIFF = (ABS(RES - TGT))
  END DO
  PRINT*, '========================================================'
  PRINT*, "RESULTADOS DA APROXIMAÇÃO PARA SIN(", N,"), COM PRECISÃO", 10.0**(-O)
  PRINT*, ' '
  PRINT*, "DESVIO:", ABS(RES - TGT)
  PRINT*, "RESULTADO DA APROXIMAÇÃO:", RES
  PRINT*, 'RESULTADO ESPERADO:      ', TGT
  PRINT*, ' '
  PRINT*, 'ORDEM NECESSÁRIA:', I
  PRINT*, ' '
END FUNCTION sen


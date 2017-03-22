PROGRAM P
  REAL*8 SEN
  F = SEN(4.0, 6)
END PROGRAM P

RECURSIVE FUNCTION fact(n) RESULT(res)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER*8 res

  IF (n == 0) THEN
     res = 1
       
  ELSE
     res = n * fact(n-1)
     
  ENDIF

END FUNCTION fact


FUNCTION sen(n, O) RESULT(res)
  REAL, INTENT(IN) :: n
  INTEGER, INTENT(IN) :: O
  REAL*8 :: res, AN, TGT
  INTEGER*8 FACT
  INTEGER ::I=0
  TGT = SIN(N)
  RES = 0.0
  
  DO WHILE (ABS(RES - TGT) > 10.0**(-O))
     AN = (-1.0)**(I) * N**(I*2+1)/FACT(I*2+1)
     RES = RES + AN
!     PRINT*,'AN', AN,'RES', RES
     I=I+1
  END DO
  PRINT*, "RESULTADOS DA APROXIMAÇÃO PARA SIN(", N,"), COM PRECISÃO", 10.0**(-O)
  PRINT*, ' '
  PRINT*, "DESVIO:", ABS(RES - TGT)
  PRINT*, "RESULTADO DA APROXIMAÇÃO:", RES
  PRINT*, 'RESULTADO ESPERADO:      ', TGT
  PRINT*, ' '
  PRINT*, 'ORDEM NECESSÁRIA:', I 
END FUNCTION sen


PROGRAM P
  REAL*16 SEN
  F = SEN(3.0, 15)
  
END PROGRAM P

FUNCTION sen(n, O) RESULT(res)
  REAL, INTENT(IN) :: n
  INTEGER, INTENT(IN) :: O
  REAL*16 :: res, AN, TGT, DIFF
  INTEGER*16 :: J, I

  I=0
  TGT = SIN(N)
  RES = 0.0
  DIF = 999999.9
  
  DO WHILE (ABS(ABS(RES) - ABS(SIN(N))) > 10.0**(-O))
!DO K=1,10
     AN = (-1.0)**(I) * N**(I*2+1)
     DO J = I*2+1, 2, -1
        AN = AN/J
     END DO
     
     RES = RES + AN
!     PRINT*,'AN', AN,'RES', RES, 'DIFF', DIFF
     I=I+1
     DIFF = ABS(RES - SIN(N))
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


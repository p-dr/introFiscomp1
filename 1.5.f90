SUBROUTINE SHIFT(RES, M, POS)
  IMPLICIT NONE
  INTEGER M, POS, I
  REAL RES(M)
  
  DO I=SIZE(RES), POS+1, -1
     RES(I) = RES(I-1)
  END DO

END SUBROUTINE SHIFT

SUBROUTINE MENORES(N, M)
  REAL RES(M), IPT
  INTEGER M, N
  RES = (/(K, K=999999999,999999999-N,-1)/)
  DO I=1, N
     READ*, IPT

     DO J=M, 1, -1
        IF (RES(J) .LE. IPT) THEN
           CALL SHIFT(RES, N, J)
           RES(J+1) = IPT
           !PRINT*, 'IF1 CALLED'
           EXIT

        ELSE IF ((J .EQ. 1) .AND. (RES(J) > IPT)) THEN
           !PRINT*, 'IF2 CALLED'
           !CALL SHIFT(RES, N, 1)
           RES(1) = IPT
           EXIT
        END IF

     END DO
  END DO
  PRINT*, M, 'MENORES NUMEROS INSERIDOS:', RES

END SUBROUTINE MENORES
  


PROGRAM P
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=8, M = 7
  REAL RES(M)
  INTEGER POS
  !  CALL SHIFT(RES, 5, 2)

do while (.true.)
  PRINT*, 'INPUTS:'
  CALL MENORES(N,M)
  print*, '============================================'
end do

END PROGRAM P

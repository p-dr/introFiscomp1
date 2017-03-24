SUBROUTINE SHIFT(RES, POS)
  INTEGER RES(M)
  INTEGER POS

  DO I=M, POS+1, -1
     RES(I) = RES(I-1)
  END DO
  PRINT*, "SHIFTED"
END SUBROUTINE SHIFT


SUBROUTINE MENORES(N, M)
  INTEGER RES(M)
  INTEGER IPT
  RES = (/ 99,98,97,96,95/)
  DO I=1, N
     READ*, IPT

     DO J=M, 1, -1
        IF (RES(J) .LE. IPT) THEN
           CALL SHIFT(RES, J)
           RES(J+1) = IPT
           PRINT*, 'IF1 CALLED'
           EXIT

        ELSE IF ((J .EQ. 1) .AND. (RES(J) > IPT)) THEN
           PRINT*, 'IF2 CALLED'
           CALL SHIFT(RES, 1)
           RES(1) = IPT
           EXIT
        END IF

     END DO
     
  PRINT*, 'RES ', RES        
  END DO


  
END SUBROUTINE MENORES


PROGRAM P
  INTEGER RES(5)
  INTEGER, PARAMETER :: N=10, M = 5
  INTEGER POS
  POS = 3
  res = (/1,2,3,4,5/)
  CALL SHIFT(RES, 3)
  PRINT*, RES
  !  CALL MENORES(N,M)
END PROGRAM P

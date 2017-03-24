SUBROUTINE SHIFT(RES, POS)
  INTEGER RES(M)
  INTEGER POS

  DO I=M, POS+1, -1
     RES(I) = RES(I-1)
  END DO
  PRINT*, RES
END SUBROUTINE SHIFT


SUBROUTINE MENORES(N, M)
  INTEGER N, M
  INTEGER RES(M)
  INTEGER IPT
  RES = (/ 0,0,0/)
  DO I=1, N
     READ*, IPT

     DO J=M, 1, -1
        IF ((IPT < RES(J)) .AND. (RES(J) .NE. 0.0)) THEN
        
           IF (J .EQ. 1) THEN
              PRINT*, 'IF2 CALLED'
              CALL SHIFT(RES, 1)
              RES(1) = IPT
              EXIT
           END IF
           
           CALL SHIFT(RES, J+1)
           RES(J+1) = IPT
           PRINT*, 'IF1 CALLED'
           EXIT
     END IF
     

     END DO
     
  PRINT*, 'RES FINAL', RES        
  END DO


  
END SUBROUTINE MENORES


PROGRAM P
  INTEGER RES(3)
  INTEGER POS
  CALL MENORES(5,3)
END PROGRAM P

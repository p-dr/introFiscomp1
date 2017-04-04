SUBROUTINE PARKMILLER(R, CH)
  
  INTEGER*16 :: R
  INTEGER*16, parameter :: A = 7**5
  CHARACTER CH

  OPEN(10, FILE=CH//'.dat')

  DO I=1,100
     R = MOD(A*R, 2147483647)
     WRITE(10, *) REAL(r)/2147483647
  END DO

  CLOSE(10)
END SUBROUTINE PARKMILLER


SUBROUTINE RDM(R)

  IMPLICIT NONE

  INTEGER R, M
  INTEGER, parameter :: A = 7, C = 4

  M = 2**4+1

  R = MOD(A*R+C, M)

END SUBROUTINE RDM


subroutine MEDIR(R)
  INTEGER R, A1

  PRINT*, '====================================='
  
  A1 = r
  PRINT*, "SEED:", A1
  PRINT*, '-------------------------------------'
  CALL RDM(R)
!  PRINT*, "fora do laço", R
  
  I = 1
  DO WHILE (R /= A1)
     PRINT*, "R",I,':', R
     CALL RDM(R)
     I = I+1
  END DO

  PRINT*, '-------------------------------------'
  PRINT*, 'O PERIODO VALE', I

!  PRINT*, 'APOS LAÇO',R
END subroutine MEDIR


PROGRAM main

  INTEGER*16 R,S,T,U,V,N
  N=5
  
  R = 1 !SEED 1
  S = 54321 !SEED 2
  U = 12345 !SEED 4
  T = 99999 !SEED 3
  V = 42 !SEED 5

  !  CALL MEDIR(R)
  !  CALL MEDIR(S)
  !  CALL MEDIR(T)
  !  CALL MEDIR(U)
  !  CALL MEDIR(V)
  
  
  DO I=82, 86
     CALL PARKMILLER(R, CHAR(I))
  END DO
END PROGRAM main

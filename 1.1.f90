PROGRAM P

  REAL :: C, CAPROX, DIFF

  OPEN(1, file='1.1.dat')
  
  DO F=0, 100, 10
     C = (F+32)/1.8
     CAPROX = (F+30)/1.8
     DIFF = ABS(C-CAPROX)/C
     WRITE(1, *) F, C, CAPROX, DIFF
  ENDDO

  CLOSE(10)

END PROGRAM P

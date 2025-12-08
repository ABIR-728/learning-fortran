      PROGRAM problem4
      IMPLICIT NONE
      INTEGER :: N
      
      PRINT *, "ENTER A POSITIVE INTEGER:"
      READ *,N
      
         PRINT *, "COLLATZ SEQUENCES:"
         DO WHILE (N /= 1)
         WRITE(*,'(I0,A)',ADVANCE='NO') N,"->"
         IF (MOD(N,2) == 0) THEN
         N = N/2
         ELSE
         N = 3*N+1
         END IF
         END DO
         PRINT *, "1"
         PRINT *, "FINISHED!"
  
      END PROGRAM

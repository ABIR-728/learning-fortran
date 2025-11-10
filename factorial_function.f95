      FUNCTION CAL_FACT(N) RESULT(FACT)
      IMPLICIT NONE
      INTEGER , INTENT(IN)::N
      INTEGER ::I,FACT
      FACT=1
       DO I = 2,N
          FACT = FACT * I
       END DO
      END FUNCTION


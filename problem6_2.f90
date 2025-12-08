  PROGRAM ARMSTRONG_NUMBER

  IMPLICIT NONE
  INTEGER :: A , B , C , NUM , SUM_OF_CUBES

  ! Count digits
  DO A = 0,9
     DO B = 0,9
        DO C = 0,9
           NUM = (100*A)+(10*B)+(C)
           SUM_OF_CUBES = (A**3)+(B**3)+(C**3)
              IF (SUM_OF_CUBES == NUM) THEN
                 PRINT *, NUM, "IS AN ARMSTRONG NUMBER."
              END IF
        END DO
     END DO
  END DO
  END PROGRAM 

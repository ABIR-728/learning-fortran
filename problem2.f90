      PROGRAM problem2
      IMPLICIT NONE
      INTEGER(kind=8) :: i
      INTEGER(KIND=8) :: num
      INTEGER(KIND=8) :: result
      num = 0
           DO i= 0,9
              num = num * 10 + i
              result = num * 9 + (i+1)
           PRINT*,num," * 9 + ", i+1 ," = ", result
           END DO
      END PROGRAM

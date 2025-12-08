      PROGRAM problem1
      IMPLICIT NONE
      INTEGER(kind=8) :: i
      INTEGER(KIND=8) :: num
      INTEGER(KIND=8) :: square
      num = 0
           DO i= 0,7
              num = num + (10 **i)
              square = num*num
           PRINT*,num,"x",num,"=",square
           END DO
      END PROGRAM

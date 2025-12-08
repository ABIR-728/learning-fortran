      PROGRAM problem3
      IMPLICIT NONE
      CHARACTER(1),DIMENSION(8,8) :: BOARD
      INTEGER :: i,j
      
      DO I = 1 , 8
         DO J = 1 , 8  
              IF (MOD(I+J,2)==0) THEN
                 BOARD(I,J) = "B"
              ELSE
                 BOARD(I,J) = "W"
              END IF   
          END DO
      END DO

      DO I = 1 , 8
         DO J = 1 , 8
            WRITE(*,'(A)',ADVANCE='NO') BOARD(I,J)//' '
         END DO
      PRINT *
      END DO
  
      END PROGRAM

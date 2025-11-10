      PROGRAM SORT_ARRAY
      IMPLICIT NONE
      INTEGER,DIMENSION (:),ALLOCATABLE :: A
      INTEGER :: n,i,j,temp,largest

      PRINT *, "ENTER THE NUMBER OF ELEMENTS"
      READ *, n
      
      ALLOCATE(A(n))

      PRINT *, "ENTER THE NUMBER OF ELEMENTS"
      READ *, A

      DO i = 1 , n-1
         DO j = 1 , n-i
            if (A(j) > A(j+1)) then
               temp = A(j)
               A(j) = A(j+1)
               A(j+1) = temp
            END IF
         END DO
      END DO

      PRINT *, "SORTED ARRAY IN ASCENDING ORDER"
      PRINT *, A
      
      largest = A(n)
      PRINT *, "THE LARGEST NUMBER OF THE ARRAY is",largest
      
      DEALLOCATE(A)
      
      END PROGRAM SORT_ARRAY
         



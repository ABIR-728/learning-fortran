             PROGRAM MATRIX_MULTIPLICATION
             IMPLICIT NONE
             INTEGER :: I, J, K
             REAL, DIMENSION(3,3) :: A, B, C

!---- Input first matrix ----
             PRINT*, "Enter elements of 3x3 matrix A (row-wise):"
             READ*, ((A(I,J), J=1,3), I=1,3)

!---- Input second matrix ----
             PRINT*, "Enter elements of 3x3 matrix B (row-wise):"
             READ*, ((B(I,J), J=1,3), I=1,3)

!---- Initialize result matrix ----
             C = 0.0

!---- Matrix multiplication ----
             DO I = 1, 3
                DO J = 1, 3
                   DO K = 1, 3
                      C(I,J) = C(I,J) + A(I,K) * B(K,J)
                   END DO
                END DO
             END DO

!---- Print result ----
             PRINT*, "Resultant matrix C = A × B is:"
             DO I = 1, 3
                PRINT*, (C(I,J), J=1,3)
             END DO

             END PROGRAM MATRIX_MULTIPLICATION

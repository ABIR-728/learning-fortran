      PROGRAM problem4
      IMPLICIT NONE
      INTEGER :: A,B,C,D
      INTEGER :: NUM1 , PRODUCT
      
      DO A = 1 , 9
         DO B = 0 , 9  
              IF (B == A) CYCLE
                 NUM1 = 100*A + 10*B + A
                 PRODUCT = NUM1 * A
                 IF (PRODUCT<100 .OR. PRODUCT>999) CYCLE
                 D = PRODUCT/100
                 C = (PRODUCT/10)-D*10
                 IF (MOD(PRODUCT,10) /= D) CYCLE
                 
                 IF (D == A .OR. D == B) CYCLE
                 IF (C == A .OR. C == B .OR. C == D) CYCLE
                 
                 PRINT *, "SOLUTION:"
                 PRINT *, "A=",A, "B=",B, "C=",C, "D=",D
                 PRINT *, NUM1, "*" , A , " = ",PRODUCT
         END DO
      END DO
  
      END PROGRAM

PROGRAM SHO_SIMPSON
  IMPLICIT NONE
  REAL,EXTERNAL :: F
  REAL :: PI,A,B,H,X,SUMF
  INTEGER :: I,N

  PI = 4.0 * ATAN(1.0)
  PRINT*, "GIVE THE VALUE OF A:"
  READ *, A
  PRINT*, "GIVE THE VALUE OF B:"
  READ *, B
  PRINT*, "GIVE THE VALUE OF N:"
  READ *, N
  H = (B-A)/N

  SUMF = F(PI,A)**2 + F(PI,B)**2

  DO I = 1 , N-1
     X = A + I * H
     IF ((I/2)*2 /= I .AND. I<N) THEN
       SUMF = SUMF + 4*F(PI,X)**2          !----------ODD!
     ELSE IF ((I/2)*2 == I .AND. I<N) THEN
       SUMF = SUMF + 2*F(PI,X)**2          !---------EVEN!
     END IF   
  END DO   

  PRINT*, SUMF * (H/3.0)
  END PROGRAM SHO_SIMPSON
 !-------------------------------------------------------!
  FUNCTION F(PI,Y)
    REAL :: F,PI,Y
    F = (1.0/PI)**0.25 * EXP(-0.5*Y*Y)
  END FUNCTION  

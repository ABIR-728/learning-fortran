      program c_from_ab
      implicit none
      real,allocatable :: A(:,:) , B(:,:) , C(:,:)
      INTEGER :: N,I,J
      PRINT *, "GIVE N"
      READ *, N
      ALLOCATE (A(N,N) , B(N,N) , C(N,N))
      
      OPEN(UNIT=11,FILE='A.DAT',STATUS='OLD')
       DO I=1,N
          READ(11,*) (A(I,J),J=1,N)
       END DO
      CLOSE(11)

      OPEN(UNIT=12,FILE='B.DAT',STATUS='OLD')
       DO I=1,N
          READ(12,*) (B(I,J),J=1,N)
       END DO
      CLOSE(12)

      C=MATMUL(A,B)
      
      OPEN(UNIT=13,FILE='C.DAT',STATUS='NEW',ACTION="WRITE")
       DO I=1,N
          WRITE(13,*) (C(I,J),J=1,N)
       END DO
      
      END PROGRAM

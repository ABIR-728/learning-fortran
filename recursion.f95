    program recursion
    implicit none
    integer,dimension(:),allocatable::A
    integer :: N,I
    print*, "give N"
    read*, N
    ALLOCATE(A(N))
    
    PRINT*, "GIVE", N ,"ELEMENTS"
    READ *, (A(I),I=1,N)
    
    PRINT*, "ORIGINAL ARRAY"
    WRITE (*,*)  (A(I),I=1,N)
    
    A(2:N) = (/(SUM (A(1:I)),I=2,N)/)
    
    PRINT*, "GENERATED ARRAY"
    WRITE (*,*) (A(I),I=1,N)

    deallocate(A)
 
    END PROGRAM


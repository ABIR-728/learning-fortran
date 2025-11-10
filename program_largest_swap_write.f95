    program largest_swap_write
    implicit none
    integer,parameter::n=4
    real :: A(n,n),B(n)
    real :: max_val,temp,atmul
    integer :: i,j,max_row
    
    open(unit=10,file='A.DAT',status='old')
        do i=1,n
           read (10,*)(A(i,j),j=1,n)
        end do
    close(10)
  
    open(unit=11,file='B.DAT',status='old')
        do i=1,n
           read (11,*)B(i)
        end do
    close(11)
    
    print*, "initial matrix(A/B)"
    do i=1,n
       write (*,*)(A(i,j),j=1,n),B(i)
    end do
 
    max_val = A(1,1)
    max_row = 1
    do i = 2,n
       if (A(i,1) > max_val) then
           max_val = A(i,1)
           max_row = i
       end if
    end do
    print*, "largest element in 1st column",max_val
    print*, "located in row",max_row
    
    if (max_row /= 1) then
       do j = 1,n
        temp = A(1,j)
        A(1,j) = A(max_row,j)
        A(max_row,j)=temp
       end do
       temp = B(1)
       B(1) = B(max_row)  
       B(max_row) = temp
    end if
    
    open(unit=12,file='C.DAT',status='new',action="write")
        do i=1,n
           write (12,*)(A(i,j),j=1,n),B(i)
        end do
    close(12)

    print*,"swapped matrix written in C.DAT"
    
    do i = 2,n
    atmul = A(i,1)/A(1,1)
        do j = 1,n
           A(i,j) = A(i,j)-A(1,j)*atmul
        end do
        B(i) = B(i)-B(1)*atmul
    end do
    
       
    open(unit=13,file='D.DAT',status='new',action="write")
        do i=1,n
           write (13,*)(A(i,j),j=1,n),B(i)
        end do
    close(13) 
    print *, "after zero matrix will be in D.DAT" 
    end program      

    

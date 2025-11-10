      program sort_array
      implicit none
      integer,dimension(:),allocatable :: A
      integer :: n,i,j,temp
      print *, "ENTER THE NUMBER OF ELEMENT"
      READ *, n
      allocate (A(n))
      
      print *, " enter the elements "
      read *, A
      
      Do i=1,n-1
         do j = 1,n-i
            if (A(j)>A(j+1)) then
               temp = A(j)
               A(j) = A(j+1)
               A(j+1) = temp
            end if
         end do
      end do
      
      print*, "sorted array"
      print*, A
      deallocate (A)
      END

      program sort_array_subroutine
      implicit none
      integer,dimension(:),allocatable :: A
      integer :: n
      print *, "ENTER THE NUMBER OF ELEMENT"
      READ *, n
      allocate (A(n))
      
      print *, " enter the elements "
      read *, A
      
      call sort (A,n)
      print*, "sorted array"
      print*, A
      
      deallocate (A)
      end
     
      subroutine sort (A,n)
      implicit none
      integer,intent(in) :: n
      integer,intent(inout)::A(n)
      integer :: i,j,temp
      
      Do i=1,n-1
         do j = 1,n-i
            if (A(j)>A(j+1)) then
               temp = A(j)
               A(j) = A(j+1)
               A(j+1) = temp
            end if
         end do
      end do
      end subroutine
      
      
   

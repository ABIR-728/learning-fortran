program ODE_simple
  implicit none
  real :: x, y, h, x_end
  integer :: n, i
  real :: k
  
  k = 1.0
  print*, "give the value of x" 
  read*, x 
  print*, "give the value of x_end"
  read*, x_end 
  print*, "give the value of y"
  read*, y
  print*, "give the value of n"
  read*, n
  h = (x_end-x)/n
  print*, "the value of h is",h          
  

  open(10, file="ODE.dat", status="new")
  write(10,*) x, y

  do i = 1, n
     y = y + h * (-k*y)
     x = x + h
     write(10,*) x, y
  end do

  close(10)
end program ODE_simple

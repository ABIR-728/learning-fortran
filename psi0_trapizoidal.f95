    program sho_trapizoidal
    implicit none
    integer :: n
    real :: a,b,dx,x,pi
    real :: psi0
    real :: integrand,integral
    integer :: i
      
    pi = 4.0 * ATAN(1.0)
    
    print *, "give the value n of "
    read *, n

    !integration limits
     print *, "give the value of a "
     read *, a
     print *, "give the value of b "    
     read *, b
     print *, "the value of dx "
     dx = (b-a)/n
     
     integral= 0.0 
   
    do i = 0,n
       x = a + i*dx
     psi0 = ((1.0/pi)**0.25)*exp(-0.5*x*x)
     integrand = psi0**2

     if (i==0 .or. i==n) then
        integral = integral+ 0.5*integrand
     else
        integral = integral+integrand
     end if
    end do
    
    integral = integral*dx
    
    print*, "integral of (psi0(x))^2 from -5 to 5 =",integral
    end program

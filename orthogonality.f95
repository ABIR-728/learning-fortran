    program psi0_psi1_trapizoidal
    implicit none
    integer :: n
    real :: a,b,dx,x,pi
    real :: psi0,psi1,integrand0,integrand1,integrand01
    real :: integral0,integral1,integral01
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
     print *, dx
     
     integral0= 0.0
     integral0= 0.0
     integral0= 0.0
     
   
    do i = 0,n
       x = a + i*dx
     psi0 = ((1.0/pi)**0.25)*exp(-0.5*x*x)
     psi1 = ((4.0/pi)**0.25)*x*exp(-0.5*x*x)
     integrand0 = psi0**2
     integrand1 = psi1**2
     integrand01 = psi0*psi1
     

     if (i==0 .or. i==n) then
        integral0 = integral0+ 0.5*integrand0
        integral1 = integral1+ 0.5*integrand1
        integral01 = integral01+ 0.5*integrand01
     else
        integral0 = integral0 + integrand0
        integral1 = integral1 + integrand1
        integral01 = integral01 + integrand01
     end if
    end do
    
    integral0 = integral0*dx
    integral1 = integral1*dx
    integral01 = integral01*dx
    
    print*, "normalization of psi0 =",integral0
    print*, "normalization of psi1 =",integral1
    print*, "orthogonality psi0*psi1 dx =",integral01
    end program

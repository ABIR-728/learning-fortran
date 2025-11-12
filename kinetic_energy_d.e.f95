    program kinetic_energy
    implicit none
    integer :: n
    real :: a,b,dx,x,pi
    real :: psi_p,psi_m,dpsi
    real :: integral
    integer :: i
      
    pi =4.0 * atan(1.0)

    !integration limits
     print *, "give the value of n "
     read *, n
     print *, "give the value of a "
     read *, a
     print *, "give the value of b "    
     read *, b
     dx = (b-a)/n
     print *, "the value of dx ",dx
     
    !generate grid and eveluate psi(x)
   
    do i = 1,n-1
       x = a + i*dx
     
       psi_p = (1.0/pi)**0.25 * exp(-0.5*(x+dx)**2)
       psi_m = (1.0/pi)**0.25 * exp(-0.5*(x-dx)**2)
    
       dpsi = (psi_p - psi_m) / (2.0*dx)
       integral = integral + dpsi**2 * dx
    end do
    
    integral = 0.5 * integral
    
    print*, "final kinetic energy", integral
    end program


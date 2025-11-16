    program kinetic_energy
    implicit none
    integer :: n
    real :: a,b,dx,x,pi,L
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
     L = b-a
     dx = L/n
     print *, "the value of dx ",dx
     
   
    do i = 2,n-1
       x = a + i*dx
     
       psi_p = ((2.0/L)**0.5) * sin(pi*(x+dx)/L)
       psi_m = ((2.0/L)**0.5) * sin(pi*(x-dx)/L)
           
       dpsi = (psi_p - psi_m) / (2.0*dx)
       integral = integral + dpsi**2 * dx
    end do
    
    integral = integral * 0.5
    
    print*, "final kinetic energy", integral
    end program


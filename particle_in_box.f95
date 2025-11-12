    program particle_in_box
    implicit none
    integer :: i,N
    real :: x,dx,L,a,b,integral1,integral2,integral12,psi1,psi2,pi,integrand1,integrand2,integrand12,hbar,m,E1,E2
    
    print *, "give the value of N"
    read *, N
   
    pi = 4.0*atan(1.0)
    print *, "give the value of a"
    read *, a
    print *, "give the value of b"
    read *, b
    L = b-a                
    dx = L / N
    print *, "give the value of dx"
    print *, dx

    integral1 = 0.0
    integral2 = 0.0
    integral12 = 0.0
    
    do i = 0, N
        x = i * dx
        psi1 = sqrt(2.0/L) * sin(pi*x/L)        
        psi2 = sqrt(2.0/L) * sin(2.0*pi*x/L)    

        integrand1 = psi1**2.0
        integrand2 = psi2**2.0
        integrand12 = psi1*psi2
        
        if (i == 0 .or. i == N) then
            integral1 = integral1+integrand1
            integral2 = integral2+integrand2 
            integral12 = integral12 + integrand12
        else
            integral1 = integral1 + 2.0 * integrand1
            integral2 = integral2 + 2.0 * integrand2
            integral12 = integral12 + 2.0 * integrand12
        end if
    end do

    integral1 = integral1 * (dx / 2.0)
    integral2 = integral2 * (dx / 2.0)
    integral12 = integrand12 * (dx / 2.0)

    print *, 'normalisation of psi1 from 0 to 10 = ', integral1
    print *, 'normalisation of psi2 from 0 to 10 = ', integral2
    print *, 'orthogonality of psi1*psi2  from 0 to 10 = ', integral12
    
    
    end program particle_in_box


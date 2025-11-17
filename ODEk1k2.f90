program consecutive_reaction
  implicit none
  integer, parameter :: nsteps = 1000
  real(8) :: dt, t
  real(8), dimension(0:nsteps) :: A, B, C
  real(8) :: k1, k2
  integer :: i

  ! Parameters
  k1 = 1.0
  k2 = 0.5
  dt = 0.01

  ! Initial values
  A(0) = 1.0
  B(0) = 0.0
  C(0) = 0.0

  ! Euler method loop
  do i = 0, nsteps - 1
    t = i * dt
    A(i+1) = A(i) - k1 * A(i) * dt
    B(i+1) = B(i) + (k1 * A(i) - k2 * B(i)) * dt
    C(i+1) = C(i) + k2 * B(i) * dt
  end do

  ! Write to file
  open(unit=10, file='data.txt', status='replace')
  do i = 0, nsteps
    t = i * dt
    write(10,*) t, A(i), B(i), C(i)
  end do
  close(10)

end program consecutive_reaction


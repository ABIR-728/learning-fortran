program armstrong

  implicit none
  integer :: n, temp, digit, sum, count, i

  print *, "Enter a number:"
  read *, n

  temp = n
  count = 0

  ! Count digits
  do while (temp > 0)
    temp = temp / 10
    count = count + 1
  end do

  ! Compute Armstrong sum
  temp = n
  sum = 0
  do while (temp > 0)
    digit = mod(temp, 10)
    sum = sum + digit**count
    temp = temp / 10
  end do

  if (sum == n) then
    print *, n, "is an Armstrong number."
  else
    print *, n, "is NOT an Armstrong number."
  end if

end program armstrong
!Dynamically allocating an array

program new1
implicit none

integer, allocatable, Dimension(:) :: A
real :: mean,sum1
integer :: i,n

print*, 'Enter number of integers required'
read*, n

allocate (A(n))

do i=1,n

read*, A(i)

end do

do i=1,n

sum1 = sum1 + A(i)

end do

mean = sum1/n

print*, mean




end program

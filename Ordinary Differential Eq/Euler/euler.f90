program euler
implicit none

real,allocatable,dimension(:) :: y
real :: h,i,n
integer :: initial,final

print*, 'Enter the no. of points'

read*, n

allocate(y(n+1))

print*, 'Enter the initial value'

read*, initial

print*, 'Enter the final value'

read*, final

h = (final - initial)/n

print*, 'Enter the function value at initial point'

read*, y(1)

do i = 1,n

y(i+1) = y(i) + (-2*y(i))*h

end do

open (unit = 25, file = 'y.txt')

do i = 1,n

write(25,*) y(i)

end do

open (unit = 20, file = 'x.txt')

do i = initial,final,h

write(20,*) i

end do

end program







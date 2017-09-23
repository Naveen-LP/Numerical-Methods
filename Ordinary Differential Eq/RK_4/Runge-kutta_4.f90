program euler
implicit none

real,allocatable,dimension(:) :: y,k1,k2,k3,k4
real :: h,i,n
integer :: initial,final

print*, 'Enter the no. of points'

read*, n

allocate(y(n+2))
allocate(k1(n+2))
allocate(k2(n+2))
allocate(k3(n+2))
allocate(k4(n+2))

print*, 'Enter the initial value'

read*, initial

print*, 'Enter the final value'

read*, final

h = (final - initial)/n

print*, 'Enter the function value at initial point'

read*, y(1)

do i = 1,n

k1(i+1) = h*(-2*y(i))

k2(i+1) = h*(-2*(y(i) + (k1(i+1)/2)))

k3(i+1) = h*(-2*(y(i) + (k2(i+1)/2)))

k4(i+1) = h*(-2*(y(i) + k3(i+1)))

y(i+1) = y(i) + (k1(i+1) + 2*k2(i+1) + 2*k3(i+1) + k4(i+1))/6

end do

do i = 1,n

print*, y(i)

end do

open (unit = 25, file = 'y1.txt')

do i = 1,n

write(25,*) y(i)

end do

open (unit = 20, file = 'x1.txt')

do i = initial,final,h

write(20,*) i

end do

end program







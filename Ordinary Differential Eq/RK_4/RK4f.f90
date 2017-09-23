program euler
implicit none

real,allocatable,dimension(:) :: y,x,k1,k2,k3,k4
real :: h,i,n
integer :: initial,final

print*, 'Enter the no. of points'

read*, n

allocate(y(n+2))
allocate(x(n+2))
allocate(k1(n+2))
allocate(k2(n+2))
allocate(k3(n+2))
allocate(k4(n+2))

print*, 'Enter the initial value'

read*, initial

print*, 'Enter the final value'

read*, final

h = (final - initial)/n

x(1) = initial

do i = 1,n

x(i+1) = x(i) + h

end do

do i = 1,n

print*, x(i)

end do

print*, 'Enter the function value at initial point'

read*, y(1)

do i = 1,n

k1(i+1) = h*f(x(i),y(i))

k2(i+1) = h*(f(x(i) + (h/2),y(i) +(k1(i+1)/2)))

k3(i+1) = h*(f(x(i) + (h/2),y(i) +(k2(i+1)/2)))

k4(i+1) = h*(f(x(i) + h,y(i) + k3(i+1)))

y(i+1) = y(i) + (k1(i+1) + 2*k2(i+1) + 2*k3(i+1) + k4(i+1))/6

end do

do i = 1,n

print*, y(i)

end do

open (unit = 25, file = 'y2.txt')

do i = 1,n

write(25,*) y(i)

end do

open (unit = 20, file = 'x2.txt')

do i = initial,final,h

write(20,*) i

end do

contains

real function f(a,b)

implicit none

real, intent(in) :: a,b

f = a
 
end function f

end program








program secorder
implicit none

real,allocatable,dimension(:) :: y,x,z,ky1,ky2,kz1,kz2
real :: h,i,n
integer :: initial,final

print*, 'Enter the no. of points'

read*, n

allocate(y(n+2))
allocate(x(n+2))
allocate(z(n+2))
allocate(ky1(n+2))
allocate(ky2(n+2))
allocate(kz1(n+2))
allocate(kz2(n+2))

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

print*, 'Enter the first derivative value at initial point'

read*, z(1)

do i = 1,n

ky1(i+1) = h*z(i)

kz1(i+1) = h*(f(x(i),y(i),z(i)))

ky2(i+1) = h*(z(i) + kz1(i+1))

kz2(i+1) = h*(f(x(i+1),y(i) + ky1(i+1),z(i) + kz1(i+1)))

y(i+1) = y(i) + (ky1(i+1) + ky2(i+1))/2

z(i+1) = z(i) + (kz1(i+1) + kz2(i+1))/2

end do

do i = 1,n

print*, y(i)

end do

open (unit = 25, file = 'y.txt')

do i = 1,n

write(25,*) y(i)

end do

open (unit = 20, file = 'x.txt')

do i = 1,n

write(20,*) x(i)

end do

open (unit = 22, file = 'z.txt')

do i = 1,n

write(22,*) z(i)

end do


contains

real function f(a,b,c)

implicit none

real, intent(in) :: a,b,c

f = -5*c - 4*b + 5
 
end function f

end program








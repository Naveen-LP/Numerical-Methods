program prcr
implicit none

real,allocatable,dimension(:) :: y,yy
real :: h,i,n,error
integer :: initial,final

print*, 'Enter the no. of points'

read*, n

allocate(y(n+1))
allocate(yy(n+1))

print*, 'Enter the initial value'

read*, initial

print*, 'Enter the final value'

read*, final

h = (final - initial)/n

print*, 'Enter the function value at initial point'

read*, y(1)

do i = 1,n

yy(i+1) = y(i) + (-2*y(i))*h

y(i+1) = y(i) + (-2*y(i) -2*yy(i+1))*(h/2)

error = abs(y(i+1) - yy(i+1))/y(i+1)

if(error > 0.0001) then

yy(i+1) = y(i+1)

end if

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







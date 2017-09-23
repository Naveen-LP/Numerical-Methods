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

if(error > 0.00001) then

yy(i+1) = y(i+1)

end if

end do

do i = 1,n

print*, y(i)

end do

do i = initial,final,h

print*, i

end do

end program







program derivative
implicit none

integer :: i
real :: y,h,f(5),x,a,centered

read*,a
read*,y
read*,h

do i=1,5

x = (i-3)*h

f(i) = ((a+x)**3)*(y**2)

end do

centered = (-f(5) + 8*f(4) - 8*f(2) + f(1))/(12*h)

print*, centered

end program



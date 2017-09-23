program derivative
implicit none

integer :: i
real :: h,f(5),x,a,forward,backward,centered

read*,a
read*,h

do i=1,5

x = (i-3)*h

f(i) = (a+x)**2

end do

centered = (-f(5) + 8*f(4) - 8*f(2) + f(1))/(12*h)

backward = (3*f(3) - 4*f(2) + f(1))/(2*h)

forward =  (-3*f(3) + 4*f(4) - f(5))/(2*h)

print*, centered,forward,backward

end program



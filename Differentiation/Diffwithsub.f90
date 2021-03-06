program derivative
implicit none

real :: a,h,fo,b,c

read*,a
read*,h

call derive(a,h,fo,b,c)

print*, fo,b,c

end program

subroutine derive(n,dx,forward,backward,centered)
implicit none

real, intent(in) :: n,dx
real, intent(out) :: forward,backward,centered
integer :: i
real :: y,f(5)

do i=1,5

y = (i-3)*dx

f(i) = -0.1*(n+y)**4 - 0.15*(n+y)**3 - 0.5*(n+y)**2 - 0.25*(n+y) + 1.2

end do

centered = (-f(5) + 8*f(4) - 8*f(2) + f(1))/(12*dx)

backward = (3*f(3) - 4*f(2) + f(1))/(2*dx)

forward =  (-3*f(3) + 4*f(4) - f(5))/(2*dx)

end subroutine


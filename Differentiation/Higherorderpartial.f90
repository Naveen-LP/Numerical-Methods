program derivative
implicit none

integer :: i,j
real :: a,b,x,y,dx,dy,centered,f(3,3)

read*,x
read*,y
read*,dx
read*,dy

do i=1,3
do j=1,3

a = (i-2)*dx
b = (j-2)*dy

f(i,j) = ((x+a)**2)*((y+b)**3) 

end do
end do

centered = (f(3,3) -f(3,1) - f(1,3) + f(1,1))/(4*dx*dy)

print*, centered

end program



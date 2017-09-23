!Code to solve system of n linear equations

program eq
implicit none

real, allocatable, dimension(:,:) :: a,x
integer :: siz,i,j,k
real :: g,f

print*,'Enter the number of equations'

read*, siz

allocate(a(siz,siz+1))
allocate(x(siz,1))

print*,'Enter the co-efficient matrix'

do i = 1,siz
do j = 1,siz+1

read*, a(i,j)

end do
end do

do j = 1,siz
do i = 1,siz

if(i > j) then

g = a(i,j)/a(j,j)

do k = 1,siz+1

a(i,k) = a(i,k) - g*(a(j,k))

end do

end if

end do
end do

x(siz,1) = a(siz,siz+1)/a(siz,siz)

do i = siz-1,1,-1

f = 0

do j = i+1,siz

f = f + a(i,j)*x(j,1)

end do

x(i,1) = (a(i,siz+1) - f)/a(i,i)

end do

do i = 1,siz

print*, x(i,1)

end do

end program

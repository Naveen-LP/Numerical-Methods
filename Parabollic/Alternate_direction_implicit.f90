program ADI
implicit none

real,allocatable,dimension(:,:) :: a,a1,z,ut,ut1,ut2,ut3
real,dimension(:),allocatable :: t1,t2
integer :: i,j,n,d,k,q1,q2,q3
real :: lambda,r,l,t,b,insidet,n1,n2,time,length,dx,dt,k1,points

print*, 'Enter the no. of points'
read*, points

print*, 'Enter the dimension'
read*, length

print*, 'Enter the value of delta t'
read*, dt

print*, 'Enter the conductivity value'
read*, k1

n = sqrt(points)

dx = length/n

allocate(ut(n+1,n+1))
allocate(ut1(n+1,n+1))
allocate(a(n-1,n))
allocate(z(n-1,n))
allocate(t1(n-1))
allocate(t2(n-1))

print*, 'Enter the temperature on the right face'
read*, r

print*, 'Enter the temperature on the left face'
read*, l

print*, 'Enter the temperature on the top face'
read*, t

print*, 'Enter the temperature on the bottom face'
read*, b

print*, 'Enter the initial temperature of the plate'
read*, insidet

lambda = k1*(dt/(dx)**2)

print*, 'Enter the end time'
read*, n1

do i = 1,n-1
do j = 1,n-1

ut(i,j) = insidet

end do
end do

do i = 0,n

ut(i,0) = b
ut(i,n) = t
ut(0,i) = l
ut(n,i) = r

ut1(i,0) = b
ut1(i,n) = t
ut1(0,i) = l
ut1(n,i) = r

end do

do time = dt,n1,dt

k = 0

do i = 1,n-1

do q3 = 1,n-1
do j = 1,n

a(q3,j) = 0

end do
end do

do q3 = 1,n-1

a(q3,q3) = 2*(1+lambda)

end do

do q3 = 1,n-2

a(q3+1,q3) = -lambda

end do

do q3 = 2,n-1

a(q3-1,q3) = -lambda

end do

q1 = 1

k = k + 1

do j = 2,n-2

q1 = q1 + 1

a(q1,n) = lambda*(ut(i-1,j)) + 2*(1-lambda)*(ut(i,j)) + lambda*(ut(i+1,j))

a(1,n) = lambda*(ut(i+1,1)) + 2*(1-lambda)*(ut(i,1)) + lambda*(ut(i-1,1)) + lambda*(ut(i,0))

a(n-1,n) = lambda*(ut(i+1,n-1)) + 2*(1-lambda)*(ut(i,n-1)) + lambda*(ut(i-1,n-1)) + lambda*(ut(i,n))

end do

call gauss(a,n-1,t1)

q2 = 0

do j = 1,n-1

q2 = q2 + 1

ut(k,j) = t1(q2)

end do

end do

do i = 1,n-1
do j = 1,n-1

ut1(i,j) = ut(i,j)

end do
end do

k = 0

do j = 1,n-1

do i = 1,n-1
do q2 = 1,n

z(i,q2) = 0

end do
end do

do i = 1,n-1

z(i,i) = 2*(1+lambda)

end do

do i = 1,n-2

z(i+1,i) = -lambda

end do

do i = 2,n-1

z(i-1,i) = -lambda

end do

q1 = 1

k = k + 1

do i = 2,n-2

q1 = q1 + 1

z(q1,n) = lambda*(ut1(i,j+1)) + 2*(1-lambda)*(ut1(i,j)) + lambda*(ut1(i,j-1))

z(1,n) = lambda*(ut1(1,j+1)) + 2*(1-lambda)*(ut1(1,j)) + lambda*(ut1(1,j-1)) + lambda*(ut1(0,j))

z(n-1,n) = lambda*(ut1(n-1,j+1)) + 2*(1-lambda)*(ut1(n-1,j)) + lambda*(ut1(n-1,j-1)) + lambda*(ut1(n,j))

end do

do i = 1,n-1
do q2 = 1,n

end do
end do

call gauss(z,n-1,t2)

q3 = 0

do i = 1,n-1

q3 = q3 + 1

ut1(i,k) = t2(q3)

end do

end do

do i = 1,n-1
do j = 1,n-1

ut(i,j) = ut1(i,j)

end do
end do

print*, '*********************'

print*, time

print*, '*********************'

do i = 1,n-1
do j = 1,n-1

print*, ut1(i,j)

end do
end do

end do

contains

subroutine gauss(a1,siz,x)
implicit none

real,dimension(:,:) :: a1
integer,intent(in) :: siz
real,dimension(:),intent(out) :: x
integer :: i,j,k
real :: g,f

do j = 1,siz
do i = 1,siz

if(i > j) then

g = a1(i,j)/a1(j,j)

do k = 1,siz+1

a1(i,k) = a1(i,k) - g*(a1(j,k))

end do

end if

end do
end do

x(siz) = a1(siz,siz+1)/a1(siz,siz)

do i = siz-1,1,-1

f = 0

do j = i+1,siz

f = f + a1(i,j)*x(j)

end do

x(i) = (a1(i,siz+1) - f)/a1(i,i)

end do

end subroutine

end program

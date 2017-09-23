program implicit
implicit none

real,allocatable,dimension(:) :: ut,ut1
real,allocatable,dimension(:,:) :: a
integer :: i,j,n
real :: lambda,g,f,t1,t2,t,ini,deltax,k,l

print*, 'Enter the value of delta x'
read*, deltax

print*, 'Enter the length of the rod'
read*, l

n = l/deltax

allocate(ut(n+2))
allocate(ut1(n+2))
allocate(a(n-1,n))

print*, 'Enter the temperature at initial point'
read*, ut(0)

print*, 'Enter the temperature at final point'
read*, ut(n)

print*, 'Enter the initial temperature of the rod'
read*, ini

print*, 'Enter the end time'
read*, t1

print*, 'Enter the time interval at which the temperature must be calculated'
read*, t2

print*, 'Enter the thermal conductivity'
read*, k

lambda = k*(t2/(deltax)**2)

do i = 1,n-1

ut(i) = ini

end do

do t = t2,t1,t2

do i = 1,n-1
do j = 1,n

a(i,j) = 0

end do
end do

do i = 1,n-1

a(i,i) = 1+(2*lambda)

end do

do i = 2,n-1

a(i,i-1) = -lambda

end do

do i = 1,n-2

a(i,i+1) = -lambda

end do

do i = 2,n-2

a(i,n) = ut(i)

end do

a(1,n) = ut(1) + lambda*(ut(0))

a(n-1,n) = ut(n-1) + lambda*(ut(n))

do j = 1,n-1
do i = 1,n-1

if(i > j) then

g = a(i,j)/a(j,j)

do k = 1,n

a(i,k) = a(i,k) - g*(a(j,k))

end do

end if

end do
end do

ut1(n-1) = a(n-1,n)/a(n-1,n-1)

do i = n-2,1,-1

f = 0

do j = i+1,n-1

f = f + a(i,j)*ut1(j)

end do

ut1(i) = (a(i,n) - f)/a(i,i)

end do

do i = 1,n-1

ut(i) = ut1(i)

end do

print*, '**********************'

print*, t

print*, '**********************'

do i = 0,n

print*, ut(i)

end do

end do

end program

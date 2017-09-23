program implicit
implicit none

real,allocatable,dimension(:) :: c,c1,sigma
real,allocatable,dimension(:,:) :: a
integer :: i,j,n,k1
real :: lambda,g,f,t1,t2,t,ini,deltax,d,l,lambdas

print*, 'Enter the value of delta x'
read*, deltax

print*, 'Enter the length of the rod'
read*, l

n = l/deltax

allocate(c(n+2))
allocate(c1(n+2))
allocate(sigma(n+2))
allocate(a(n-1,n))

print*, 'Enter the concentration at initial point'
read*, c(0)

print*, 'Enter the concentration at final point'
read*, c(n)

print*, 'Enter the initial concentration of the rod'
read*, ini

print*, 'Enter the end time'
read*, t1

print*, 'Enter the time interval at which the concentration must be calculated'
read*, t2

print*, 'Enter the thermal diffusivity'
read*, d

print*, 'Enter the value of lambda(stress)'
read*, lambdas

print*, 'Enter the stress values'

do i = 0,n

read*, sigma(i)

end do

lambda = d*(t2/(deltax)**2)

do i = 1,n-1

c(i) = ini

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

a(i,n) = c(i) + lambdas*(sigma(i+1) - 2*sigma(i) + sigma(i-1))

end do

a(1,n) = c(1) + lambda*(c(0)) + lambdas*(sigma(2) - 2*sigma(1) + sigma(0))

a(n-1,n) = c(n-1) + lambda*(c(n)) + lambdas*(sigma(n) - 2*sigma(n-1) + sigma(n-2))

do j = 1,n-1
do i = 1,n-1

if(i > j) then

g = a(i,j)/a(j,j)

do k1 = 1,n

a(i,k1) = a(i,k1) - g*(a(j,k1))

end do

end if

end do
end do

c1(n-1) = a(n-1,n)/a(n-1,n-1)

do i = n-2,1,-1

f = 0

do j = i+1,n-1

f = f + a(i,j)*c1(j)

end do

c1(i) = (a(i,n) - f)/a(i,i)

end do

do i = 1,n-1

c(i) = c1(i)

end do

print*, '**********************'

print*, t

print*, '**********************'

do i = 0,n

print*, c(i)

end do

end do

end program

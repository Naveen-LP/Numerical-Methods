program explicit
implicit none

real,allocatable,dimension(:) :: ut,ut1,x
integer :: i,n
real :: lambda,t,t1,t2,ini,deltax,l,k

print*, 'Enter the end time'
read*, t1

print*, 'Enter the interval at which you need the temperature calculated'
read*, t2

print*, 'Enter the value of thermal conductivity'
read*, k

print*, 'Enter the value of delta x'
read*, deltax

print*, 'Enter the length of the rod'
read*, l

n = l/deltax

allocate(ut(n+2))
allocate(ut1(n+2))

print*, 'Enter the initial temperature of the rod'
read*, ini

print*, 'Enter the temperature at initial point'
read*, ut(0)

print*, 'Enter the temperature at final point'
read*, ut(n)

lambda = k*(t2/(deltax)**2)

print*, lambda

do i = 1,n-1

ut(i) = ini

end do

do t = t2,t1,t2

do i = 1,n-1

ut1(i) = ut(i) + (lambda)*(ut(i+1) - 2*(ut(i)) + ut(i-1))

end do

do i = 1,n-1

ut(i) = ut1(i)

end do

print*, t

print*, '***************'

do i = 0,n

print*, ut(i)

end do

print*, '***************'

end do

end program

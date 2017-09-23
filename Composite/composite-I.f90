program implicit
implicit none

real,allocatable,dimension(:) :: ut,ut1,k,lambda,div,c,ut2,x
real,allocatable,dimension(:,:) :: a
integer :: n,number,q,ier,pgbeg
real :: g,f,t1,t2,t,ini,deltax,l,c1,c2,i,j

print*, 'Enter the no. of composite materials'
read*, number

print*, 'Enter the value of delta x'
read*, deltax

print*, 'Enter the length of the rod'
read*, l

n = l/deltax

allocate(k(n+1))
allocate(lambda(n+1))
allocate(div(number-1))
allocate(c(number-1))
allocate(ut(n+2))
allocate(ut1(n+2))
allocate(ut2(n+2))
allocate(x(n+2))
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

print*, 'Enter the junction points'

do i = 1,number-1
  read*, div(i)
end do

print*, 'Enter the conductivity of material 1'
read*, c1

k(0:div(1)) = c1

if(number > 2) then

print*, 'Enter the conductivities of material 2 to material', number - 1

do i = 1,number-2
read*, c(i)
end do

do i = 1,number-2
  k(div(i):div(i+1)) = c(i)
end do

else

print*, 'Enter the conductivity of material',number
read*, c2

k(div(number-1):l) = c2

do i = deltax,l-deltax,deltax

lambda(i) = k(i)*(t2/(deltax)**2)

end do

do i = 1,n-1

ut(i) = ini

end do

do t = t2,t1,t2

do i = 1,n-1
do j = 1,n

a(i,j) = 0

end do
end do

j = 0

do i = 1,n-1

j = j + deltax

a(i,i) = 1+(2*lambda(j))

end do

j = 0

do i = 2,n-1

j = j + deltax

a(i,i-1) = -lambda(j)

end do

j = deltax

do i = 1,n-2

j = j + deltax

a(i,i+1) = -lambda(j)

end do

do i = 2,n-2

a(i,n) = ut(i)

end do

a(1,n) = ut(1) + lambda(deltax)*(ut(0))

a(n-1,n) = ut(n-1) + lambda(l-deltax)*(ut(n))

do j = 1,n-1
do i = 1,n-1

if(i > j) then

g = a(i,j)/a(j,j)

do q = 1,n

a(i,q) = a(i,q) - g*(a(j,q))

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

end if

j = 0

do i = 0,n

x(i+1) = j

j = j + deltax

end do

do i = 0,n

ut2(i+1) = ut(i)

end do

print*, ut(99)
print*, ut(101)

ier = pgbeg(0,'?',1,1)

if (ier.NE.1) stop

call pgenv(0.,l,0.,100.,0,1)

call pglab('(Rod)', '(Temperature)', 'Temperature Profile of a rod')

call pgline(n+1,x,ut2)

end program

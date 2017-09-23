program explicit
implicit none

real,allocatable,dimension(:) :: ut,ut1,ut2,x,k,div,c,lambda
integer :: n,pgbeg,ier
real :: t,t1,t2,ini,deltax,l,number,i,j,c1,c2

print*, 'Enter the no. of composite materials'
read*, number

print*, 'Enter the end time'
read*, t1

print*, 'Enter the interval at which you need the temperature calculated'
read*, t2

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

print*, 'Enter the initial temperature of the rod'
read*, ini

print*, 'Enter the temperature at initial point'
read*, ut(0)

print*, 'Enter the temperature at final point'
read*, ut(n)

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

  j = 0

  do i = 1,n-1

  j = j + deltax

ut1(i) = ut(i) + (lambda(j))*(ut(i+1) - 2*(ut(i)) + ut(i-1))

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

j = 0

do i = 0,n

x(i+1) = j

j = j + deltax

end do

do i = 0,n

ut2(i+1) = ut(i)

end do

end if

ier = pgbeg(0,'?',1,1)

if (ier.NE.1) stop

call pgenv(0.,l,0.,100.,0,1)

call pglab('(x)', '(y)', 'A Simple Graph')

call pgpt(n,x,ut2,0)

call pgline(n+1,x,ut2)

end program

!=======================================
! Solves 2D steady state heat equation
! Temperatures fixed at all point
!=======================================


program laplace
implicit none

real,allocatable,dimension(:,:) :: un,uo
real,dimension(1000) :: g,errr
integer :: i,j,n,k,PGBEG,IER
real :: points,r,l,t,b,error

print*, 'Enter the number of points'
read*, points

n = sqrt(points)

allocate(un(n+2,n+2))
allocate(uo(n+2,n+2))

print*, 'Enter the temperature on the right face'
read*, r

print*, 'Enter the temperature on the left face'
read*, l

print*, 'Enter the temperature on the top face'
read*, t

print*, 'Enter the temperature on the bottom face'
read*, b

do i = 0,n

un(i,0) = b
un(i,n) = t
un(0,i) = l
un(n,i) = r
uo(i,0) = b
uo(i,n) = t
uo(0,i) = l
uo(n,i) = r

end do

do i = 1,n-1
do j = 1,n-1

uo(i,j) = 0

end do
end do

k = 0

iteration: do

k = k + 1

g(k) = k

do i = 1,n-1
do j = 1,n-1

un(i,j) = (uo(i+1,j) + uo(i-1,j) + uo(i,j+1) + uo(i,j-1))/4

end do
end do

!un(1:n-1,1:n-1) = (uo(2:n,1:n-1) + uo(0:n-2,1:n-1) + uo(1:n-1,2:n) + uo(1:n-1,0:n-2))/4

error = maxval(abs(un(1:n-1,1:n-1) - uo(1:n-1,1:n-1)))

errr(k) =  error

do i = 1,n-1
do j = 1,n-1

uo(i,j) = un(i,j)

end do
end do

if (error < 0.0000001)  exit iteration

end do iteration

do i = 0,n
  do j = 0,n
    print*,uo(i,j)
  end do
end do

open(unit = 22, file = 'Iteration.txt')

do i = 1,k

write(22,*) g(i)

end do

open(unit = 20, file = 'Error.txt')

do i = 1,k

write(20,*) errr(i)

end do

open(unit = 23, file = 'Temperature.txt')

do i = 0,n
do j = 0,n

write(23,*) uo(i,j)

end do
end do

IER = PGBEG(0,'?',1,1)

if (IER.NE.1) stop

call PGENV(0.,400.,0.,45.,0,1)
call PGLAB('(x)', '(y)', 'A Simple Graph')
call PGLINE(k,g,errr)
call PGEND

end program

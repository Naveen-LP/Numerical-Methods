!=======================================
! Solves 2D steady state heat equation 
! Temperatures fixed at 3 points, 1 point insulated
!=======================================

program laplace
implicit none

real,allocatable,dimension(:,:) :: un,uo
integer :: i,j,n,k
real :: points,r,l,t,b,error

print*, 'Enter the number of points'
read*, points

n = sqrt(points)

allocate(un(n+2,-1:n+2))
allocate(uo(n+2,-1:n+2))

print*, 'Enter the temperature on the right face'
read*, r

print*, 'Enter the temperature on the left face'
read*, l

print*, 'Enter the temperature on the top face'
read*, t

do i = 0,n

un(i,n) = t
un(0,i) = l
un(n,i) = r

uo(i,n) = t
uo(0,i) = l
uo(n,i) = r

end do

do i = 1,n-1
do j = 1,n-1

uo(i,j) = 0

end do
end do

do i = 1,n-1

uo(i,-1) = 0

end do

k = 0

iteration: do

k = k + 1

do i = 1,n-1
do j = 0,n-1

un(i,j) = (uo(i+1,j) + uo(i-1,j) + uo(i,j+1) + uo(i,j-1))/4

end do 
end do

do i = 1,n-1

uo(i,-1) = un(i,1)

end do

!un(1:n-1,1:n-1) = (uo(2:n,1:n-1) + uo(0:n-2,1:n-1) + uo(1:n-1,2:n) + uo(1:n-1,0:n-2))/4

error = maxval(abs(un(1:n-1,0:n-1) - uo(1:n-1,0:n-1)))

print*, error

do i = 1,n-1
do j = 0,n-1

uo(i,j) = un(i,j)

end do
end do

if (error < 0.0000000000000001)  exit iteration

end do iteration

print*, '********************'

do i = 0,n
do j = 0,n

print*, uo(i,j)

end do
end do

print*, k

end program






















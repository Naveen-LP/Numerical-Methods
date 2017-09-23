!=======================================
! Solves 2D steady state heat equation
! Temperatures fixed at all point
!=======================================


program laplace
implicit none

real,allocatable,dimension(:,:) :: un,uo,flux_x,flux_y,flux_mag,theta
real,dimension(1000) :: g,errr
integer :: i,j,n,PGBEG,IER,k
real :: points,r,l,t,b,error,length,dx,k1,pi = 3.14159265359

print*, 'Enter the number of points'
read*, points

print*, 'Enter the length of the length'
read*, length

n = sqrt(points)

dx = length/n

allocate(un(n+2,n+2))
allocate(uo(n+2,n+2))
allocate(flux_x(n+2,n+2))
allocate(flux_y(n+2,n+2))
allocate(flux_mag(n+2,n+2))
allocate(theta(n+2,n+2))

print*, 'Enter the temperature on the right face'
read*, r

print*, 'Enter the temperature on the left face'
read*, l

print*, 'Enter the temperature on the top face'
read*, t

print*, 'Enter the temperature on the bottom face'
read*, b

print*, 'Enter the thermal conductivity'
read*, k1

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

do i = 1,n-1
  do j = 1,n-1

flux_x(i,j) = -k1*(uo(i+1,j) - uo(i-1,j))/(2*dx)

flux_y(i,j) = -k1*(uo(i,j+1) - uo(i,j-1))/(2*dx)

end do
end do

do i = 1,n-1
  do j = 1,n-1
      theta(i,j) = atan(flux_y(i,j)/flux_x(i,j))*(180/pi)
    end do
end do

do i = 1,n-1
  do j = 1,n-1

print*, uo(i,j)

end do
end do

print*, '********************************'
print*, 'Magnitude of Flux in x direction'
print*, '********************************'

do i = 1,n-1
do j = 1,n-1

print*, flux_x(i,j)

end do
end do

print*, '********************************'
print*, 'Magnitude of Flux in y direction'
print*, '********************************'

do i = 1,n-1
do j = 1,n-1

print*, flux_y(i,j)

end do
end do

do i = 1,n-1
  do j = 1,n-1

    flux_mag(i,j) = sqrt((flux_x(i,j))**2 + (flux_y(i,j))**2)

end do
end do

print*, '*****************'
print*, 'Magnitude of Flux'
print*, '*****************'

do i = 1,n-1
  do j = 1,n-1
    print*, flux_mag(i,j)
  end do
end do

print*, '******************'
print*, 'Direction of Flux'
print*, '******************'

do i = 1,n-1
  do j = 1,n-1
    print*, theta(i,j)
  end do
end do

end program

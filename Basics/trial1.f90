program trial
implicit none

real,allocatable,dimension(:,:) :: a
integer,allocatable,dimension(:,:) :: u
integer :: n,i,j,k,g

read*, n

g = (n-1)**2

allocate(a(n,g))
allocate(u(n,n))

do i = 1,n
do j = 1,n

read*, a(i,j)

end do
end do

do i = 1,n
do j = 1,n

read*, u(i,j)

end do
end do

k = 0

do i = 1,n-1
do j = 1,n-1

k = k + 1

print*, k

end do
end do

end program



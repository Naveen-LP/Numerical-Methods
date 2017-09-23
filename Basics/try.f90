program test_ptrs
implicit none

integer :: i,j,n,l,k
real, dimension (:,:),allocatable :: a
real, dimension (:),allocatable :: x

read*, n

l = n**2

allocate(x(l))
allocate(a(n,n))

do i = 1,l

read*, x(i)

end do

k = 0

do i = 1,n
do j = 1,n

k = k + 1

a(i,j) = x(k) 

end do
end do

do i = 1,n
do j = 1,n

print*, a(i,j)

end do
end do

end program test_ptrs



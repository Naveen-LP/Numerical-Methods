!Converting a 2D array to 1D

program e
implicit none

real,allocatable,dimension(:) :: a
real,allocatable,dimension(:,:) :: u

integer :: i,j,k,n

read*, n

allocate(u(n,n))
allocate(a(n))

do i = 1,n
do j = 1,n

read*, u(i,j)

end do
end do

do i = 1,n
do k = 1,n

a(k) = u(i,:)

end do 
end do

end program





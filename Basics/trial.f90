program trial
implicit none

real,dimension(:,:),pointer,contiguous :: a
real,dimension(:),pointer :: b,c
integer :: i,j

allocate(a(3,3))

do i = 1,3
do j = 1,3

read*, a(i,j)

end do
end do

b(1:9) => a

a(1:3,1:3) => b

do i = 1,3
do j = 1,3

print*, a(i,j)

end do
end do

end program



!Writing a data to a .dat file

program plot
implicit none

real,dimension(:),allocatable :: x,y
integer :: n,i

allocate(x(n+1),y(n+1))

do i = 1,n

read*, x(i)

end do

do i = 1,n

read*, y(i)

end do

open(unit = 1, file = 'data.dat')

do i = 1,n

write(1,*) x(i), ' ' , y(i)

end do

end program

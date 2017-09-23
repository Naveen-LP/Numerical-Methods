!Directly plotting from fortran

program plot
implicit none

real,dimension(11) :: x,y
integer :: ier,pgbeg,i

do i = 0,10

x(i+1) = i

y(i+1) = x(i+1)**2

end do

ier = pgbeg(0,'?',1,1)
if (ier.NE.1) stop
call pgenv(0.,10.,0.,100.,0,1)
call pgsci(8)
call pgsls(4)
call pgslw(5)
call pgline(11,x,y)
call pglab('(x)','(y)','Y = X^2')
call pgpt(11,x,y,3)


end program

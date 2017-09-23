program fft
implicit none

integer :: nnn,i
real,dimension(:),allocatable :: f
real :: k,h

read*, nnn

allocate(f(2*nnn))

do i = 1,2*nnn
read*, f(i)
end do

call four1(f,nnn,1)

do i = 1,2*nnn

print*, f(i)/nnn 

end do

end program

SUBROUTINE four1(data,nn,isign)
INTEGER isign,nn
REAL data(2*nn)
INTEGER i,istep,j,m,mmax,n
REAL tempi,tempr,theta,wi,wpi,wpr,wr,wtemp

n=2*nn
j=1
do i=1,n,2
if(j.gt.i)then
tempr=data(j)
tempi=data(j+1)
data(j)=data(i)
data(j+1)=data(i+1)
data(i)=tempr
data(i+1)=tempi
endif
m=n/2
1 if ((m.ge.2).and.(j.gt.m)) then
j=j-m
m=m/2
goto 1
endif
j=j+m
end do

mmax=2
2 if (n.gt.mmax) then
istep=2*mmax
theta=6.2831/(isign*mmax)
wpr=-2*sin(0.5*theta)**2
wpi=sin(theta)
wr=1
wi=0
do m=1,mmax,2

do i=m,n,istep
j=i+mmax
tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
data(j)=data(i)-tempr
data(j+1)=data(i+1)-tempi
data(i)=data(i)+tempr
data(i+1)=data(i+1)+tempi
end do
wtemp=wr
wr=wr*wpr-wi*wpi+wr
wi=wi*wpr+wtemp*wpi+wi
end do
mmax=istep
goto 2
endif

return
end subroutine four1

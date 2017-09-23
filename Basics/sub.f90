!Basic code with a subroutine

program summation

implicit none

Integer :: x,y,z,ans,t

read*, x,y,z
 
call sum1(x,y,z,ans)

t = sum1(x+y+z)

print *, ans 
end program
 
subroutine sum1(a,b,c,result1)

implicit none

real :: a,b,c,result1

result1 = a+b+c

end subroutine sum1


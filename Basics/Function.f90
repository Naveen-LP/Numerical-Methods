!Simple program to show how a function can be used in fortran

program func
  implicit none

  real,dimension(100) :: a1,b1,c1
  integer i

  do i = 1,100

    a1(i) = i
    b1(i) = i

  end do

  do i = 1,100

    c1(i) = f(a1(i),b1(i))

  end do

  print*, c1(1:100)

contains

  real function f(a,b)

    implicit none

    real,intent(in) :: a,b

    f = a+b

  end function f

end program

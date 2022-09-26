program main
use gauss
implicit none
integer(1) :: i
real(dp) :: xmin,xmax,total,xi(n),wi(n)
xmin = 1d0
xmax = 200d0



! Gauss-Legendre
call gleg(xmin,xmax,xi,wi)
total = 0d0
do i = 1,n
  total = total + wi(i) * func(xi(i))
enddo
write(*,*) "Gauss-Legendre    :",total



! Gauss-Chebyshev (first kind)
call gche1(xmin,xmax,xi,wi)
total = 0d0
do i = 1,n
  total = total + wi(i) * func(xi(i))
enddo
write(*,*) "Gauss-Chebyshev(1):",total



! Gauss-Chebyshev (second kind)
call gche2(xmin,xmax,xi,wi)
total = 0d0
do i = 1,n
  total = total + wi(i) * func(xi(i))
enddo
write(*,*) "Gauss-Chebyshev(2):",total



! Gauss-Laguerre
call glag(xi,wi)
total = 0d0
do i = 1,n
  if(xi(i).ge.xmin .and. xi(i).le.xmax) then
    total = total + wi(i) * func(xi(i))
  endif
enddo
write(*,*) "Gauss-Laguerre    :",total



! Exact result
!write(*,*) "Exact             :", bessel_j0(xmin) - bessel_j0(xmax)
write(*,*) "Exact             :", bessel_y0(xmin) - bessel_y0(xmax)



contains
!integrand function
function func(x) result(fx)
real(dp), intent(in) :: x
real(dp) :: fx
!fx = bessel_j1(x)
fx = bessel_y1(x)
end function func
end program main


      program main

      use vint
      IMPLICIT NONE
      double precision, dimension(6) :: region
      double precision :: tgral,sd,chi2a
      double precision :: func
      INTEGER :: init,ncall,itmx,nprn
      external :: func
      real :: t1,t2
      double precision :: rad,pi=4d0*atan(1d0)
      common /radius/ rad

      call cpu_time(t1)

      rad = 3d0

      init = 0
      ncall = 1e6
      itmx = 5
      nprn = 0
      region(1) = -rad  ! dx
      region(2) = -rad  ! dy
      region(3) = -rad  ! dz
      region(4) = +rad
      region(5) = +rad
      region(6) = +rad

      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      write(*,*) tgral, sd

      ! correct answer: 4/3 * PI * R^3
      write(*,*) 4d0/3d0 * pi * rad**3

      call cpu_time(t2)
      write(*,*) 'time elapsed:',t2-t1,'seconds.'

      end program main


      function func(dx,wgt)

      IMPLICIT NONE
      double precision, DIMENSION(:), intent(in) :: dx
      double precision, intent(in) :: wgt
      double precision :: func
      double precision :: x,y,z
      double precision :: rad
      common /radius/ rad

      x = dx(1)
      y = dx(2)
      z = dx(3)

      func = 0d0
      if(x*x+y*y+z*z .le. rad*rad) func = 1d0

      return
      end function func

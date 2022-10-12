      program main
      use vint
      implicit none
      double precision, dimension(8) :: region
      double precision :: tgral,sd,chi2a
      double precision :: func
      integer :: init,ncall,itmx,nprn
      external :: func
      real :: t1,t2
      double precision :: jmin,ymin,smin,cmin
      double precision :: jmax,ymax,smax,cmax
      double precision, parameter :: PI = 4d0*atan(1d0)
      double precision :: ans

      jmin = 0d0    ;   jmax = 50d0
      ymin = 1d0    ;   ymax = 50d0
      smin = 0d0    ;   smax = PI
      cmin = 0d0    ;   cmax = PI/2d0

      ans = (bessel_j0(jmin) - bessel_j0(jmax)) *
     &      (bessel_y0(ymin) - bessel_y0(ymax)) *
     &      (cos(smin) - cos(smax)) *
     &      (sin(cmin) - sin(cmax)) * (-1)

      nprn = -1
      region(1) = jmin;   region(5) = jmax
      region(2) = ymin;   region(6) = ymax
      region(3) = smin;   region(7) = smax
      region(4) = cmin;   region(8) = cmax

      write(*,*) "VEGAS adaptive Monte-Carlo test"
      write(*,*) "using oscillatory test functions"
      write(*,*) "ans:",ans,new_line('a')

      ! plain integration
      write(*,*) "plain:"
      call cpu_time(t1)
      ncall = 2e8;  itmx = 1;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      ! iterative integration
      write(*,*) "iterative:"
      call cpu_time(t1)
      ncall = 2e7;  itmx = 10;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      ! adaptive integration
      write(*,*) "adaptive:"
      call cpu_time(t1)
      ncall = 1e4;  itmx = 10;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      ncall = 2e8;  itmx = 1;   init = +1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      end program main


      function func(dx,wgt)
      implicit none
      double precision, dimension(:), intent(in) :: dx
      double precision, intent(in) :: wgt
      double precision :: func
      double precision :: j,y,s,c
      j = dx(1)
      y = dx(2)
      s = dx(3)
      c = dx(4)
      func = bessel_j1(j) * bessel_y1(y) * sin(s) * cos(c)
      return
      end function func
